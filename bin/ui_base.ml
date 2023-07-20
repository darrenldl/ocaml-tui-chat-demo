open Lwd_infix

module Vars = struct
  let term : Notty_unix.Term.t option ref = ref None
end

let term () =
  Option.get !Vars.term

module Log_pane = struct
  let main (logs : Notty.image list Lwd.var) : Nottui.ui Lwd.t =
    let$ l = Lwd.get logs in
    l
    |> List.rev
    |> List.map Nottui.Ui.atom
    |> Nottui.Ui.vcat
end

module Input_prompt = struct
  let main
      ~prompt_label
      ~focus_handle
      ~f
    : Nottui.ui Lwd.t =
    let edit_field = Lwd.var ("", 0) in
    Nottui_widgets.hbox
      [
        (Notty.I.strf "%s: " prompt_label |> Nottui.Ui.atom |> Lwd.return);
        Nottui_widgets.edit_field (Lwd.get edit_field)
          ~focus:focus_handle
          ~on_change:(fun (text, x) -> Lwd.set edit_field (text, x))
          ~on_submit:(fun _ ->
              let s = fst @@ Lwd.peek edit_field in
              if s <> "" then (
                f s;
                Lwd.set edit_field ("", 0)
              )
            );
      ]
end

let term () =
  Option.get !Vars.term

module Chat_and_log_pane = struct
  let main ~logs ~prompt_label ~on_receive_input : Nottui.ui Lwd.t =
    let (_term_width, term_height) = Notty_unix.Term.size (term ()) in
    let log_pane =
      let$ pane = Log_pane.main logs in
      Nottui.Ui.resize
        ~h:(term_height - 1)
        ~crop:(Nottui.Gravity.make ~h:`Negative ~v:`Positive)
        pane
    in
    let input_field_focus_handle = Nottui.Focus.make () in
    Nottui_widgets.vbox
      [
        log_pane;
        Input_prompt.main
          ~prompt_label
          ~focus_handle:input_field_focus_handle
          ~f:on_receive_input;
      ]

end

let ui_loop ~quit root =
  let renderer = Nottui.Renderer.make () in
  let term = term () in
  let root =
    let$ root = root in
    root
    |> Nottui.Ui.event_filter (fun x ->
        match x with
        | `Key (`Escape, []) -> (
            Lwd.set quit true;
            `Handled
          )
        | _ -> `Unhandled
      )
  in
  let rec loop () =
    if not (Lwd.peek quit) then (
      Nottui.Ui_loop.step
        ~process_event:true
        ~timeout:0.01
        ~renderer
        term
        (Lwd.observe @@ root);
      Eio.Fiber.yield ();
      loop ()
    )
  in
  loop ();
  Notty_unix.Term.release term
