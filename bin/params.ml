let debug = ref false

let initial_connect_timeout_s = 10.0

let msg_timeout_s = 5.0

let client_default_dest_host = "127.0.0.1"

let client_default_dest_port = 5000

let server_default_listen_host = "127.0.0.1"

let server_default_listen_port = client_default_dest_port

let server_conn_queue = 10

let concurrent_client_count = 10
