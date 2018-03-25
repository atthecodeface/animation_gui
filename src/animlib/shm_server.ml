let shm_lock_filename = "/tmp/animation_shm.lock"
let shm_lock_key = 12345
let max_clients = 4

type t_ba8 = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
module Server = 
struct
    type t = {
    shm     : Shm_ipc.Shm.t_shm;
    data    : Shm_ipc.Shm.t_shm_data;
    data_ba : t_ba8;
    server  : Shm_ipc.Ipc.t_s;
    mutable alive : bool;
      }

  let create _ =
    let (shm, data, server) = Shm_ipc.create_server "AnimationSever" max_clients shm_lock_filename shm_lock_key 65536L in
    let data_ba = Shm_ipc.Shm.data_ba data in
    { shm; data; data_ba; server; alive=true }

  let is_alive t = t.alive

  let msg_alloc t size = 
    let msg = Shm_ipc.Ipc.Server.msg_alloc t.server size in
    let msg_ba = Shm_ipc.Ipc.msg_ba msg in
    (msg, msg_ba)

  let msg_free t msg = 
    Shm_ipc.Ipc.Server.msg_free t.server msg

  let poll t msg_callback timeout =
    let timeout_callback _ = Some () in
    (
      match (Shm_ipc.server_thread_poll t.server timeout timeout_callback msg_callback ()) with
      | None -> t.alive <- false
      | _ -> ()
    );
    t.alive

end

module Client = 
struct
    type t = {
    shm     : Shm_ipc.Shm.t_shm;
    data    : Shm_ipc.Shm.t_shm_data;
    data_ba : t_ba8;
    client  : Shm_ipc.Ipc.t_c;
    mutable alive : bool;
      }

  let create _ =
    let (shm, data, client) = Shm_ipc.create_client shm_lock_filename shm_lock_key 65536L in
    let data_ba = Shm_ipc.Shm.data_ba data in
    { shm; data; data_ba; client; alive=true }

  let is_alive t = t.alive

  let msg_alloc t size = 
    let msg = Shm_ipc.Ipc.Client.msg_alloc t.client size in
    let msg_ba = Shm_ipc.Ipc.msg_ba msg in
    (msg, msg_ba)

  let msg_free t msg = 
    Shm_ipc.Ipc.Client.msg_free t.client msg

  let send t msg = Shm_ipc.Ipc.Client.send t.client msg

  let poll t msg_callback timeout =
    let timeout_callback _ = Some () in
    (
      match (Shm_ipc.client_thread_poll t.client timeout timeout_callback msg_callback ()) with
      | None -> t.alive <- false
      | _ -> ()
    );
    t.alive

end

