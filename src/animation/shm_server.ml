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

  let poll t msg_callback timeout =
    let timeout_callback _ = Some () in
    (
      match (Shm_ipc.server_thread_poll t.server timeout timeout_callback msg_callback ()) with
      | None -> t.alive <- false
      | _ -> ()
    );
    t.alive

  let idle t = ()

end

