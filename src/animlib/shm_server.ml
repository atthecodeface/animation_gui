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

  let create shm_size =
    let (shm, data, server) = Shm_ipc.create_server "AnimationSever" max_clients shm_lock_filename shm_lock_key shm_size in
    let data_ba = Shm_ipc.Shm.data_ba data in
    { shm; data; data_ba; server; alive=true }

  let is_alive t = t.alive

  let msg_alloc t size = 
    let msg = Shm_ipc.Ipc.Server.msg_alloc t.server size in
    let msg_ba = Shm_ipc.Ipc.msg_ba msg in
    (msg, msg_ba)

  let msg_free t msg = 
    Shm_ipc.Ipc.Server.msg_free t.server msg

  let send t client msg =
    Shm_ipc.Ipc.Server.send t.server client msg

  let poll t msg_callback timeout =
    let timeout_callback _ = Some () in
    let msg_cb client msg =
      let msg_ba = Shm_ipc.Ipc.msg_ba msg in
      let msg_id = Shm_ipc.Ba.retype_sub Bigarray.int32 Bigarray.c_layout msg_ba 0 4 in
      let id = Int32.to_int msg_id.{0} in
      let len = Bigarray.Array1.dim msg_ba in
      let msg_ba = Bigarray.Array1.sub msg_ba 4 (len-4) in
      msg_callback client id msg msg_ba
    in
    (
      match (Shm_ipc.server_thread_poll t.server timeout timeout_callback msg_cb ()) with
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
    mutable next_msg_id: int;
      }

  let create _ =
    let (shm, data, client) = Shm_ipc.create_client shm_lock_filename shm_lock_key 65536L in
    let data_ba = Shm_ipc.Shm.data_ba data in
    { shm; data; data_ba; client; alive=true; next_msg_id=0; }

  let is_alive t = t.alive

  let msg_alloc t size = 
    let id = t.next_msg_id in
    t.next_msg_id <- t.next_msg_id + 1;
    let msg = Shm_ipc.Ipc.Client.msg_alloc t.client (size+4) in
    let msg_ba = Shm_ipc.Ipc.msg_ba msg in
    let msg_id = Shm_ipc.Ba.retype_sub Bigarray.int32 Bigarray.c_layout msg_ba 0 4 in
    msg_id.{0} <- Int32.of_int id;
    let msg_ba = Bigarray.Array1.sub msg_ba 4 size in
    (msg, msg_ba, id)

  let msg_free t msg = 
    Shm_ipc.Ipc.Client.msg_free t.client msg

  let send t msg = Shm_ipc.Ipc.Client.send t.client msg

  let poll t msg_callback timeout =
    let timeout_callback _ = Some () in
    let msg_cb msg =
      let msg_ba = Shm_ipc.Ipc.msg_ba msg in
      let msg_id = Shm_ipc.Ba.retype_sub Bigarray.int32 Bigarray.c_layout msg_ba 0 4 in
      let id = Int32.to_int msg_id.{0} in
      let len = Bigarray.Array1.dim msg_ba in
      let msg_ba = Bigarray.Array1.sub msg_ba 4 (len-4) in
      msg_callback id msg msg_ba
    in
    (
      match (Shm_ipc.client_thread_poll t.client timeout timeout_callback msg_cb ()) with
      | None -> t.alive <- false
      | _ -> ()
    );
    t.alive

end

