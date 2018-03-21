module Shm=Shm_ipc.Shm
let shm_lock_filename = "/tmp/animation_shm.lock"
let shm_lock_key = 12345
let max_clients = 4

type t_ba8 = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
module Server = 
struct
    type t = {
    shm     : Shm.t_shm;
    data    : Shm.t_shm_data;
    data_ba : t_ba8;
    server  : Shm.t_shm_ipc_s;
    mutable alive : bool;
      }

  let create _ =
    let shm     = Shm.init () in
    let data    = Shm.data_alloc shm ~create:1 shm_lock_filename shm_lock_key 65536L in
    let data_ba = Shm.data_ba data in
    let server  = Shm.server_create data_ba "AnimationServer" max_clients in
    { shm; data; data_ba; server; alive=true }

  let is_alive t = t.alive

  let poll t msg_callback timeout =
    if t.alive then (
      let quit = (
          let (rc,client,msg) = Shm.server_poll t.server timeout in
          match rc with 
          | Shm.Timeout -> false
          | Shm.Message -> msg_callback client msg
          | _ -> true
        ) in
      if quit then t.alive <- false
    );
    t.alive

  let idle t = ()


  (* reset : unit -> result
  model_create : model_name -> coords -> indices -> triangles/lines/patches -> result
  object_create : object -> model -> result
  object_place  : object -> position -> quaternion -> scale -> result (?time?)
  get_time  : unit -> time result
  set_time  : time -> result
   *)
end

