module A = Animlib.Animation

let sfmt = Printf.sprintf

let ba_as_floats ba ofs len = Shm_ipc.Ba.retype_sub Bigarray.float32        Bigarray.c_layout ba ofs len
let ba_as_int16s ba ofs len = Shm_ipc.Ba.retype_sub Bigarray.int16_unsigned Bigarray.c_layout ba ofs len

module Ac = Animlib.BasicClient

let create_model_msg model_id coords indices element_type num_elements =
  let num_floats  = Array.length coords in
  let num_indices = Array.length indices in
  let indices_start = 128 in
  let floats_start = indices_start + (2*num_indices) in
  let floats_end   = floats_start  + (4*num_floats) in
  let ba = Bigarray.(Array1.create char c_layout floats_end) in
  let okay = Ac.rpc_model_create_msg ba indices_start model_id Int32.([|of_int floats_start;of_int num_floats;of_int indices_start; of_int num_indices; of_int element_type; of_int num_elements |]) in
  if okay then (
    let floats_ba  = ba_as_floats ba floats_start (floats_end - floats_start) in
    let indices_ba = ba_as_int16s ba indices_start (floats_start - indices_start) in
    Array.iteri (fun i f -> floats_ba.{i} <- f) coords;
    Array.iteri (fun i n -> indices_ba.{i} <- n) indices;
    Some ba
  ) else (
    None
  )


let cube_id = 23
let cube_coords = [|  1.; 1.;-1.;  1.; 1.;-1.; 1.;0.;0.; 0.;0.;
                      1.;-1.;-1.;  1.;-1.;-1.; 1.;0.;0.; 0.;0.;
                     -1.;-1.;-1.; -1.;-1.;-1.; 1.;0.;0.; 0.;0.;
                     -1.; 1.;-1.; -1.; 1.;-1.; 1.;0.;0.; 0.;0.;
                      1.; 1.; 1.;  1.; 1.; 1.; 1.;0.;0.; 0.;0.;
                      1.;-1.; 1.;  1.;-1.; 1.; 1.;0.;0.; 0.;0.;
                     -1.;-1.; 1.; -1.;-1.; 1.; 1.;0.;0.; 0.;0.;
                     -1.; 1.; 1.; -1.; 1.; 1.; 1.;0.;0.; 0.;0.;
                  |]
let cube_indices = [|6;7;2;3;0;7;4;6;5;2;1;0;5;4|]
let cube_element_type = 0
let cube_num_elements = Array.length cube_indices

let cube_msg =
  match create_model_msg cube_id cube_coords cube_indices cube_element_type cube_num_elements with
  | None -> (Printf.printf "Failed to create cube\n"; raise Not_found)
  | Some x -> x


(*Assoc (map , [("place", (ArrayFloat32 [||])), ("orient", ]]
parser_of_key k = Shm_ipc.Mbf.([| RepFloat parse_place ; RepFloat parse_orient |]).(k)
 *)
let send_and_wait client msg delay =  
  ignore (Animlib.Shm_server.Client.send client msg);
  let msg_callback msg =
    Printf.printf "Got a message\n"; (Some ())
    in
  ignore (Animlib.Shm_server.Client.poll client msg_callback delay);
  ()

let move_to client msg msg_ba time delay id x y z =
  ignore (Ac.rpc_object_set_target_msg msg_ba 1024 0 time 1 [|x;y;z;1.;|]);
  send_and_wait client msg delay

let animate client msg msg_ba why time =
  ignore (Ac.rpc_animate_msg msg_ba 1024 why time);
  send_and_wait client msg 10000

let _ =
  let client = Animlib.Shm_server.Client.create () in
  let (msg,msg_ba) = Animlib.Shm_server.Client.msg_alloc client 1024 in

  ignore (Ac.rpc_reset_msg msg_ba 1024);
  send_and_wait client msg 10000;

  let (msg,msg_ba) = Animlib.Shm_server.Client.msg_alloc client 1024 in
  let l = Bigarray.Array1.dim cube_msg in
  Bigarray.Array1.(blit cube_msg (sub msg_ba 0 l));
  send_and_wait client msg 10000;

  let (msg,msg_ba) = Animlib.Shm_server.Client.msg_alloc client 1024 in
  ignore (Ac.rpc_object_create_msg msg_ba 1024 0 cube_id);
  send_and_wait client msg 10000;

  let (msg,msg_ba) = Animlib.Shm_server.Client.msg_alloc client 1024 in
  animate client msg msg_ba 2 0.012;

  let (msg,msg_ba) = Animlib.Shm_server.Client.msg_alloc client 1024 in
  move_to client msg msg_ba (-1.) (10000) 0 0. 0. 0.;

  for i=0 to 100 do
    let angle = 0.5 *. (float i) in
    let x = 0.5 *. ( sin angle) in
    let y = 0.5 *. ( cos angle) in
    let z = 0.3 *. ( sin (1.5*.(angle +. 0.3))) in
    let time = (float i)*.1.0 in
    let (msg,msg_ba) = Animlib.Shm_server.Client.msg_alloc client 1024 in
    animate client msg msg_ba 1 (time -. 1.0);
    let (msg,msg_ba) = Animlib.Shm_server.Client.msg_alloc client 1024 in
    move_to client msg msg_ba time (1000*1000) 0 x y z;
  done;
  ()

(*  let a = Ac.create () in
  let ba = Bigarray.(Array1.create char c_layout 1024) in
  let okay = Ac.rpc_model_create_msg ba 1024 0 [|320l;8l;256l;4l;0l;4l|] in
  Printf.printf "Create message okay %b\n" okay;
  Ac.parse_shm_msg a ba 0 1024;
  let okay = Ac.rpc_object_create_msg ba 1024 0 0 in
  Ac.parse_shm_msg a ba 0 1024;
  Ac.iter_models a (fun i m -> Printf.printf "Model %d '%s'\n" i m);
  Ac.iter_objects a (fun i m -> Printf.printf "Object %d '%s'\n" i m);
  let okay = Ac.rpc_reset_msg ba 1024 in
  Printf.printf "Reset message okay %b\n" okay;
  Ac.parse_shm_msg a ba 0 1024;
  Ac.iter_models a (fun i m -> Printf.printf "Model %d '%s'\n" i m);
  Ac.iter_objects a (fun i m -> Printf.printf "Object %d '%s'\n" i m);
  Ac.parse_shm_msg a cube_msg 0 1024;
  Ac.iter_models a (fun i m -> Printf.printf "Model %d '%s'\n" i m);
  Ac.iter_objects a (fun i m -> Printf.printf "Object %d '%s'\n" i m);
  ()

 *)
