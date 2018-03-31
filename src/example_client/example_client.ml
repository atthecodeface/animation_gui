module A = Animlib.Animation

let sfmt = Printf.sprintf

let ba_float_array len = Bigarray.(Array1.create float32 c_layout len)
let ba_uint8_array  len = Bigarray.(Array1.create int8_unsigned c_layout len)
let ba_uint16_array  len = Bigarray.(Array1.create int16_unsigned c_layout len)
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
let cube_coords = [|  1.; 1.;-1.;  1.; 1.;-1.; 1.;0.;0.; 1.;1.;
                      1.;-1.;-1.;  1.;-1.;-1.; 1.;0.;0.; 1.;0.;
                     -1.;-1.;-1.; -1.;-1.;-1.; 1.;0.;0.; 0.;0.;
                     -1.; 1.;-1.; -1.; 1.;-1.; 1.;0.;0.; 0.;1.;
                      1.; 1.; 1.;  1.; 1.; 1.; 1.;0.;0.; 0.;0.;
                      1.;-1.; 1.;  1.;-1.; 1.; 1.;0.;0.; 0.;1.;
                     -1.;-1.; 1.; -1.;-1.; 1.; 1.;0.;0.; 1.;1.;
                     -1.; 1.; 1.; -1.; 1.; 1.; 1.;0.;0.; 1.;0.;
                  |]
let cube_indices = [|6;7;2;3;0;7;4;6;5;2;1;0;5;4|]
let cube_element_type = 0
let cube_num_elements = Array.length cube_indices

let cube_msg =
  match create_model_msg cube_id cube_coords cube_indices cube_element_type cube_num_elements with
  | None -> (Printf.printf "Failed to create cube\n"; raise Not_found)
  | Some x -> x


let _ =
  let client = Ac.create () in

  Ac.reset client;

  let image = Animlib.Image.create_from_image_file "sample.png" in
  let ofs = 128 in
  let ba_size = Animlib.Image.ba_size image in
  let width  = Animlib.Image.width image in
  let height = Animlib.Image.height image in
  let (msg,msg_ba,msg_id) = Ac.msg_alloc client (ba_size+ofs) in
  ignore (Ac.rpc_texture_create_msg msg_ba ofs 17 width height 0 ofs);
  let ba = Shm_ipc.Ba.retype_sub Bigarray.int8_unsigned Bigarray.c_layout msg_ba ofs ba_size in
  Animlib.Image.blit_ba image ba;
  Ac.send_and_wait client msg msg_id;

  Ac.send_msg_from_ba client cube_msg;

  Ac.create_object client 0 cube_id;

  let playback_speed = 2.0 in
  let ticks_per_second = (1000.*.1000./.playback_speed) in
  Ac.animate client 2 (0.01*. playback_speed);

  Ac.set_material client (-1.) 0 17;

  Ac.move_to client (-1.) 0 0. 0. 0.;

  Ac.animate client 1 (-1.0);         (* Run setting time to (i-1*1) *)

  let rec move i =
    let angle = 0.5 *. (float i) in
    let x = 0.5 *. ( sin angle) in
    let y = 0.5 *. ( cos angle) in
    let z = 0.3 *. ( sin (1.5*.(angle +. 0.3))) in
    let time = (float i)*.1.0 in
    Ac.move_to client time 0 x y z; (* Animate to i *)
    if (Ac.wait_for_time ~ticks_per_second:ticks_per_second client (time-.0.01)) then (
      move (i+1)
    ) else (
      ()
    )
  in
  move 0


