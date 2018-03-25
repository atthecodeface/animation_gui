module A = Animlib.Animation

let sfmt = Printf.sprintf

module Client =
struct
    type t_model   = string
    type t_object  = string
    type t_texture = string
    let create_model    s cs is es   = Some (sfmt "model %d"   s)
    let create_texture  s t w h data = Some (sfmt "texture %d" s)
    let create_object   s m          = Some (sfmt "object %d"  s)
    let object_place    s ba         = ()
    let object_orient   s ba         = ()
    let delete_model    m = ()
    let delete_object   m = ()
    let delete_texture  m = ()
end

module Ac = A(Client)

let _ =
  let a = Ac.create () in
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
  ()

let test_0 () =
  Alcotest.(check bool) "test 0" true (1=1)


let test_set = [
  "test 0", `Slow, test_0;
  ]
