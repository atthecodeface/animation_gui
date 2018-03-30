open Tgl4
type t_vap = int * int * Tgl4.Gl.enum * bool * int *int (* index size type_ normalized stride offset *)
type t_ba_float32s = (float, Bigarray.float32_elt,        Bigarray.c_layout) Bigarray.Array1.t
type t_ba_uint16s  = (int,   Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type t_ba_uint32s  = (int,   Bigarray.int32_elt,          Bigarray.c_layout) Bigarray.Array1.t
type t_ba_chars    = (char,  Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
let ba_floats fs  = Bigarray.(Array1.of_array float32 c_layout fs)

(*f gl_int_val, gl_with_int - to feed ints and get ints back from ctypes Opengl *)
let ba_int32_1    = Bigarray.(Array1.create int32 c_layout 1)
let gl_int_val  f   = f ba_int32_1 ; Int32.to_int ba_int32_1.{0}
let gl_with_int f i = ba_int32_1.{0} <- Int32.of_int i; f ba_int32_1

module Model =
struct
    type t = {
      id : int;
      vao_glid : int;
      index_glid : int;
      data_glid : int;
      elements : (int * int) array;
      }
    let vnct_desc = [ (0,3,Gl.float,false,(11*4),0);     (* vertices *)
                                (1,3,Gl.float,false,(11*4),(3*4)); (* normals *)
                                (2,3,Gl.float,false,(11*4),(6*4)); (* colors *)
                                (3,2,Gl.float,false,(11*4),(9*4)); (* UVs *)
                                ]

    let create id (desc:(t_vap list)) (coords:t_ba_float32s) (indices:t_ba_uint16s) elements =
      Printf.printf "Creating model id %d\n%!" id;

      (*b Create vao_glid, data_glid, index_glid *)
      let vao_glid = gl_int_val (Gl.gen_vertex_arrays 1) in
      let data_glid = gl_int_val (Gl.gen_buffers 1) in
      let index_glid = gl_int_val (Gl.gen_buffers 1) in

      (*b Bind the VBO to its big_array element and attrib pointers *)
      let size = Gl.bigarray_byte_size coords in
      Gl.bind_vertex_array vao_glid;
      Gl.bind_buffer Gl.array_buffer data_glid;
      Gl.buffer_data Gl.array_buffer size (Some coords) Gl.static_draw;
      let bind_vap (index,size,type_,normalized,stride,offset) =
        Gl.enable_vertex_attrib_array index;
        Gl.vertex_attrib_pointer index size type_ normalized stride (`Offset offset);
      in                                                                              
      List.iter bind_vap desc;
      let size  = Gl.bigarray_byte_size indices in
      Gl.bind_buffer Gl.element_array_buffer index_glid;
      Gl.buffer_data Gl.element_array_buffer size (Some indices) Gl.static_draw;
      { id; vao_glid; index_glid; data_glid; elements }

    let draw t =
        Gl.bind_vertex_array t.vao_glid;
        Gl.draw_elements Gl.triangle_strip 12 Gl.unsigned_short (`Offset 0)

    let delete t =
      gl_with_int (Gl.delete_buffers 1) t.index_glid;
      gl_with_int (Gl.delete_buffers 1) t.data_glid;
      gl_with_int (Gl.delete_vertex_arrays 1) t.vao_glid;
      ()
    
end

(*m Animatable_sig *)
module type Animatable_sig =
sig
  type t
  val create          : unit -> t
  val interpolate_set : float -> t -> t -> t -> unit
  val set_array       : t -> float array  -> t
  val set_bigarray    : t -> t_ba_float32s -> t
  val set             : t -> t -> t
end

(*m Animated *)
module Animated(A:Animatable_sig) =
struct

  (*t structure t *)
  type t = {
    mutable interpolating : bool;
    mutable last_time     : float;
    mutable target_time   : float;
    last_value    : A.t;
    target_value  : A.t;
    current_value : A.t;
    }

  (*f create *)
  let create _ =
    let last_value = A.create () in
    let target_value = A.create () in
    let current_value = A.create () in
    {last_value; target_value; current_value; interpolating=false; last_time=0.; target_time=0.;}

  (*f current_value *)
  let current_value t = t.current_value

  (*f set_value_array *)
  let set_value_array t time floats =
    t.target_time <- time;
    ignore (A.set_array t.current_value floats);
    t.interpolating <- false;
    ()

  (*f set_value_bigarray *)
  let set_value_bigarray t time floats =
    t.target_time <- time;
    ignore (A.set_bigarray t.current_value floats);
    t.interpolating <- false;
    ()

  (*f set_target_array *)
  let set_target_array t time floats =
    t.last_time <- t.target_time;
    t.target_time <- time;
    ignore (A.set_array t.target_value floats);
    t.interpolating <- true;
    ()

  (*f set_target_bigarray *)
  let set_target_bigarray t time floats =
    t.last_time <- t.target_time;
    t.target_time <- time;
    ignore (A.set t.last_value t.current_value);
    ignore (A.set_bigarray t.target_value floats);
    t.interpolating <- true;
    ()

  (*f set_time *)
  let set_time t time =
    if not t.interpolating then (
      t.target_time <- time;
    ) else if (compare time t.last_time) = (compare time t.target_time) then (
      t.target_time <- time;
      t.interpolating <- false
    ) else if (compare t.last_time t.target_time)=0 then (
      t.target_time <- time;
      t.interpolating <- false
    ) else (
      let tdiff = t.target_time -. t.last_time in
      A.interpolate_set ((time -. t.last_time) /. tdiff) t.last_value t.target_value t.current_value
    )

  (*f All done *)
end

(*m Animatable_Linear_Vec4 *)
module Animatable_Linear_Vec4 =
struct
  (*t struct t *)
  type t = float array

  (*f create *)
  let create _ = Array.make 4 0.

  (*f set *)
  let set     t0 t1 = Array.iteri (fun i v->t0.(i)<-v) t1; t0

  (*f set_array *)
  let set_array     t a = Array.iteri (fun i v->t.(i)<-v) a; t

  (*f set_bigarray *)
  let set_bigarray  t a = Array.iteri (fun i _->t.(i)<-a.{i}) t; t

  (*f interpolate_set *)
  let interpolate_set  s t0 t1 t =
    let oms = 1.0 -. s in
    Printf.printf "Interpoloate %f %f\n%!" s oms;
    Array.iteri (fun i _ -> t.(i) <- (oms *. t0.(i)) +. (s *. t1.(i))) t

  (*f All done *)
end

(*m Animated_vec4 *)
module Animated_vec4 = Animated(Animatable_Linear_Vec4)

(*m Object *)
module Object =
struct
  (*t t structure *)
  type t = {
      id : int;
      model : Model.t;
      transformation : t_ba_float32s;
      translation : Animated_vec4.t;
      orientation : Animated_vec4.t;
    }

  (*v identity *)
  let identity = [|1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.; 0.;0.;0.;0.|]

  (*f create *)
  let create id model =
    let transformation = ba_floats identity in
    { id; model; transformation;
      translation=Animated_vec4.create ();
      orientation=Animated_vec4.create ();
    }

  (*f delete *)
  let delete t = ()

  (*f draw *)
  let draw t uids =
    Gl.uniform_matrix4fv uids.(1) 1 true t.transformation;
    Model.draw t.model

  (*f set_target *)
  let set_target t time reason ba =
    Printf.printf "Set target %f\n%!" time;
    let l = Bigarray.Array1.dim ba in
    (
      match reason with
      | 1 -> if (time<=0.) then (Animated_vec4.set_value_bigarray t.translation time ba) else (Animated_vec4.set_target_bigarray t.translation time ba)
      | 2 -> Animated_vec4.set_target_bigarray t.orientation time ba
      | _ -> (
        if (l=16) then (
          Bigarray.Array1.blit ba t.transformation
        );
      )
    );
    ()

  (*f set_time *)
  let set_time t time =
    Animated_vec4.set_time t.translation time;
    Animated_vec4.set_time t.orientation time;
    let cv = Animated_vec4.current_value t.translation in
    let co = Animated_vec4.current_value t.orientation in
    t.transformation.{3} <- cv.(0);
    t.transformation.{7} <- cv.(1);
    t.transformation.{11} <- cv.(2);
    t.transformation.{15} <- cv.(3);
    ()

  (*f All done *)   
end

(*m AnimateTiming *)
module AnimateTiming =
struct
  type t = {
    mutable paused : bool;
    mutable time_now : float;
    mutable time_per_idle : float;
    }

  let create _ = 
    {
      paused = true;
      time_now = 0.;
      time_per_idle = 0.;
    }

  let animate t why time =
    (
    match why with
    | 0 -> t.paused <- true
    | 1 -> t.time_now <- time
    | 2 -> t.paused <- false; t.time_per_idle <- time
    | 3 -> t.paused <- false;
    | _ -> t.paused <- true
    );
    if t.paused then (256, t.time_now) else (257, t.time_now)

  let tick t =
    if t.paused then None else (
      t.time_now <- t.time_now +. t.time_per_idle;
      (* Printf.printf "Time now %f\n%!" t.time_now;*) 
      Some t.time_now
    )
end

(*m Client *)
module Client =
struct
    type t = {
        server : Animlib.Shm_server.Server.t;
        timing : AnimateTiming.t
      }
    type t_model   = Model.t
    type t_object  = Object.t
    type t_texture = int
    let create_model    t id cs is es   = Some (Model.create id Model.vnct_desc cs is es)
    let create_texture  t id t w h data = Some 0
    let create_object   t id m          = Some (Object.create id m)
    let delete_model    t m = Model.delete m
    let delete_object   t o = Object.delete o
    let delete_texture  t m = ()
    let object_set_target t s time reason ba  = Object.set_target s time reason ba
    let animate         t why time = AnimateTiming.animate t.timing why time
end
module ClientObject = Object

module Ac = Animlib.Animation(Client)
module AnimationServer = 
struct
  include Ac

  let create shm_size =
    let server = Animlib.Shm_server.Server.create shm_size in
    let timing = AnimateTiming.create () in
    animation_create {server; timing}

  let is_alive t = Animlib.Shm_server.Server.is_alive t.parent.server

  let idle t = 
    (
      match AnimateTiming.tick t.parent.timing with
      | Some time -> Ac.iter_objects t (fun i o -> ClientObject.set_time o time)
      | _ -> ()
    );
    let msg_callback client id msg msg_ba =
      let len = Bigarray.Array1.dim msg_ba in
      let rc = (match (Ac.parse_shm_msg t msg_ba 0 len) with
      | None     -> 0
      | Some err -> Printf.printf "Received message, error '%s'\n" err; 1
      ) in
      ignore (Animlib.Shm_server.Server.send t.parent.server client msg);
      Some ()
    in
    let _ = Animlib.Shm_server.Server.poll t.parent.server msg_callback 0 in
    ()

end

