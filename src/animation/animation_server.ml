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

    let delete t =
      gl_with_int (Gl.delete_buffers 1) t.index_glid;
      gl_with_int (Gl.delete_buffers 1) t.data_glid;
      gl_with_int (Gl.delete_vertex_arrays 1) t.vao_glid;
      ()
    
end

module Object =
struct
    type t = {
      id : int;
      model : Model.t;
      transformation : t_ba_float32s;
      }
    let identity = [|1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.; 0.;0.;0.;0.|]
    let create id model =
      let transformation = ba_floats identity in
      { id; model; transformation }
    let place t ba = ()
    let orient t ba = ()
    let delete t = ()
end

module Client =
struct
    type t_model   = Model.t
    type t_object  = Object.t
    type t_texture = int
    let create_model    id cs is es   = Some (Model.create id Model.vnct_desc cs is es)
    let create_texture  id t w h data = Some 0
    let create_object   id m          = Some (Object.create id m)
    let object_place    o ba         = Object.place o ba
    let object_orient   o ba         = Object.orient o ba
    let delete_model    m = Model.delete m
    let delete_object   o = Object.delete o
    let delete_texture  m = ()
end
module AnimationServer = Animlib.Animation(Client)