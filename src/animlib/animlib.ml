(** Copyright (C) 2018,  Gavin J Stark.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @file     animation.ml
 * @brief    Shared-memory animation viewer using Ogl_gui
 *
 *)

type t_ba_float32s = (float, Bigarray.float32_elt,        Bigarray.c_layout) Bigarray.Array1.t
type t_ba_uint16s  = (int,   Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type t_ba_uint32s  = (int,   Bigarray.int32_elt,          Bigarray.c_layout) Bigarray.Array1.t
type t_ba_uint8s   = (int,   Bigarray.int8_unsigned_elt,  Bigarray.c_layout) Bigarray.Array1.t
type t_ba_chars    = (char,  Bigarray.int8_unsigned_elt,  Bigarray.c_layout) Bigarray.Array1.t

let ba_as_floats ba ofs len = Shm_ipc.Ba.retype_sub Bigarray.float32        Bigarray.c_layout ba ofs len
let ba_as_int16s ba ofs len = Shm_ipc.Ba.retype_sub Bigarray.int16_unsigned Bigarray.c_layout ba ofs len
let ba_as_int8s ba ofs len = Shm_ipc.Ba.retype_sub Bigarray.int8_unsigned Bigarray.c_layout ba ofs len

module Shm_server = Shm_server
module Option = Batteries.Option
let sfmt = Printf.sprintf

(*a Support modules *)
(*m Image *)
module Image =
struct
  type t_ba_2d = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t
  type t = {
    red_ba   : t_ba_2d;
    green_ba : t_ba_2d;
    blue_ba  : t_ba_2d;
    alpha_ba : t_ba_2d;
    width : int;
    height : int;
    }

  let create_from_image_file filename = 
    let image = ImageLib.openfile filename in
    let (r,g,b,a) = 
      match image.pixels with
      | RGB (r,g,b)    -> (r,g,b,r)
      | RGBA (r,g,b,a) -> (r,g,b,a)
      | _ -> raise Not_found
    in
    let red_ba   = match r with Pix8 r_ba -> r_ba | _ -> raise Not_found in
    let green_ba = match g with Pix8 g_ba -> g_ba | _ -> raise Not_found in
    let blue_ba  = match b with Pix8 b_ba -> b_ba | _ -> raise Not_found in
    let alpha_ba = match a with Pix8 a_ba -> a_ba | _ -> raise Not_found in
    let width  = Bigarray.Array2.dim1 red_ba in
    let height = Bigarray.Array2.dim2 red_ba in
    { red_ba; green_ba; blue_ba; alpha_ba;
      width; height; }

  let ba_size t = 4*t.width*t.height

  let width  t = t.width
  let height t = t.height
  let blit_ba t ba = 
    for x=0 to (t.width-1) do
      for y=0 to (t.height-1) do
        ba.{(x+y*t.width)*4+0} <- t.red_ba.{x,y};
        ba.{(x+y*t.width)*4+1} <- t.green_ba.{x,y};
        ba.{(x+y*t.width)*4+2} <- t.blue_ba.{x,y};
        ba.{(x+y*t.width)*4+3} <- if t.alpha_ba==t.red_ba then 255 else (t.alpha_ba.{x,y});
      done
    done
end

(*a Animation modules *)
(*m Animation_sig *)
module type Animation_sig =
sig
  type t
  type t_model
  type t_object
  type t_texture
  val create_model        : t -> int -> t_ba_float32s -> t_ba_uint16s -> (int * int) array -> t_model option
  val create_texture      : t -> int -> int -> int -> int -> t_ba_chars -> t_texture option
  val create_object       : t -> int -> t_model -> t_object option
  val object_set_target   : t -> t_object -> float -> int -> t_ba_float32s -> unit
  val object_set_material : t -> t_object -> float -> int -> t_ba_float32s -> unit
  val delete_model        : t -> t_model -> unit
  val delete_object       : t -> t_object -> unit
  val delete_texture      : t -> t_texture -> unit
  val animate             : t -> int -> float -> (int * float)
end

(*m Animation *)
module Animation (A:Animation_sig) =
struct

  (*t Types *)
  type t_model   = A.t_model
  type t_object   = A.t_object
  type t_texture   = A.t_texture
  type t = {
    parent   : A.t;
    models   : (int, t_model) Hashtbl.t;
    objects  : (int, t_object) Hashtbl.t;
    textures : (int, t_texture) Hashtbl.t;
    mutable opt_err : string option;
    }

  (*f animation_create *)
  let animation_create parent =
    let models   = Hashtbl.create 128 in
    let objects  = Hashtbl.create 1024 in
    let textures = Hashtbl.create 128 in
    let opt_err = None in
    { parent; models; objects; textures; opt_err }

  (*f has_error *)
  let has_error t = Option.is_some t.opt_err

  (*f error *)
  let error t = Option.get t.opt_err

  (*f add_model *)
  let add_model t id model =
    if (Hashtbl.mem t.models id) then (t.opt_err <- Some (sfmt "Duplicate model id %d" id));
    Hashtbl.replace t.models id model

  (*f add_object *)
  let add_object t id obj =
    if (Hashtbl.mem t.objects id) then (t.opt_err <- Some (sfmt "Duplicate object id %d" id));
    Printf.printf "Adding object %d\n%!" id;
    Hashtbl.replace t.objects id obj

  (*f add_texture *)
  let add_texture t id texture =
    if (Hashtbl.mem t.textures id) then (t.opt_err <- Some (sfmt "Duplicate texture id %d" id));
    Hashtbl.replace t.textures id texture

  (*f iter_textures *)
  let iter_textures t f =
    Hashtbl.iter f t.textures

  (*f iter_models *)
  let iter_models t f =
    Hashtbl.iter f t.models

  (*f iter_objects *)
  let iter_objects t f =
    Hashtbl.iter f t.objects

  (*f model_of_id *)
  let model_of_id t id =
    Hashtbl.find_opt t.models id

  (*f object_of_id *)
  let object_of_id t id =
    Hashtbl.find_opt t.objects id

  (*f texture_of_id *)
  let texture_of_id t id =
    Hashtbl.find_opt t.textures id

  (*f map_model_of_id *)
  let map_model_of_id t id default f =
    match model_of_id t id with 
    | None -> default
    | Some o -> f o

  (*f map_object_of_id *)
  let map_object_of_id t id default f =
    match object_of_id t id with 
    | None -> default
    | Some o -> f o

  (*f map_texture_of_id *)
  let map_texture_of_id t id default f =
    match texture_of_id t id with 
    | None -> default
    | Some o -> f o

  (*f remove_model *)
  let remove_model t id =
    map_model_of_id t id () (fun o -> (A.delete_model t.parent o; Hashtbl.remove t.models id))

  (*f remove_object *)
  let remove_object t id =
    map_object_of_id t id () (fun o -> (A.delete_object t.parent o; Hashtbl.remove t.objects id))

  (*f remove_texture *)
  let remove_texture t id =
    map_texture_of_id t id () (fun o -> (A.delete_texture t.parent o; Hashtbl.remove t.textures id))

  (*f reset *)
  let reset t = 
    iter_textures t (fun i tx -> A.delete_texture t.parent tx);
    iter_objects  t (fun i o -> A.delete_object t.parent o);
    iter_models   t (fun i m -> A.delete_model t.parent m);
    Hashtbl.reset t.textures;
    Hashtbl.reset t.objects;
    Hashtbl.reset t.models;
    ()

  (*m Rpc *)
  module Rpc =
    struct
      let rpc_types = ref []

      let add_type s f =
        let l = List.length !rpc_types in
        rpc_types := (!rpc_types) @ [(s,(l,f))]

      let key_of_rpc k =
       let (l,f) = List.assoc k !rpc_types in
       l
                                                   
      let parser_of_key k =
       let (s,(l,f)) = List.nth !rpc_types k in
       f

    end

  (*m Model *)
  module Model = struct
    (*t exceptin BadArgs *)
    exception BadArgs of string

    (*t type tc - model under construction *)
    type tc = {
        mutable id : int;
        mutable coords : int * int;
        mutable indices : int * int;
        mutable elements : (int * int) array;
      }

    (*f create *)
    let create _ = 
      {
        id = 0;
        coords = (0,0);
        indices = (0,0);
        elements = [||];
      }

    (*f rpc_create_args *)
    let rpc_create_args id cs_is_es =
      if (Array.length cs_is_es)<>6 then (
        raise (BadArgs "cs_is_es must be ofs,len,ofs,len,ofs,len")
      ) else (
        Shm_ipc.Mbf.Make.(List [Int id; ArrayInt32 cs_is_es])
      )

    (*f parse_id - callback for parser to set data from an rpc message *)
    let parse_id mc s = mc.id <- s; mc

    (*f parse_cs_is_es - callback for parser to set data from an rpc message *)
    let parse_cs_is_es mc ba =
      let l = Bigarray.Array1.dim ba in
      if l>=2 then mc.coords  <- Int32.(to_int ba.{0}, to_int ba.{1});
      if l>=4 then mc.indices <- Int32.(to_int ba.{2}, to_int ba.{3});
      if l>=6 then mc.elements <- Array.init ((l-4)/2) (fun i -> Int32.(to_int ba.{(i*2)+4}, to_int ba.{(i*2)+5}));
      mc

    (*f parser_of_key - map key number to callback *)
    let parser_of_key k = Shm_ipc.Mbf.([| Int parse_id ; RepInt32 parse_cs_is_es |]).(k)

    (*f rpc_parse_msg - parse an RPC 'create model' message *)
    let rpc_parse_msg a msg_ba ofs len =
      let mc = create () in
      let mc = Shm_ipc.Mbf.fold_message msg_ba parser_of_key mc ofs len in
      let (cs_ofs,cs_len) = mc.coords in
      let (is_ofs,is_len) = mc.indices in
      let cs = Shm_ipc.Ba.retype_sub Bigarray.float32        Bigarray.c_layout msg_ba cs_ofs (cs_len*4) in
      let is = Shm_ipc.Ba.retype_sub Bigarray.int16_unsigned Bigarray.c_layout msg_ba is_ofs (is_len*2) in
      let opt_model = A.create_model a.parent mc.id cs is mc.elements in
      (mc.id, opt_model)

    (*f All done *)
  end

  (*m Object *)
  module Object = struct
    (*t BadArgs exception *)
    exception BadArgs of string

    (*t tc - object under construction *)
    type tc = {
        mutable id : int;
        mutable model_id : int;
      }

    (*t st - set target under construction *)
    type st = {
        mutable st_id : int;
        mutable st_time : float;
        mutable st_reason : int;
        mutable st_ba : t_ba_float32s
      }
    let ba_dummy_floats = Bigarray.(Array1.of_array float32 c_layout [||])

    (*f create _ - create object under construction *)
    let create _ = 
      {
        id = 0;
        model_id = 0;
      }

    (*f create_parse_id - mbf parse callback to set object id *)
    let create_parse_id mc s =
      mc.id <- s; mc

    (*f create_parse_model_id - mbf parse callback to set model id *)
    let create_parse_model_id mc v =
      mc.model_id <- v; mc

    (*f rpc_create_args - return the structure of a shm ipc mbf for create object id of model_id *)
    let rpc_create_args id model_id =
      Shm_ipc.Mbf.Make.(List [Int id; Int model_id])

    (*f create_parser_of_key - map from mbf key to parser (must match rpc_create_args) *)
    let create_parser_of_key k = Shm_ipc.Mbf.([| Int create_parse_id ; Int create_parse_model_id |]).(k)

    (*f rpc_create_parse_msg - parse object create message *)
    let rpc_create_parse_msg a msg_ba ofs len =
      let mc = create () in
      let mc = Shm_ipc.Mbf.fold_message msg_ba create_parser_of_key mc ofs len in
      match model_of_id a mc.model_id with
      | Some model_id -> (mc.id, A.create_object a.parent mc.id model_id)
      | _ -> (mc.id, None)

    (*f set_target_parse_* - mbf parse callback to set object id *)
    let set_target_parse_id     st id     = st.st_id      <- id     ; st
    let set_target_parse_time   st time   = st.st_time    <- time   ; st
    let set_target_parse_reason st reason = st.st_reason  <- reason ; st
    let set_target_parse_args   st ba     = st.st_ba      <- ba     ; st

    (*f rpc_set_target_args *)
    let rpc_set_target_args id time reason args =
      Shm_ipc.Mbf.Make.(List [Int id; Float time; Int reason; ArrayFloat args])

    (*f set_target_parser_of_key - map from mbf key to parser (must match rpc_set_target_args) *)
    let set_target_parser_of_key k = Shm_ipc.Mbf.([| Int set_target_parse_id ; Float set_target_parse_time ; Int set_target_parse_reason ; RepFloat set_target_parse_args |]).(k)

    (*f rpc_set_target_parse_msg - parse object set target message *)
    let rpc_set_target_parse_msg a msg_ba ofs len =
      let st = {st_id=0;st_time=0.;st_reason=0;st_ba=ba_dummy_floats} in
      let st = Shm_ipc.Mbf.fold_message msg_ba set_target_parser_of_key st ofs len in
      Some (st.st_id, st.st_time, st.st_reason, st.st_ba)

    (*f set_material_parse_* - mbf parse callback to set object id *)
    let set_material_parse_id     st id     = st.st_id      <- id     ; st
    let set_material_parse_time   st time   = st.st_time    <- time   ; st
    let set_material_parse_reason st reason = st.st_reason  <- reason ; st
    let set_material_parse_args   st ba     = st.st_ba      <- ba     ; st

    (*f rpc_set_material_args *)
    let rpc_set_material_args id time reason args =
      Shm_ipc.Mbf.Make.(List [Int id; Float time; Int reason; ArrayFloat args])

    (*f set_material_parser_of_key - map from mbf key to parser (must match rpc_set_material_args) *)
    let set_material_parser_of_key k = Shm_ipc.Mbf.([| Int set_material_parse_id ; Float set_material_parse_time ; Int set_material_parse_reason ; RepFloat set_material_parse_args |]).(k)

    (*f rpc_set_material_parse_msg - parse object set material message *)
    let rpc_set_material_parse_msg a msg_ba ofs len =
      let st = {st_id=0;st_time=0.;st_reason=0;st_ba=ba_dummy_floats} in
      let st = Shm_ipc.Mbf.fold_message msg_ba set_material_parser_of_key st ofs len in
      Some (st.st_id, st.st_time, st.st_reason, st.st_ba)

    (*f All done *)
  end

  (*m Texture *)
  module Texture = struct
    (*t BadArgs exception *)
    exception BadArgs of string

    (*t tc - texture under construction *)
    type tc = {
        mutable id : int;
        mutable width : int;
        mutable height : int;
        mutable data_type : int;
        mutable data_ofs : int;
      }

    (*f create _ - create object under construction *)
    let create _ = 
      {
        id = 0;
        width = 0;
        height = 0;
        data_type = 0;
        data_ofs = 0;
      }

    (*f create_parse_id - mbf parse callback to set texture id *)
    let create_parse_id mc s = mc.id <- s; mc

    (*f create_parse_width - mbf parse callback to set texture width *)
    let create_parse_width mc s = mc.width <- s; mc

    (*f create_parse_height - mbf parse callback to set texture height *)
    let create_parse_height mc s = mc.height <- s; mc

    (*f create_parse_data_type - mbf parse callback to set texture data_type *)
    let create_parse_data_type mc s = mc.data_type <- s; mc

    (*f create_parse_data_ofs - mbf parse callback to set texture data_ofs *)
    let create_parse_data_ofs mc s = mc.data_ofs <- s; mc

    (*f rpc_create_msg - return the structure of a shm ipc mbf for create object id of model_id *)
    let rpc_create_msg id width height data_type data_ofs =
      Shm_ipc.Mbf.Make.(List [Int id; Int width; Int height; Int data_type; Int data_ofs])

    (*f create_parser_of_key - map from mbf key to parser (must match rpc_create_args) *)
    let create_parser_of_key k = Shm_ipc.Mbf.([| Int create_parse_id ;
                                                 Int create_parse_width;
                                                 Int create_parse_height;
                                                 Int create_parse_data_type;
                                                 Int create_parse_data_ofs;
                                              |]).(k)

    (*f rpc_parse_create_msg - parse texture create message *)
    let rpc_parse_create_msg a msg_ba ofs len =
      let mc = create () in
      let mc = Shm_ipc.Mbf.fold_message msg_ba create_parser_of_key mc ofs len in
      let data_ba = Shm_ipc.Ba.retype_sub Bigarray.char Bigarray.c_layout msg_ba mc.data_ofs (len-mc.data_ofs) in
      let opt_texture = A.create_texture a.parent mc.id mc.data_type mc.width mc.height data_ba in
      (mc.id, opt_texture)

    (*f All done *)
  end

  (*m Timing *)
  module Timing = struct
    (*t BadArgs exception *)
    exception BadArgs of string

    (*t ta - animate *)
    type ta = {
        mutable why : int;
        mutable time : float;
      }

    (*f animate_parse_* - mbf parse callback to set object id *)
    let animate_parse_why    ta why    = ta.why   <- why     ; ta
    let animate_parse_time   ta time   = ta.time  <- time   ; ta

    (*f rpc_animate_args *)
    let rpc_animate_args why time =
      Shm_ipc.Mbf.Make.(List [Int why; Float time;])

    (*f animate_parser_of_key - map from mbf key to parser (must match rpc_animate_args) *)
    let animate_parser_of_key k = Shm_ipc.Mbf.([| Int animate_parse_why ; Float animate_parse_time |]).(k)

    (*f rpc_animate_parse_msg - parse object animate message *)
    let rpc_animate_parse_msg a msg_ba ofs len =
      let ta = {why=0; time=0.;} in
      let ta = Shm_ipc.Mbf.fold_message msg_ba animate_parser_of_key ta ofs len in
      Some (ta.why, ta.time)

    (*f All done *)
  end

  (*f rpc_msg *)
  let rpc_msg ba size name args =
    let msg = Shm_ipc.Mbf.Make.(Assoc (Rpc.key_of_rpc, [(name, args)])) in
    let actual_size = Shm_ipc.Mbf.Make.write ba size 0 0 msg in
    actual_size <= size

  (*f rpc_reset_msg *)
  let rpc_reset_msg ba size =
    let args = Shm_ipc.Mbf.Make.Int 0 in
    rpc_msg ba size "reset" args

  (*f rpc_model_create_msg *)
  let rpc_model_create_msg ba size id cs_is_es =
    let args = Model.rpc_create_args id cs_is_es in
    rpc_msg ba size "model_create" args

  (*f rpc_texture_create_msg *)
  let rpc_texture_create_msg ba size id width height data_type data_ofs =
    let args = Texture.rpc_create_msg id width height data_type data_ofs in
    rpc_msg ba size "texture_create" args

  (*f rpc_object_create_msg *)
  let rpc_object_create_msg ba size id model_id =
    let args = Object.rpc_create_args id model_id in
    rpc_msg ba size "object_create" args

  (*f rpc_object_delete_msg *)
  let rpc_object_delete_msg ba size id =
    let args = Shm_ipc.Mbf.Make.Int id in
    rpc_msg ba size "object_delete" args

  (*f parse_reset *)
  let parse_reset t _ = 
    reset t;
    t

  (*f parse_model_create *)
  let parse_model_create t (msg_ba,ofs,len) = 
    (match (Model.rpc_parse_msg t msg_ba ofs len) with 
     | (id, Some model) -> add_model t id model
     | _ -> ()
    );
    t
                        
  (*f parse_texture_create *)
  let parse_texture_create t (msg_ba,ofs,len) = 
    (match (Texture.rpc_parse_create_msg t msg_ba ofs len) with 
     | (id, Some texture) -> add_texture t id texture
     | _ -> ()
    );
    t
                        
  (*f parse_object_create *)
  let parse_object_create t (msg_ba,ofs,len) = 
    (match (Object.rpc_create_parse_msg t msg_ba ofs len) with
     | (id, Some obj) -> add_object t id obj
     | _ -> ()
    );
    t

  (*f parse_object_delete *)
  let parse_object_delete t obj_id = 
    remove_object t obj_id;
    t

  (*f parse_object_set_target *)
  let parse_object_set_target t (msg_ba,ofs,len) = 
    (match (Object.rpc_set_target_parse_msg t msg_ba ofs len) with
     | Some (id, time, reason, ba) -> map_object_of_id t id () (fun o -> A.object_set_target t.parent o time reason ba)
     | _ -> ()
    );
    t

  (*f rpc_object_set_target_msg *)
  let rpc_object_set_target_msg ba size id time reason args =
    let args = Object.rpc_set_target_args id time reason args in
    rpc_msg ba size "object_set_target" args

  (*f parse_object_set_material *)
  let parse_object_set_material t (msg_ba,ofs,len) = 
    (match (Object.rpc_set_material_parse_msg t msg_ba ofs len) with
     | Some (id, time, reason, ba) -> map_object_of_id t id () (fun o -> A.object_set_material t.parent o time reason ba)
     | _ -> ()
    );
    t

  (*f rpc_object_set_material_msg *)
  let rpc_object_set_material_msg ba size id time reason args =
    let args = Object.rpc_set_material_args id time reason args in
    rpc_msg ba size "object_set_material" args

  (*f parse_animate *)
  let parse_animate t (msg_ba,ofs,len) = 
    (match (Timing.rpc_animate_parse_msg t msg_ba ofs len) with
     | Some (why, time) -> (
       let (why, time) = A.animate t.parent why time in
       let msg_ba = Shm_ipc.Ba.retype Bigarray.int32 Bigarray.c_layout msg_ba in
       msg_ba.{2} <- Int32.of_int why;
       let msg_ba = Shm_ipc.Ba.retype Bigarray.float32 Bigarray.c_layout msg_ba in
       msg_ba.{4} <- time;
     )
     | _ -> ()
    );
    t

  (*f rpc_animate_msg *)
  let rpc_animate_msg ba size why time =
    let args = Timing.rpc_animate_args why time in
    rpc_msg ba size "animate" args

  (*f init *)
  let _ =                      
    Rpc.add_type "reset"              (Shm_ipc.Mbf.Int parse_reset);
    Rpc.add_type "model_create"       (Shm_ipc.Mbf.Blob parse_model_create);
    Rpc.add_type "texture_create"     (Shm_ipc.Mbf.Blob parse_texture_create);
    Rpc.add_type "object_create"      (Shm_ipc.Mbf.Blob parse_object_create);
    Rpc.add_type "object_delete"      (Shm_ipc.Mbf.Int parse_object_delete);
    Rpc.add_type "object_set_target"  (Shm_ipc.Mbf.Blob parse_object_set_target);
    Rpc.add_type "object_set_material"  (Shm_ipc.Mbf.Blob parse_object_set_material);
    Rpc.add_type "animate"            (Shm_ipc.Mbf.Blob parse_animate);
    ()
  
  (*f parser_of_key *)
  let parser_of_key k = Rpc.parser_of_key k

  (*f parse_shm_msg *)
  let parse_shm_msg t msg_ba ofs len = 
    ignore (Shm_ipc.Mbf.fold_root_message ~verbose:false msg_ba parser_of_key t ofs len);
    t.opt_err

  (*f All done *)
end

(*a Top level *)
(*m BasicClient_base *)
module type ClientType_sig =
sig
    type t
end
module BasicClient_base(ClientType:ClientType_sig) =
struct
    type t = ClientType.t
    type t_model   = string
    type t_object  = string
    type t_texture = string
    let create_model    t s cs is es   = Some (sfmt "model %d"   s)
    let create_texture  t s tx w h data = Some (sfmt "texture %d" s)
    let create_object   t s m          = Some (sfmt "object %d"  s)
    let object_set_target    t s time reason ba         = ()
    let object_set_material  t s what arg ba         = ()
    let delete_model    t m = ()
    let delete_object   t m = ()
    let delete_texture  t m = ()
    let animate         t why time = (why,time)
end

(*m BasicClient *)
module BasicClientType =
struct
  type t = {
    client : Shm_server.Client.t;
  }
end
module BasicClient =
  struct
    include Animation(BasicClient_base(BasicClientType))
    type bct = BasicClientType.t

    (*v statics *)
    let long_delay = 1000*1000

    (*f create *)
    let create _ = 
      let client = Shm_server.Client.create () in
      let t:bct = {client;} in
      ignore (animation_create t);
      t

    (*f client - return the base client *)
    let client (t:bct) = t.client

    (*f msg_alloc - Allocate a message - return msg * msg_ba * msg_id *)
    let msg_alloc (t:bct) byte_size =
      Shm_server.Client.msg_alloc t.client byte_size

    (*f poll_idle - poll for a delay (freeing returned messages) *)
    let poll_idle (t:bct) delay =
      let msg_callback rx_id msg msg_ba =
        ignore (Shm_server.Client.msg_free t.client msg);
        (Some ())
      in
      Shm_server.Client.poll t.client msg_callback delay

    (*f poll_for_msg - poll until a particular message id returns

        freeing messages, calling back on desired message first

        Return false if message not received within delay
     *)
    let poll_for_msg (t:bct) ?callback id delay =
      let rec poll _ =
        let response_received = ref None in
        let msg_callback rx_id msg msg_ba =
          if (rx_id=id) then (Option.may (fun cb->cb msg_ba) callback);
          ignore (Shm_server.Client.msg_free t.client msg);
          response_received := Some rx_id;
          (Some ())
        in
        response_received := None;
        if (Shm_server.Client.poll t.client msg_callback delay) then (
          match (!response_received) with
          | None -> false
          | Some rx_id when (rx_id=id) -> true
          | _ -> poll ()
        ) else (
          false
        )
      in
      poll ()

    (*f send - send a message *)
    let send (t:bct) msg msg_id =  
      (* Printf.printf "Send message %d\n%!" msg_id; *)
      ignore (Shm_server.Client.send t.client msg);
      msg_id

    (*f send_and_wait - send a message and wait for its response with a callback

        Returns false if the message reply was not received in time
     *)
    let send_and_wait ?delay:(delay=100000) ?callback (t:bct) msg id =  
      poll_for_msg t ?callback:callback (send t msg id) delay

    (*f reset - reset the animation server (clears all models, textures, objects) *)
    let reset t =
      let (msg,msg_ba,id) = msg_alloc t 64 in
      ignore (rpc_reset_msg msg_ba 64);
      send_and_wait ~delay:long_delay t msg id

    (*f animate - set time, play, pause, etc *)
    let animate t why time =
      let (msg,msg_ba,id) = msg_alloc t 64 in
      ignore (rpc_animate_msg msg_ba 64 why time);
      send_and_wait t msg id

    (*f send_msg_from_ba - create and send a message packed in another ba *)
    let send_msg_from_ba t ?size ba =
      let l = Option.default (Bigarray.Array1.dim ba) size in
      let (msg,msg_ba,msg_id) = msg_alloc t l in
      Bigarray.Array1.(blit ba msg_ba);
      send t msg msg_id

    (*f create_object - create and object given a model *)
    let create_object t obj_id model_id =
      let (msg,msg_ba,msg_id) = msg_alloc t 128 in
      ignore (rpc_object_create_msg msg_ba 128 obj_id model_id);
      send_and_wait t msg msg_id

    (*f delete_object - delete an object, don't wait *)
    let delete_object t obj_id =
      let (msg,msg_ba,msg_id) = msg_alloc t 128 in
      ignore (rpc_object_delete_msg msg_ba 128 obj_id);
      send t msg msg_id

    (*f create_texture_of_file *)
    let create_texture_of_file t tex_id filename =
      let image = Image.create_from_image_file filename in
      let ofs = 128 in
      let ba_size = Image.ba_size image in
      let width  = Image.width image in
      let height = Image.height image in
      let (msg,msg_ba,msg_id) = msg_alloc t (ba_size+ofs) in
      ignore (rpc_texture_create_msg msg_ba ofs tex_id width height 0 ofs);
      let ba = Shm_ipc.Ba.retype_sub Bigarray.int8_unsigned Bigarray.c_layout msg_ba ofs ba_size in
      Image.blit_ba image ba;
      send_and_wait t msg msg_id

    (*f move_to - move an object to an x/y/z, wait for response *)
    let move_to t time id x y z =
      let (msg,msg_ba,msg_id) = msg_alloc t 128 in
      ignore (rpc_object_set_target_msg msg_ba 128 id time 1 [|x;y;z;1.;|]);
      send t msg msg_id

    (*f set_material - wait for response *)
    let set_material t time id tex_id =
      let (msg,msg_ba,msg_id) = msg_alloc t 128 in
      ignore (rpc_object_set_material_msg msg_ba 128 id time tex_id [|0.;|]);
      send t msg msg_id

    (*f wait_for_time - wait for animation to reach time *)
    let wait_for_time ?ticks_per_second:(tps=1000.*.1000.) t time =
      let rec wait _ =
        let current_time = ref None in
        let current_time_cb msg_ba =
          let msg_ba = Shm_ipc.Ba.retype Bigarray.int32 Bigarray.c_layout msg_ba in
          if (Int32.to_int msg_ba.{2})=257 then (
            let msg_ba = Shm_ipc.Ba.retype Bigarray.float32 Bigarray.c_layout msg_ba in
            current_time := Some msg_ba.{4}
          )
        in
        let (msg,msg_ba,id) = msg_alloc t 1024 in
        ignore (rpc_animate_msg msg_ba 1024 3 0.);
        if (send_and_wait ~callback:current_time_cb t msg id) then (
          match !current_time with 
          | Some sim_time -> (
            let delay = int_of_float ((time-.sim_time) *. tps) in
            (*Printf.printf "Time %f %f delay %d\n%!" time sim_time delay;*)
            if delay<1000 then true
            else if (poll_idle t (delay/3)) then  (* don't wait the whole amount to make it smoother *)
              wait ()
            else false (* not alive any more *)
          )
          | _ -> false (* no useful reply *)
        ) else ( (* timed-out waiting for reply to animate message *)
         false
        )
      in
      wait ()

    (*f create_model_ba *)
    let create_model_ba model_id coords indices element_type num_elements =
      let num_floats  = Array.length coords in
      let num_indices = Array.length indices in
      let indices_start = 128 in
      let floats_start = indices_start + (2*num_indices) in
      let floats_end   = floats_start  + (4*num_floats) in
      let ba = Bigarray.(Array1.create char c_layout floats_end) in
      let okay = rpc_model_create_msg ba indices_start model_id Int32.([|of_int floats_start;of_int num_floats;of_int indices_start; of_int num_indices; of_int element_type; of_int num_elements |]) in
      if okay then (
        let floats_ba  = ba_as_floats ba floats_start (floats_end - floats_start) in
        let indices_ba = ba_as_int16s ba indices_start (floats_start - indices_start) in
        Array.iteri (fun i f -> floats_ba.{i} <- f) coords;
        Array.iteri (fun i n -> indices_ba.{i} <- n) indices;
        Some ba
      ) else (
        None
      )

    (*f All done *)
end

