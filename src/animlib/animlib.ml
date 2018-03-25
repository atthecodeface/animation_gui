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
type t_ba_chars    = (char,  Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Shm_server = Shm_server
module Option = Batteries.Option
let sfmt = Printf.sprintf

module type Animation_sig =
sig
  type t_model
  type t_object
  type t_texture
  val create_model     : int -> t_ba_float32s -> t_ba_uint16s -> (int * int) array -> t_model option
  val create_texture   : int -> int -> int -> int -> t_ba_chars -> t_texture option
  val create_object    : int -> t_model -> t_object option
  val object_place     : t_object -> t_ba_float32s -> unit
  val object_orient    : t_object -> t_ba_float32s -> unit
  val delete_model     : t_model -> unit
  val delete_object    : t_object -> unit
  val delete_texture   : t_texture -> unit
end

(*m Animation *)
module Animation (A:Animation_sig) =
struct

  (*t Types *)
  type t_model   = A.t_model
  type t_object   = A.t_object
  type t_texture   = A.t_texture
  type t = {
    models   : (int, t_model) Hashtbl.t;
    objects  : (int, t_object) Hashtbl.t;
    textures : (int, t_texture) Hashtbl.t;
    mutable opt_err : string option;
    }

  (*f create *)
  let create _ =
    let models   = Hashtbl.create 128 in
    let objects  = Hashtbl.create 1024 in
    let textures = Hashtbl.create 128 in
    let opt_err = None in
    { models; objects; textures; opt_err }

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

  (*f reset *)
  let reset t = 
    iter_textures t (fun i t -> A.delete_texture t);
    iter_objects  t (fun i t -> A.delete_object t);
    iter_models   t (fun i t -> A.delete_model t);
    Hashtbl.reset t.textures;
    Hashtbl.reset t.objects;
    Hashtbl.reset t.models;
    ()

  let model_of_id t id =
    Hashtbl.find_opt t.models id

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
    exception BadArgs of string
    type tc = {
        mutable id : int;
        mutable coords : int * int;
        mutable indices : int * int;
        mutable elements : (int * int) array;
      }

    let create _ = 
      {
        id = 0;
        coords = (0,0);
        indices = (0,0);
        elements = [||];
      }

    let rpc_create_args id cs_is_es =
      if (Array.length cs_is_es)<>6 then (
        raise (BadArgs "cs_is_es must be ofs,len,ofs,len,ofs,len")
      ) else (
        Shm_ipc.Mbf.Make.(List [Int id; ArrayInt32 cs_is_es])
      )

    let parse_id mc s =
      mc.id <- s; mc

    let parse_cs_is_es mc ba =
      let l = Bigarray.Array1.dim ba in
      if l>=2 then mc.coords  <- Int32.(to_int ba.{0}, to_int ba.{1});
      if l>=4 then mc.indices <- Int32.(to_int ba.{2}, to_int ba.{3});
      if l>=6 then mc.elements <- Array.init ((l-4)/2) (fun i -> Int32.(to_int ba.{(i*2)+4}, to_int ba.{(i*2)+5}));
      mc

    let parser_of_key k = Shm_ipc.Mbf.([| Int parse_id ; RepInt32 parse_cs_is_es |]).(k)

    let rpc_parse_msg a msg_ba ofs len =
      let mc = create () in
      let mc = Shm_ipc.Mbf.fold_message msg_ba parser_of_key mc ofs len in
      let (cs_ofs,cs_len) = mc.coords in
      let (is_ofs,is_len) = mc.indices in
      let cs = Shm_ipc.Ba.retype_sub Bigarray.float32        Bigarray.c_layout msg_ba cs_ofs (cs_len*4) in
      let is = Shm_ipc.Ba.retype_sub Bigarray.int16_unsigned Bigarray.c_layout msg_ba is_ofs (is_len*2) in
      let opt_model = A.create_model mc.id cs is mc.elements in
      (mc.id, opt_model)

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

    (*f create _ - create object under construction *)
    let create _ = 
      {
        id = 0;
        model_id = 0;
      }

    (*f parse_id - mbf parse callback to set object id *)
    let parse_id mc s =
      mc.id <- s; mc

    (*f parse_model_id - mbf parse callback to set model id *)
    let parse_model_id mc v =
      mc.model_id <- v; mc

    (*f rpc_create_args - return the structure of a shm ipc mbf for create object id of model_id *)
    let rpc_create_args id model_id =
      Shm_ipc.Mbf.Make.(List [Int id; Int model_id])

    (*f parse_of_key - map from mbf key to parser (must match rpc_create_args) *)
    let parser_of_key k = Shm_ipc.Mbf.([| Int parse_id ; Int parse_model_id |]).(k)

    (*f rpc_parse_msg - parse object create message *)
    let rpc_parse_msg a msg_ba ofs len =
      let mc = create () in
      let mc = Shm_ipc.Mbf.fold_message msg_ba parser_of_key mc ofs len in
      match model_of_id a  mc.model_id with
      | Some model_id -> (mc.id, A.create_object mc.id model_id)
      | _ -> (mc.id, None)

    (*f All done *)
  end

  (*f rpc_msg *)
  let rpc_msg ba size name args =
    let msg = Shm_ipc.Mbf.Make.(Assoc (Rpc.key_of_rpc, [(name, args)])) in
    let actual_size = Shm_ipc.Mbf.Make.write ba size 0 0 msg in
    actual_size <= size

  (*f rpc_reset_msg *)
  let rpc_reset_msg ba size =
    let args = Shm_ipc.Mbf.Make.Callback (fun _ _ -> ()) in
    let args = Shm_ipc.Mbf.Make.Int 0 in
    rpc_msg ba size "reset" args

  (*f rpc_model_create_msg *)
  let rpc_model_create_msg ba size id cs_is_es =
    let args = Model.rpc_create_args id cs_is_es in
    rpc_msg ba size "model_create" args

  (*f rpc_object_create_msg *)
  let rpc_object_create_msg ba size id model_id =
    let args = Object.rpc_create_args id model_id in
    rpc_msg ba size "object_create" args

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
                        
  (*f parse_object_create *)
  let parse_object_create t (msg_ba,ofs,len) = 
    (match (Object.rpc_parse_msg t msg_ba ofs len) with
     | (id, Some obj) -> add_object t id obj
     | _ -> ()
    );
    t

  (*f init *)
  let _ =                      
    Rpc.add_type "reset"          (Shm_ipc.Mbf.Int parse_reset);
    Rpc.add_type "model_create"   (Shm_ipc.Mbf.Blob parse_model_create);
    Rpc.add_type "object_create"  (Shm_ipc.Mbf.Blob parse_object_create);
    ()
  
  (*f parser_of_key *)
  let parser_of_key k = Rpc.parser_of_key k

  (*f parse_shm_msg *)
  let parse_shm_msg t msg_ba ofs len = 
    Shm_ipc.Mbf.fold_root_message ~verbose:false msg_ba parser_of_key t ofs len;
    t.opt_err

  (*f All done *)
end
  (*
  object_place  : object -> position -> quaternion -> scale -> result (?time?)
  get_time  : unit -> time result
  set_time  : time -> result
   *)

