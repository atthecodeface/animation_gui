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
type t_ba_uint32s  = (int,   Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type t_ba_chars    = (char,  Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type Animation_sig =
sig
  type t_model
  type t_object
  type t_texture
  val create_model     : string -> t_ba_float32s -> t_ba_uint32s -> (int * int) array -> t_model option
  val create_texture   : string -> int -> int -> int -> t_ba_chars -> t_texture option
  val create_object    : string -> t_model -> t_object option
  val object_place     : t_object -> t_ba_float32s -> unit
  val object_orient    : t_object -> t_ba_float32s -> unit
  val delete_model     : t_model -> unit
  val delete_object    : t_object -> unit
  val delete_texture   : t_texture -> unit
end

module Animation =
struct
  module Rpc =
    struct
      let rpc_types = [|"reset"; "model_create"; "object_create"; "object_place"|]
      let key_of_rpc k =
        let rec find i = 
          if i<0 then 0 else
            if (String.compare k rpc_types.(i))=0 then i else find (i-1)
        in
        find ((Array.length rpc_types)-1)
                                                   
    end

  module Model = struct
    type tc = {
        mutable name : string;
        coords : int * int;
        indices : int * int;
        elements : (int * int) array;
      }

    let rpc_create_args name cs_is_es = Shm_ipc.Mbf.Make.(List [String name; ArrayInt32 cs_is_es])

    let rpc_create_msg name cs_is_es =
      let args = rpc_create_args name cs_is_es in
      Shm_ipc.Mbf.Make.(Assoc (Rpc.key_of_rpc, [("model_create", args)]))

    let parse_name mc s =
      mc.name <- s; mc

    let parse_cs_is_es mc a =
      Printf.printf "cs_is_es has length %d\n" (Bigarray.Array1.dim a);
      mc

    let parser_of_key k = Shm_ipc.Mbf.([| String parse_name ; RepInt32 parse_cs_is_es |]).(k)

    let rpc_parse_msg a msg_ba ofs len =
    let mc = {
    name = "";
    coords = (0,0);
    indices = (0,0);
    elements = [||];
      } in
      Shm_ipc.Mbf.fold_message msg_ba parser_of_key mc ofs len

  end
  type t_model
  type t_object
  type t_texture
  type t = {
    models   : (string, t_model) Hashtbl.t;
    objects  : (string, t_object) Hashtbl.t;
    textures : (string, t_texture) Hashtbl.t;
    }

  let create _ =
    let models   = Hashtbl.create 128 in
    let objects  = Hashtbl.create 1024 in
    let textures = Hashtbl.create 128 in
    { models; objects; textures }

  let reset t = ()

  let rpc_reset_msg t size =
    let none _ _ = () in
    let msg = Shm_ipc.Mbf.Make.(Assoc (Rpc.key_of_rpc, [("reset", Callback none)])) in
    let actual_size = Shm_ipc.Mbf.Make.write t size (-8) 0 msg in
    actual_size <= size

  let parse_reset t (msg_ba,ofs,len) = 
    reset t;
    t
          
  let parse_model_create t (msg_ba,ofs,len) = 
    Model.rpc_parse_msg t msg_ba ofs len;
    t
                        
  let parser_of_key k = Shm_ipc.Mbf.([| Blob parse_reset ;
(*                                        Blob parse_object_create ;
                                        Blob parse_object_place ;
 *)                                        Blob parse_model_create |]).(k)
  let parse_shm_msg t msg_ba ofs len = 
    Shm_ipc.Mbf.fold_message msg_ba parser_of_key t ofs len

end
  (* reset : unit -> result
  model_create : model_name -> coords -> indices -> triangles/lines/patches -> result
  object_create : object -> model -> result
  object_place  : object -> position -> quaternion -> scale -> result (?time?)
  get_time  : unit -> time result
  set_time  : time -> result


let reset_msg        = Shm_ipc.Mbf.Make.(Assoc (key_of_rpc, [("reset", Callback post_name)]))
let model_create_args name cs_is_es = Shm_ipc.Mbf.Make.(List [String name; ArrayInt cs_is_es])
let model_create_msg = Shm_ipc.Mbf.Make.(Assoc (key_of_rpc, [("model_create", 

  let msg_size = Shm_ipc.Mbf.Make.size 0 0 msg in
   *)

