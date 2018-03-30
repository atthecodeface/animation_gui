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
  type t
  type t_model
  type t_object
  type t_texture
  val create_model       : t -> int -> t_ba_float32s -> t_ba_uint16s -> (int * int) array -> t_model option
  val create_texture     : t -> int -> int -> int -> int -> t_ba_chars -> t_texture option
  val create_object      : t -> int -> t_model -> t_object option
  val object_set_target  : t -> t_object -> float -> int -> t_ba_float32s -> unit
  val delete_model       : t -> t_model -> unit
  val delete_object      : t -> t_object -> unit
  val delete_texture     : t -> t_texture -> unit
  val animate            : t -> int -> float -> (int * float)
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
      let opt_model = A.create_model a.parent mc.id cs is mc.elements in
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

    (*f rpc_animate_parse_msg - parse object set target message *)
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
    Rpc.add_type "object_create"      (Shm_ipc.Mbf.Blob parse_object_create);
    Rpc.add_type "object_delete"      (Shm_ipc.Mbf.Int parse_object_delete);
    Rpc.add_type "object_set_target"  (Shm_ipc.Mbf.Blob parse_object_set_target);
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

    let create _ = 
      let client = Shm_server.Client.create () in
      let t:bct = {client;} in
      ignore (animation_create t);
      t

    let client (t:bct) = t.client

    let msg_alloc (t:bct) byte_size =
      Shm_server.Client.msg_alloc t.client byte_size

    let poll_idle (t:bct) delay =
      let msg_callback rx_id msg msg_ba =
        ignore (Shm_server.Client.msg_free t.client msg);
        (Some ())
      in
      Shm_server.Client.poll t.client msg_callback delay

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

    let send (t:bct) msg id =  
      ignore (Shm_server.Client.send t.client msg);
      ()

    let send_and_wait ?delay:(delay=100000) ?callback (t:bct) msg id =  
      send t msg id;
      poll_for_msg t ?callback:callback id delay

    let reset t =
      let (msg,msg_ba,id) = msg_alloc t 1024 in
      ignore (rpc_reset_msg msg_ba 1024);
      send_and_wait t msg id

    let move_to t time id x y z =
      let (msg,msg_ba,msg_id) = msg_alloc t 1024 in
      ignore (rpc_object_set_target_msg msg_ba 1024 id time 1 [|x;y;z;1.;|]);
      send_and_wait t msg msg_id

    let animate t why time =
      let (msg,msg_ba,id) = msg_alloc t 1024 in
      ignore (rpc_animate_msg msg_ba 1024 why time);
      send_and_wait t msg id

    let wait_for_time t time =
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
        send_and_wait ~callback:current_time_cb t msg id;
        match !current_time with 
        | Some sim_time -> (
          let delay = int_of_float ((time-.sim_time) *. 100000.) in
          Printf.printf "Time %f %f delay %d\n%!" time sim_time delay;
          if delay<1000 then () else (poll_idle t delay; wait ())
        )
        | _ -> ()
      in
      wait ()


(*

  let create_model_ba model_id coords indices element_type num_elements =
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

 *)
end


  (*
  object_place  : object -> position -> quaternion -> scale -> result (?time?)
   *)

