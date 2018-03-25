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
open Sdl_ogl_gui (* replace with cocoa_ogl_gui if you want... *)
open Atcflib
open Tgl4
module Option   = Batteries.Option

(*a Useful functions *)
(*f trace - use with trace __POS__ *)
let trace pos = 
    let (a,b,c,d) = pos in
    Printf.printf "trace:%s:%d:%d:%d\n%!" a b c d

(*f >>= standard monadic function *)
let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

(*f ba creator functions *)
let ba_float_array   len = Bigarray.(Array1.create float32 c_layout len)
let ba_uint16_array  len = Bigarray.(Array1.create int16_unsigned c_layout len)
let ba_uint16s fs = Bigarray.(Array1.of_array int16_unsigned c_layout fs)
let ba_floats  fs = Bigarray.(Array1.of_array float32 c_layout fs)
let ba_uint8_array  len = Bigarray.(Array1.create int8_unsigned c_layout len)
type t_ba_float32s = (float, Bigarray.float32_elt,        Bigarray.c_layout) Bigarray.Array1.t
type t_ba_uint16s  = (int,   Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(*f gl_int_val, gl_with_int - to feed ints and get ints back from ctypes Opengl *)
let ba_int32_1    = Bigarray.(Array1.create int32 c_layout 1)
let ba_int32s len = Bigarray.(Array1.create int32 c_layout len)
let gl_int_val  f   = f ba_int32_1 ; Int32.to_int ba_int32_1.{0}
let gl_with_int f i = ba_int32_1.{0} <- Int32.of_int i; f ba_int32_1

(*a Global variables - colors, lights for now *)
(*v colors, lights *)
let light       = ba_floats [| (0.5); (0.5); (0.71)|]
let ambient_col = ba_floats [| (0.7); (0.7); (0.7)|]
let light_col   = ba_floats [| (0.5); (0.5); (0.5)|]

(*a Ogl_obj arrays *)
(*m OOVnc_type - vertex/normal/color type with 11 floats per coord *)
module OOVnc_type =
struct
  let fpc = 11 (* 11 floats per coordinate - vertex, normal, color, tex coord *)
end

(*m OOVnc - extendable arrays of coordinates and indices *)
module OOVnc =
struct
  include Ogl_gui.Obj.Arrays(OOVnc_type)

  let add_xy t x y nx ny nz cr cg cb u v =
    let n = t.num_cs in
    t.num_cs <- t.num_cs + 1;
    t.cs.{11*n+0} <- x;
    t.cs.{11*n+1} <- y;
    t.cs.{11*n+2} <- 0.;
    t.cs.{11*n+3} <- nx;
    t.cs.{11*n+4} <- ny;
    t.cs.{11*n+5} <- nz;
    t.cs.{11*n+6} <- cr;
    t.cs.{11*n+7} <- cg;
    t.cs.{11*n+8} <- cb;
    t.cs.{11*n+9} <- u;
    t.cs.{11*n+10} <- v;
    ()

end

(*a Texture reading *)
(*c image_ba *)
let map_filename = "sample.png"
let image_ba = 
   Printf.printf "Open image %s\n" map_filename;
   let image = ImageLib.openfile map_filename in
   Printf.printf "Opened image %s\n" map_filename;
   let (r,g,b,a) =
     match image.pixels with
     | RGB (r,g,b)    -> (r,g,b,r)
     | RGBA (r,g,b,a) -> (r,g,b,a)
     | _ -> raise Not_found
    in
    let r = match r with Pix8 r -> r | _ -> raise Not_found in
    let g = match g with Pix8 g -> g | _ -> raise Not_found in
    let b = match b with Pix8 b -> b | _ -> raise Not_found in
    let a = match a with Pix8 a -> a | _ -> raise Not_found in
    let width = Bigarray.Array2.dim1 r in
    let height = Bigarray.Array2.dim2 r in
    let ba = ba_uint8_array (width*height*4) in
    for x=0 to (width-1) do
        for y=0 to (height-1) do
            ba.{(x+y*width)*4+0} <- r.{x,y};
            ba.{(x+y*width)*4+1} <- g.{x,y};
            ba.{(x+y*width)*4+2} <- b.{x,y};
            ba.{(x+y*width)*4+3} <- (if (a!=r) then 255 else a.{x,y});
        done
    done;
    (width, height, ba)

(*a Obj class *)
(*c ogl_obj_animation *)
class ogl_obj_animation =
    object (self)
      inherit Ogl_gui.Obj.ogl_obj as super
      val mutable plot_pts = [];
      val mutable plot_strips = [];
      method create_geometry ~offset =
        let oovnc_t = OOVnc.create 1024 1024 10 in
        (*OOVnc.ensure oovnc_t num_vncs num_is;*)
        OOVnc.add_xy oovnc_t (-1.) (-1.)  0. 0. 1.   0. 1. 1.  0. 0.;
        OOVnc.add_xy oovnc_t (-1.) 1.     0. 0. 1.   1. 0. 1.  0. 1.;
        OOVnc.add_xy oovnc_t 1. 1.        0. 0. 1.   1. 1. 0.  1. 1.;
        OOVnc.add_xy oovnc_t 1. (-1.)     0. 0. 1.   1. 1. 1.  1. 0.;
        ignore (OOVnc.add_index oovnc_t 0);
        ignore (OOVnc.add_index oovnc_t 3);
        ignore (OOVnc.add_index oovnc_t 1);
        ignore (OOVnc.add_index oovnc_t 2);
        OOVnc.add_strip oovnc_t 0 4;
        plot_pts    <- OOVnc.points oovnc_t;
        plot_strips <- OOVnc.strips oovnc_t;
        OOVnc.display oovnc_t;
        Printf.printf "Got %d points to plot and %d strips to plot\n" (List.length plot_pts) (List.length plot_strips);
        self # create_vao [ ( [ (0,3,Gl.float,false,(11*4),0);     (* vertices *)
                                (1,3,Gl.float,false,(11*4),(3*4)); (* normals *)
                                (2,3,Gl.float,false,(11*4),(6*4)); (* colors *)
                                (3,2,Gl.float,false,(11*4),(9*4)); (* UVs *)
                                ], OOVnc.cs oovnc_t)
          ] >>= fun _ -> (
        self # add_indices_to_vao (OOVnc.is oovnc_t);
        Ok ())
      method draw view_set other_uids =
        Gl.bind_vertex_array vao_glid;
        List.iter (fun (ofs,num)->Gl.draw_elements Gl.points num Gl.unsigned_short (`Offset (ofs*2))) plot_pts;
        List.iter (fun (ofs,num)->Gl.draw_elements Gl.triangle_strip num Gl.unsigned_short (`Offset (ofs*2))) plot_strips;
        Gl.bind_vertex_array 0;
        ()
    end

(*a Widget viewer class *)
(*c ogl_widget_animation_server  - viewer widget *)
class ogl_widget_animation_server stylesheet name_values server =
  object (self)
    inherit Ogl_gui.Widget.ogl_widget_viewer stylesheet name_values as super
    val location = Array.make 3 0.;    
    val mutable angle=0.;
    val mutable tex_glid = -1;

    method create_geometry =
      let (w,h,ba) = image_ba in
      tex_glid <- Ogl_gui.Texture.Texture.create_from_ba w h ba;
      super # create_geometry

    (*f mouse - handle a mouse action along the action vector *)
    method create app =
      opt_material <- Some (app#get_material "vnc_vertex") ;
      scale := 2.0;
      super#create app

    (*f draw_content *)
    method draw_content view_set transformation =
      if (Option.is_none opt_material) then () else
      begin    
        let material = (Option.get opt_material) in
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.identity translation);
        ignore (Matrix.set 0 3 (-. (Atcflib.Vector.get center 0)) translation);
        ignore (Matrix.set 1 3 (-. (Atcflib.Vector.get center 1)) translation);
        ignore (Matrix.set 2 3 (-. (Atcflib.Vector.get center 2)) translation);
        ignore (Matrix.assign_m_m rotation translation view);
        let ar_scale = (min (super#get_content_draw_dims).(0) (super#get_content_draw_dims).(1)) *. 0.35 *. !scale in
        ignore (Matrix.(set 1 1 ar_scale (set 0 0 ar_scale (identity tmp))));  (* Make -1/1 fit the width - but do not scale z *)
        ignore (Matrix.assign_m_m tmp view tmp2);  (* Make -1/1 fit the width - but do not scale z *)
        let other_uids = Ogl_gui.View.Ogl_view.set view_set (Some material) transformation in
        Gl.uniform_matrix4fv other_uids.(0) 1 true (Ogl_gui.Utils.ba_of_matrix4 tmp2); (* 0 -> V *)
        Gl.uniform_matrix4fv other_uids.(1) 1 true Ogl_gui.Utils.identity4; (* 1 -> M *)

        light.{0} <- 0.7 *. (sin angle);
        light.{1} <- 0.7 *. (cos angle);
        Gl.active_texture Gl.texture0 (* + shader *);
        Gl.bind_texture   Gl.texture_2d tex_glid;
        Gl.point_size 4.0;
        Gl.uniform3fv other_uids.(3) 1 ambient_col;
        Gl.uniform3fv other_uids.(4) 1 light;
        Gl.uniform3fv other_uids.(5) 1 light_col;
        Gl.cull_face Gl.back;
        (*Gl.enable Gl.cull_face_enum;*)
        Gl.uniform1i      other_uids.(2) 0 (* T0 = texture sampler 0 *);

        List.iter (fun o -> o#draw view_set other_uids) objs;
        let anim_obj i o =
           Animation_server.Object.draw o other_uids
        in
        Animation_server.AnimationServer.iter_objects server anim_obj;
        Gl.bind_vertex_array 0;
      end

    (*f idle *)
    method idle _ = 
      Animation_server.AnimationServer.idle server;
      if self # is_key_down ',' then self#move_forward ((-0.1) /. !scale);
      if self # is_key_down 'l' then self#move_forward (0.1 /. !scale);
      if self # is_key_down 'q' then self#move_left ((-0.01) /. !scale);
      if self # is_key_down 'w' then self#move_left (0.01 /. !scale);
      if self # is_key_down '.' then self#pitch 0.005;
      if self # is_key_down ';' then self#pitch (-0.005);
      if self # is_key_down 'x' then self#yaw 0.005;
      if self # is_key_down 'z' then self#yaw (-0.005);
      if self # is_key_down 's' then self#roll 0.005;
      if self # is_key_down 'a' then self#roll (-0.005);
      if self # is_key_down '\'' then scale := !scale *. 1.05;
      if self # is_key_down '/' then  scale := !scale /. 1.05;
      let v = self # joystick_axis_value 1 in
      if (v!=0) then self # move_forward ((float (-v)) /. 32768.0 /. 120.);
      let v = self # joystick_axis_value 0 in
      if (v!=0) then self # move_left ((float (-v)) /. 32768.0 /. 120.);
      let v = self # joystick_axis_value 2 in
      if (v!=0) then self # yaw ((float v) /. 32768.0 /. 40.);
      let v = self # joystick_axis_value 3 in
      if (v!=0) then self # pitch ((float v) /. 32768.0 /. 40.);
        let anim_obj i o =
          Printf.printf "Model %d\n" i;
        in
        Animation_server.AnimationServer.iter_objects server anim_obj;
      if self # is_key_down '=' then None
      else if not (Animation_server.AnimationServer.is_alive server) then None
      else
        (self#request_redraw ; Some 10)

    method mouse action mouse vector options = None
end

