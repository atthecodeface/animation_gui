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
 * @file     app.ml
 * @brief    Application code for the animation server
 *
 *)
open Sdl_ogl_gui (* replace with cocoa_ogl_gui if you want... *)
open Tgl4

(*a Useful functions *)
(*f ba creator functions *)
let ba_float_array   len = Bigarray.(Array1.create float32 c_layout len)
let ba_uint16_array  len = Bigarray.(Array1.create int16_unsigned c_layout len)
let ba_uint16s fs = Bigarray.(Array1.of_array int16_unsigned c_layout fs)
let ba_floats  fs = Bigarray.(Array1.of_array float32 c_layout fs)
let ba_uint8_array  len = Bigarray.(Array1.create int8_unsigned c_layout len)

(*f >>= standard monadic function *)
let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

(*c ogl_app_animation_server - viewer app *)
class ogl_app_animation_server stylesheet ogl_displays server: Ogl_gui.Types.t_ogl_app = 
  object (self)
    inherit Ogl_gui.App.ogl_app stylesheet ogl_displays as super
    method create_shaders =
      super#create_shaders >>= 
        fun _ -> (
          let gl_program_desc = Ogl_gui.Program.Gl_program.make_desc "vertex_standard.glsl" "fragment_texture.glsl" [] ["M"; "V"; "G"; "P"; "T0"; "A"; "L_dir"; "L_col"] in
          self#add_program "vnc_vertex" gl_program_desc >>= fun _ ->
          Ok ()
        )

    method create_materials =
      super#create_materials >>=
        fun _ -> (
          self#add_material "vnc_vertex" "vnc_vertex" [|"V"; "M"; "T0"; "A"; "L_dir"; "L_col"|] >>= fun _ ->
          Ok ()
        )

  (*f button_pressed *)
  method button_pressed widget =
    Printf.printf "Button pressed %s\n%!" (widget#get_id);
     ()
end

(*f xml_additions *)
let xml_additions server = 
[
("mbtile", fun app _ name_values ->
    (
      let bg = new Ogl_gui.Obj.ogl_obj_geometry
                     Gl.triangle_strip 4 
                     [| 0; 3; 1; 2; |] (* indices *)
                     [ ( [(0,3,Gl.float,false,11*4,0); (1,3,Gl.float,false,11*4,3*4); (2,3,Gl.float,false,11*4,6*4); (3,2,Gl.float,false,11*4,9*4); ],
                      ba_floats [| -1.;-1.; 0.5;   0.;0.;1.;   0.1;0.4;0.1; 0.;0.;
                                    1.;-1.;0.;    0.;0.;1.;   0.1;0.4;0.0; 1.;0.;
                                    1.;1.;-0.5;     0.;0.;1.;   0.1;0.5;0.1; 1.;1.;
                                    -1.;1.;0.;    0.;0.;1.;   0.1;0.4;0.2; 0.;1.; |] (* vertices, normals, colors, uv *)
                     ) ]
      in
      let objs = [(bg :> Ogl_gui.Obj.ogl_obj); new Widget.ogl_obj_animation] in
      let widget = new Widget.ogl_widget_animation_server app.Ogl_gui.AppBuilder.stylesheet name_values server in
      widget # set_objs objs;
      widget # name_value_args name_values;
      Ogl_gui.AppBuilder.add_child app (widget :> Ogl_gui.Types.t_ogl_widget)
    ))
]

let create server app_xml =
  let stylesheet = Ogl_gui.create_stylesheet () in
  let app_creator displays = (new ogl_app_animation_server stylesheet displays server) in
  Ogl_gui.AppBuilder.create_app_from_xml app_xml stylesheet (xml_additions server) app_creator
