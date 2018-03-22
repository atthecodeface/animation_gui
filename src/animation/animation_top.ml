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

(*v app_xml *)
let app_xml = "<?xml?><app>
<window width='1000' height='800' dims='100,100,100' fill='3,3,3' border='1,1,1,1,1,1' border_color='0.3,0.3,0.3' align='0,1,0'>
  <grid fill='3,3,3' align='0,0,0' id='main_grid'>
    <grid_span axis='x' weights='1.0,0.0,0.0'/>
    <grid_span axis='y' weights='1.0,0'/>
    <grid_span axis='z'/>
    <grid_element base='0,1,0'>
      <label text='Mbtile' font_size='15' border_color='0.5,0.1,0.1' fill='3,0,0'/>
    </grid_element>
    <grid_element base='0,0,0'>
      <mbtile dims='50,50,100' fill='3,3,3' border='1,1,1,1,1,1' border_color='0.9,0.9,0.9' id='viewer'/>
    </grid_element>
  </grid>
</window>
</app>"

(*a Top level *)
let main () =
  let exec = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf "Usage: %s [OPTION]\nPlots something\nOptions:" exec in
  let options =
    [ ]
  in
  let anon _ = raise (Arg.Bad "no arguments are supported") in
  Arg.parse (Arg.align options) anon usage;

  let server = Shm_server.Server.create () in
  let x = Animation.Animation.create () in
  match App.create server app_xml with
    None -> 
    (
      Printf.printf "Failed to create app\n"; exit 1
    )
  | Some app ->
     (
       match (Sdl_ogl_gui.run_app ~ogl_root_dir:"." app) with
         Ok () -> exit 0
       | Error msg -> Printf.printf "%s\n" msg; exit 1
     )

let _ =
  main ()


