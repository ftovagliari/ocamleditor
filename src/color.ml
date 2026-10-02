(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)


let clamp x = int_of_float (255. *. (max 0. (min x 1.0)));;

let name = function `RGB (red, green, blue) -> Printf.sprintf "#%02X%02X%02X" red green blue

let name_of_gdk color =
  let r = Gdk.Color.red   color / 257 in
  let g = Gdk.Color.green color / 257 in
  let b = Gdk.Color.blue  color / 257 in
  name (`RGB (r, g, b))

let rgb255 f name =
  try Scanf.sscanf name "#%2x%2x%2x" f
  with _ ->
    Scanf.sscanf name "#%1x%1x%1x" (fun r g b -> f (r * 17) (g * 17) (b * 17))

let rgb f name =
  rgb255 (fun r g b ->
      f (float_of_int r /. 255.0)
        (float_of_int g /. 255.0)
        (float_of_int b /. 255.0)) name

let rgb_of_gdk color =
  let r, g, b = (Gdk.Color.red color, Gdk.Color.green color, Gdk.Color.blue color) in
  let r, g, b =
    truncate ((float r) /. 65535. *. 255.),
    truncate ((float g) /. 65535. *. 255.),
    truncate ((float b) /. 65535. *. 255.) in
  r, g, b

let hsv_of_name r g b f =
  let r = (float r) /. 255. in
  let g = (float g) /. 255. in
  let b = (float b) /. 255. in
  let mi = min (min r g) b in
  let ma = max (max r g) b in
  let v = ma in
  let delta = ma -. mi in
  if ma = 0. then f 0. 0. v
  else begin
    let s = delta /. ma in
    let h =
      if r = ma then (g -. b) /. delta
      else if g = ma then 2. +. (b -. r) /. delta
      else 4. +. (r -. g) /. delta
    in
    let h = h *. 60. in
    let h = if h < 0. then h +. 360. else h in
    f h s v
  end;;

let name_of_hsv h s v =
  let r, g, b =
    if s = 0. then v, v, v else begin
      let h = h /. 60. in
      let i = floor h in
      let f = h -. i in
      let p = v *. (1. -. s) in
      let q = v *. (1. -. s *. f) in
      let t = v *. (1. -. s *. (1. -. f)) in
      let i = int_of_float i in
      if i = 0 then v, t, p
      else if i = 1 then q, v, p
      else if i = 2 then p, v, t
      else if i = 3 then p, q, v
      else if i = 4 then t, p, v
      else v, p, q
    end
  in
  let r = clamp r in
  let g = clamp g in
  let b = clamp b in
  Printf.sprintf "#%02x%02x%02x" r g b;;

let add_value name ?(sfact=1.0) x =
  let xs = x *. sfact in
  let xv = x in
  rgb255 hsv_of_name name (fun h s v -> name_of_hsv h (min 1. (s +. xs)) (min 1. (v -. xv)));;

let modify name ~sat ~value =
  rgb255 hsv_of_name name (fun h s v -> name_of_hsv h (min 1. (s +. sat)) (min 1. (v +. value)));;

(*let hsl_of_name r g b =
  let r = (float r) /. 255. in
  let g = (float g) /. 255. in
  let b = (float b) /. 255. in
  let v = max (max r g) b in
  let m = min (min r g) b in
  let l = (m +. v) /. 2. in
  if l <= 0. then (0., 0., 0.)
  else begin
    let vm = v -. m in
    let s = vm in
    if s > 0. then begin
      let s = s /. (if l <= 0.5 then v +. m else 2. -. v -. m) in
      let r2 = (v -. r) /. vm in
      let g2 = (v -. g) /. vm in
      let b2 = (v -. b) /. vm in
      let h =
        if r = v then (if g = m then 5. +. b2 else 1. -. g2)
        else if g = v then (if b = m then 1. +. r2 else 3. -. b2)
        else (if r = m then 3. +. g2 else 5. -. r2)
      in
      h /. 6., s, l
    end else (0., s, l)
  end;;*)

(* Deprecated: use add_value *)
let set_value value =
  let value = if value < 0. then 0.
    else if value > 1.0 then 1.
    else value
  in fun color ->
    let gdk = GDraw.color color in
    `RGB (
      (int_of_float (float (Gdk.Color.red gdk) *. value)),
      (int_of_float (float (Gdk.Color.green gdk) *. value)),
      (int_of_float (float (Gdk.Color.blue gdk) *. value))
    );;

let avg (r, g, b) = (float r) /. 256. +. (float g) /. 256. +. (float b) /. 256.

