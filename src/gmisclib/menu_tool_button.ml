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


class menu_tool_button ~(toolbar:GButton.toolbar) ?homogeneous ?stock ?(label="") ?spacing ?packing () =
  let tool_item = GButton.tool_item ?homogeneous ?packing () in
  object
    inherit Button_menu.button_menu ~label ?stock ~relief:`NONE ?spacing ~packing:tool_item#add () as super

    initializer
      if (toolbar#toolbar_style = `ICONS || toolbar#toolbar_style = `BOTH_HORIZ) && stock <> None then begin
        super#button#unset_image();
        List.iter super#button#remove super#button#children;
        super#button#add (GMisc.image ?stock ~icon_size:toolbar#icon_size ())#coerce;
      end;

    method as_tool_item = tool_item
  end

let create = new menu_tool_button










