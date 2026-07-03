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


let magic = "1";;

type 'a dump = Dump of string * 'a

exception Bad_magic_number
exception Invalid_linechar of Merlin_j.pos

let open_dump = function Dump (m, a) ->
  if m = magic then a
  else raise Bad_magic_number;;

(** Symbol cache *)

type symbol_kind =
  | Pvalue
  | Pfunc
  | Pattribute
  | Pmethod | Pmethod_private | Pmethod_virtual | Pmethod_private_virtual
  | Ptype
  | Plabel
  | Pconstructor
  | Pexception
  | Pmodule
  | Pmodtype
  | Pclass
  | Pcltype
  | Ptype_abstract | Ptype_variant | Ptype_record
  | Std_lib
  | Lib

type symbol = {
  sy_id                       : string list; (* Value path, head is root *)
  sy_kind                     : symbol_kind;
  sy_type                     : string;
  sy_filename                 : string;
  sy_local                    : bool;
}

type symbol_cache = {
  mutable syt_table           : symbol list;
  mutable syt_ts              : (string, float) Hashtbl.t; (* filename * last read *)
  mutable syt_odoc            : (Odoc_info.Name.t * symbol_kind option, Odoc_info.Search.result_element) Hashtbl.t; (* value_path * markup *)
  syt_critical                : Mutex.t;
}


(** Annot *)
type annot = {
  annot_blocks                : annot_block list;
  annot_mtime                 : float;
}

and annot_block = {
  annot_start                 : annot_position;
  annot_stop                  : annot_position;
  annot_annotations           : annotation list
}

and annot_position = {
  annot_fname                 : string;
  annot_lnum                  : int;
  annot_bol                   : int;
  annot_cnum                  : int;
}

and annotation =
  | Type of string
  | Def of (string * annot_position * annot_position option) (* scope *)
  | Int_ref of (string * annot_position * annot_position) (* location of the definition *)
  | Ext_ref of string
  | Stack
  | Tail


(** Errors *)
type error_level =
  | Warning of int * string option
  | Alert of string
  | Error

type error_message = {
  er_filename                 : string;
  er_level                    : error_level;
  er_lines                     : int * int;
  er_characters               : int * int;
  er_location                 : string;
  er_message                  : string;
}

type error = {
  er_warnings                 : error_message list;
  er_errors                   : error_message list;
}


(** Bookmarks *)
type bm_location = Mark of Gtk.text_mark | Offset of int

type bookmark = {
  bm_num            : int;
  bm_filename       : string;
  mutable bm_loc    : bm_location;
  mutable bm_marker : Gutter.marker option;
}


(** Browser *)
type browser_maximized_view_action = {
  mva_menubar                 : bool;
  mva_toolbar                 : bool;
  mva_tabbar                  : bool;
  mva_messages                : bool;
  mva_fullscreen              : bool;
  mva_decorated               : bool;
}

class type outline =
  object
    method attach : unit -> unit
    method detach : unit -> unit
    method get : Merlin_j.outline list
    method is_valid : bool
    method update : ?force:bool -> unit -> unit
    method connect : outline_signals
  end
and outline_signals =
  object ('a)
    method after : 'a
    method changed : callback:(unit -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> unit
  end
