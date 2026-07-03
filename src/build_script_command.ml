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


open Printf

type t = [`Show | `Build | `Install | `Uninstall | `Install_lib | `Clean | `Distclean]

let commands = [`Show; `Build; `Install; `Uninstall; `Install_lib; `Clean; `Distclean]

exception Unrecognized_command of string

let string_of_command = function
  | `Show -> "show"
  | `Build -> "build"
  | `Install -> "install"
  | `Uninstall -> "uninstall"
  | `Install_lib -> "install-lib"
  | `Clean -> "clean"
  | `Distclean -> "distclean"

let command_of_string = function
  | "show" -> `Show
  | "build" -> `Build
  | "install" -> `Install
  | "uninstall" -> `Uninstall
  | "install-lib" -> `Install_lib
  | "clean" -> `Clean
  | "distclean" -> `Distclean
  | x -> raise (Unrecognized_command (sprintf "'%s' is not a recognized command." x));;

let code_of_command = function
  | `Show -> "`Show"
  | `Build -> "`Build"
  | `Install -> "`Install"
  | `Uninstall -> "`Uninstall"
  | `Install_lib -> "`Install_lib"
  | `Clean -> "`Clean"
  | `Distclean -> "`Distclean"
