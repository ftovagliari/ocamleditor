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

(** Derives dune files from the project model, keeping them in step with it. *)

type severity = [ `INFO | `WARN | `ERROR ]

(** What the model could not express, or expressed in a way dune reads
    differently. Each one is also emitted as a comment in the file it concerns,
    which is where it is most likely to be read. *)
type diagnostic = {
  dg_severity : severity;
  dg_file     : string;   (** project-relative, e.g. "src/oebuild/dune"; "" if none *)
  dg_target   : string;   (** target name, "" if none *)
  dg_task     : string;   (** task name, "" if none *)
  dg_message  : string;
}

type outcome = {
  oc_changed     : string list;  (** files actually rewritten, project-relative *)
  oc_removed     : string list;
  oc_diagnostics : diagnostic list;
}

val string_of_severity : severity -> string

(** [render project] is the dune files the project describes, as
    (project-relative path, content) pairs, together with the diagnostics
    gathered while deriving them.

    It writes nothing. The filesystem is only read, to tell a directory
    dependency from a file one and to find [.mli] files with no implementation.
    Useful on its own for comparing against hand-written files. *)
val render : Prj.t -> (string * string) list * diagnostic list

(** When set, [write] produces [<dir>/dune.generated] rather than [<dir>/dune]
    and removes nothing, so generated output can be diffed against what is
    already there. Initialized from [OCAMLEDITOR_DUNE_DRY_RUN]. *)
val dry_run : bool ref

(** [write project] brings the dune files under the project source directory in
    line with the model.

    A file is rewritten only when its content actually changes, so mtimes stay
    put and dune is not made to rebuild for nothing; [oc_changed] therefore lists
    only what really moved. A file whose first line is not the generated-by
    marker is left untouched and reported instead, and one that no longer
    corresponds to any target or rule is removed. Directories with neither are
    never touched. *)
val write : Prj.t -> outcome
