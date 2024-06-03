(*
    Trees.ml -- (c) 2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Trees.ml implements tools to represent and process phylogenetic trees.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

(* Complete module including parser(s) *)
module Newick:
  sig
    include module type of Trees_Base.Newick
    val of_string: ?rich_format:bool -> string -> t
  end
= struct
    include Trees_Base.Newick
    let of_string ?(rich_format = true) s =
      (* This adds an implicit rooted token to the first tree *)
      let s = "\n" ^ s and state = Trees_Lex.Newick.create ~rich_format () in
      Trees_Parse.newick_tree (Trees_Lex.newick state) (Lexing.from_string ~with_positions:true s)

  end

