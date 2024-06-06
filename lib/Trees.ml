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
    val array_of_string: ?rich_format:bool -> string -> t array
    val of_file: ?rich_format:bool -> string -> t
    val array_of_file: ?rich_format:bool -> string -> t array
  end
= struct
    include Trees_Base.Newick
    let _of_string ?(rich_format = true) f s =
      (* This adds an implicit unrooted token to the first tree *)
      let s = "\n" ^ s and state = Trees_Lex.Newick.create ~rich_format () in
      f (Trees_Lex.newick state) (Lexing.from_string ~with_positions:true s)
    let of_string ?(rich_format = true) s = _of_string ~rich_format Trees_Parse.newick_tree s
    let array_of_string ?(rich_format = true) s =
      _of_string ~rich_format Trees_Parse.zero_or_more_newick_trees s |> Array.of_list
    let _of_file ?(rich_format = true) f s =
      (* Here we have to reimplement buffering due to the initial unrooted tag *)
      let buf = ref "\n" and ic = open_in s and eof_reached = ref false in
      let lexbuf payload n =
        let res = ref 0 in
        while !res = 0 && not !eof_reached do
          let len = String.length !buf in
          if len > 0 then begin
            res := min len n;
            (*Printf.eprintf "READ='%s'\n%!" !buf;*)
            String.blit !buf 0 payload 0 !res;
            buf := String.sub !buf !res (len - !res)
          end else begin
            (* Here buf is empty *)
            res := input ic payload 0 n;
            (*Printf.eprintf "READ='%s'\n%!" (String.sub payload 0 !res);*)
            if !res = 0 then begin
              close_in ic;
              eof_reached := true
            end
          end
        done;
        (* Here either res > 0 or !eof_reached == true *)
        (*Printf.eprintf "RES(%d)='%s'\n%!" !res (String.escaped (String.sub payload 0 !res));*)
        !res
      and state = Trees_Lex.Newick.create ~rich_format () in
      f (Trees_Lex.newick state) (Lexing.from_function ~with_positions:true lexbuf)
    let of_file ?(rich_format = true) s = _of_file ~rich_format Trees_Parse.newick_tree s
    let array_of_file ?(rich_format = true) s =
      _of_file ~rich_format Trees_Parse.zero_or_more_newick_trees s |> Array.of_list
  end

