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

open Better

(* Complete modules including parser(s) and I/O *)

module Newick:
  sig
    include module type of Trees_Base.Newick
    (* Input *)
    val of_string: ?rich_format:bool -> string -> t
    val array_of_string: ?rich_format:bool -> string -> t array
    val of_file: ?rich_format:bool -> string -> t
    val array_of_file: ?rich_format:bool -> string -> t array
    (* Output *)
    val to_string: ?rich_format:bool -> t -> string
    val array_to_string: ?rich_format:bool -> t array -> string
    val to_file: ?rich_format:bool -> t -> string -> unit
    val array_to_file: ?rich_format:bool -> t array -> string -> unit
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
    let add_to_buffer ?(rich_format = true) buf t =
      let add_hybrid_info buf hy =
        match rich_format, hy with
        | true, Some (Hybridization id) -> Printf.bprintf buf "#H%d" id
        | true, Some (GeneTransfer id) -> Printf.bprintf buf "#LGT%d" id
        | true, Some (Recombination id) -> Printf.bprintf buf "#R%d" id
        | true, None | false, _ -> ()
      and add_dict_info buf dict =
        if rich_format && dict <> StringMap.empty then begin
          Buffer.add_string buf "[&";
          StringMap.iteri
            (fun i k v ->
              if i > 0 then
                Buffer.add_char buf ',';
              Printf.bprintf buf "%s=%s" k v)
            dict;
          Buffer.add_char buf ']'
        end in
      begin match rich_format, get_is_root t with
      | true, true -> Buffer.add_string buf "[&R]"
      | true, false -> Buffer.add_string buf "[&U]"
      | false, _ -> ()
      end;
      dfs_iter
        (fun _ num_edges ->
          if num_edges > 0 then
            Buffer.add_char buf '(')
        (fun i _ ->
          if i > 0 then
            Buffer.add_char buf ',')
        (fun _ edge ->
          let dict = get_edge_dict edge in
          begin match get_edge_values edge with
          | -1., -1., -1. ->
            if dict <> StringMap.empty then
              (* Here the only unambiguous way to print out things is by adding an empty length *)
              Buffer.add_string buf ":0"
          | -1., -1., p -> Printf.bprintf buf ":::%.10g" p
          | -1., b, -1. -> Printf.bprintf buf "::%.10g" b
          | l, -1., -1. -> Printf.bprintf buf ":%.10g" l
          | -1., b, p -> Printf.bprintf buf "::%.10g:%.10g" b p
          | l, -1., p -> Printf.bprintf buf ":%.10g::%.10g" l p
          | l, b, -1. -> Printf.bprintf buf ":%.10g:%.10g" l b
          | l, b, p -> Printf.bprintf buf ":%.10g:%.10g:%.10g" l b p
          end;
          add_dict_info buf dict)
        (fun node num_edges ->
          if num_edges > 0 then
            Buffer.add_char buf ')';
          get_node_name node |> Buffer.add_string buf;
          get_node_hybrid node |> add_hybrid_info buf;
          get_node_dict node |> add_dict_info buf)
        t;
      Buffer.add_char buf ';';
      buf
    let to_string ?(rich_format = true) t =
      add_to_buffer ~rich_format (Buffer.create 1024) t |> Buffer.contents
    let array_to_string ?(rich_format = true) a =
      let buf = Buffer.create 1024 in
      Array.iter (fun t -> Buffer.add_char (add_to_buffer ~rich_format buf t) '\n') a;
      Buffer.contents buf
    let to_file ?(rich_format = true) t f =
      let f = open_out f and buf = Buffer.create 1024 in
      Buffer.add_char (add_to_buffer ~rich_format buf t) '\n';
      Buffer.output_buffer f buf;
      close_out f
    let array_to_file ?(rich_format = true) a f =
      let f = open_out f and buf = Buffer.create 1024 in
      Array.iter (fun t -> Buffer.add_char (add_to_buffer ~rich_format buf t) '\n') a;
      Buffer.output_buffer f buf;
      close_out f
  end

module Splits:
  sig
    include module type of Trees_Base.Splits
    (* Input *)
    val of_string: string -> t
    val array_of_string: string -> t array
    val of_file: string -> t
    val array_of_file: string -> t array
    exception Incompatible_archive_version of string * string
    val of_channel: in_channel -> t
    val of_binary: ?verbose:bool -> string -> t
    (* Output *)
    val to_string: ?precision:int -> t -> string
    val array_to_string: ?precision:int -> t array -> string
    val to_file: ?precision:int -> t -> string -> unit
    val array_to_file: ?precision:int -> t array -> string -> unit
    val to_channel: out_channel -> t -> unit
    val to_binary: ?verbose:bool -> t -> string -> unit
  end
= struct
    include Trees_Base.Splits
    (* Input *)
    let _of_string f s =
      let state = Trees_Lex.Splits.create () in
      f (Trees_Lex.splits state) (Lexing.from_string ~with_positions:true s)
    let of_string = _of_string Trees_Parse.split_set
    let array_of_string s = _of_string Trees_Parse.zero_or_more_split_sets s |> Array.of_list
    let make_filename_text = function
      | w when String.length w >= 5 && String.sub w 0 5 = "/dev/" -> w
      | prefix -> prefix ^ ".PhyloSplits.txt"
    let _of_file f prefix =
      let fname = make_filename_text prefix in
      let input = open_in fname and state = Trees_Lex.Splits.create () in
      let res = f (Trees_Lex.splits state) (Lexing.from_channel ~with_positions:true input) in
      close_in input;
      res
    let of_file = _of_file Trees_Parse.split_set
    let array_of_file s = _of_file Trees_Parse.zero_or_more_split_sets s |> Array.of_list
    (* Output *)
    let add_to_buffer ?(precision = 15) buf t =
      let names = get_names t in
      if Array.length names > 0 then begin
        Array.iteri
          (fun i name ->
            if i > 0 then
              Buffer.add_char buf ' ';
            Trees_Lex.quote_string_if_needed name |> Buffer.add_string buf)
          names;
        let num_splits = cardinal t in
        if num_splits > 0 then begin
          Buffer.add_char buf ':';
          iter (fun split weight -> Printf.bprintf buf " 0d%s#%.*g" (Split.to_string split) precision weight) t
        end;
        Buffer.add_char buf ';'
      end;
      buf
    let to_string ?(precision = 15) t =
      add_to_buffer ~precision (Buffer.create 1024) t |> Buffer.contents
    let array_to_string ?(precision = 15) a =
      let buf = Buffer.create 1024 in
      Array.iter (fun t -> Buffer.add_char (add_to_buffer ~precision buf t) '\n') a;
      Buffer.contents buf
    let to_file ?(precision = 15) t prefix =
      let fname = make_filename_text prefix in
      let output = open_out fname and buf = Buffer.create 1024 in
      Buffer.add_char (add_to_buffer ~precision buf t) '\n';
      Buffer.output_buffer output buf;
      close_out output
    let array_to_file ?(precision = 15) a prefix =
      let fname = make_filename_text prefix in
      let output = open_out fname and buf = Buffer.create 1024 in
      Array.iter (fun t -> Buffer.add_char (add_to_buffer ~precision buf t) '\n') a;
      Buffer.output_buffer output buf;
      close_out output
    (* *)
    let archive_version = "2025-02-05"
    (* *)
    let make_filename_binary = function
      | w when String.length w >= 5 && String.sub w 0 5 = "/dev/" -> w
      | prefix -> prefix ^ ".PhyloSplits"
    let to_channel output ss =
      archive_version |> output_value output;
      output_value output ss
    let to_binary ?(verbose = false) ss prefix =
      let fname = make_filename_binary prefix in
      let output = open_out fname in
      if verbose then
        Printf.eprintf "(%s): Outputting DB to file '%s'...%!" __FUNCTION__ fname;
      to_channel output ss;
      close_out output;
      if verbose then
        Printf.eprintf " done.\n%!"
    exception Incompatible_archive_version of string * string
    let of_channel input =
      let version = (input_value input: string) in
      if version <> archive_version then
        Incompatible_archive_version (version, archive_version) |> raise;
      (input_value input: t)
    let of_binary ?(verbose = false) prefix =
      let fname = make_filename_binary prefix in
      let input = open_in fname in
      if verbose then
        Printf.eprintf "(%s): Reading DB from file '%s'...%!" __FUNCTION__ fname;
      let res = of_channel input in
      close_in input;
      if verbose then
        Printf.eprintf " done.\n%!";
      res
  end

