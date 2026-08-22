(*
    Annotations_Tbl.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_Tbl.ml writes the NCBI submission feature table
    consumed by [table2asn].  The format cannot be read back into a
    register -- it has no slot for a hierarchy and [table2asn] infers
    the gene/mRNA/CDS relations it does not carry -- so this module
    satisfies [Writer_t] rather than [Format_t].

    This program was designed and developed by the author(s),
    with the assistance of the following AI tool(s):
      2026 Claude (Anthropic).
    The final logic and implementation were reviewed and verified in
    their entirety by the author(s).

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
open Annotations_Common
(* The base AST, unqualified, as every format reader and writer uses it. *)
open Annotation

(* NCBI's submission feature table, the [.tbl] that table2asn consumes.  It is
   write-only by nature rather than by omission: it has no slot for GFF3's
   source column, no parent link and no annotation metadata, and table2asn
   INFERS the gene/mRNA/CDS relations from coordinate overlap and shared
   identifiers instead of reading them.  Inference is not encoding, so a
   register cannot be recovered from one, and the format therefore satisfies
   [Writer_t] rather than [Format_t].
   It is also a stanza format wearing a TSV costume -- a block header, coordinate
   lines, and tab-indented qualifier continuation lines -- so it is NOT the
   awk-per-record shape the tabular format above provides.  The two exist for
   different jobs. *)
module Tbl: Writer_t = struct
  let to_buffer buf ann =
    (* One [>Feature] block per sequence, its features in register order. *)
    let by_seq = Hashtbl.create 16 and order = ref [] in
    iter_paths (fun ~path feature ->
      let seq = seq_name ann feature in
      if not (Hashtbl.mem by_seq seq) then List.accum order seq;
      let prev = try Hashtbl.find by_seq seq with Not_found -> [] in
      Hashtbl.replace by_seq seq ((path, feature) :: prev)) ann;
    List.iter (fun seq ->
      Printf.bprintf buf ">Feature %s\n" seq;
      (try Hashtbl.find by_seq seq with Not_found -> [])
        |> List.rev
        |> List.iter (fun (path, feature) ->
          let category = match List.rev path with leaf :: _ -> leaf | [] -> "" in
          let reverse =
            match feature.strand with
            | Some (Sequences.Types.Reverse _) -> true
            | _ -> false in
          (* There is no strand column: the minus strand is spelled by giving a
             range stop-first.  Intervals also run 5' to 3' ALONG THE FEATURE
             rather than along the sequence, so a minus-strand feature lists
             them in the reverse of the genomic order they are stored in --
             which is also the correct reading of complement(join(A,B)), namely
             B' followed by A'. *)
          let ivs = if reverse then List.rev feature.intervals else feature.intervals in
          List.iteri (fun i (iv: Sequences.Types.simple_interval_t) ->
            if iv.length = 0 then
              Exception.raise __FUNCTION__ IO_Format
                (Printf.sprintf
                   "Feature %S on %S is zero-length, which a feature table has no way to spell"
                   category seq);
            let lo, hi = OneBased.bounds iv in
            let a, b = if reverse then hi, lo else lo, hi in
            if i = 0 then Printf.bprintf buf "%d\t%d\t%s\n" a b category
            else Printf.bprintf buf "%d\t%d\n" a b) ivs;
          (* Qualifiers sit on their own continuation lines, indented by three
             empty columns.  A valueless one simply omits the fifth. *)
          let has_codon_start = ref false in
          attr_iter ann (fun k vs ->
            if k = "codon_start" then has_codon_start := true;
            match vs with
            | [] -> Printf.bprintf buf "\t\t\t%s\n" k
            | _ ->
              List.iter (fun v ->
                if v = "" then Printf.bprintf buf "\t\t\t%s\n" k
                else Printf.bprintf buf "\t\t\t%s\t%s\n" k v) vs) feature;
          (* A phase that arrived in a GFF3 column rather than as a qualifier
             still has to travel, and /codon_start is the only slot for it.
             It is 1-based against the 0-based phase. *)
          (match feature.phase with
           | Some n when not !has_codon_start ->
             Printf.bprintf buf "\t\t\tcodon_start\t%d\n" (n + 1)
           | _ -> ()))) (List.rev !order)
  let to_string = to_string_via_buffer to_buffer
  let to_file = to_file_via_buffer to_buffer
end

