(*
    Annotations_Formats.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_Formats.ml gathers the per-format modules under one
    name and adds the two runtime dispatchers over them, [Format] for
    the formats that can be read and [Writer] for everything that can
    be written.  It carries no format logic of its own: each reader
    and writer lives in its own file, and the scaffolding they share
    is in [Annotations_Common], which this module re-exports so that
    including it reaches everything.

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
open Annotations_Base
(* Re-exported so that a consumer including this module also gets the
   hierarchy parser, the field codecs and the format interfaces. *)
include Annotations_Common

module GFF3 = Annotations_GFF3.GFF3
module GTF = Annotations_GTF.GTF
module GenBank = Annotations_GenBank.GenBank
module Tabular = Annotations_Tabular.Tabular
module Tbl = Annotations_Tbl.Tbl

(* A serialisable handle on the three formats, used by the
   [AnnoTools] CLI and by any caller that wants to dispatch on
   format at runtime.  The constructor names mirror the module
   names but live in their own namespace ([Format.GFF3] vs
   [GFF3]), so the two never clash. *)
module Format = struct
  type t = GFF3 | GTF | GenBank | Tabular
  let all = [ GFF3; GTF; GenBank; Tabular ]
  let module_of: t -> (module Format_t) = function
    | GFF3 -> (module GFF3)
    | GTF -> (module GTF)
    | GenBank -> (module GenBank)
    | Tabular -> (module Tabular)
  let to_string = function
    | GFF3 -> "gff3"
    | GTF -> "gtf"
    | GenBank -> "genbank"
    | Tabular -> "tsv"
  (* Match the canonical name (lower-cased) plus a small set of
     the informal spellings users tend to type on the command
     line. *)
  let of_string s =
    match String.lowercase_ascii s with
    | "gff3" | "gff" -> GFF3
    | "gtf" -> GTF
    | "genbank" | "gb" -> GenBank
    | "tsv" | "tabular" | "table" -> Tabular
    | _ ->
      Exception.raise __FUNCTION__ Initialize
        (Printf.sprintf "Unknown annotation format %S (have: %s)"
           s (String.concat ", " (List.map to_string all)))
  (* Resolve a dialect name against the format's
     [Format_t.dialects] association list, raising if the name
     is unknown.  Comparison is case-insensitive on the dialect
     key, which lets the CLI accept both [gencode] and
     [Gencode]. *)
  let dialect_of f name =
    let module F = (val module_of f) in
    let q = String.lowercase_ascii name in
    match List.find_opt
      (fun (k, _) -> String.lowercase_ascii k = q) F.dialects with
    | Some (_, h) -> h
    | None ->
      Exception.raise __FUNCTION__ Initialize
        (Printf.sprintf "Unknown dialect %S for format %s (have: %s)"
           name (to_string f)
           (String.concat ", " (List.map fst F.dialects)))
end

(* A serialisable handle on everything that can be WRITTEN, which is every
   format plus the write-only feature table.  Keeping it distinct from
   [Format.t] is what stops [--from-tbl] being expressible: the reading side of
   the CLI dispatches over [Format.t] and the writing side over this, so the
   type system records which formats can do which. *)
module Writer = struct
  type t =
    | Format of Format.t
    | Tbl
  let all = List.map (fun f -> Format f) Format.all @ [ Tbl ]
  let module_of: t -> (module Writer_t) = function
    | Format f ->
      (* [Format_t] includes [Writer_t], so a format's module is already a
         writer; this only narrows the packed signature. *)
      let module F = (val Format.module_of f) in
      (module F: Writer_t)
    | Tbl -> (module Tbl)
  let to_string = function
    | Format f -> Format.to_string f
    | Tbl -> "tbl"
  let of_string s =
    match String.lowercase_ascii s with
    | "tbl" | "feature-table" | "featuretable" -> Tbl
    | _ -> Format (Format.of_string s)
end

