(*
    Annotations.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations.ml is the public API of the annotation subsystem.  It
    re-exports [Annotations_Common] whole -- the [Hierarchy] parser,
    [GenBankLocation], the field codecs and the [Writer_t] /
    [Format_t] interfaces -- names the five per-format modules that
    live in files of their own, dispatches over them through [Format]
    and [Writer], and re-exports the [Path], [Seq], [AttrKey],
    [AttrMap], [Value] and [ValueTable] interning modules from
    [Annotations_Base], so that a consumer needs one import.  To those
    it adds what belongs to no single format: the [Annotation] AST
    extended with binary I/O, the validation actions and the
    DNA/protein extraction primitives, plus [Selection], the predicate
    that picks a subset of a register's features.

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
(* [Annotations_Common] itself includes [Annotations_Base], so this one line
   carries the whole spine: the AST and its interning tables from the base, and
   on top of them the file and buffer plumbing, the FASTA renderer, the
   [Hierarchy] parser, the field codecs, the [GenBankLocation] AST and the
   [Writer_t] / [Format_t] interfaces.  A consumer of [Annotations] therefore
   reaches all of it from one import, and there is no list of re-exports to
   drift out of step with what the base holds -- which is what happened to the
   list this replaces, [GenBankRecord] having been left out of it although
   [GenBank.parse_records] returns one.
   The scaffolding cannot live in this file: the per-format modules below sit
   between it and here, so folding it in would close a cycle.
   The constraint is what lets [Annotations_Common] be private: an unconstrained
   include re-exports by aliasing, and an alias into a private module does not
   resolve.  [module type of] is the constraint that repeats nothing -- it names
   the signature the module already has, and asks for a copy of it. *)
include (Annotations_Common: module type of Annotations_Common)

(* The per-format readers and writers, each in its own file because together
   they were seven eighths of what this module used to be.
   Each include is CONSTRAINED, and deliberately so: including a module path
   unconstrained re-exports its contents as aliases, and an alias into a module
   that dune has made private does not resolve, so the file could not then be
   sealed.  Naming the signature copies it instead, which both closes the door
   behind each format module and writes down what it offers -- and, since these
   signatures are the ones the modules already declare, saying so twice costs
   four lines and is checked by the compiler. *)
include (Annotations_GFF3: sig
    module GFF3: sig include Format_t val gencode_hierarchy: Hierarchy.t end
  end)
include (Annotations_GTF: sig module GTF: Format_t end)
include (Annotations_GenBank: sig
    module GenBank: sig
        include Format_t
        val parse_records: string -> GenBankRecord.t list
      end
  end)
include (Annotations_Tabular: sig module Tabular: Format_t end)
include (Annotations_Tbl: sig module Tbl: Writer_t end)

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

(* Annotation: the AST module from [Annotations_Base], extended
   with binary I/O (mirroring [Trees.Splits.{to,of}_binary]: the
   archive is an OCaml [Marshal]-encoded value preceded by a
   version string; the default file suffix is [.Annotation]
   unless the prefix points under [/dev/*]) and validation
   actions.  Each [validate_*] raises on failure and requires a
   reference to be set on the annotation; calling without one
   raises. *)
module Annotation:
  sig
    include module type of Annotations_Base.Annotation
    val to_binary: ?verbose:bool -> t -> string -> unit
    val of_binary: ?verbose:bool -> string -> t
    val to_channel: out_channel -> t -> unit
    val of_channel: in_channel -> t
    (* Feature sequence.  [feature_dna] stitches a feature's
       intervals in the order they are stored, reading each one
       on the forward strand and reverse-complementing the
       whole result once when the feature is on the minus
       strand.  [feature_table] is the /transl_table qualifier
       when the feature carries one, and the reference's
       per-sequence default otherwise.  [feature_protein] drops
       the phase bases from the 5' end of [feature_dna] and
       translates that with [feature_table].  All three require
       a reference to be attached and raise when there is
       none. *)
    val feature_dna: t -> feature_t -> string
    val feature_table: t -> feature_t -> Sequences.Translation.t
    val feature_protein: t -> feature_t -> string
    (* Validation.  Each [validate_*] iterates over the
       annotation register; on every violation it calls the
       supplied [?on_violation] callback with the violating
       feature's path, the feature's id (or [""] if none was
       parsed), and a human-readable message.  The default
       callback raises [Validation_failed] with the same
       payload, which preserves the historical fail-fast
       behaviour for callers that have not opted in.  Passing
       a non-raising callback (e.g.\ one that writes to a
       file) makes the walk run through the whole register and
       collect every violation. *)
    exception Validation_failed of {
      path: string;
      feature_id: string;
      message: string
    }
    type on_violation_t =
      path:string -> feature_id:string -> message:string -> unit
    val validate_sequences_present:
      ?on_violation:on_violation_t -> t -> unit
    val validate_feature_bounds:
      ?on_violation:on_violation_t -> t -> unit
    val validate_translation:
      ?on_violation:on_violation_t -> t -> unit
  end
= struct
    include Annotations_Base.Annotation
    (* *)
    (* Bumped for two changes, neither of which an older archive survives:
       [feature_t] gained its [score] slot, so the record shape differs; and
       sibling lists are now stored most-recent-first, so an archive written in
       insertion order would read back with every level reversed -- which,
       being a shape the type still accepts, would corrupt silently rather than
       fail.  The previous released version is 2026-05-09; no archive carrying
       an intermediate value ever left this branch, so one bump covers both. *)
    let archive_version = "2026-08-21"
    let make_filename_binary = function
      | w when String.length w >= 5 && String.sub w 0 5 = "/dev/" -> w
      | prefix -> prefix ^ ".Annotation"
    let to_channel output ann =
      archive_version |> output_value output;
      output_value output ann
    let of_channel input =
      let version = (input_value input: string) in
      if version <> archive_version then
        Exception.raise_incompatible_archive_version
          __FUNCTION__ version archive_version;
      (input_value input: t)
    let to_binary ?(verbose = false) ann prefix =
      let path = make_filename_binary prefix in
      if verbose then
        Printf.eprintf "(%s): writing annotation to %s\n%!"
          __FUNCTION__ path;
      let oc = open_out path in
      to_channel oc ann;
      close_out oc
    let of_binary ?(verbose = false) prefix =
      let path = make_filename_binary prefix in
      if verbose then
        Printf.eprintf "(%s): reading annotation from %s\n%!"
          __FUNCTION__ path;
      let ic = open_in path in
      let res = of_channel ic in
      close_in ic;
      res
    (* *)
    (* [Initialize] rather than [Algorithm]: asking for a feature's sequence
       when no reference has been attached is an ordinary mistake by the caller,
       not a broken invariant inside the library.  [Exception.handle] prints the
       usage and a plain FATAL line for the first, but treats the second as a
       bug -- dumping a backtrace and inviting the user to report it, for
       something they can simply fix by loading a reference. *)
    let require_reference ann who =
      match reference ann with
      | Some r -> r
      | None ->
        Exception.raise __FUNCTION__ Initialize
          (Printf.sprintf
             "%s: no reference sequence is attached to this annotation -- load one before \
              asking for a feature's sequence" who)
    (* Read each interval on the FORWARD strand and complement the stitched
       result once at the end, rather than reading a minus-strand feature's
       intervals as Reverse.  [Sequences.Reference] keeps the reverse strand in
       its own coordinate frame, so a per-interval Reverse lookup would have to
       flip every position as well; doing it this way keeps the coordinate
       arithmetic in one frame throughout. *)
    let feature_dna ann feature =
      let r = require_reference ann "feature_dna" in
      let name = seq_name ann feature in
      let stitched =
        List.map (fun (i: Sequences.Types.simple_interval_t) ->
          let str_iv: Sequences.Types.stranded_interval_t = {
            low = { name = Sequences.Types.Forward name; position = i.low };
            length = i.length
          } in
          Sequences.Reference.get_sequence r str_iv) feature.intervals
        |> String.concat "" in
      match feature.strand with
      | Some (Sequences.Types.Reverse _) -> Sequences.Lint.rc stitched
      | _ -> stitched
    let feature_table ann feature =
      let r = require_reference ann "feature_table" in
      let name = seq_name ann feature in
      (* The reference records one table per sequence, keyed by a one-base
         interval at the origin; a feature-level /transl_table overrides it. *)
      let per_sequence () =
        try
          Sequences.Reference.get_table r {
            low = { name = Sequences.Types.Forward name; position = 0 };
            length = 1
          }
        with _ -> Sequences.Translation.Table_1 in
      match attr_get ann feature "transl_table" with
      | Some (n :: _) -> (try Sequences.Translation.of_string n with _ -> per_sequence ())
      | _ -> per_sequence ()
    let feature_protein ann feature =
      let dna = feature_dna ann feature in
      let phase = Option.value ~default:0 feature.phase in
      let coding =
        if phase >= String.length dna then ""
        else String.sub dna phase (String.length dna - phase) in
      Sequences.Translation.translate
        ~replace_alternative_start_codons_with_methionine:true ~stop_on_first_stop:true
        (feature_table ann feature) coding
    exception Validation_failed of {
      path: string;
      feature_id: string;
      message: string
    }
    type on_violation_t =
      path:string -> feature_id:string -> message:string -> unit
    let default_on_violation ~path ~feature_id ~message =
      raise (Validation_failed { path; feature_id; message })
    let feature_id_of feature =
      match feature.id with Some s -> s | None -> ""
    let validate_sequences_present
        ?(on_violation = default_on_violation) ann =
      let r = require_reference ann "validate_sequences_present" in
      iter (fun ~path feature ->
        let name = seq_name ann feature in
        if name <> "" then
          try
            let _ = Sequences.Reference.find r
              (Sequences.Types.Forward name) in ()
          with _ ->
            on_violation
              ~path:(Path.to_string (paths ann) path)
              ~feature_id:(feature_id_of feature)
              ~message:(Printf.sprintf
                "sequence %S referenced by feature not in reference" name)
      ) ann
    let validate_feature_bounds
        ?(on_violation = default_on_violation) ann =
      let r = require_reference ann "validate_feature_bounds" in
      iter (fun ~path feature ->
        let name = seq_name ann feature in
        if name <> "" then begin
          let len =
            try Sequences.Reference.length r
                  (Sequences.Types.Forward name)
            with _ -> -1 in
          if len < 0 then ()
          else
            List.iter (fun (i : Sequences.Types.simple_interval_t) ->
              if i.low < 0 || i.low + i.length > len then
                on_violation
                  ~path:(Path.to_string (paths ann) path)
                  ~feature_id:(feature_id_of feature)
                  ~message:(Printf.sprintf
                    "feature on %s [%d, %d) extends past sequence length %d"
                    name i.low (i.low + i.length) len)
            ) feature.intervals
        end
      ) ann
    let validate_translation
        ?(on_violation = default_on_violation) ann =
      (* Resolve the reference here rather than at the first CDS, so that an
         annotation carrying none is reported the same way whether or not it
         happens to contain one. *)
      let _ = require_reference ann "validate_translation" in
      (* GenBank carries the terminating stop in /translation for some entries
         and omits it in others, so compare without it either way. *)
      let strip_trailing_stop s =
        let n = String.length s in
        if n > 0 && s.[n - 1] = '*' then String.sub s 0 (n - 1) else s in
      iter (fun ~path feature ->
        if Path.leaf_category (paths ann) path = "CDS" then
          match attr_get ann feature "translation" with
          | None | Some [] -> ()
          | Some (claimed :: _) ->
            let computed = feature_protein ann feature
            and expected = strip_trailing_stop claimed in
            if computed <> expected then
              on_violation
                ~path:(Path.to_string (paths ann) path)
                ~feature_id:(feature_id_of feature)
                ~message:(Printf.sprintf
                  "CDS on %s [phase=%d, intervals=%d]: claimed translation \
                   does not match computed"
                  (seq_name ann feature) (Option.value ~default:0 feature.phase)
                  (List.length feature.intervals))
      ) ann
  end

open Annotation

(* A criterion picking out a subset of an annotation's features.  Asking an
   annotation what it contains is a library concern rather than a CLI one, so
   that every consumer -- and the test suite -- can do it; AnnoTools adds only
   the command-line spelling of a criterion on top of this.
   A criterion is evaluated afresh against whatever register it is applied to,
   rather than resolved once into a set of features: an [Annotation.t] is
   rebuilt wholesale by every replace-style read, so a captured set would go
   stale against the annotation it was meant to describe. *)
module Selection =
  struct
    type t =
      | All
      | Labels of StringSet.t
      | Regexps of (string * Str.regexp) list
      | Not of t
    let rec to_string = function
      | All -> "everything"
      | Labels s -> Printf.sprintf "labels {%s}" (StringSet.elements s |> String.concat ",")
      | Regexps l ->
        Printf.sprintf "regexps {%s}"
          (List.map (fun (f, _) -> if f = "" then "<label>" else f) l |> String.concat ",")
      | Not t -> Printf.sprintf "not (%s)" (to_string t)
    let label_of feature = Option.value ~default:"" feature.id
    (* Resolve one criterion's field against a feature.  An empty field name
       matches the feature's label; every other name is a structural field
       first and an attribute key otherwise.  An attribute can hold several
       values, hence the list: the field matches when any one of them does. *)
    let field_of ann ~path feature = function
      | "" | "id" | "label" -> [ label_of feature ]
      | "seq" -> [ seq_name ann feature ]
      | "path" -> [ path_to_string path ]
      | "type" -> [ (match List.rev path with leaf :: _ -> leaf | [] -> "") ]
      | "source" -> [ Option.value ~default:"" (feature_source ann feature) ]
      | "strand" ->
        [ (match feature.strand with
           | Some (Sequences.Types.Forward _) -> "+"
           | Some (Sequences.Types.Reverse _) -> "-"
           | None -> ".") ]
      | key -> Option.value ~default:[] (attr_get ann feature key)
    (* Several field-and-regexp criteria are ANDed. *)
    let rec matches ann ~path feature = function
      | All -> true
      | Labels s -> StringSet.mem (label_of feature) s
      | Regexps l ->
        List.for_all
          (fun (f, re) -> List.exists (Str.matches re) (field_of ann ~path feature f)) l
      | Not t -> not (matches ann ~path feature t)
    (* Iterate the features a criterion selects, in register order. *)
    let iter ann selection f =
      iter_paths (fun ~path feature -> if matches ann ~path feature selection then f ~path feature)
        ann
    let count ann selection =
      let n = ref 0 in
      iter ann selection (fun ~path:_ _ -> incr n);
      !n
  end

