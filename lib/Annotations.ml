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
    [Attributes], [Value] and [ValueTable] interning modules from
    [Annotations_Base], so that a consumer needs one import.  To those
    it adds what belongs to no single format: the [Annotation] AST
    extended with binary I/O, the validation actions and the
    DNA/protein extraction primitives, plus [Selection], the predicate
    that picks a subset of an annotation's features.

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

(* The signature below is the whole of what [Annotations] offers, and it exists
   to stop the scaffolding escaping.  [Annotations_Common] holds both the public
   spine and the plumbing the five format modules are built on -- the file and
   buffer helpers, the column codecs, the TSV iterator -- and an unsealed file
   would republish all of it, so that [Annotations.read_file] was callable and
   [fasta_width_override] was a writable [ref] that walked straight past the
   validation in [set_fasta_width].  Nineteen names stop here; the two the CLI
   genuinely wants, [set_fasta_width] and [wrap_sequence], go through.
   The signature was drafted by [ocamlc -i] and then cut down, but it names no
   private module anywhere, and cannot: an alias into a sealed module does not
   resolve for a consumer, and neither does a [module type of] on one.  So what
   exists only inside a sealed module is written out here -- that is the price
   of sealing, and it is also just an interface -- while anything with a public
   interface elsewhere is referred to rather than restated: the field and
   argument types reach for [Better.Hashtbl] and [Sequences.Types] by name.
   What repeats within the signature is named too, rather than repeated: [Seq]
   and [AttrKey] are one functor applied twice and share [Intern_t], and the
   format modules nest through [Writer_t] and [Format_t] as they do in the
   implementation, so that [GFF3] is [Format_t] plus the one name it adds. *)
include (
  struct
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
      module GFF3: sig
        include Format_t
        val gencode_hierarchy: Hierarchy.t
      end
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
    module Format =
      struct
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
    module Writer =
      struct
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
           annotation; on every violation it calls the
           supplied [?on_violation] callback with the violating
           feature's path, the feature's id (or [""] if none was
           parsed), and a human-readable message.  The default
           callback raises through [Exception], which preserves the
           fail-fast behaviour for callers that have not opted in.
           Passing
           a non-raising callback (e.g.\ one that writes to a
           file) makes the walk run through the whole annotation and
           collect every violation. *)
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
        (* Bumped for two changes, neither of which an older archive survives:
           [feature_t] gained its [score] slot, so the record shape differs; and
           sibling lists are now stored most-recent-first, so an archive written in
           insertion order would read back with every level reversed -- which,
           being a shape the type still accepts, would corrupt silently rather than
           fail.  The previous released version is 2026-05-09; no archive carrying
           an intermediate value ever left this branch, so one bump covers both. *)
        let archive_version = "2026-08-25"
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
          (* The whole thing is reverse-complemented once at the end when the
             feature is on the minus strand, which is why the pieces are stored
             in genomic order.  A piece whose own strand DISAGREES with the
             feature's -- only a mixed-strand join produces one -- has to be
             complemented here so that the final pass puts it back: a forward
             piece of a reverse feature is complemented twice and comes out
             forward, which is what trans-splicing means. *)
          let is_rev = function Some (Sequences.Types.Reverse _) -> true | _ -> false in
          let feature_rev = is_rev feature.strand in
          let stitched =
            List.map (fun (s: Segment.t) ->
              let str_iv: Sequences.Types.stranded_interval_t = {
                low = { name = Sequences.Types.Forward name; position = s.span.low };
                length = s.span.length
              } in
              let piece = Sequences.Reference.get_sequence r str_iv in
              let piece_rev =
                match s.strand with None -> feature_rev | some -> is_rev some in
              if piece_rev <> feature_rev then Sequences.Lint.rc piece else piece)
              feature.intervals
            |> String.concat "" in
          if feature_rev then Sequences.Lint.rc stitched else stitched
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
          (* A 5'-partial CDS has no start codon in the record: the real one lies
             off the end of what was sequenced, and the record says so with a
             [<] or a [>].  Rewriting its first codon to methionine would assert
             exactly what the record denies -- a TTG at a truncated 5' end is
             leucine, not a start.  Which end is 5' depends on the strand: the
             low end of the FIRST piece going forward, the high end of the LAST
             going back, the pieces being stored in genomic order either way. *)
          let five_prime_partial =
            let is_rev =
              match feature.strand with Some (Sequences.Types.Reverse _) -> true | _ -> false in
            match feature.intervals with
            | [] -> false
            | segs ->
              if is_rev then (List.rev segs |> List.hd).Segment.partial_high
              else (List.hd segs).Segment.partial_low in
          Sequences.Translation.translate
            ~replace_alternative_start_codons_with_methionine:(not five_prime_partial)
            ~stop_on_first_stop:true
            (feature_table ann feature) coding
        type on_violation_t =
          path:string -> feature_id:string -> message:string -> unit
        (* Stop at the first violation, which is what a caller that did not ask
           for a report wants.  The path and the id go into the message rather
           than into a payload of their own: the structured form of a violation
           is the [on_violation] callback, and what the library raises goes
           through [Exception] like everything else it raises. *)
        let default_on_violation ~path ~feature_id ~message =
          let where =
            if path = "" && feature_id = "" then
              ""
            else
              Printf.sprintf " (at path=%s, feature_id=%S)" path feature_id in
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Validation failed: %s%s" message where)
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
                List.iter (fun (s: Segment.t) ->
                  let i = s.span in
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
       A criterion is evaluated afresh against whatever annotation it is applied to,
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
        (* Iterate the features a criterion selects, in storage order. *)
        let iter ann selection f =
          iter_paths
            (fun ~path feature -> if matches ann ~path feature selection then f ~path feature) ann
        let count ann selection =
          let n = ref 0 in
          iter ann selection (fun ~path:_ _ -> incr n);
          !n
      end
  end: sig
    (* A GenBank flat file as it was parsed, before any of it is interpreted:
       the header lines as key/value pairs, the feature table as one record per
       FEATURES entry with its LOCATION string still unparsed, and the ORIGIN
       sequence when the file carried one.  Returned by [GenBank.parse_records]
       for callers that want to inspect an input without building an annotation. *)
    module GenBankRecord:
      sig
        type feature_t = {
          name: string;
          location: string;
          qualifiers: (string * string) list;
        }
        type t = {
          headers: (string * string) list;
          features: feature_t list;
          origin: string option;
        }
      end
    (* Where a feature sits in the hierarchy, as an interned handle rather than
       a list of labels: a GENCODE-sized input has millions of features spread
       over a few thousand distinct paths, so the labels are stored once in a
       [Table.t] and each feature keeps an integer.  [t] is [private int], so it
       can be read as an integer but not forged from one; the table it was
       interned against is the only thing that can turn it back into labels. *)
    module Path:
      sig
        type t = private int
        module Table:
          sig
            type t
            val create: unit -> t
            val intern: t -> string list -> int
            val to_list: t -> int -> string list
            val cardinal: t -> int
          end
        val intern: Table.t -> string list -> t
        val to_list: Table.t -> t -> string list
        (* [sep] defaults to [/], so that a path prints as [gene/mRNA/CDS]. *)
        val to_string: ?sep:string -> Table.t -> t -> string
        val of_string: ?sep:string -> Table.t -> string -> t
        (* The last label of the path -- the feature's own category. *)
        val leaf_category: Table.t -> t -> string
        val equal: t -> t -> bool
        val compare: t -> t -> int
        val hash: t -> int
      end
    (* [Seq] and [AttrKey] are the same functor applied twice -- one interning
       sequence names, the other the column-9 keys -- so they share an interface
       rather than repeating one.  [Path.Table] deliberately is not this: it
       interns a list of labels rather than a string, and its [t] stays abstract.
       Each table is per-annotation, and an id is only meaningful against the
       table it came from: two annotations will number the same name differently. *)
    module type Table_t =
      sig
        type t = {
          mutable next_id: int;
          to_id: (string, int) Better.Hashtbl.t;
          from_id: (int, string) Better.Hashtbl.t;
        }
        val create: unit -> t
        val intern: t -> string -> int
        val to_string: t -> int -> string
        val cardinal: t -> int
      end
    module type Intern_t =
      sig
        type t = int
        module Table: Table_t
        val intern: Table.t -> string -> t
        val to_string: Table.t -> t -> string
        val equal: 'a -> 'a -> bool
        val compare: 'a -> 'a -> int
        val hash: 'a -> int
      end
    (* Interned sequence names -- the first column of GFF3 and GTF, the LOCUS of
       GenBank.  Resolve with [Annotation.seq_name] rather than reaching for the
       table directly. *)
    module Seq: Intern_t
    (* Interned attribute keys: the column-9 names ([ID], [Parent], [gene_id],
       ...).  GENCODE has some twenty of them across millions of rows. *)
    (* One piece of a feature's location.  Most features are a single one of
       these; a GenBank [join(...)], or a set of GFF3 rows sharing an ID, is
       several, in genomic order.
       Two things live here rather than on the feature, and both because a join
       can disagree with itself.  Partiality is per piece -- [<1..9] says the
       feature began before what was sequenced, and only the first piece knows
       that -- and so is the strand, since [complement(join(A,B))] and
       [join(complement(A),B)] are both legal INSDC, the second being how
       trans-splicing is spelled.  A [strand] of [None] means the feature's
       own, which is the ordinary case. *)
    module Segment:
      sig
        type t = {
          span: Sequences.Types.simple_interval_t;
          partial_low: bool;
          partial_high: bool;
          strand: Sequences.Types.strand_t option
        }
        val make:
          ?partial_low:bool -> ?partial_high:bool -> ?strand:Sequences.Types.strand_t ->
          Sequences.Types.simple_interval_t -> t
      end
    module AttrKey: Intern_t
    (* A feature's attributes, keyed by [AttrKey.t] and IN THE ORDER THE FILE
       GAVE THEM.  The values are an array rather than a single item so that a
       genuinely repeated qualifier -- a GenBank [/db_xref], a GFF3 comma
       list -- survives without being joined into one string.  Reach for
       [Annotation.attr_get] in preference.
       A list rather than a map, and the order is the reason: keyed by intern
       id, a map ordered a feature's qualifiers by when each key was first seen
       ANYWHERE in the file, which says nothing about this feature and silently
       reordered a round trip. *)
    module Attributes:
      sig
        type 'a t = (AttrKey.t * 'a) list
        val empty: 'a t
        val is_empty: 'a t -> bool
        val find_opt: AttrKey.t -> 'a t -> 'a option
        val iter: (AttrKey.t -> 'a -> unit) -> 'a t -> unit
        (* Replacing a key keeps the position it had; a new one goes last *)
        val add: AttrKey.t -> 'a -> 'a t -> 'a t
      end
    (* How an attribute value is held.  A string seen exactly once is kept
       inline as [String]; one seen twice or more is promoted into the
       annotation's [ValueTable] and kept as [Hashed].  The distinction is an
       internal space optimisation and not something a caller need act on:
       [Annotation.attr_get] and [attr_iter] hand back the original string
       either way. *)
    module Value:
      sig
        type t =
          | Hashed of int
          | String of string
      end
    (* The promotion table behind [Value.Hashed], one per annotation.  It also
       carries a Bloom sketch of what has been seen once, which is what makes
       the promotion decision cheap during a parse; [drop_bloom] releases that
       sketch when parsing is over and the table is only being read. *)
    module ValueTable:
      sig
        type t
        val create: unit -> t
        val intern: t -> string -> Value.t
        val to_string: t -> Value.t -> string
        val find_id: t -> string -> int option
        val cardinal: t -> int
        val drop_bloom: t -> unit
      end
    (* Override the FASTA line width for every writer at once, which is what
       [AnnoTools --fasta-width] does.  [None] restores each writer's own
       default -- 60 for the GFF3 [##FASTA] section, unwrapped for the tabular
       sidecar -- and [Some 0] means never wrap.  A negative width raises. *)
    val set_fasta_width: int option -> unit
    (* Render a sequence as the body of a FASTA record, without a trailing
       newline.  [width] is the caller's default and is overridden by
       [set_fasta_width] when one has been set; zero returns the sequence
       whole. *)
    val wrap_sequence: ?width:int -> string -> string
    (* The schema an annotation is read under: a tree of category labels saying
       which feature types may nest inside which.  It is what turns a flat file
       into an annotation with parent links, and it is written and read as an
       S-expression -- [(gene ((mRNA (exon, CDS)), pseudogene))].  [validate]
       asks whether a path is legal under it; [children_of] asks what may
       follow. *)
    module Hierarchy:
      sig
        type t
        val node: string -> t list -> t
        val name: t -> string
        val children: t -> t list
        val find: t -> path:string list -> t option
        val children_of: t -> path:string list -> string list
        val validate: t -> path:string list -> bool
        val to_string: t -> string
        val of_string: string -> t
        val of_file: string -> t
      end
    (* Coordinates as the file formats write them, which is not how the library
       holds them: intervals are 0-based half-open internally and 1-based
       inclusive on the wire.  [Range (lo, hi)] is the ordinary case; [Between]
       is the INSDC [lo^hi] site, which names the gap between two adjacent bases
       and so has no width.  Use these rather than adding and subtracting one at
       each call site -- the two zero-length conventions do not agree, and that
       is precisely where the off-by-one lives. *)
    module OneBased:
      sig
        type t =
          | Range of int * int
          | Between of int * int
        val of_interval: Sequences.Types.simple_interval_t -> t
        val to_interval: t -> Sequences.Types.simple_interval_t
        val to_string: t -> string
        val of_string: string -> t
        (* The 1-based inclusive endpoints of an interval, for a writer that
           wants two numbers rather than a [t]. *)
        val bounds: Sequences.Types.simple_interval_t -> int * int
        val interval_of_bounds: lo:int -> hi:int -> Sequences.Types.simple_interval_t
      end
    (* The GenBank LOCATION grammar, parsed rather than pattern-matched: it
       nests, and [complement(join(...))] does not mean the same as
       [join(complement(...))].  [Point] and [Range] carry [<] and [>] partial
       markers as [fuzzy_left] / [fuzzy_right]; [Remote] is a location on
       another accession.  [intervals] resolves the tree into a flat list, each
       interval tagged with its accession when it is remote, plus the strand
       implied by the outermost [Complement]. *)
    module GenBankLocation:
      sig
        type endpoint_t = {
          pos: int;
          fuzzy_left: bool;
          fuzzy_right: bool;
        }
        type t =
          | Point of endpoint_t
          | Range of endpoint_t * endpoint_t
          | Between of int * int
          | Complement of t
          | Join of t list
          | Order of t list
          | Remote of string * int option * t
        val of_string: string -> t
        (* Each piece carries its own partiality and strand, a join being able
           to disagree with itself on both; the second component is the strand
           of the feature as a whole, and is [None] when they do not agree *)
        val intervals:
          t -> (string option * Segment.t) list * Sequences.Types.strand_t option
      end
    (* A whole annotation, format-independent: the hierarchy it was read under,
       the four interning tables its features refer to, the features themselves,
       whatever metadata the source file carried, and optionally a reference
       sequence.  [t] is immutable in the sense that every operation adding
       something returns a new one; the interning tables inside it are shared
       and mutable, which is why a [feature_t] is only meaningful together with
       the annotation it came from. *)
    module Annotation:
      sig
        type feature_t = {
          seq: Seq.t;
          source: Value.t option;
          intervals: Segment.t list;
          score: float option;
          strand: Sequences.Types.strand_t option;
          phase: int option;
          id: string option;
          attributes: Value.t array Attributes.t;
        }
        val empty_feature: feature_t
        type t
        val create: Hierarchy.t -> t
        val hierarchy: t -> Hierarchy.t
        (* The interning tables.  Needed to resolve a [Path.t] or a [Seq.t] by
           hand; the accessors below do it for the common cases. *)
        val paths: t -> Path.Table.t
        val seqs: t -> Seq.Table.t
        val attr_keys: t -> AttrKey.Table.t
        val values: t -> ValueTable.t
        val seq_name: t -> feature_t -> string
        val intern_seq: t -> string -> Seq.t
        val feature_source: t -> feature_t -> string option
        val intern_source: t -> string -> Value.t
        (* Attributes by name, with the [Value] representation resolved away.
           [attr_set] returns a new feature; it does not modify the one given. *)
        val attr_get: t -> feature_t -> string -> string list option
        val attr_iter: t -> (string -> string list -> unit) -> feature_t -> unit
        val attr_set: t -> feature_t -> key:string -> values:string list -> feature_t
        (* Run at the end of a parse: rewrites in place those [Value.String]s
           that turned out to be repeated after all, so that afterwards
           [Value.String s] holds only when [s] occurs exactly once. *)
        val cleanup_values: t -> unit
        (* The reference sequence, which the extraction and validation
           functions below require and which the readers attach when the input
           carried one (GenBank ORIGIN, a GFF3 [##FASTA] section). *)
        val reference: t -> Sequences.Reference.t option
        val set_reference: t -> Sequences.Reference.t -> t
        (* Whatever the source file said about itself outside the feature
           table -- pragmas, header lines -- kept so a round trip does not
           silently drop it. *)
        val get_metadata: t -> string -> string list
        val add_metadata: t -> key:string -> value:string -> t
        val all_metadata: t -> string list Better.StringMap.t
        (* [add] places a feature at a path, creating the intermediate levels
           the hierarchy calls for.  The [_paths] variants of the traversals
           hand the path back as labels rather than as an interned [Path.t],
           which costs a table lookup per feature and saves the caller one. *)
        val add: t -> path:string list -> feature_t -> t
        val iter: (path:Path.t -> feature_t -> unit) -> t -> unit
        val fold: (path:Path.t -> feature_t -> 'a -> 'a) -> 'a -> t -> 'a
        val iter_paths: (path:string list -> feature_t -> unit) -> t -> unit
        val fold_paths: (path:string list -> feature_t -> 'a -> 'a) -> 'a -> t -> 'a
        val path_to_string: ?sep:string -> string list -> string
        val path_of_string: ?sep:string -> string -> string list
        (* Binary archive: a [Marshal]ed value behind a version string, so a
           annotation that took minutes to parse can be reloaded in seconds.  The
           version is checked on read and refuses an archive written by an
           incompatible release rather than misreading it.  The default suffix
           is [.Annotation], unless the prefix points under [/dev]. *)
        val to_binary: ?verbose:bool -> t -> string -> unit
        val of_binary: ?verbose:bool -> string -> t
        val to_channel: out_channel -> t -> unit
        val of_channel: in_channel -> t
        (* Feature sequence.  [feature_dna] stitches a feature's intervals in
           the order they are stored, reading each on the forward strand and
           reverse-complementing the whole result once when the feature is on
           the minus strand.  [feature_table] is the [/transl_table] qualifier
           when the feature carries one and the reference's per-sequence default
           otherwise.  [feature_protein] drops the phase bases from the 5' end
           of [feature_dna] and translates the rest.  All three need a reference
           and raise when there is none. *)
        val feature_dna: t -> feature_t -> string
        val feature_table: t -> feature_t -> Sequences.Translation.t
        val feature_protein: t -> feature_t -> string
        (* Validation.  Each [validate_*] walks the annotation and calls
           [on_violation] with the offending feature's path, its id (or [""])
           and a message.  The default callback raises, which keeps the
           fail-fast behaviour; passing one that does not raise makes the walk
           continue and collect every violation. *)
        type on_violation_t =
          path:string -> feature_id:string -> message:string -> unit
        val validate_sequences_present: ?on_violation:on_violation_t -> t -> unit
        val validate_feature_bounds: ?on_violation:on_violation_t -> t -> unit
        val validate_translation: ?on_violation:on_violation_t -> t -> unit
      end
    (* The write half of a format, kept separate so that a format which can only
       be written is expressible.  NCBI's submission feature table is one: it
       has no source column, no parent link and no metadata, and [table2asn]
       infers the gene/mRNA/CDS relations from coordinate overlap rather than
       reading them, so an annotation cannot be recovered from one. *)
    module type Writer_t =
      sig
        val to_buffer: Buffer.t -> Annotation.t -> unit
        val to_string: Annotation.t -> string
        val to_file: Annotation.t -> string -> unit
      end
    (* A format that round-trips: everything a writer has, plus the readers.
       [read] and [read_from_file] add to an existing annotation, which is how
       several files are merged into one; [of_string] and [of_file] start from
       an empty one under [hierarchy], defaulting to the format's own.
       [dialects] are the named hierarchies the format knows, head first. *)
    module type Format_t =
      sig
        include Writer_t
        val dialects: (string * Hierarchy.t) list
        val default_hierarchy: Hierarchy.t
        val read: Annotation.t -> string -> Annotation.t
        val read_from_file: Annotation.t -> string -> Annotation.t
        val of_string: ?hierarchy:Hierarchy.t -> string -> Annotation.t
        val of_file: ?hierarchy:Hierarchy.t -> string -> Annotation.t
      end
    (* GFF3.  Structure travels in column 9 as [ID] and [Parent], which are
       derived from the annotation on the way out rather than echoed from the way
       in, and rows sharing an [ID] are merged back into one discontinuous
       feature on the way in.  A [##FASTA] section is read and written. *)
    module GFF3:
      sig
        include Format_t
        val gencode_hierarchy: Hierarchy.t
      end
    (* GTF, the Ensembl/GENCODE dialect: no [ID]/[Parent], the hierarchy being
       implied by [gene_id] and [transcript_id] instead. *)
    module GTF: Format_t
    (* GenBank flat files.  [parse_records] exposes the raw parse for a caller
       that wants to look at an input without building an annotation from it. *)
    module GenBank:
      sig
        include Format_t
        val parse_records: string -> GenBankRecord.t list
      end
    (* The library's own tabular format: one feature per line, the hierarchy
       declared in the document rather than implied, and the sequence in a
       sidecar.  Being one record per line is the point -- [awk], [cut] and
       [sort] can take it a line at a time. *)
    module Tabular: Format_t
    (* NCBI's submission feature table.  Write-only; see [Writer_t]. *)
    module Tbl: Writer_t
    (* A runtime handle on the formats, for dispatching on one chosen at the
       command line.  The constructors share their names with the modules but
       live in their own namespace, so [Format.GFF3] and [GFF3] do not clash.
       [of_string] accepts the canonical name and the informal spellings people
       type ([gff], [gb], [table]); [dialect_of] resolves a dialect name against
       the format's [dialects], case-insensitively. *)
    module Format:
      sig
        type t = GFF3 | GTF | GenBank | Tabular
        val all: t list
        val module_of: t -> (module Format_t)
        val to_string: t -> string
        val of_string: string -> t
        val dialect_of: t -> string -> Hierarchy.t
      end
    (* The same for everything that can be written, which is every format plus
       the write-only feature table.  Keeping it distinct from [Format.t] is
       what makes [--from-tbl] inexpressible rather than merely broken: the
       reading side of a CLI dispatches over [Format.t] and the writing side
       over this, so the type records which formats can do which. *)
    module Writer:
      sig
        type t =
          | Format of Format.t
          | Tbl
        val all: t list
        val module_of: t -> (module Writer_t)
        val to_string: t -> string
        val of_string: string -> t
      end
    (* A predicate over an annotation's features, in the library rather than in the
       CLI so that it is not only a command line that can express one.
       [Labels] matches a feature's label, [Regexps] matches named fields
       against patterns, and [Not] complements.  The selectable field names are
       [seq], [path], [type], [source], [strand], [id] and any attribute key; an
       empty name means the feature label. *)
    module Selection:
      sig
        type t =
          | All
          | Labels of Better.StringSet.t
          | Regexps of (string * Better.Str.regexp) list
          | Not of t
        val to_string: t -> string
        val label_of: Annotation.feature_t -> string
        val field_of:
          Annotation.t -> path:string list -> Annotation.feature_t -> string -> string list
        val matches: Annotation.t -> path:string list -> Annotation.feature_t -> t -> bool
        val iter:
          Annotation.t -> t -> (path:string list -> Annotation.feature_t -> unit) -> unit
        val count: Annotation.t -> t -> int
      end
  end
)
