(*
    Annotations_GFF3.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_GFF3.ml reads and writes GFF3, the nine-column TSV
    of the current INSDC standard, including the [##FASTA] directive
    that carries a reference alongside the annotation.  It ships two
    dialects: a broad default hierarchy and [gencode_hierarchy],
    derived from a survey of GENCODE v47 basic.

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
open Annotations_Common
(* The base AST, unqualified, as every format reader and writer uses it. *)
open Annotation

(* GFF3 satisfies [Format_t] and additionally exposes the
   built-in [gencode_hierarchy] under its own name (also
   reachable via the [dialects] association). *)
module GFF3:
  sig
    include Format_t
    val gencode_hierarchy: Hierarchy.t
  end
= struct
  let default_hierarchy = default_gff3_hierarchy
  (* GENCODE files (and most Ensembl GFF3) collapse every
     transcript biotype into the single type [transcript]
     (with the actual biotype carried as a transcript_type
     attribute), and use
     [stop_codon_redefined_as_selenocysteine] for
     selenocysteines.  Distinct top-level types are gene
     only.  Derived from a survey of
     gencode.v47.basic.annotation.gff3. *)
  let gencode_hierarchy =
    Hierarchy.of_string
      "(gene \
          ((transcript \
             (exon, \
              (CDS (stop_codon_redefined_as_selenocysteine)), \
              five_prime_UTR, three_prime_UTR, \
              start_codon, stop_codon))))"
  let dialects = [
    "standard", default_hierarchy;
    "gencode", gencode_hierarchy
  ]
  (* *)
  let parse_attributes s =
    let lexbuf = Lexing.from_string ~with_positions:true s in
    Annotations_Parse.gff_attribute_list Annotations_Lex.gff_attributes lexbuf
  (* Single GFF3 row -> (id, parent_id option, type, feature).
     The [seq] and [attributes] of [row_feature] are
     pre-interned against the supplied [seqs] / [attr_keys]
     tables so an entire file's worth of "chr1" or "gene_id"
     strings collapse to a handful of ints. *)
  type row_t = {
    row_id: string option;
    row_parent: string option;
    row_type: string;
    row_feature: feature_t
  }
  let parse_row ~seqs ~attr_keys ~values line_no fields =
    if Array.length fields <> 9 then
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "On line %d: GFF3 row has %d columns, expected 9"
           line_no (Array.length fields));
    let seq = Seq.intern seqs fields.(0)
    and source =
      let s = fields.(1) in
      if s = "" || s = "." then None
      else Some (ValueTable.intern values s)
    and ftype = fields.(2)
    and lo = int_of_string fields.(3)
    and hi = int_of_string fields.(4)
    and score = score_of_field fields.(5)
    and strand = strand_of_field fields.(6)
    and phase = phase_of_field fields.(7)
    and attrs = parse_attributes fields.(8) in
    let attr_map =
      List.fold_left (fun m (k, vs) ->
        let kid = AttrKey.intern attr_keys k in
        let arr =
          Array.of_list
            (List.map (ValueTable.intern values) vs) in
        AttrMap.add kid arr m
      ) AttrMap.empty attrs in
    let id_key = AttrKey.intern attr_keys "ID"
    and parent_key = AttrKey.intern attr_keys "Parent" in
    let first_string m k =
      match AttrMap.find_opt k m with
      | Some arr when Array.length arr > 0 ->
        Some (ValueTable.to_string values arr.(0))
      | _ -> None in
    let id = first_string attr_map id_key
    and parent = first_string attr_map parent_key in
    let feature = {
      seq;
      source;
      intervals = [ interval_of_1_based ~lo ~hi ];
      score;
      strand;
      phase;
      id;
      attributes = attr_map
    } in
    { row_id = id; row_parent = parent; row_type = ftype; row_feature = feature }
  let read_rows ~seqs ~attr_keys ~values s =
    let pragmas = ref [] and rows = ref [] and sequence = ref "" in
    iter_tsv_lines s
      ~pragma:(fun body -> List.accum pragmas body)
      ~fasta:(fun body -> sequence := body)
      ~data:(fun lnum fields ->
        List.accum rows
          (lnum, parse_row ~seqs ~attr_keys ~values lnum fields));
    List.rev !pragmas, List.rev !rows, !sequence
  (* Walk the parent-ID DAG, computing each row's full path
     from root and emitting (path, feature) pairs in DFS
     pre-order suitable for [Annotation.add].  Rows without a
     [Parent] sit at top level; their path is just
     [[row_type]].  Rows with a parent need the parent's path,
     so a topological pass over [Parent=]-edges is required. *)
  let walk_dfs hierarchy rows =
    let root_name = Hierarchy.name hierarchy in
    let by_id = Hashtbl.create 64 in
    List.iter (fun (_, r) ->
      match r.row_id with
      | Some id -> Hashtbl.replace by_id id r
      | None -> ()) rows;
    let path_cache = Hashtbl.create 64 in
    let rec path_of r =
      match r.row_id with
      | Some id when Hashtbl.mem path_cache id ->
        Hashtbl.find path_cache id
      | _ ->
        let p =
          match r.row_parent with
          | None -> [ root_name; r.row_type ]
          | Some pid ->
            let pr =
              try Hashtbl.find by_id pid
              with Not_found ->
                Exception.raise __FUNCTION__ IO_Format
                  (Printf.sprintf
                     "GFF3: feature with Parent=%s but no record \
                      with ID=%s was seen" pid pid) in
            path_of pr @ [ r.row_type ] in
        if not (Hierarchy.validate hierarchy ~path:p) then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf
               "GFF3: path %s violates the active hierarchy %s"
               (path_to_string p) (Hierarchy.to_string hierarchy));
        (match r.row_id with
         | Some id -> Hashtbl.add path_cache id p
         | None -> ());
        p in
    let kids_of = Hashtbl.create 64 in
    let toplevel = ref [] in
    List.iter (fun (lnum, r) ->
      match r.row_parent with
      | None -> List.accum toplevel (lnum, r)
      | Some pid ->
        let prev =
          try Hashtbl.find kids_of pid with Not_found -> [] in
        Hashtbl.replace kids_of pid ((lnum, r) :: prev)
    ) rows;
    let toplevel = List.rev !toplevel in
    Hashtbl.filter_map_inplace
      (fun _ v -> Some (List.rev v)) kids_of;
    let acc = ref [] in
    let rec emit (_, r) =
      List.accum acc ((path_of r, r.row_feature));
      match r.row_id with
      | None -> ()
      | Some id ->
        (try List.iter emit (Hashtbl.find kids_of id)
         with Not_found -> ()) in
    List.iter emit toplevel;
    List.rev !acc
  (* Carrier-based reader: install the GFF3 features and
     pragmas encoded in [s] into [ann_in], using the carrier's
     hierarchy for validation.  The carrier's interning tables
     are extended in place; pragmas are appended to the
     carrier's metadata map. *)
  let read ann_in s =
    let ann = ref ann_in in
    let hierarchy = Annotation.hierarchy !ann in
    let pragmas, rows, sequence =
      read_rows
        ~seqs:(seqs !ann) ~attr_keys:(attr_keys !ann)
        ~values:(values !ann) s in
    add_dfs_with_seq_bloom ann (walk_dfs hierarchy rows);
    List.iter (fun pragma ->
      match String.index_opt pragma ' ' with
      | None -> ann := add_metadata !ann ~key:pragma ~value:""
      | Some i ->
        let k = String.sub pragma 0 i
        and v = String.sub pragma (i + 1) (String.length pragma - i - 1) in
        ann := add_metadata !ann ~key:k ~value:v
    ) pragmas;
    (* A [##FASTA] section is the annotation's own reference.  Read it with the
       identity linter: what the file says is what it means, and folding an
       IUPAC code to N here would quietly change the sequence. *)
    if sequence <> "" then begin
      let base =
        match Annotation.reference !ann with
        | Some r -> r
        | None -> Sequences.Reference.empty in
      ann :=
        Annotation.set_reference !ann
          (* Upper-cased, and nothing else.  The codon tables are upper-case
             only, so a soft-masked genome read in verbatim translates to X
             throughout; folding to N instead -- which is what the default
             linter does -- would destroy the ambiguity codes rather than the
             case.  This is the linter AnnoTools already applies to a reference
             given as FASTA, and the three ways in should agree. *)
          (Sequences.Reference.add_from_fasta_string ~linter:String.uppercase_ascii
             base sequence)
    end;
    cleanup_values !ann;
    !ann
  let read_from_file ann path = read ann (read_file path)
  let of_string ?(hierarchy = default_hierarchy) s =
    read (create hierarchy) s
  let of_file ?(hierarchy = default_hierarchy) path =
    read_from_file (create hierarchy) path
  (* Output: for each feature, emit one GFF3 row using the
     feature's [seq] / [intervals] / etc., and the
     path-leaf as the type column.  Multi-interval features
     emit one row per interval sharing the [ID]. *)
  (* The bytes that carry structure in column 9.  The lexer percent-decodes on
     the way in, so the writer has to percent-encode on the way out, or a value
     containing one of them changes meaning on the next read: a comma splits it
     into two values, a semicolon into two attributes.
     Space is in the set although GFF3 permits it unencoded, because this
     lexer treats a space as a token separator: encoding it is what makes
     [product=hypothetical protein] survive being written and read again. *)
  let column_9_reserved = ";=&, "
  let attribute_string ann feature =
    let encode = Annotations_Lex.url_encode ~reserved:column_9_reserved in
    let s =
      attribute_pairs ann feature
      |> List.map (fun (k, vs) -> encode k ^ "=" ^ (List.map encode vs |> String.concat ","))
      |> String.concat ";" in
    (* Column 9 is mandatory in GFF3 and uses [.] as the
       attribute-less placeholder; an empty string is not
       valid. *)
    if s = "" then "." else s
  let row_of_feature ann path feature =
    let ftype = match List.rev path with [] -> "" | x :: _ -> x in
    let seq = seq_name ann feature
    and src =
      match feature_source ann feature with
      | Some s -> s | None -> "."
    and score = field_of_score feature.score
    and strand =
      match feature.strand with
      | Some Sequences.Types.Forward _ -> "+"
      | Some Sequences.Types.Reverse _ -> "-"
      | None -> "."
    and attrs = attribute_string ann feature in
    (* Column 8 is per ROW, not per feature: it says how many bases of the first
       codon of THIS row lie in the previous rows.  Stamping the feature's phase
       on every row of a multi-exon CDS is right only for the first.  Intervals
       are stored in transcription order -- the order [feature_dna] splices them
       -- so the running total of coding bases already 5' of each row gives it
       directly, with no strand special case. *)
    let phase_of consumed =
      match feature.phase with
      | None -> "."
      | Some p -> string_of_int (((p - consumed) mod 3 + 3) mod 3) in
    let _, rows =
      List.fold_left (fun (consumed, acc) (ivl: Sequences.Types.simple_interval_t) ->
        let lo, hi = OneBased.bounds ivl in
        consumed + ivl.length,
        Printf.sprintf "%s\t%s\t%s\t%d\t%d\t%s\t%s\t%s\t%s"
          seq src ftype lo hi score strand (phase_of consumed) attrs :: acc)
        (0, []) feature.intervals in
    List.rev rows
  let to_buffer buf ann =
    let has_gff_version =
      StringMap.mem "gff-version" (all_metadata ann) in
    if not has_gff_version then
      Buffer.add_string buf "##gff-version 3\n";
    StringMap.iter (fun k vs ->
      List.iter (fun v ->
        Printf.bprintf buf "##%s %s\n" k v
      ) vs
    ) (all_metadata ann);
    iter_paths (fun ~path feature ->
      let rows = row_of_feature ann path feature in
      List.iter (fun r ->
        Buffer.add_string buf r;
        Buffer.add_char buf '\n'
      ) rows
    ) ann;
    (* [##FASTA] is a standard GFF3 directive saying the rest of the file is
       sequence, so a register that has a reference can carry it here rather
       than leaving it to be supplied separately -- which is what makes a
       GenBank record survive a trip through GFF3 whole. *)
    (match reference ann with
     | None -> ()
     | Some r ->
       Buffer.add_string buf "##FASTA\n";
       write_fasta buf r)
  let to_string = to_string_via_buffer to_buffer
  let to_file = to_file_via_buffer to_buffer
end

