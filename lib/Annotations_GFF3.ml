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
  (* GFF3 spells a discontinuous feature -- a CDS across two exons, say -- as
     several rows sharing one [ID].  They are one feature with several
     intervals, and reading them as several features produced a register in
     which two features claimed the same identity, which is not a thing an
     [Annotation.t] should be able to hold.  The rows are combined here, before
     the walk, so that everything downstream sees one row per identity.
     A row carrying no [ID] has nothing to be combined on and stands alone. *)
  let coalesce_rows rows =
    let cells = Hashtbl.create 64 and order = ref [] in
    List.iter (fun (lnum, r) ->
      match r.row_id with
      | None -> List.accum order (lnum, ref r)
      | Some id ->
        match Hashtbl.find_opt cells id with
        | None ->
          let cell = ref r in
          Hashtbl.add cells id cell;
          List.accum order (lnum, cell)
        | Some cell ->
          let prev = !cell in
          (* The spec has the rows of one feature agreeing on everything but
             their coordinates.  Disagreeing on the sequence, the type or the
             strand means the file is saying two different things under one
             name, which is worth refusing rather than silently picking one. *)
          if prev.row_type <> r.row_type then
            Exception.raise __FUNCTION__ IO_Format
              (Printf.sprintf
                 "On line %d: ID=%s was already used for a %s, and this row is a %s"
                 lnum id prev.row_type r.row_type);
          if prev.row_feature.seq <> r.row_feature.seq then
            Exception.raise __FUNCTION__ IO_Format
              (Printf.sprintf "On line %d: ID=%s appears on two different sequences" lnum id);
          if prev.row_feature.strand <> r.row_feature.strand then
            Exception.raise __FUNCTION__ IO_Format
              (Printf.sprintf "On line %d: ID=%s appears on two different strands" lnum id);
          (* Intervals accumulate in file order, which for a reverse feature is
             the order the writer put them in.  Phase, score and attributes are
             the first row's: the feature carries one of each, and the writer
             recomputes the per-row phase from it. *)
          cell :=
            { prev with
              row_feature =
                { prev.row_feature with
                  intervals = prev.row_feature.intervals @ r.row_feature.intervals } }
    ) rows;
    List.rev !order |> List.map (fun (lnum, cell) -> lnum, !cell)
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
    add_dfs_with_seq_bloom ann (walk_dfs hierarchy (coalesce_rows rows));
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
  (* [ID] and [Parent] are where GFF3 keeps structure, and the register keeps
     the same thing in its forest.  They are written from the forest here, and
     any [ID] or [Parent] sitting among the attributes -- left there by a GFF3
     reader, which stores column 9 whole as well as reading the structure out of
     it -- is deliberately dropped rather than echoed.  Echoing them was what
     the writer used to do, and it only looked right: they are a second copy of
     what the forest says, and the moment a register is edited or merged they
     are a stale one, so the file would assert a shape the register no longer
     had.  A register that never came from GFF3, a GenBank one for instance, has
     no such attributes at all, which is why its structure used to vanish. *)
  let attribute_string ann ~id ~parent feature =
    let encode = Annotations_Lex.url_encode ~reserved:column_9_reserved in
    let structural =
      (match id with Some i -> [ "ID=" ^ encode i ] | None -> [])
      @ (match parent with Some p -> [ "Parent=" ^ encode p ] | None -> []) in
    let s =
      structural
      @ (attribute_pairs ann feature
         |> List.filter (fun (k, _) -> k <> "ID" && k <> "Parent")
         |> List.map (fun (k, vs) ->
              encode k ^ "=" ^ (List.map encode vs |> String.concat ",")))
      |> String.concat ";" in
    (* Column 9 is mandatory in GFF3 and uses [.] as the
       attribute-less placeholder; an empty string is not
       valid. *)
    if s = "" then "." else s
  let row_of_feature ann ~id ~parent path feature =
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
    and attrs = attribute_string ann ~id ~parent feature in
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
    (* Identifiers already in the register, so that a synthesised one cannot
       collide with a real one. *)
    let used = Hashtbl.create 64 in
    iter_paths (fun ~path:_ feature ->
      match feature.id with
      | Some i when i <> "" -> Hashtbl.replace used i ()
      | _ -> ()) ann;
    let counter = ref 0 in
    let rec fresh () =
      incr counter;
      let candidate = Printf.sprintf "feature%d" !counter in
      if Hashtbl.mem used candidate then fresh ()
      else begin
        Hashtbl.replace used candidate ();
        candidate
      end in
    (* [id_of_path] remembers the identifier of the most recent feature seen at
       each path prefix which, the walk being in DFS pre-order, is exactly the
       parent of whatever comes next one level below -- the same device the
       tabular writer uses for its parent column. *)
    let id_of_path = Hashtbl.create 64 in
    iter_paths (fun ~path feature ->
      let depth = List.length path in
      (* Depth 2 is a feature directly under the root, which has no parent to
         name.  A feature deeper than that takes the identifier standing at its
         path minus its own category. *)
      let parent =
        if depth <= 2 then None
        else Hashtbl.find_opt id_of_path (List.filteri (fun i _ -> i < depth - 1) path) in
      (* Its own identifier where it has one, so that what a file said about
         itself survives; a synthesised one otherwise, since without it a
         feature spanning several intervals could not be rejoined and its
         children would have nothing to point at. *)
      let id =
        match feature.id with
        | Some i when i <> "" -> i
        | _ -> fresh () in
      Hashtbl.replace id_of_path path id;
      let rows = row_of_feature ann ~id:(Some id) ~parent path feature in
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

