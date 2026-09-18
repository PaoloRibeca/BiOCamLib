(*
    Annotations_GFF3.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_GFF3.ml reads and writes GFF3, the nine-column TSV
    of the current INSDC standard, including the [##FASTA] directive
    that carries a reference alongside the annotation.  Its default
    hierarchy is open, [*]: a GFF3 file states its own structure through
    [Parent=], and the reader takes it as stated.  Two dialects check a
    file against a fixed vocabulary instead: [gencode], derived from a
    survey of GENCODE v47 basic, and [broad], the list that used to be
    the default.

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
  (* OPEN.  A GFF3 file states its structure through [Parent=], so the reader
     takes it as the file states it, and leaves the register holding the
     structure actually read -- see [read].  A fixed default checked every path
     against a vocabulary no fixed list can cover: NCBI's own GFF3 nests a
     [mature_protein_region_of_CDS] beneath a CDS and puts a [terminal_repeat]
     at the top, and was refused for both.  A caller who wants a file checked
     names a hierarchy, a dialect below or one of its own. *)
  let default_hierarchy = Hierarchy.of_string "*"
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
    "broad", default_gff3_hierarchy;
    "gencode", gencode_hierarchy
  ]
  (* COLUMN 9 AS FILES WRITE IT, and not only as the specification says it
     should be.  The specification reserves [;] [=] [&] [,] and has them
     percent-encoded inside a value, and a good many files -- this library's own,
     before it escaped anything, among them -- leave some of them raw.  The string
     is repaired before the grammar sees it, so that a compliant one is parsed
     exactly as before, and every repair is reported through [warn]:
     - a fragment after a [;] that cannot be an attribute, having no [=] and not
       being a bare key, is the rest of the value before it, the [;] having been
       meant literally, as in [note="similar to Bov2.b3; earlystop codon"];
     - a bare key, [pseudo], is an attribute present with no value, [pseudo=];
     - a raw [=] or [&] inside a value, and a [%] not starting an escape, are
       escaped.
     A comma cannot be told from the separator between values, and is left
     alone.  Quotes mean nothing in GFF3 and are kept, as data. *)
  let is_key s =
    s <> ""
    && String.for_all
         (function 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '.' | ':' | '-' -> true | _ -> false)
         s
  let escape_value ~warn ~key v =
    let buf = Buffer.create (String.length v) and n = String.length v in
    let hex = function '0'..'9' | 'a'..'f' | 'A'..'F' -> true | _ -> false in
    String.iteri (fun i c ->
      match c with
      | ';' -> Buffer.add_string buf "%3B"
      | '=' ->
        warn (Printf.sprintf "an unescaped '=' in the value of '%s' was read as part of it" key);
        Buffer.add_string buf "%3D"
      | '&' ->
        warn (Printf.sprintf "an unescaped '&' in the value of '%s' was read as part of it" key);
        Buffer.add_string buf "%26"
      | '%' when not (i + 2 < n && hex v.[i + 1] && hex v.[i + 2]) ->
        warn (Printf.sprintf "a '%%' in the value of '%s' starts no escape, and was read as itself" key);
        Buffer.add_string buf "%25"
      | c -> Buffer.add_char buf c) v;
    Buffer.contents buf
  let repair_attributes ~warn s =
    let attrs = ref [] in
    List.iter (fun piece ->
      let trimmed = String.trim piece in
      if trimmed <> "" then
        match String.index_opt piece '=' with
        | Some i when is_key (String.trim (String.sub piece 0 i)) ->
          List.accum attrs
            (String.trim (String.sub piece 0 i),
             Some (String.sub piece (i + 1) (String.length piece - i - 1)))
        | _ when is_key trimmed ->
          warn (Printf.sprintf "attribute '%s' has no '=', and was read as present with no value" trimmed);
          List.accum attrs (trimmed, Some "")
        | _ ->
          match !attrs with
          | (key, Some v) :: rest ->
            warn (Printf.sprintf "an unescaped ';' in the value of '%s' was read as part of it" key);
            attrs := (key, Some (v ^ ";" ^ piece)) :: rest
          | _ ->
            (* Nothing to attach it to: left as it is, for the grammar to refuse. *)
            List.accum attrs (piece, None))
      (String.split_on_char ';' s);
    List.rev_map (fun (key, v) ->
        match v with
        | None -> key
        | Some v -> key ^ "=" ^ escape_value ~warn ~key v)
      !attrs
    |> String.concat ";"
  let parse_attributes ?(warn = fun _ -> ()) s =
    match String.trim s with
    (* GFF3's empty column, which is a "." only when it is the whole column: one
       inside it is a value like any other, as in [start_range=.,1]. *)
    | "" | "." -> []
    | _ ->
      let s = repair_attributes ~warn s in
      let lexbuf = Lexing.from_string ~with_positions:true s in
      parse_with ~what:(Printf.sprintf "GFF3 attributes %S" s)
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
  let parse_row ?(warn = fun _ -> ()) ~seqs ~attr_keys ~values line_no fields =
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
    and attrs = parse_attributes ~warn fields.(8) in
    let attr_map =
      List.fold_left (fun m (k, vs) ->
        let kid = AttrKey.intern attr_keys k in
        let arr =
          Array.of_list
            (List.map (ValueTable.intern values) vs) in
        Attributes.add kid arr m
      ) Attributes.empty attrs in
    let id_key = AttrKey.intern attr_keys "ID"
    and parent_key = AttrKey.intern attr_keys "Parent" in
    let first_string m k =
      match Attributes.find_opt k m with
      | Some arr when Array.length arr > 0 ->
        Some (ValueTable.to_string values arr.(0))
      | _ -> None in
    let id = first_string attr_map id_key
    and parent = first_string attr_map parent_key in
    let feature = {
      seq;
      source;
      intervals = [ Segment.make (interval_of_1_based ~lo ~hi) ];
      score;
      strand;
      phase;
      id;
      attributes = attr_map
    } in
    { row_id = id; row_parent = parent; row_type = ftype; row_feature = feature }
  let read_rows ?(warn = fun _ _ -> ()) ~seqs ~attr_keys ~values s =
    let pragmas = ref [] and rows = ref [] and sequence = ref "" in
    iter_tsv_lines s
      ~pragma:(fun body -> List.accum pragmas body)
      ~fasta:(fun body -> sequence := body)
      ~data:(fun lnum fields ->
        List.accum rows
          (lnum, parse_row ~warn:(warn lnum) ~seqs ~attr_keys ~values lnum fields));
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
  (* What [repair_attributes] did, on standard error: the first few repairs with
     their lines, then how many more there were.  Reading goes on regardless, and a
     compliant file says nothing. *)
  let max_reported_repairs = 10
  let report_repairs repairs n =
    if n > 0 then begin
      let program = Filename.basename Sys.executable_name |> Filename.remove_extension in
      List.iter (fun (lnum, message) ->
        Printf.eprintf "(%s): GFF3 line %d: %s\n%!" program lnum message) repairs;
      if n > List.length repairs then
        Printf.eprintf "(%s): GFF3: %d more repairs like these\n%!" program (n - List.length repairs)
    end
  (* Carrier-based reader: install the GFF3 features and
     pragmas encoded in [s] into [ann_in], using the carrier's
     hierarchy for validation.  The carrier's interning tables
     are extended in place; pragmas are appended to the
     carrier's metadata map. *)
  let read ann_in s =
    let ann = ref ann_in in
    let hierarchy = Annotation.hierarchy !ann in
    (* Repairs of column 9, reported once the file has been read. *)
    let repairs = ref [] and n_repairs = ref 0 in
    let warn lnum message =
      incr n_repairs;
      if !n_repairs <= max_reported_repairs then List.accum repairs (lnum, message) in
    let pragmas, rows, sequence =
      read_rows ~warn
        ~seqs:(seqs !ann) ~attr_keys:(attr_keys !ann)
        ~values:(values !ann) s in
    add_dfs_with_seq_bloom ann (walk_dfs hierarchy (coalesce_rows rows));
    (* An open hierarchy -- the default -- admitted whatever the file stated, and
       stays open: the paths actually read are recorded in it beside its [*], so
       that the register lists the structure it holds while a later read into it is
       as open as this one was.  Closing it is the caller's to decide, by naming a
       hierarchy. *)
    if Hierarchy.is_open hierarchy then begin
      let seen = Hashtbl.create 64 and order = ref [] in
      iter_paths (fun ~path _ ->
        let p = List.tl path in
        if not (Hashtbl.mem seen p) then begin
          Hashtbl.add seen p ();
          List.accum order p
        end) !ann;
      ann :=
        with_hierarchy !ann
          (Hierarchy.merge hierarchy (Hierarchy.of_paths (List.rev !order)))
    end;
    report_repairs (List.rev !repairs) !n_repairs;
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
     A space is not in the set.  GFF3 permits it unencoded, nobody encodes it,
     and the lexer keeps it inside a value, so [product=hypothetical protein]
     reads back as written.  A leading or a trailing one is still escaped,
     being what a reader is likeliest to trim. *)
  let column_9_reserved = ";=&,"
  let encode_column_9 s =
    let e = Annotations_Lex.url_encode ~reserved:column_9_reserved s in
    let n = String.length e in
    let lead = ref 0 and trail = ref 0 in
    while !lead < n && e.[!lead] = ' ' do incr lead done;
    while !trail < n - !lead && e.[n - 1 - !trail] = ' ' do incr trail done;
    if !lead = 0 && !trail = 0 then
      e
    else
      String.concat "" (List.init !lead (fun _ -> "%20"))
      ^ String.sub e !lead (n - !lead - !trail)
      ^ String.concat "" (List.init !trail (fun _ -> "%20"))
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
    let encode = encode_column_9 in
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
      List.fold_left (fun (consumed, acc) (ivl: Segment.t) ->
        let lo, hi = OneBased.bounds ivl.Segment.span in
        consumed + ivl.Segment.span.length,
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
    let identifier = identifiers ann in
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
         children would have nothing to point at.  No two features get the same
         one, see [identifiers]. *)
      let id =
        identifier ~category:(match List.rev path with c :: _ -> c | [] -> "") feature.id in
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

