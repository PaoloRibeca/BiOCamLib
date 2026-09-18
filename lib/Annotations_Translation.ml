(*
    Annotations_Translation.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_Translation.ml renders a register in the vocabulary and
    nesting of another format, following a table.  Readers and writers are
    faithful to what they are given; what a conversion renames, nests, invents
    or drops is data, one table per ordered pair of formats, so that it can be
    read, corrected and replaced without touching a reader or a writer.

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

module Translation:
  sig
    type t
    val of_string: string -> t
    val of_file: string -> t
    val to_string: t -> string
    (* The source and the target format, as the table's header names them. *)
    val formats: t -> string * string
    (* The tables that come with the library, by the names of the two formats. *)
    val builtin: from:string -> into:string -> t option
    val builtin_pairs: (string * string) list
    (* What the table makes of one path, with no feature to test a condition on,
       so that only unconditioned rows apply: [None] where no row lists it, [Some
       None] where a row drops it.  [drop_levels] as in [apply]. *)
    val path: ?drop_levels:string list -> t -> string list -> string list option option
    (* The standard path in format [into] ending with a category, as the built-in
       table into that format spells it, or [None] where it has none.
       [drop_levels] takes levels out of it as in [apply], never the category. *)
    val complete: ?drop_levels:string list -> into:string -> string -> string list option
    (* What a translation did that was not forced, every list in order of first
       appearance and with its count: the source paths no row lists, those a row
       drops, those an option drops, and the target levels made up. *)
    type report = {
      placed: int;
      unlisted: (string * int) list;
      dropped: (string * int) list;
      dropped_by_option: (string * int) list;
      invented: (string * int) list;
      ties: int
    }
    (* [drop_levels] removes categories from every target path, dropping a feature
       whose own category is one of them; [keep_only], when not empty, drops a
       feature whose category is not in it. *)
    val apply:
      ?drop_levels:string list -> ?keep_only:string list ->
      t -> Annotation.t -> Annotation.t * report
  end
= struct
    type condition =
      | Always
      | Present of string
      | Equals of string * string
    type row = {
      (* Without the root.  A wildcard row matches any path ending with it. *)
      from_path: string list;
      wildcard: bool;
      condition: condition;
      (* [None] drops what the row matches, and says that it was meant to. *)
      into: string list option
    }
    type t = {
      from_format: string;
      into_format: string;
      rows: row list;
      (* The qualifiers saying that two features belong to one locus, preferred first. *)
      links: string list;
      (* Top-level target categories spanning a whole record, as GenBank's [source]:
         made up once per sequence rather than once per locus. *)
      containers: string list;
      (* Categories made up once for each feature that needs one, as an mRNA is for
         each CDS; every other level made up is shared by the locus. *)
      per_feature: string list;
      (* Attribute keys renamed, or dropped where the new name is [None]. *)
      renames: (string * string option) list
    }

    let formats t = t.from_format, t.into_format

    let fail lnum message =
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "Translation table, line %d: %s" lnum message)
    let format_name lnum field =
      if String.length field > 1 && field.[0] = '#' then
        String.sub field 1 (String.length field - 1) |> String.lowercase_ascii
      else
        fail lnum (Printf.sprintf "a format is named as '#name', not as %S" field)
    let commas s =
      String.split_on_char ',' s |> List.map String.trim |> List.filter ((<>) "")
    let split_on_equals lnum what s =
      match String.index_opt s '=' with
      | Some i when i > 0 -> String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)
      | _ -> fail lnum (Printf.sprintf "%s is written 'name=value', not %S" what s)

    let of_string text =
      let header = ref None and rows = ref [] and links = ref [] and containers = ref []
      and per_feature = ref [] and renames = ref [] in
      List.iteri
        (fun i line ->
          let lnum = i + 1 in
          match String.split_on_char '\t' line |> List.map String.trim with
          | fields when List.for_all ((=) "") fields -> ()
          | "#link" :: rest -> links := !links @ commas (String.concat "," rest)
          | "#container" :: rest -> containers := !containers @ commas (String.concat "," rest)
          | "#per_feature" :: rest -> per_feature := !per_feature @ commas (String.concat "," rest)
          | "#attribute" :: rest ->
            renames :=
              !renames
              @ List.map
                  (fun pair ->
                    let key, renamed = split_on_equals lnum "an attribute rename" pair in
                    key, if renamed = "." then None else Some renamed)
                  (commas (String.concat "," rest))
          | from :: into :: _ when !header = None ->
            header := Some (format_name lnum from, format_name lnum into)
          | _ when !header = None ->
            fail lnum "the header names the two formats, as '#from' and '#to'"
          | first :: _ when first <> "" && first.[0] = '#' ->
            fail lnum (Printf.sprintf "unknown directive %S" first)
          | from :: into :: rest ->
            let condition =
              match rest with
              | [] | [ "" ] -> Always
              | [ c ] when String.contains c '=' ->
                let key, value = split_on_equals lnum "a condition" c in
                Equals (key, value)
              | [ c ] -> Present c
              | _ -> fail lnum "a row has at most three columns" in
            let wildcard, from_path =
              match path_of_string from with
              | [ "*" ] | [] -> fail lnum "a source path names at least one category"
              | "*" :: tail -> true, tail
              | path -> false, path in
            let into = if into = "." then None else Some (path_of_string into) in
            List.accum rows { from_path; wildcard; condition; into }
          | _ -> fail lnum "a row has a source path and a target path")
        (String.split_on_char '\n' text);
      match !header with
      | None ->
        Exception.raise __FUNCTION__ IO_Format
          "Translation table: no header naming the two formats"
      | Some (from_format, into_format) ->
        { from_format; into_format; rows = List.rev !rows; links = !links;
          containers = !containers; per_feature = !per_feature; renames = !renames }
    let of_file path = of_string (read_file path)

    let to_string t =
      let buf = Buffer.create 4096 in
      Printf.bprintf buf "#%s\t#%s\t#when\n" t.from_format t.into_format;
      List.iter
        (fun r ->
          Printf.bprintf buf "%s%s\t%s%s\n"
            (if r.wildcard then "*->" else "") (path_to_string r.from_path)
            (match r.into with None -> "." | Some p -> path_to_string p)
            (match r.condition with
             | Always -> ""
             | Present key -> "\t" ^ key
             | Equals (key, value) -> Printf.sprintf "\t%s=%s" key value))
        t.rows;
      if t.links <> [] then
        Printf.bprintf buf "#link\t%s\n" (String.concat "," t.links);
      if t.containers <> [] then
        Printf.bprintf buf "#container\t%s\n" (String.concat "," t.containers);
      if t.per_feature <> [] then
        Printf.bprintf buf "#per_feature\t%s\n" (String.concat "," t.per_feature);
      if t.renames <> [] then
        Printf.bprintf buf "#attribute\t%s\n"
          (List.map (fun (key, renamed) -> key ^ "=" ^ Option.value ~default:"." renamed)
             t.renames
           |> String.concat ",");
      Buffer.contents buf

    (* THE ROW FOR A PATH.  An exact row before a wildcard, a longer wildcard before a
       shorter, a row with a condition before one without, and otherwise the first:
       the SO's own mapping is written the same way, a feature key alone and then the
       qualifiers that refine it. *)
    let ends_with ~suffix path =
      let lp = List.length path and ls = List.length suffix in
      lp >= ls && List.filteri (fun i _ -> i >= lp - ls) path = suffix
    let holds get = function
      | Always -> true
      | Present key -> get key <> None
      | Equals (key, value) ->
        (match get key with Some values -> List.mem value values | None -> false)
    let row_for t path get =
      List.filter
        (fun r ->
          (if r.wildcard then ends_with ~suffix:r.from_path path else r.from_path = path)
          && holds get r.condition)
        t.rows
      |> List.stable_sort
           (fun a b ->
             let rank r =
               (if r.wildcard then 1 else 0), - List.length r.from_path,
               (if r.condition = Always then 1 else 0) in
             compare (rank a) (rank b))
      |> function r :: _ -> Some r | [] -> None

    type report = {
      placed: int;
      unlisted: (string * int) list;
      dropped: (string * int) list;
      dropped_by_option: (string * int) list;
      invented: (string * int) list;
      ties: int
    }
    (* Counts, in order of first appearance. *)
    let counter () =
      let table = Hashtbl.create 16 and order = ref [] in
      (fun key ->
        match Hashtbl.find_opt table key with
        | Some n -> Hashtbl.replace table key (n + 1)
        | None -> Hashtbl.add table key 1; List.accum order key),
      (fun () -> List.rev_map (fun key -> key, Hashtbl.find table key) !order)

    type node = {
      (* Source order.  A level made up takes the order of the feature it was made for. *)
      n_index: int;
      n_target: string list;
      (* [None] for a level made up. *)
      n_feature: feature_t option;
      n_seq: string;
      n_group: int;
      n_link: (string * string) option;
      mutable n_parent: node option;
      mutable n_children: node list
    }

    let bounds (s: Segment.t) = s.span.low, s.span.low + s.span.length
    (* Every interval of [inner] inside one of [outer]'s: for a spliced parent, the
       child's introns fit inside the parent's, which is stricter than a span covering
       a span and is what tells one isoform from another. *)
    let contains ~outer ~inner =
      List.for_all
        (fun i ->
          let lo, hi = bounds i in
          List.exists (fun o -> let olo, ohi = bounds o in olo <= lo && hi <= ohi) outer)
        inner
    let prefix_of path = List.filteri (fun i _ -> i < List.length path - 1) path


    let apply ?(drop_levels = []) ?(keep_only = []) t ann =
      let count_unlisted, unlisted = counter () and count_dropped, dropped = counter ()
      and count_by_option, dropped_by_option = counter ()
      and count_invented, invented = counter () and ties = ref 0 in
      (* The source in pre-order, each feature with the index of its parent: in
         pre-order that is the last feature seen one level up. *)
      let source =
        let acc = ref [] and last_at_depth = Hashtbl.create 16 and n = ref 0 in
        iter_paths
          (fun ~path feature ->
            let depth = List.length path in
            let parent = if depth <= 2 then None else Hashtbl.find_opt last_at_depth (depth - 1) in
            Hashtbl.replace last_at_depth depth !n;
            List.accum acc (List.tl path, feature, parent);
            incr n)
          ann;
        Array.of_list (List.rev !acc) in
      let targets =
        Array.map
          (fun (path, feature, _) ->
            let shown = path_to_string path in
            match row_for t path (attr_get ann feature) with
            | None -> count_unlisted shown; None
            | Some { into = None; _ } -> count_dropped shown; None
            | Some { into = Some into; _ } ->
              let category = List.nth into (List.length into - 1) in
              if List.mem category drop_levels
                  || (keep_only <> [] && not (List.mem category keep_only)) then begin
                count_by_option shown;
                None
              end else
                Some (List.filter (fun c -> not (List.mem c drop_levels)) into))
          source in
      let link_of feature =
        List.find_map
          (fun key ->
            match attr_get ann feature key with
            | Some (value :: _) -> Some (key, value)
            | _ -> None)
          t.links in
      (* GROUPS.  Features sharing a locus tag are one group -- split where its members
         lie apart on the sequence, the copies of a multicopy gene sharing one symbol.
         A feature with no tag belongs with its source parent when the two translate
         into one branch of the target, and is a group of its own otherwise. *)
      let group = Array.make (Array.length source) (-1) and next_group = ref 0 in
      let fresh () = let g = !next_group in incr next_group; g in
      let by_link = Hashtbl.create 64 and link_order = ref [] in
      Array.iteri
        (fun i (_, feature, _) ->
          match targets.(i), link_of feature with
          | Some _, Some link ->
            if not (Hashtbl.mem by_link link) then List.accum link_order link;
            Hashtbl.replace by_link link (i :: (try Hashtbl.find by_link link with Not_found -> []))
          | _ -> ())
        source;
      List.iter
        (fun link ->
          let extent i =
            let _, (f: feature_t), _ = source.(i) in
            seq_name ann f,
            List.fold_left (fun a s -> min a (fst (bounds s))) max_int f.intervals,
            List.fold_left (fun a s -> max a (snd (bounds s))) min_int f.intervals, i in
          Hashtbl.find by_link link
          |> List.map extent
          |> List.sort compare
          |> List.fold_left
               (fun current (seq, lo, hi, i) ->
                 match current with
                 | Some (g, cseq, chi) when cseq = seq && lo < chi ->
                   group.(i) <- g;
                   Some (g, cseq, max chi hi)
                 | _ ->
                   let g = fresh () in
                   group.(i) <- g;
                   Some (g, seq, hi))
               None
          |> ignore)
        (List.rev !link_order);
      Array.iteri
        (fun i (_, _, parent) ->
          match targets.(i) with
          | Some into when group.(i) < 0 ->
            group.(i) <-
              (match parent with
               | Some p when group.(p) >= 0
                             && (match targets.(p) with
                                 | Some pinto -> List.hd pinto = List.hd into
                                 | None -> false) ->
                 group.(p)
               | _ -> fresh ())
          | _ -> ())
        source;
      (* PLACEMENT, shallowest target first so that a parent is always placed before
         what goes beneath it, and in source order within a depth. *)
      let node_of = Array.make (Array.length source) None and roots = ref [] in
      let is_container prefix = match prefix with [ c ] -> List.mem c t.containers | _ -> false in
      let key_of ~group ~seq prefix =
        (if is_container prefix then Either.Right seq else Either.Left group), prefix in
      let candidates = Hashtbl.create 64 and on_sequence = Hashtbl.create 64 in
      let register n =
        let push table key = Hashtbl.replace table key (n :: (try Hashtbl.find table key with Not_found -> [])) in
        push candidates (key_of ~group:n.n_group ~seq:n.n_seq n.n_target);
        push on_sequence (n.n_seq, n.n_target) in
      let attach parent child =
        child.n_parent <- Some parent;
        parent.n_children <- child :: parent.n_children in
      (* A real feature at [prefix] able to hold [intervals] -- in the same group when
         the feature names its locus, and anywhere on its sequence when it names none,
         there being then nothing a parent could disagree with.  Among several, the last
         before [index] in the source, or failing that the first after it: a flat file
         lists a CDS and then its peptides, which is how NCBI's SARS-CoV-2 record tells
         nsp1 of ORF1ab from nsp1 of ORF1a at the same coordinates. *)
      let choose ~group ~seq ~link ~index ~intervals prefix =
        let fits =
          (try
             if link = None && not (is_container prefix) then Hashtbl.find on_sequence (seq, prefix)
             else Hashtbl.find candidates (key_of ~group ~seq prefix)
           with Not_found -> [])
          |> List.filter
               (fun c ->
                 c.n_seq = seq
                 && contains ~outer:(Option.get c.n_feature).intervals ~inner:intervals) in
        if List.length fits > 1 then incr ties;
        let latest l = List.fold_left (fun a c -> if c.n_index > a.n_index then c else a) (List.hd l) l
        and earliest l = List.fold_left (fun a c -> if c.n_index < a.n_index then c else a) (List.hd l) l in
        match List.partition (fun c -> c.n_index < index) fits with
        | (_ :: _ as before), _ -> Some (latest before)
        | [], (_ :: _ as after) -> Some (earliest after)
        | [], [] -> None in
      (* LEVELS MADE UP.  One the table names [#per_feature], standing directly above a
         feature, is made for that feature, as NCBI and BioPerl make an mRNA for each CDS
         that has none; every other is shared by the locus, as one gene is by its CDSs,
         or by the sequence for a container. *)
      let shared = Hashtbl.create 16 in
      let rec invent ~group ~seq ~link ~index ~intervals ~own prefix =
        let make () =
          count_invented (path_to_string prefix);
          let v =
            { n_index = index; n_target = prefix; n_feature = None; n_seq = seq; n_group = group;
              n_link = link; n_parent = None; n_children = [] } in
          (match prefix with
           | [ _ ] -> List.accum roots v
           | _ ->
             let above = prefix_of prefix in
             attach
               (match choose ~group ~seq ~link ~index ~intervals above with
                | Some p -> p
                | None -> invent ~group ~seq ~link ~index ~intervals ~own:false above)
               v);
          v in
        if own && List.mem (List.nth prefix (List.length prefix - 1)) t.per_feature
           && not (is_container prefix) then
          make ()
        else begin
          let key = key_of ~group ~seq prefix in
          match Hashtbl.find_opt shared key with
          | Some v -> v
          | None ->
            let v = make () in
            Hashtbl.add shared key v;
            v
        end in
      (* A parent the source states: the placed translation of the source parent -- or,
         where that was dropped, of its own -- or the first of its ancestors in the target
         standing at [prefix]. *)
      let rec stated_parent i prefix =
        match source.(i) with
        | _, _, None -> None
        | _, _, Some p ->
          (match node_of.(p) with
           | Some m ->
             let rec up = function
               | Some a when a.n_target = prefix -> Some a
               | Some a -> up a.n_parent
               | None -> None in
             up (Some m)
           | None when targets.(p) = None -> stated_parent p prefix
           | None -> None) in
      let order =
        Array.to_list targets
        |> List.mapi (fun i target -> i, target)
        |> List.filter_map (fun (i, target) -> Option.map (fun target -> i, target) target)
        |> List.stable_sort (fun (i, a) (j, b) -> compare (List.length a, i) (List.length b, j)) in
      List.iter
        (fun (i, target) ->
          let _, (feature: feature_t), _ = source.(i) in
          let n =
            { n_index = i; n_target = target; n_feature = Some feature;
              n_seq = seq_name ann feature; n_group = group.(i); n_link = link_of feature;
              n_parent = None; n_children = [] } in
          node_of.(i) <- Some n;
          (match target with
           | [ _ ] -> List.accum roots n
           | _ ->
             let prefix = prefix_of target in
             attach
               (match stated_parent i prefix with
                | Some p -> p
                | None ->
                  match choose ~group:n.n_group ~seq:n.n_seq ~link:n.n_link ~index:i
                          ~intervals:feature.intervals prefix with
                  | Some p -> p
                  | None ->
                    invent ~group:n.n_group ~seq:n.n_seq ~link:n.n_link ~index:i
                      ~intervals:feature.intervals ~own:true prefix)
               n);
          register n)
        order;
      (* A level made up spans exactly what lies beneath it, on the strand all of that
         agrees on, its intervals in transcription order as every feature's are. *)
      let rec strand_of n =
        match n.n_feature with
        | Some f -> f.strand
        | None ->
          (match List.map strand_of n.n_children with
           | s :: rest when List.for_all ((=) s) rest -> s
           | _ -> None) in
      let rec intervals_of n =
        match n.n_feature with
        | Some f -> f.intervals
        | None ->
          List.concat_map intervals_of n.n_children
          |> List.map bounds
          |> List.sort compare
          |> List.fold_left
               (fun acc (lo, hi) ->
                 match acc with
                 | (plo, phi) :: rest when lo <= phi -> (plo, max phi hi) :: rest
                 | _ -> (lo, hi) :: acc)
               []
          |> List.rev
          |> List.map (fun (lo, hi) -> Segment.make { Sequences.Types.low = lo; length = hi - lo })
          |> (match strand_of n with Some (Sequences.Types.Reverse _) -> List.rev | _ -> Fun.id) in
      let by_index = List.sort (fun a b -> compare a.n_index b.n_index) in
      let paths = Hashtbl.create 64 and path_order = ref [] in
      let rec collect n =
        if not (Hashtbl.mem paths n.n_target) then begin
          Hashtbl.add paths n.n_target ();
          List.accum path_order n.n_target
        end;
        List.iter collect (by_index n.n_children) in
      List.iter collect (by_index !roots);
      (* The target hierarchy is the paths actually used, in order of first use. *)
      let result = ref (create (Hierarchy.of_paths (List.rev !path_order))) in
      (match reference ann with Some r -> result := set_reference !result r | None -> ());
      StringMap.iter
        (fun key values -> List.iter (fun value -> result := add_metadata !result ~key ~value) values)
        (all_metadata ann);
      let copy (f: feature_t) =
        let pairs = ref [] in
        attr_iter ann (fun key values -> List.accum pairs (key, values)) f;
        List.fold_left
          (fun g (key, values) ->
            match List.assoc_opt key t.renames with
            | Some None -> g
            | Some (Some renamed) -> attr_set !result g ~key:renamed ~values
            | None -> attr_set !result g ~key ~values)
          { f with
            seq = intern_seq !result (seq_name ann f);
            source = Option.map (intern_source !result) (feature_source ann f);
            attributes = empty_feature.attributes }
          (List.rev !pairs) in
      let made_up n =
        let feature =
          { empty_feature with
            seq = intern_seq !result n.n_seq;
            intervals = intervals_of n;
            strand = strand_of n;
            id = Option.map snd n.n_link } in
        match n.n_link with
        | Some (key, value) -> attr_set !result feature ~key ~values:[ value ]
        | None -> feature in
      let rec emit n =
        let feature = match n.n_feature with Some f -> copy f | None -> made_up n in
        result := add !result ~path:(implicit_root_name :: n.n_target) feature;
        List.iter emit (by_index n.n_children) in
      List.iter emit (by_index !roots);
      !result,
      { placed = List.length order; unlisted = unlisted (); dropped = dropped ();
        dropped_by_option = dropped_by_option (); invented = invented (); ties = !ties }

    (* THE TABLES THAT COME WITH THE LIBRARY.  GenBank to GFF3 follows the Sequence
       Ontology's INSDC synonyms, and NCBI's practice where the two part: [source] is
       NCBI's [region], which the SO's table lacks, and a [mat_peptide] NCBI's
       [mature_protein_region_of_CDS], more specific than the SO's
       [mature_protein_region].  A gene model is the eukaryotic one, gene -> mRNA ->
       CDS; [apply ~drop_levels:[ "mRNA" ]] gives the viral and prokaryotic one.  GFF3
       to GTF follows AGAT and Ensembl: the first level is a gene, the second a
       transcript, and a type GTF has no row for is kept under its own name --
       [apply ~keep_only] is how to drop it instead. *)
    let table header rows directives =
      List.map (String.concat "\t") (header :: rows @ directives)
      |> String.concat "\n"
      |> of_string

    let genbank_to_gff3 = lazy (table [ "#genbank"; "#gff3"; "#when" ] ([
        [ "source"; "region" ];
        [ "source->gene"; "gene" ];
        [ "source->mRNA"; "gene->mRNA" ];
        [ "source->CDS"; "gene->mRNA->CDS" ];
        (* A part of a gene model nests where it names its locus, and stands alone where
           it names none: SARS-CoV-2's UTRs carry no /gene, and NCBI's GFF3 leaves them at
           the top rather than inventing a gene for each. *)
        [ "source->exon"; "exon" ];
        [ "source->intron"; "intron" ];
        [ "source->5'UTR"; "five_prime_UTR" ];
        [ "source->3'UTR"; "three_prime_UTR" ];
        [ "source->five_prime_UTR"; "five_prime_UTR" ];
        [ "source->three_prime_UTR"; "three_prime_UTR" ] ]
      @ List.concat_map (fun tag -> [
        [ "source->exon"; "gene->mRNA->exon"; tag ];
        [ "source->intron"; "gene->mRNA->intron"; tag ];
        [ "source->5'UTR"; "gene->mRNA->five_prime_UTR"; tag ];
        [ "source->3'UTR"; "gene->mRNA->three_prime_UTR"; tag ];
        [ "source->five_prime_UTR"; "gene->mRNA->five_prime_UTR"; tag ];
        [ "source->three_prime_UTR"; "gene->mRNA->three_prime_UTR"; tag ] ]) [ "locus_tag"; "gene" ]
      @ [
        [ "source->mat_peptide"; "gene->mRNA->CDS->mature_protein_region_of_CDS" ];
        [ "source->sig_peptide"; "gene->mRNA->CDS->signal_peptide_region_of_CDS" ];
        [ "source->transit_peptide"; "gene->mRNA->CDS->transit_peptide_region_of_CDS" ];
        [ "source->propeptide"; "gene->mRNA->CDS->propeptide_region_of_CDS" ];
        [ "source->tRNA"; "gene->tRNA" ];
        [ "source->rRNA"; "gene->rRNA" ];
        [ "source->tmRNA"; "gene->tmRNA" ];
        [ "source->misc_RNA"; "gene->transcript" ];
        [ "source->precursor_RNA"; "gene->primary_transcript" ];
        [ "source->prim_transcript"; "gene->primary_transcript" ];
        [ "source->scRNA"; "gene->scRNA" ];
        [ "source->snRNA"; "gene->snRNA" ];
        [ "source->snoRNA"; "gene->snoRNA" ];
        [ "source->ncRNA"; "gene->ncRNA" ] ]
      @ List.map (fun (cls, term) -> [ "source->ncRNA"; "gene->" ^ term; "ncRNA_class=" ^ cls ]) [
        "antisense_RNA", "antisense_RNA"; "autocatalytically_spliced_intron", "autocatalytically_spliced_intron";
        "guide_RNA", "guide_RNA"; "hammerhead_ribozyme", "hammerhead_ribozyme"; "lncRNA", "lnc_RNA";
        "miRNA", "miRNA"; "piRNA", "piRNA"; "rasiRNA", "rasiRNA"; "ribozyme", "ribozyme";
        "RNase_MRP_RNA", "RNase_MRP_RNA"; "RNase_P_RNA", "RNase_P_RNA"; "scRNA", "scRNA";
        "siRNA", "siRNA"; "snoRNA", "snoRNA"; "snRNA", "snRNA"; "SRP_RNA", "SRP_RNA";
        "telomerase_RNA", "telomerase_RNA"; "vault_RNA", "vault_RNA"; "Y_RNA", "Y_RNA" ]
      @ [
        [ "source->C_region"; "gene->C_gene_segment" ];
        [ "source->D_segment"; "gene->D_gene_segment" ];
        [ "source->J_segment"; "gene->J_gene_segment" ];
        [ "source->V_segment"; "gene->V_gene_segment" ];
        [ "source->N_region"; "N_region" ];
        [ "source->S_region"; "S_region" ];
        [ "source->V_region"; "V_region" ];
        [ "source->misc_feature"; "sequence_feature" ];
        [ "source->misc_binding"; "binding_site" ];
        [ "source->misc_difference"; "sequence_difference" ];
        [ "source->misc_recomb"; "recombination_feature" ];
        [ "source->misc_structure"; "sequence_secondary_structure" ];
        [ "source->mobile_element"; "mobile_genetic_element" ];
        [ "source->modified_base"; "modified_DNA_base" ];
        [ "source->operon"; "operon" ];
        [ "source->oriT"; "oriT" ];
        [ "source->oriC"; "origin_of_replication" ];
        [ "source->rep_origin"; "origin_of_replication" ];
        [ "source->polyA_site"; "polyA_site" ];
        [ "source->primer_bind"; "primer_binding_site" ];
        [ "source->protein_bind"; "protein_binding_site" ];
        [ "source->stem_loop"; "stem_loop" ];
        [ "source->STS"; "STS" ];
        [ "source->telomere"; "telomere" ];
        [ "source->centromere"; "centromere" ];
        [ "source->D-loop"; "D_loop" ];
        [ "source->gap"; "gap" ];
        [ "source->assembly_gap"; "gap" ];
        [ "source->iDNA"; "iDNA" ];
        [ "source->unsure"; "sequence_uncertainty" ];
        [ "source->variation"; "sequence_alteration" ];
        [ "source->repeat_region"; "repeat_region" ];
        [ "source->LTR"; "long_terminal_repeat" ];
        [ "source->satellite"; "satellite_DNA" ];
        [ "source->regulatory"; "regulatory_region" ];
        [ "source->promoter"; "promoter" ];
        [ "source->enhancer"; "enhancer" ];
        [ "source->terminator"; "terminator" ];
        [ "source->attenuator"; "attenuator" ];
        [ "source->polyA_signal"; "polyA_signal_sequence" ];
        [ "source->RBS"; "ribosome_entry_site" ];
        [ "source->TATA_signal"; "TATA_box" ];
        [ "source->CAAT_signal"; "CAAT_signal" ];
        [ "source->GC_signal"; "GC_rich_promoter_region" ];
        [ "source->-10_signal"; "minus_10_signal" ];
        [ "source->-35_signal"; "minus_35_signal" ];
        [ "source->misc_signal"; "regulatory_region" ] ]
      @ List.map (fun (cls, term) -> [ "source->regulatory"; term; "regulatory_class=" ^ cls ]) [
        "attenuator", "attenuator"; "CAAT_signal", "CAAT_signal";
        "DNase_I_hypersensitive_site", "DNAseI_hypersensitive_site"; "enhancer", "enhancer";
        "enhancer_blocking_element", "enhancer_blocking_element"; "GC_signal", "GC_rich_promoter_region";
        "imprinting_control_region", "imprinting_control_region"; "insulator", "insulator";
        "locus_control_region", "locus_control_region"; "matrix_attachment_region", "matrix_attachment_site";
        "minus_10_signal", "minus_10_signal"; "minus_35_signal", "minus_35_signal";
        "polyA_signal_sequence", "polyA_signal_sequence"; "promoter", "promoter";
        "recoding_stimulatory_region", "recoding_stimulatory_region";
        "replication_regulatory_region", "replication_regulatory_region";
        "response_element", "response_element"; "ribosome_binding_site", "ribosome_entry_site";
        "riboswitch", "riboswitch"; "silencer", "silencer"; "TATA_box", "TATA_box";
        "terminator", "terminator";
        "transcriptional_cis_regulatory_region", "transcriptional_cis_regulatory_region" ]
      @ List.map (fun (kind, term) -> [ "source->repeat_region"; term; "rpt_type=" ^ kind ]) [
        "centromeric_repeat", "centromeric_repeat"; "direct", "direct_repeat";
        "dispersed", "dispersed_repeat"; "flanking", "flanking_repeat"; "inverted", "inverted_repeat";
        "long_terminal_repeat", "long_terminal_repeat"; "nested", "nested_repeat";
        "tandem", "tandem_repeat"; "telomeric_repeat", "telomeric_repeat"; "terminal", "terminal_repeat" ])
      [ [ "#link"; "locus_tag,gene" ];
        [ "#per_feature"; "mRNA" ];
        [ "#attribute"; "db_xref=Dbxref,note=Note" ] ])

    let gff3_to_genbank = lazy (table [ "#gff3"; "#genbank" ] ([
        [ "region"; "source" ];
        [ "*->gene"; "source->gene" ];
        [ "*->pseudogene"; "source->gene" ];
        [ "*->mRNA"; "source->mRNA" ];
        [ "*->transcript"; "source->misc_RNA" ];
        [ "*->primary_transcript"; "source->precursor_RNA" ];
        [ "*->tRNA"; "source->tRNA" ];
        [ "*->rRNA"; "source->rRNA" ];
        [ "*->tmRNA"; "source->tmRNA" ];
        [ "*->exon"; "source->exon" ];
        [ "*->intron"; "source->intron" ];
        [ "*->CDS"; "source->CDS" ];
        [ "*->five_prime_UTR"; "source->5'UTR" ];
        [ "*->three_prime_UTR"; "source->3'UTR" ];
        [ "*->mature_protein_region_of_CDS"; "source->mat_peptide" ];
        [ "*->mature_protein_region"; "source->mat_peptide" ];
        [ "*->signal_peptide_region_of_CDS"; "source->sig_peptide" ];
        [ "*->signal_peptide"; "source->sig_peptide" ];
        [ "*->transit_peptide_region_of_CDS"; "source->transit_peptide" ];
        [ "*->transit_peptide"; "source->transit_peptide" ];
        [ "*->propeptide_region_of_CDS"; "source->propeptide" ];
        [ "*->propeptide"; "source->propeptide" ];
        [ "*->C_gene_segment"; "source->C_region" ];
        [ "*->D_gene_segment"; "source->D_segment" ];
        [ "*->J_gene_segment"; "source->J_segment" ];
        [ "*->V_gene_segment"; "source->V_segment" ];
        [ "*->sequence_feature"; "source->misc_feature" ];
        [ "*->binding_site"; "source->misc_binding" ];
        [ "*->sequence_difference"; "source->misc_difference" ];
        [ "*->recombination_feature"; "source->misc_recomb" ];
        [ "*->sequence_secondary_structure"; "source->misc_structure" ];
        [ "*->mobile_genetic_element"; "source->mobile_element" ];
        [ "*->modified_DNA_base"; "source->modified_base" ];
        [ "*->operon"; "source->operon" ];
        [ "*->oriT"; "source->oriT" ];
        [ "*->origin_of_replication"; "source->rep_origin" ];
        [ "*->polyA_site"; "source->polyA_site" ];
        [ "*->primer_binding_site"; "source->primer_bind" ];
        [ "*->protein_binding_site"; "source->protein_bind" ];
        [ "*->stem_loop"; "source->stem_loop" ];
        [ "*->STS"; "source->STS" ];
        [ "*->telomere"; "source->telomere" ];
        [ "*->centromere"; "source->centromere" ];
        [ "*->D_loop"; "source->D-loop" ];
        [ "*->gap"; "source->gap" ];
        [ "*->iDNA"; "source->iDNA" ];
        [ "*->sequence_uncertainty"; "source->unsure" ];
        [ "*->sequence_alteration"; "source->variation" ];
        [ "*->N_region"; "source->N_region" ];
        [ "*->S_region"; "source->S_region" ];
        [ "*->V_region"; "source->V_region" ] ]
      @ List.map (fun term -> [ "*->" ^ term; "source->ncRNA" ]) [
        "ncRNA"; "antisense_RNA"; "autocatalytically_spliced_intron"; "guide_RNA";
        "hammerhead_ribozyme"; "lnc_RNA"; "miRNA"; "piRNA"; "rasiRNA"; "ribozyme"; "RNase_MRP_RNA";
        "RNase_P_RNA"; "scRNA"; "siRNA"; "snoRNA"; "snRNA"; "SRP_RNA"; "telomerase_RNA";
        "vault_RNA"; "Y_RNA" ]
      @ List.map (fun term -> [ "*->" ^ term; "source->regulatory" ]) [
        "regulatory_region"; "promoter"; "enhancer"; "terminator"; "attenuator";
        "polyA_signal_sequence"; "ribosome_entry_site"; "TATA_box"; "CAAT_signal";
        "GC_rich_promoter_region"; "minus_10_signal"; "minus_35_signal";
        "DNAseI_hypersensitive_site"; "enhancer_blocking_element"; "imprinting_control_region";
        "insulator"; "locus_control_region"; "matrix_attachment_site"; "recoding_stimulatory_region";
        "replication_regulatory_region"; "response_element"; "riboswitch"; "silencer";
        "transcriptional_cis_regulatory_region" ]
      @ List.map (fun term -> [ "*->" ^ term; "source->repeat_region" ]) [
        "repeat_region"; "long_terminal_repeat"; "satellite_DNA"; "centromeric_repeat";
        "direct_repeat"; "dispersed_repeat"; "flanking_repeat"; "inverted_repeat"; "nested_repeat";
        "tandem_repeat"; "telomeric_repeat"; "terminal_repeat" ])
      [ [ "#container"; "source" ];
        [ "#attribute"; "ID=.,Parent=.,Dbxref=db_xref,Note=note" ] ])

    let gff3_to_gtf = lazy (table [ "#gff3"; "#gtf" ] ([
        [ "*->gene"; "gene" ];
        [ "*->pseudogene"; "gene" ];
        [ "*->exon"; "gene->transcript->exon" ];
        [ "*->CDS"; "gene->transcript->CDS" ];
        [ "*->five_prime_UTR"; "gene->transcript->five_prime_utr" ];
        [ "*->three_prime_UTR"; "gene->transcript->three_prime_utr" ];
        [ "*->UTR"; "gene->transcript->UTR" ];
        [ "*->start_codon"; "gene->transcript->start_codon" ];
        [ "*->stop_codon"; "gene->transcript->stop_codon" ];
        [ "*->stop_codon_redefined_as_selenocysteine"; "gene->transcript->Selenocysteine" ];
        [ "*->intron"; "gene->transcript->intron" ];
        [ "*->mature_protein_region_of_CDS"; "gene->transcript->mature_protein_region_of_CDS" ];
        [ "*->mature_protein_region"; "gene->transcript->mature_protein_region" ];
        [ "*->signal_peptide_region_of_CDS"; "gene->transcript->signal_peptide_region_of_CDS" ];
        [ "*->transit_peptide_region_of_CDS"; "gene->transcript->transit_peptide_region_of_CDS" ];
        [ "*->propeptide_region_of_CDS"; "gene->transcript->propeptide_region_of_CDS" ];
        [ "region"; "." ] ]
      @ List.map (fun term -> [ "*->" ^ term; "gene->transcript" ]) [
        "mRNA"; "transcript"; "primary_transcript"; "pseudogenic_transcript"; "tRNA"; "rRNA";
        "tmRNA"; "ncRNA"; "antisense_RNA"; "guide_RNA"; "lnc_RNA"; "miRNA"; "piRNA"; "rasiRNA";
        "ribozyme"; "RNase_MRP_RNA"; "RNase_P_RNA"; "scRNA"; "siRNA"; "snoRNA"; "snRNA"; "SRP_RNA";
        "telomerase_RNA"; "vault_RNA"; "Y_RNA" ])
      [ [ "#per_feature"; "transcript" ] ])

    let gtf_to_gff3 = lazy (table [ "#gtf"; "#gff3" ] [
        [ "gene"; "gene" ];
        [ "gene->transcript"; "gene->transcript" ];
        [ "*->exon"; "gene->transcript->exon" ];
        [ "*->CDS"; "gene->transcript->CDS" ];
        [ "*->five_prime_utr"; "gene->transcript->five_prime_UTR" ];
        [ "*->three_prime_utr"; "gene->transcript->three_prime_UTR" ];
        [ "*->5UTR"; "gene->transcript->five_prime_UTR" ];
        [ "*->3UTR"; "gene->transcript->three_prime_UTR" ];
        [ "*->UTR"; "gene->transcript->UTR" ];
        [ "*->start_codon"; "gene->transcript->start_codon" ];
        [ "*->stop_codon"; "gene->transcript->stop_codon" ];
        [ "*->Selenocysteine"; "gene->transcript->stop_codon_redefined_as_selenocysteine" ];
        [ "*->intron"; "gene->transcript->intron" ];
        [ "*->mature_protein_region_of_CDS"; "gene->transcript->mature_protein_region_of_CDS" ] ]
      [])

    let builtin_pairs = [ "genbank", "gff3"; "gff3", "genbank"; "gff3", "gtf"; "gtf", "gff3" ]
    let builtin ~from ~into =
      match String.lowercase_ascii from, String.lowercase_ascii into with
      | "genbank", "gff3" -> Some (Lazy.force genbank_to_gff3)
      | "gff3", "genbank" -> Some (Lazy.force gff3_to_genbank)
      | "gff3", "gtf" -> Some (Lazy.force gff3_to_gtf)
      | "gtf", "gff3" -> Some (Lazy.force gtf_to_gff3)
      | _ -> None

    (* ONE PATH AT A TIME, for whoever declares paths rather than reads features -- a
       NailIt index deriving one format's column from another's.  With no feature,
       there is nothing to test a condition on, and only unconditioned rows apply. *)
    let path ?(drop_levels = []) t p =
      match row_for t p (fun _ -> None) with
      | None -> None
      | Some { into = None; _ } -> Some None
      | Some { into = Some into; _ } ->
        let category = List.nth into (List.length into - 1) in
        if List.mem category drop_levels then Some None
        else Some (Some (List.filter (fun c -> not (List.mem c drop_levels)) into))
    (* The standard path in a format for a category, as the built-in table into that
       format spells it: the target of its first unconditioned row ending there.  GFF3's
       comes from the GenBank table, GTF's from the GFF3 one, GenBank's from the GFF3
       one. *)
    let complete ?(drop_levels = []) ~into category =
      let table =
        match String.lowercase_ascii into with
        | "gff3" -> Some genbank_to_gff3
        | "gtf" -> Some gff3_to_gtf
        | "genbank" -> Some gff3_to_genbank
        | _ -> None in
      Option.bind table (fun table ->
        List.find_map
          (fun r ->
            match r.condition, r.into with
            | Always, Some p when List.nth p (List.length p - 1) = category ->
              Some (List.filter (fun c -> c = category || not (List.mem c drop_levels)) p)
            | _ -> None)
          (Lazy.force table).rows)
  end
