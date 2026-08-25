(*
    Consensus.ml -- (c) 2017-2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Consensus.ml implements:
     * a module to compute a consensus from (multiple) alignments
     * a module to compute a consensus from a pileup.

    Reading the pileup is not done here: that is Mpileup.ml, which counts a
    position into genotypes.  What this file adds is the second reading of
    those counts -- the one that asks which genotype won rather than how
    much evidence each has -- together with the machinery for insertions,
    which are the part a consensus cannot take one position at a time.

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

let ( .@() ) = Bytes.( .@() )
let ( .@()<- ) = Bytes.( .@()<- )

include (
  struct
    (* PRIVATE *)
    (* Captured before the submodule below shadows the name *)
    module Mpileup' = Mpileup
    let dashes_re = Str.regexp "[-]+"
    let check_percentage __FUNCTION__ arg_name n =
      if n < 0 || n >= 100 then
        Exception.raise __FUNCTION__ Initialize
          (Printf.sprintf "Argument '%s' must be a percentage (found %d)" arg_name n);
    (* PUBLIC *)
    module Alignment =
      struct
        let remove_tips ?(tip_gap_multiplier = 2.5) ?(max_tip_threshold = 30) s =
          if tip_gap_multiplier < 0. then
            Exception.raise __FUNCTION__ Initialize
              (Printf.sprintf "Argument 'tip_gap_multiplier' cannot be negative (found %g)" tip_gap_multiplier);
          check_percentage __FUNCTION__ "max_tip_threshold" max_tip_threshold;
          let split_at_dashes = String.Split.full_as_list dashes_re s in
          let max_deleted_len =
            begin
              List.fold_left
                (fun sum -> function
                  | Str.Text s -> sum + String.length s
                  | Str.Delim _ -> sum) 0
                split_at_dashes
            end * max_tip_threshold / 100 in
          let rec process_rec rem res acc_seq_len acc_len =
            (* The invariant is to have rem as Str.Delim :: tl where the delimiter has already been processed *)
            match rem with
            | [] -> res
            | Str.Text _ :: [] -> (* Can only happen at the beginning *)
              assert (res = []);
              rem
            | Str.Text s :: Str.Delim d :: tl -> (* Can only happen at the beginning *)
              assert (res = []);
              process_rec (Str.Delim "" :: Str.Text s :: Str.Delim d :: tl) res acc_seq_len acc_len
            (* [res] holds what has already been cleared, in reverse, and the
               remainder is still forward, which is why the two are combined this
               way -- exactly as the bail-out below does.  Returning [rem] alone
               dropped the whole prefix, and for an aligned line that is not a
               truncation one can recover from: every column past the cut shifts. *)
            | Str.Delim _ :: [] | Str.Delim _ :: Str.Text _ :: [] ->
              List.rev_append rem res
            | Str.Delim d_1 :: Str.Text s :: Str.Delim d_2 :: tl ->
              (* Not that at this point the first gap has already been processed *)
              let l_d_1 = String.length d_1 and l_s = String.length s and l_d_2 = String.length d_2 in
              let l_d_2_f = float_of_int l_d_2 and acc_seq_len = acc_seq_len + l_s in
              if acc_seq_len > max_deleted_len then
                List.rev_append rem res
              else begin
                if l_d_2_f >= tip_gap_multiplier *. float_of_int acc_seq_len then begin
                  (* Replace everything with dashes *)
                  let l_d = acc_len + l_d_1 + l_s + l_d_2 in
                  (* We keep track of the length of the sequence we've erased *)
                  process_rec (Str.Delim (String.make l_d '-') :: tl) [] acc_seq_len 0
                end else
                  (* Carry on *)
                  process_rec (Str.Delim d_2 :: tl) (Str.Text s :: Str.Delim d_1 :: res)
                    acc_seq_len (acc_len + l_d_1 + l_s)
              end
            | _ ->
              assert false in
          (* As we are processing things twice, from left to right and from right to left,
              the final order will be correct *)
          let res = process_rec split_at_dashes [] 0 0 in
          let res = process_rec res [] 0 0 and buf = Buffer.create 1024 in
          List.iter
            (function Str.Delim s | Str.Text s -> Buffer.add_string buf s)
            res;
          Buffer.contents buf
        let replace_side_dashes_bytes ?(replacement = ' ') seq =
          let seq_len = Bytes.length seq and first_non_dash_idx = ref 0 in
          while !first_non_dash_idx < seq_len && seq.@(!first_non_dash_idx) = '-' do
            seq.@(!first_non_dash_idx) <- replacement;
            incr first_non_dash_idx
          done;
          let last_non_dash_idx = seq_len - 1 |> ref in
          while !last_non_dash_idx >= 0 && seq.@(!last_non_dash_idx) = '-' do
            seq.@(!last_non_dash_idx) <- replacement;
            decr last_non_dash_idx
          done;
          !first_non_dash_idx, !last_non_dash_idx
        let replace_side_dashes ?(replacement = ' ') seq =
          let seq = Bytes.of_string seq in
          let first_non_dash_idx, last_non_dash_idx = replace_side_dashes_bytes ~replacement seq in
          Bytes.to_string seq, first_non_dash_idx, last_non_dash_idx
    end
    (* Processes a string.t array *)
    let of_alignment
        ?(tip_gap_multiplier = 2.5) ?(max_tip_threshold = 30) ?(min_branch_threshold = 40)
        ?(consensus_window = 7) ?(min_coverage = 5) al =
      if tip_gap_multiplier < 0. then
        Exception.raise __FUNCTION__ Initialize
          (Printf.sprintf "Argument 'tip_gap_multiplier' cannot be negative (found %g)" tip_gap_multiplier);
      check_percentage __FUNCTION__ "max_tip_threshold" max_tip_threshold;
      check_percentage __FUNCTION__ "min_branch_threshold" min_branch_threshold;
      if consensus_window < 1 then
        Exception.raise __FUNCTION__ Initialize
          (Printf.sprintf "Argument 'consensus_window' must be positive (found %d)" consensus_window);
      if min_coverage < 0 then
        Exception.raise __FUNCTION__ Initialize
          (Printf.sprintf "Argument 'min_coverage' cannot be negative (found %d)" min_coverage);
      let n_seqs = Array.length al in
      if n_seqs = 0 then
        ""
      else begin
        let seq_len = String.length al.(0) in
        if consensus_window < 0 || consensus_window > seq_len then
          Exception.raise __FUNCTION__ Initialize
            (Printf.sprintf
              "Argument 'consensus_window' cannot be negative or greater than the alignment length %d (found %d)"
              seq_len consensus_window);
        (* We lint sequences and remove tips *)
        let al =
          Array.mapi
            (fun i seq ->
              if String.length seq <> seq_len then
                Exception.raise __FUNCTION__ IO_Format
                  (Printf.sprintf "Incompatible sequence length on line %d (expected %d, found %d)"
                    i seq_len (String.length seq));
              Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:true seq
                |> Alignment.remove_tips ~tip_gap_multiplier ~max_tip_threshold |> Bytes.of_string)
            al in
        (* We replace stretches of dashes on the sides with spaces and compute coverage *)
        let cov = Array.make seq_len 0 in
        Array.iter
          (fun seq ->
            let first_non_dash_idx, last_non_dash_idx = Alignment.replace_side_dashes_bytes ~replacement:' ' seq in
            for i = first_non_dash_idx to last_non_dash_idx do
              cov.(i) <- cov.(i) + 1
            done)
          al;
        let max_res = Array.make seq_len 0 and res = Bytes.make seq_len 'n' in
        (* We instate gaps *)
        for i = 0 to seq_len - 1 do
          (* We compute the most frequent character in the column *)
          let stats = ref CharMap.empty in
          Array.iter
            (fun seq ->
              let c = seq.@(i) in
              match CharMap.find_opt c !stats with
              | None ->
                stats := CharMap.add c (ref 1) !stats
              | Some n ->
                incr n)
            al;
          let max_n = ref 0 and max_c = ref 'n' in
          CharMap.iter
            (fun c n ->
              if !n > !max_n then begin
                max_n := !n;
                max_c := c
              end)
            !stats;
          let max_n = !max_n in
          if !max_c = '-' && 100 * max_n >= min_branch_threshold * cov.(i) then begin
            max_res.(i) <- max_n;
            res.@(i) <- '-'
          end
        done;
        (* We compute the consensus by sliding k-mer window *)
        let kmers =
          Array.init n_seqs
            (fun i ->
              Bytes.sub_string al.(i) 0 consensus_window |> KMers.SlidingWindow.make) in
        let n_spaces =
          Array.init n_seqs
            (fun i ->
              let kmer = KMers.SlidingWindow.contents kmers.(i) and n = ref 0 in
              String.iter
                (function
                  | ' ' -> incr n
                  | _ -> ())
                kmer;
              !n) in
        for i_col = 0 to seq_len - consensus_window do
          (* We determine the most frequent k-mer at this position *)
          let stats = ref StringMap.empty in
          Array.iteri
            (fun i_seq kmer ->
              (* We update k-mer and number of spaces *)
              if i_col > 0 then begin
                let c = al.(i_seq).@(consensus_window + i_col - 1) in
                let old_c = KMers.SlidingWindow.add_char kmer c in
                if old_c = ' ' then
                  n_spaces.(i_seq) <- n_spaces.(i_seq) - 1;
                if c = ' ' then
                  n_spaces.(i_seq) <- n_spaces.(i_seq) + 1
              end;
              let k = KMers.SlidingWindow.contents kmer in
              (* Side k-mers containing spaces are not taken into account *)
              if n_spaces.(i_seq) = 0 then begin
                match StringMap.find_opt k !stats with
                | None ->
                  stats := StringMap.add k (ref 1) !stats
                | Some n ->
                  incr n
              end)
            kmers;
          let max_n = ref 0 and max_k = ref "" in
          StringMap.iter
            (fun k n ->
              if !n > !max_n then begin
                max_n := !n;
                max_k := k
              end)
            !stats;
          let max_n = !max_n in
          String.iteri
            (fun i c ->
              let i_c = i_col + i in
              if max_n > max_res.(i_c) && 100 * max_n >= min_branch_threshold * cov.(i_c) then begin
                max_res.(i_c) <- max_n;
                res.@(i_c) <-
                  (* The case conveys information about coverage *)
                  if max_n >= min_coverage then
                    Char.uppercase_ascii c
                  else
                    Char.lowercase_ascii c
              end)
            !max_k
        done;
        Bytes.to_string res
      end
    (* PUBLIC *)
    module Mpileup =
      struct
        (* The consensus's reading of a position, which is not a variant caller's.
           A caller wants every genotype kept apart and its evidence with it; a
           caller building a sequence wants only to know which genotype won, so
           the counts are inverted here into frequency -> the symbols holding it,
           and what is left to decide is the tie at the top.
           Two departures from the summary are deliberate.  A deleted base is a
           vote here -- the consensus at that position IS the deletion -- where a
           variant caller counts it as a read that voted for nothing.  And a
           deletion genotype is dropped rather than counted, because '-1G' is a
           statement about the NEXT position, which writes its own '*' when it
           arrives; counting both would count one deletion twice. *)
        type t = {
          seq: string;
          pos: int;
          (* Frequency -> the symbols seen that many times, one character each,
             '*' being a base deleted with respect to the reference *)
          symbols: string array IntMap.t;
          (* Frequency -> the sequences inserted between this position and the
             next one that many times *)
          insertions: string array IntMap.t
        }
        (* Statistics over a run.  They are returned rather than printed because
           a library has no business deciding what a caller wants said, and
           because a number that is returned can be asserted by a test *)
        type stats_t = {
          positions: int;
          (* Ties broken by drawing, which are the positions at which the
             consensus is one of several equally supported readings *)
          ambiguities: int;
          insertions: int
        }
        let invert m =
          let res = ref IntMap.empty in
          StringMap.iter
            (fun symbol count ->
              res :=
                IntMap.add count (symbol :: Option.value ~default:[] (IntMap.find_opt count !res))
                  !res)
            m;
          IntMap.map Array.of_list !res
        let of_summary (u: Mpileup'.Summary.t) =
          let symbols = ref StringMap.empty and insertions = ref StringMap.empty in
          List.iter
            (fun (g: Mpileup'.Genotype.t) ->
              match g.kind with
              | Mpileup'.Genotype.Base -> symbols := StringMap.add g.symbol g.count !symbols
              | Mpileup'.Genotype.Short_indel | Mpileup'.Genotype.Long_indel ->
                if g.symbol.[0] = '+' then
                  insertions :=
                    StringMap.add (String.sub g.symbol 1 (String.length g.symbol - 1)) g.count
                      !insertions)
            u.genotypes;
          if u.gaps > 0 then
            symbols := StringMap.add "*" u.gaps !symbols;
          { seq = u.seq; pos = u.pos; symbols = invert !symbols; insertions = invert !insertions }
        (* Which of several equally frequent symbols to take.  The tool this
           comes from drew on the global PRNG, which nothing in it ever seeded --
           so the choice was already the same from one run to the next, but from
           a seed nobody had chosen and nobody could state.  It is drawn here
           from a state the caller seeds, which keeps that determinism and makes
           it something that can be written down, varied deliberately, and
           reproduced elsewhere *)
        let disambiguate ?(verbose = false) ~ambiguities random line what freq symbols =
          match Array.length symbols with
          | 0 ->
            Exception.raise __FUNCTION__ Algorithm
              (Printf.sprintf "No %s to choose from at '%s':%d" what line.seq line.pos)
          | 1 -> symbols.(0)
          | n ->
            incr ambiguities;
            let chosen = symbols.(Random.State.int random n) in
            if verbose then
              Printf.eprintf "%s: At '%s':%d: ambiguous %s %s at frequency %d - chose '%s'\n%!"
                __FUNCTION__ line.seq line.pos what
                (Array.to_list symbols |> List.map (fun s -> "'" ^ s ^ "'") |> String.concat ", ")
                freq chosen;
            chosen
        (* The machinery to recognise staggered insertions, which is what a
           repetitive region does to them: one insertion event is reported by
           different reads at different offsets, so what happened once looks like
           several separate things.  An open insertion is therefore carried
           across positions, its expected remaining sequence rewritten at each
           one as the consensus takes a character, and written out once its
           accumulated support has cleared the thresholds *)
        module OpenInsertions =
          struct
            type t = {
              (* Accumulated count of compatible insertions so far *)
              acc: int;
              (* The LOWEST coverage of a non-indel genotype seen within the
                 insertion's support, which is what its fraction is taken of *)
              cov: int;
              (* Remaining span of the insertion.  At zero it leaves the set *)
              rem: int;
              state: state_t
            }
            and state_t =
              (* It has occurred but has not been written, its support being too
                 low so far *)
              | Regular
              (* It has been written, and will not be written again *)
              | Output
              (* Another insertion was written while this one was open.  This one
                 is written only if it explicitly occurs again later *)
              | Shadowed
            let is_nucl = function
              | "A" | "a" | "C" | "c" | "G" | "g" | "T" | "t" -> true
              | _ -> false
            (* Update the open insertions given the most frequent non-insertion
               symbol and the most frequent insertion at this position.  Returns
               the insertion to be written into the consensus here, or "" *)
            let update ?(min_fraction = 0.6) ?(min_coverage = 2) ?(multiple_insertions = false)
                ?(verbose = false) ~ambiguities random line ois symbol sym_freq insertion ins_freq =
              if String.length symbol <> 1 then
                Exception.raise __FUNCTION__ Algorithm
                  (Printf.sprintf "Expected a single-character symbol, found '%s'" symbol);
              (* First the expected sequence of every open insertion, given the
                 character the consensus has just taken *)
              let res = ref StringMap.empty in
              if symbol = "*" then
                (* The consensus takes no character at a deletion, so the open
                   insertions do not advance either *)
                res := !ois
              else
                StringMap.iter
                  (fun seq payl ->
                    let new_seq = String.sub seq 1 (String.length seq - 1) ^ symbol in
                    let payl =
                      (* Not obviously reachable, but should two open insertions
                         become the same sequence, the better supported wins *)
                      match StringMap.find_opt new_seq !res with
                      | Some found when found.acc > payl.acc -> found
                      | Some _ | None -> payl in
                    if payl.rem > 1 then begin
                      let cov = if is_nucl symbol then min sym_freq payl.cov else payl.cov in
                      res := StringMap.add new_seq { payl with cov = cov; rem = payl.rem - 1 } !res
                    end else
                      (* It has run out of its original support *)
                      res := StringMap.remove new_seq !res)
                  !ois;
              (* Then the insertion seen here, if there is one *)
              if insertion <> "" then begin
                let acc, cov, state =
                  match StringMap.find_opt insertion !res with
                  | Some found ->
                    (* Here cov has been updated already *)
                    found.acc + ins_freq, found.cov, begin
                      match found.state, multiple_insertions with
                      (* Under multiple_insertions, a shadowed insertion occurring
                         again becomes writable once more *)
                      | Regular, _ | Shadowed, true -> Regular
                      | Shadowed, false -> Shadowed
                      (* One insertion is written at most once, whatever happens *)
                      | Output, _ -> Output
                    end
                  | None -> ins_freq, sym_freq, Regular in
                (* The span is always reset *)
                res :=
                  StringMap.add insertion
                    { acc = acc; cov = cov; rem = String.length insertion - 1; state = state } !res
              end;
              (* The most frequent open insertion that has cleared both thresholds
                 and has not been written yet, if there is one *)
              let ok = ref IntMap.empty in
              StringMap.iter
                (fun ins payl ->
                  if begin
                    float_of_int payl.acc >= min_fraction *. float_of_int payl.cov &&
                    payl.acc >= min_coverage && begin
                      match payl.state with
                      | Regular -> true
                      | Shadowed | Output -> false
                    end
                  end then
                    ok :=
                      IntMap.add payl.acc
                        (ins :: Option.value ~default:[] (IntMap.find_opt payl.acc !ok)) !ok)
                !res;
              if IntMap.is_empty !ok then begin
                ois := !res;
                ""
              end else begin
                let acc, candidates = IntMap.max_binding !ok in
                let chosen =
                  disambiguate ~verbose ~ambiguities random line "open insertions" acc
                    (Array.of_list candidates) in
                ois :=
                  StringMap.mapi
                    (fun ins payl ->
                      if ins = chosen then
                        { payl with state = Output }
                      else
                        match payl.state with
                        | Regular | Shadowed -> { payl with state = Shadowed }
                        | Output -> payl)
                    !res;
                chosen
              end
          end
        (* Build a consensus sequence, and the coverage track beside it, from a
           pileup.  Both are written as the pileup is read, one sequence at a
           time, so that neither is ever held whole beyond the sequence in hand *)
        let from_mpileup ?(insertion_min_fraction = 0.6) ?(insertion_min_coverage = 2)
            ?(multiple_insertions = false) ?(seed = 0) ?(verbose = false) ?(quality_offset = 33)
            ~sequence ~bedgraph input =
          if insertion_min_fraction <= 0. then
            Exception.raise __FUNCTION__ Initialize
              (Printf.sprintf "Argument 'insertion_min_fraction' must be positive (found %g)"
                insertion_min_fraction);
          if insertion_min_coverage < 1 then
            Exception.raise __FUNCTION__ Initialize
              (Printf.sprintf "Argument 'insertion_min_coverage' must be at least 1 (found %d)"
                insertion_min_coverage);
          let random = Random.State.make [| seed |] and curr_seq_name = ref ""
          and curr_seq = Buffer.create 1048576 and curr_bg = ref [] and line_number = ref 0
          and positions = ref 0 and ambiguities = ref 0 and written = ref 0 in
          let add_to_bg length value =
            match !curr_bg with
            | [] -> curr_bg := [ length, value ]
            | (curr_len, curr_val) :: tl ->
              curr_bg :=
                if value = curr_val then
                  (curr_len + length, curr_val) :: tl
                else
                  (length, value) :: !curr_bg
          and output_current () =
            if !curr_seq_name <> "" then begin
              Printf.fprintf sequence ">%s\n%s\n" !curr_seq_name (Buffer.contents curr_seq);
              let idx = ref 0 in
              List.iter
                (fun (length, value) ->
                  (* A BedGraph interval is zero-based and half-open, so its end
                     is one past the last base rather than the last base itself.
                     Written inclusively, a run of one base would be an interval
                     of none *)
                  Printf.fprintf bedgraph "%s\t%d\t%d\t%d\n" !curr_seq_name !idx (!idx + length)
                    value;
                  idx := !idx + length)
                (List.rev !curr_bg)
            end;
            Buffer.clear curr_seq;
            curr_bg := [] in
          let open_insertions = ref StringMap.empty in
          begin try
            while true do
              incr line_number;
              let line =
                input_line input
                  |> Mpileup'.summarize ~quality_offset ~line_number:!line_number
                  |> of_summary in
              if line.seq <> !curr_seq_name then begin
                output_current ();
                (* An insertion still open at the end of one sequence has nothing
                   to say about the beginning of the next one *)
                open_insertions := StringMap.empty;
                curr_seq_name := line.seq
              end;
              incr positions;
              let symbol, sym_freq =
                if IntMap.is_empty line.symbols then begin
                  (* A position with no coverage, which is not a deletion: a
                     deletion is covered by reads that say so, while here there
                     is no information at all.  The segment is kept by writing an
                     'N', and that N goes to the insertions too, invalidating
                     them *)
                  Buffer.add_char curr_seq 'N';
                  add_to_bg 1 0;
                  "N", 0
                end else begin
                  let freq, symbols = IntMap.max_binding line.symbols in
                  let symbol =
                    disambiguate ~verbose ~ambiguities random line "consensus" freq symbols in
                  if symbol <> "*" then begin
                    Buffer.add_string curr_seq symbol;
                    add_to_bg 1 freq
                  end;
                  symbol, freq
                end in
              let insertion, ins_freq =
                if IntMap.is_empty line.insertions then
                  "", 0
                else begin
                  let freq, insertions = IntMap.max_binding line.insertions in
                  disambiguate ~verbose ~ambiguities random line "insertions" freq insertions, freq
                end in
              (* The open insertions must be updated at every position, or their
                 remaining span stops meaning what it says *)
              let to_be_output =
                OpenInsertions.update ~min_fraction:insertion_min_fraction
                  ~min_coverage:insertion_min_coverage ~multiple_insertions ~verbose ~ambiguities
                  random line open_insertions symbol sym_freq insertion ins_freq in
              if verbose && not (StringMap.is_empty !open_insertions) then begin
                let header_printed = ref false in
                StringMap.iter
                  (fun insertion payl ->
                    (* An arbitrary threshold, only to keep the amount of output
                       down *)
                    let acc = payl.OpenInsertions.acc in
                    if float_of_int acc >= float_of_int payl.cov /. 3. then begin
                      if not !header_printed then begin
                        Printf.eprintf "%s: Large accumulated insertions at '%s':%d:" __FUNCTION__
                          line.seq line.pos;
                        header_printed := true
                      end;
                      Printf.eprintf " '%s'x%d%s/%d;" insertion acc begin
                        match payl.state with
                        | OpenInsertions.Regular -> ""
                        | OpenInsertions.Output -> "(!)"
                        | OpenInsertions.Shadowed -> "(x)"
                      end payl.cov
                    end)
                  !open_insertions;
                if !header_printed then
                  Printf.eprintf "\n%!"
              end;
              if to_be_output <> "" then begin
                let payl = StringMap.find to_be_output !open_insertions in
                Buffer.add_string curr_seq to_be_output;
                add_to_bg (String.length to_be_output) payl.OpenInsertions.acc;
                incr written
              end
            done
          with End_of_file ->
            output_current ()
          end;
          { positions = !positions; ambiguities = !ambiguities; insertions = !written }
      end
  end: sig
    module Alignment:
      sig
        (* Replace dashes from both sides of an aligned sequence.
           Returns indices of the first and last character of what is left, zero-based and inclusive *)
        val replace_side_dashes: ?replacement:char -> string -> string * int * int
        val replace_side_dashes_bytes: ?replacement:char -> bytes -> int * int
        (* Remove tips (short blocks separated from the core by a long gap) from both sides of an aligned sequence.
          Parameters have the following meaning:
          * tip_gap_multiplier:
              eliminate terminal alignment segment if surrounded by gaps which are longer than
              tip_gap_multiplier * length(segment)
          * max_tip_threshold:
              eliminate terminal alignment segment if its number of non-gaps is no more than
              max_tip_threshold / 100 of the total number of non-gaps in the line *)
        val remove_tips: ?tip_gap_multiplier:float -> ?max_tip_threshold:int -> string -> string
      end
    val of_alignment: ?tip_gap_multiplier:float -> ?max_tip_threshold:int -> ?min_branch_threshold:int ->
                      ?consensus_window:int -> ?min_coverage:int -> string array -> string
    module Mpileup:
      sig
        (* What a run did.  These are returned rather than printed: a library has
           no business deciding what a caller wants said, and a number that is
           returned is a number a test can assert *)
        type stats_t = {
          positions: int;
          (* Ties broken by drawing -- the positions at which the consensus is
             one of several equally supported readings *)
          ambiguities: int;
          insertions: int
        }
        (* Build a consensus sequence, and the BedGraph coverage track beside it,
           from a pileup read line by line from the given channel.
           An insertion is written into the consensus once its accumulated
           support reaches [insertion_min_fraction] of the lowest coverage seen
           within its span (default 0.6) and is at least [insertion_min_coverage]
           reads (default 2).  Staggered copies of one insertion, which is what a
           repetitive region makes of it, are recognised as the one event they
           are; [multiple_insertions] additionally lets an insertion that another
           has shadowed be written if it occurs again.
           [seed] fixes the draw that breaks a tie between equally supported
           readings, so that a run can be reproduced exactly or its ties
           deliberately explored *)
        val from_mpileup:
          ?insertion_min_fraction:float -> ?insertion_min_coverage:int ->
          ?multiple_insertions:bool -> ?seed:int -> ?verbose:bool -> ?quality_offset:int ->
          sequence:out_channel -> bedgraph:out_channel -> in_channel -> stats_t
      end
  end
)

