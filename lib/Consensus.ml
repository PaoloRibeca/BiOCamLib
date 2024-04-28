(*
    Consensus.ml -- (c) 2017-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Consensus.ml implements:
     * a module to compute a consensus from (multiple) alignments
     * an interface to parse mpileup files
     * a module to compute a consensus from mpileup files.

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

include (
  struct
    (* PRIVATE *)
    let dashes_re = Str.regexp "[-]+"
    let check_percentage arg_name n =
      if n < 0 || n >= 100 then
        Printf.sprintf "(%s): Invalid value %d to percentage argument '%s'" __FUNCTION__ n arg_name
          |> failwith
    (* PUBLIC *)
    let remove_tips ?(tip_gap_multiplier = 2.5) ?(max_tip_threshold = 30) s =
      if tip_gap_multiplier < 0. then
        Printf.sprintf "(%s): Negative value %g to argument 'tip_gap_multiplier'" __FUNCTION__ tip_gap_multiplier
          |> failwith;
      check_percentage "max_tip_threshold" max_tip_threshold;
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
        | Str.Delim _ :: [] -> rem
        | Str.Delim _ :: Str.Text _ :: [] -> rem
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
    (* Processes a string.t array *)
    let of_alignment
        ?(tip_gap_multiplier = 2.5) ?(max_tip_threshold = 30) ?(min_branch_threshold = 40)
        ?(consensus_window = 7) ?(min_coverage = 5) al =
      if tip_gap_multiplier < 0. then
        Printf.sprintf "(%s): Negative value %g to argument 'tip_gap_multiplier'" __FUNCTION__ tip_gap_multiplier
          |> failwith;
      check_percentage "max_tip_threshold" max_tip_threshold;
      check_percentage "min_branch_threshold" min_branch_threshold;
      if consensus_window < 1 then
        Printf.sprintf "(%s): Argument 'consensus_window' must be positive (found %d)" __FUNCTION__ consensus_window
          |> failwith;
      if min_coverage < 0 then
        Printf.sprintf "(%s): Negative value %d to argument 'min_coverage'" __FUNCTION__ min_coverage |> failwith;
      let n_seqs = Array.length al in
      if n_seqs = 0 then
        ""
      else begin
        let seq_len = String.length al.(0) in
        if consensus_window < 0 || consensus_window > seq_len then
          Printf.sprintf "(%s): Argument 'consensus_window' must be non-negative and no greater than alignment length"
            __FUNCTION__ |> failwith;
        (* We lint sequences and remove tips *)
        let al =
          Array.mapi
            (fun i seq ->
              if String.length seq <> seq_len then
                Printf.sprintf "(%s): Incompatible sequence length on line %d (expected %d, found %d)"
                  __FUNCTION__ i seq_len (String.length seq)
                |> failwith;
              Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:true seq
                |> remove_tips ~tip_gap_multiplier ~max_tip_threshold |> Bytes.of_string)
            al in
        (* We replace stretches of dashes on the sides with spaces and compute coverage *)
        let cov = Array.make seq_len 0 in
        Array.iter
          (fun seq ->
            let first_non_dash_idx = ref 0 in
            while !first_non_dash_idx < seq_len && seq.Bytes.@(!first_non_dash_idx) = '-' do
              seq.Bytes.@(!first_non_dash_idx) <- ' ';
              incr first_non_dash_idx
            done;
            let last_non_dash_idx = seq_len - 1 |> ref in
            while !last_non_dash_idx >= 0 && seq.Bytes.@(!last_non_dash_idx) = '-' do
              seq.Bytes.@(!last_non_dash_idx) <- ' ';
              decr last_non_dash_idx
            done;
            for i = !first_non_dash_idx to !last_non_dash_idx do
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
              let c = seq.Bytes.@(i) in
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
            res.Bytes.@(i) <- '-'
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
                let c = al.(i_seq).Bytes.@(consensus_window + i_col - 1) in
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
                res.Bytes.@(i_c) <-
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
    module MpileupLine:
      sig
        

      end
    = struct
      


      end

  end: sig
    (* Remove tips (short blocks separated from the core by a long gap) from both sides of an alignment.
       Parameters have the following meaning:
       * tip_gap_multiplier:
          eliminate terminal alignment segment if surrounded by gaps which are longer than
          tip_gap_multiplier * length(segment)
       * max_tip_threshold:
          eliminate terminal alignment segment if its number of non-gaps is no more than
          max_tip_threshold / 100 of the total number of non-gaps in the line *)
    val remove_tips: ?tip_gap_multiplier:float -> ?max_tip_threshold:int -> string -> string
    val of_alignment: ?tip_gap_multiplier:float -> ?max_tip_threshold:int -> ?min_branch_threshold:int ->
                      ?consensus_window:int -> ?min_coverage:int -> string array -> string
  



  end
)