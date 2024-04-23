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
    (* PUBLIC.
       Processes a string.t array *)
    exception IncompatibleLengthOnLine of int
    let of_alignment ?(tip_gap_multiplier = 2.5) ?(max_tip_threshold = 30) al =
      let remove_tips s =
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
        Buffer.contents buf in
      let len =
        if Array.length al > 0 then
          String.length al.(0)
        else
          0 in
      let al =
        Array.mapi
          (fun i seq ->
            if String.length seq <> len then
              IncompatibleLengthOnLine i |> raise;
            Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:true seq |> remove_tips |> Bytes.of_string)
          al in




      Array.map Bytes.to_string al
  
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
    val of_alignment: ?tip_gap_multiplier:float -> ?max_tip_threshold:int -> string array -> string array
  



  end
)