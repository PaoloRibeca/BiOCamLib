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
        let split_at_dashes = String.Split.full_as_array dashes_re s in
        let max_non_dash_len =
          begin
            Array.fold_left
              (fun sum -> function
                | Str.Text s -> sum + String.length s
                | Str.Delim _ -> sum) 0
              split_at_dashes
          end * max_tip_threshold / 100 in
  
        s in
      let len =
        if Array.length al > 0 then
          String.length al.(0)
        else
          0 in
      let al =
        Array.mapi
          (fun i seq ->
            if Array.length seq <> len then
              IncompatibleLengthOnLine i |> raise;
            Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:true seq |> remove_tips |> Bytes.of_string) al in
      
      


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