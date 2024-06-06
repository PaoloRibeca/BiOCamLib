(*
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

(* General matrix class.
   We include in order not to have a repeated module prefix *)
include (
  struct
    type t = {
      idx_to_col_names: string array;
      idx_to_row_names: string array;
      storage: Float.Array.t array
    }
    let empty =
      { idx_to_col_names = [||]; idx_to_row_names = [||]; storage = [||] }
    let to_file ?(precision = 15) ?(threads = 1) ?(elements_per_step = 40000) ?(verbose = false) m fname =
      let n_cols = Array.length m.idx_to_col_names and n_rows = Array.length m.idx_to_row_names
      and output = open_out fname in
      if n_rows > 0 && n_cols > 0 then begin
        (* We output column names *)
        Printf.fprintf output "";
        Array.iter
          (fun name ->
            Printf.fprintf output "\t%s" name)
          m.idx_to_col_names;
        Printf.fprintf output "\n%!";
        let rows_per_step = max 1 (elements_per_step / n_cols) and processed_rows = ref 0
        and buf = Buffer.create 1048576 in
        Processes.Parallel.process_stream_chunkwise
          (fun () ->
            if !processed_rows < n_rows then
              let to_do = min rows_per_step (n_rows - !processed_rows) in
              let new_processed_rows = !processed_rows + to_do in
              let res = !processed_rows, new_processed_rows - 1 in
              processed_rows := new_processed_rows;
              res
            else
              raise End_of_file)
          (fun (lo_row, hi_row) ->
            Buffer.clear buf;
            (* We output rows *)
            for i = lo_row to hi_row do
              (* We output the row name *)
              m.idx_to_row_names.(i) |> Printf.bprintf buf "%s";
              Float.Array.iter (Printf.bprintf buf "\t%.*g" precision) m.storage.(i);
              Printf.bprintf buf "\n"
            done;
            hi_row - lo_row + 1, Buffer.contents buf)
          (fun (n_processed, block) ->
            Printf.fprintf output "%s" block;
            let new_processed_rows = !processed_rows + n_processed in
            if verbose && new_processed_rows / rows_per_step > !processed_rows / rows_per_step then
              Printf.eprintf "%s\r(%s): Writing table to file '%s': done %d/%d rows%!"
                String.TermIO.clear __FUNCTION__ fname new_processed_rows n_rows;
            processed_rows := new_processed_rows)
          threads
      end;
      if verbose then
        Printf.eprintf "%s\r(%s): Writing table to file '%s': done %d/%d rows.\n%!"
          String.TermIO.clear __FUNCTION__ fname n_rows n_rows;
      close_out output
    exception Quotes_in_name of string
    let re_quote = Str.regexp "\""
    let strip_external_quotes_and_check s =
      match String.length s, s with
      | 0, _ -> ""
      | 1, "\"" -> Quotes_in_name s |> raise
      | l, _ ->
        let s =
          if s.[0] = '"' && s.[l - 1] = '"' then
            String.sub s 1 (l - 2)
          else
            s in
        try
          Str.search_forward re_quote s 0 |> ignore;
          Quotes_in_name s |> raise
        with Not_found ->
          s
    exception Wrong_number_of_columns of int * int * int
    let of_file ?(threads = 1) ?(bytes_per_step = 4194304) ?(verbose = false) filename =
      let input = open_in filename and line_num = ref 0
      and idx_to_col_names = ref [||] and idx_to_row_names = ref [] and storage = ref [] in
      begin try
        (* We process the header *)
        let line = input_line input |> String.Split.on_char_as_array '\t' in
        incr line_num;
        let l = Array.length line in
        (* We assume the matrix always to have row names, and ignore the first name in the header if present *)
        let num_cols = l - 1 in
        idx_to_col_names := Array.make num_cols "";
        Array.iteri
          (fun i name ->
            if i > 0 then
              !idx_to_col_names.(i - 1) <- strip_external_quotes_and_check name)
          line;
        (* We process the rest of the lines in parallel. The first element will be the name *)
        let end_reached = ref false and elts_read = ref 0 in
        Processes.Parallel.process_stream_chunkwise
          (fun () ->
            if !end_reached then
              raise End_of_file;
            let res = ref [] in
            begin try
              let cntr = ref 0 in
              while !cntr < bytes_per_step do
                let line = input_line input in
                incr line_num;
                List.accum res (!line_num, line);
                cntr := !cntr + String.length line
              done
            with End_of_file ->
              end_reached := true;
              if !res = [] then
                raise End_of_file
            end;
            List.rev !res)
          (List.map
            (fun (line_num, line) ->
              (* We decorate the line number with the results of parsing the line *)
              let line = String.Split.on_char_as_array '\t' line in
              let l = Array.length line in
              if l <> num_cols + 1 then
                Wrong_number_of_columns (line_num, l, num_cols + 1) |> raise;
              let array = Float.Array.create num_cols in
              Array.iteri
                (fun i el ->
                  if i > 0 then
                    (* The first element is the name *)
                    float_of_string el |> Float.Array.set array (i - 1))
                line;
              line_num, strip_external_quotes_and_check line.(0), array))
          (List.iter
            (fun (obs_line_num, name, numbers) ->
              incr line_num;
              assert (obs_line_num = !line_num);
              (* Only here do we actually fill out the memory for the result *)
              List.accum idx_to_row_names name;
              List.accum storage numbers;
              let new_elts_read = !elts_read + num_cols in
              if verbose && new_elts_read / 100000 > !elts_read / 100000 then
                Printf.eprintf "%s\r(%s): On line %d of file '%s': Read %d elements%!"
                  String.TermIO.clear __FUNCTION__ !line_num filename new_elts_read;
              elts_read := new_elts_read))
          threads;
        close_in input;
        if verbose then
          Printf.eprintf "%s\r(%s): On line %d of file '%s': Read %d elements.\n%!"
            String.TermIO.clear __FUNCTION__ !line_num filename !elts_read
      with End_of_file ->
        (* Empty file *)
        close_in input
      end;
      { idx_to_col_names = !idx_to_col_names;
        idx_to_row_names = Array.of_rlist !idx_to_row_names;
        storage = Array.of_rlist !storage }
    let [@warning "-27"] transpose_single_threaded ?(verbose = false) m =
      { idx_to_col_names = m.idx_to_row_names;
        idx_to_row_names = m.idx_to_col_names;
        storage =
          Array.init (Array.length m.idx_to_col_names)
            (fun old_col ->
              Float.Array.init (Array.length m.idx_to_row_names)
                (fun old_row -> Float.Array.get m.storage.(old_row) old_col)) }
    let transpose ?(threads = 1) ?(elements_per_step = 10000) ?(verbose = false) m =
      let n_rows = Array.length m.idx_to_col_names and n_cols = Array.length m.idx_to_row_names in
      let storage = Array.init n_rows (fun _ -> Float.Array.create 0)
      and rows_per_step = max 1 (elements_per_step / n_cols) and processed_rows = ref 0 in
      (* Generate points to be computed by the parallel process *)
      Processes.Parallel.process_stream_chunkwise
        (fun () ->
          if !processed_rows < n_rows then (* The original columns *)
            let to_do = min rows_per_step (n_rows - !processed_rows) in
            let new_processed_rows = !processed_rows + to_do in
            let res = !processed_rows, new_processed_rows - 1 in
            processed_rows := new_processed_rows;
            res
          else
            raise End_of_file)
        (fun (lo_row, hi_row) ->
          let res = ref [] in
          (* We iterate backwards so as to avoid to have to reverse the list in the end *)
          for i = hi_row downto lo_row do
            (* The new row is the original column *)
            Float.Array.init n_cols (fun col -> Float.Array.get m.storage.(col) i) |> List.accum res
          done;
          lo_row, !res)
        (fun (lo_row, rows) ->
          List.iteri
            (fun offs_i row_i ->
              storage.(lo_row + offs_i) <- row_i;
              if verbose && !processed_rows mod rows_per_step = 0 then
                Printf.eprintf "%s\r(%s): Done %d/%d rows%!"
                  String.TermIO.clear __FUNCTION__ !processed_rows n_rows;
              incr processed_rows)
            rows)
        threads;
      if verbose then
        Printf.eprintf "%s\r(%s): Done %d/%d rows.\n%!" String.TermIO.clear __FUNCTION__ !processed_rows n_rows;
      { idx_to_col_names = m.idx_to_row_names;
        idx_to_row_names = m.idx_to_col_names;
        storage = storage }
    exception Incompatible_geometries of string array * string array
    exception Duplicate_label of string
    let merge_rowwise ?(verbose = false) m1 m2 =
      let merged_idx_to_col_names =
        if m1 = empty then
          m2.idx_to_col_names
        else m1.idx_to_col_names in
      if merged_idx_to_col_names <> m2.idx_to_col_names then
        Incompatible_geometries (m1.idx_to_col_names, m2.idx_to_col_names) |> raise;
      if verbose then
        Printf.eprintf "(%s): Merging matrices (%d+%d rows)...%!"
          __FUNCTION__ (Array.length m1.idx_to_row_names) (Array.length m2.idx_to_row_names);
      let merged_rows = ref StringMap.empty in
      Array.iteri
        (fun i name ->
          (* There ought to be no repeated names here *)
          merged_rows := StringMap.add name m1.storage.(i) !merged_rows)
        m1.idx_to_row_names;
      Array.iteri
        (fun i name ->
          match StringMap.find_opt name !merged_rows with
          | Some _ ->
            Duplicate_label name |> raise
          | None ->
            merged_rows := StringMap.add name m2.storage.(i) !merged_rows)
        m2.idx_to_row_names;
      let row_num = StringMap.cardinal !merged_rows in
      let merged_storage = Array.init row_num (fun _ -> Float.Array.create 0)
      and merged_idx_to_row_names = Array.make row_num "" in
      StringMap.iteri
        (fun i name arr ->
          merged_storage.(i) <- arr;
          merged_idx_to_row_names.(i) <- name)
        !merged_rows;
      if verbose then
        Printf.eprintf " done.\n%!";
      { idx_to_col_names = merged_idx_to_col_names;
        idx_to_row_names = merged_idx_to_row_names;
        storage = merged_storage }
    let multiply_matrix_vector_single_threaded ?(verbose = false) m v =
      if Array.length m.idx_to_col_names <> Float.Array.length v then
        Incompatible_geometries (m.idx_to_col_names, Array.make (Float.Array.length v) "") |> raise;
      let d = Array.length m.idx_to_row_names in
      (* We immediately allocate all the needed memory, as we already know how much we will need *)
      let res = Float.Array.create d and elts_done = ref 0 in
      (* We decorate each vector element coordinate with the respective value *)
      Array.iteri
        (fun i row ->
          let acc = ref 0. in
          Float.Array.iter2
            (fun el_1 el_2 ->
              acc := !acc +. (el_1 *. el_2))
            row v;
          Float.Array.set res i !acc;
          incr elts_done;
          if verbose && !elts_done mod 100 = 0 then
            Printf.eprintf "%s\r(%s): Done %d/%d elements%!" String.TermIO.clear __FUNCTION__ !elts_done d)
        m.storage;
      if verbose then
        Printf.eprintf "%s\r(%s): Done %d/%d elements.\n%!" String.TermIO.clear __FUNCTION__ !elts_done d;
      res
    type sparse_vector_t = {
      length: int;
      elements: float IntMap.t
    }
    let multiply_matrix_sparse_vector_single_threaded ?(verbose = false) m s_v =
      if Array.length m.idx_to_col_names <> s_v.length then
        Incompatible_geometries (m.idx_to_col_names, Array.make (s_v.length) "") |> raise;
      let d = Array.length m.idx_to_row_names in
      (* We immediately allocate all the needed memory, as we already know how much we will need *)
      let res = Float.Array.make d 0. and elts_done = ref 0 in
      (* We decorate each vector element coordinate with the respective value *)
      Array.iteri
        (fun i row ->
          let acc = ref 0. in
          IntMap.iter
            (fun j el ->
              acc := !acc +. (Float.Array.get row j *. el))
            s_v.elements;
          Float.Array.set res i !acc;
          incr elts_done;
          if verbose && !elts_done mod 100 = 0 then
            Printf.eprintf "%s\r(%s): Done %d/%d elements%!" String.TermIO.clear __FUNCTION__ !elts_done d)
        m.storage;
      if verbose then
        Printf.eprintf "%s\r(%s): Done %d/%d elements.\n%!" String.TermIO.clear __FUNCTION__ !elts_done d;
      res
    let multiply_matrix_vector ?(threads = 1) ?(elements_per_step = 10000) ?(verbose = false) m v =
      let n_rows = Array.length m.idx_to_row_names and n_cols = Array.length m.idx_to_col_names in
      if n_cols <> Float.Array.length v then
        Incompatible_geometries (m.idx_to_col_names, Array.make (Float.Array.length v) "") |> raise;
      (* We immediately allocate all the needed memory, as we already know how much we will need *)
      let res = Float.Array.create n_rows
      and rows_per_step = max 1 (elements_per_step / n_cols) and processed_rows = ref 0 in
      (* Generate points to be computed by the parallel process *)
      Processes.Parallel.process_stream_chunkwise
        (fun () ->
          if !processed_rows < n_rows then
            let to_do = min rows_per_step (n_rows - !processed_rows) in
            let new_processed_rows = !processed_rows + to_do in
            let res = !processed_rows, new_processed_rows - 1 in
            processed_rows := new_processed_rows;
            res
          else
            raise End_of_file)
        (fun (lo_row, hi_row) ->
          let res = ref [] in
          (* We iterate backwards so as to avoid to have to reverse the list in the end *)
          for i = hi_row downto lo_row do
            (* We decorate each vector element coordinate with the respective value *)
            let acc = ref 0. in
            Float.Array.iter2
              (fun el_1 el_2 ->
                acc := !acc +. (el_1 *. el_2))
              m.storage.(i) v;
            List.accum res !acc
          done;
          lo_row, !res)
        (fun (lo_row, v) ->
          List.iteri
            (fun offs_i el ->
              (* Only here do we actually fill out the memory for the result *)
              Float.Array.set res (lo_row + offs_i) el;
              if verbose && !processed_rows mod rows_per_step = 0 then
                Printf.eprintf "%s\r(%s): Done %d/%d rows%!"
                  String.TermIO.clear __FUNCTION__ !processed_rows n_rows;
              incr processed_rows)
            v)
        threads;
      if verbose then
        Printf.eprintf "%s\r(%s): Done %d/%d rows.\n%!" String.TermIO.clear __FUNCTION__ !processed_rows n_rows;
      res
    let multiply_matrix_matrix ?(threads = 1) ?(elements_per_step = 10000) ?(verbose = false) m1 m2 =
      if m1.idx_to_col_names <> m2.idx_to_row_names then
        Incompatible_geometries (m1.idx_to_col_names, m2.idx_to_row_names) |> raise;
      let row_num = Array.length m1.idx_to_row_names and col_num = Array.length m2.idx_to_col_names in
      (* We immediately allocate all the needed memory, as we already know how much we will need *)
      let storage = Array.init row_num (fun _ -> Float.Array.create col_num) in
      (* Generate points to be computed by the parallel process *)
      let prod = row_num * col_num in
      let i = ref 0 and j = ref 0 and elts_done = ref 0 and end_reached = ref (prod = 0) in
      Processes.Parallel.process_stream_chunkwise
        (fun () ->
          if !end_reached then
            raise End_of_file;
          let res = ref [] in
          begin try
            let cntr = ref 0 in
            while !cntr < elements_per_step do
              List.accum res (!i, !j);
              incr j;
              if !j = col_num then begin
                incr i;
                if !i = row_num then begin
                  end_reached := true;
                  raise Exit
                end;
                j := 0
              end;
              incr cntr
            done
          with Exit -> ()
          end;
          List.rev !res)
        (List.map
          (* We decorate each matrix element coordinate with the respective value *)
          (fun (i, j) ->
            let acc = ref 0. in
            Float.Array.iteri
              (fun k el ->
                acc := !acc +. (el *. Float.Array.get m2.storage.(k) j))
              m1.storage.(i);
            i, j, !acc))
        (List.iter
          (fun (i, j, el) ->
            (* Only here do we actually fill out the memory for the result *)
            Float.Array.set storage.(i) j el;
            if verbose && !elts_done mod elements_per_step = 0 then
              Printf.eprintf "%s\r(%s): Done %d/%d elements%!" String.TermIO.clear __FUNCTION__ !elts_done prod;
            incr elts_done))
        threads;
      if verbose then
        Printf.eprintf "%s\r(%s): Done %d/%d elements.\n%!" String.TermIO.clear __FUNCTION__ !elts_done prod;
      { idx_to_col_names = m2.idx_to_col_names;
        idx_to_row_names = m1.idx_to_row_names;
        storage = storage }
  end: sig
    type t = {
      (* We number rows and columns starting from 0 *)
      idx_to_col_names: string array;
      idx_to_row_names: string array;
      (* Stored row-wise *)
      storage: Float.Array.t array
    }
    val empty: t
    (* We read in a matrix which has conditions as row names
        and a (large) number of tags (genes, k-mers, etc.) as column names.
       In keeping with the convention accepted by R, the first row would be a header,
        and the first column the row names.
       Names might be quoted *)
    exception Quotes_in_name of string
    val strip_external_quotes_and_check: string -> string
    exception Wrong_number_of_columns of int * int * int
    val of_file: ?threads:int -> ?bytes_per_step:int -> ?verbose:bool -> string -> t
    val to_file: ?precision:int -> ?threads:int -> ?elements_per_step:int -> ?verbose:bool -> t -> string -> unit
    val transpose_single_threaded: ?verbose:bool -> t -> t
    val transpose: ?threads:int -> ?elements_per_step:int -> ?verbose:bool -> t -> t
    exception Incompatible_geometries of string array * string array
    exception Duplicate_label of string
    val merge_rowwise: ?verbose:bool -> t -> t -> t
    val multiply_matrix_vector:
      ?threads:int -> ?elements_per_step:int -> ?verbose:bool -> t -> Float.Array.t -> Float.Array.t
    val multiply_matrix_vector_single_threaded: ?verbose:bool -> t -> Float.Array.t -> Float.Array.t
    type sparse_vector_t = {
      length: int;
      elements: float IntMap.t
    }
    val multiply_matrix_sparse_vector_single_threaded: ?verbose:bool -> t -> sparse_vector_t -> Float.Array.t
    val multiply_matrix_matrix: ?threads:int -> ?elements_per_step:int -> ?verbose:bool -> t -> t -> t
  end
)

