(*
    Tests_Matrix.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Matrix.ml exercises the float matrix: its geometry under
    transposition and row-wise merging, and the products.  The matrices
    here are built as records rather than read from a file, so that
    each check states the exact geometry it is about; the parallel
    chunking of the readers and writers is a separate concern and is
    not what these check.

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

open BiOCamLib
open Better

(* Helpers. *)

let matrix rows cols data = {
  Matrix.row_names = Array.of_list rows;
  col_names = Array.of_list cols;
  data = Array.of_list (List.map Float.Array.of_list data)
}

(* Rows as "name:v,v,v", joined by "; " -- enough to see geometry and content
   at once when a check fails. *)
let show (m: Matrix.t) =
  Array.to_list m.Matrix.row_names
  |> List.mapi (fun i name ->
       Printf.sprintf "%s:%s" name
         (Float.Array.to_list m.Matrix.data.(i)
          |> List.map (Printf.sprintf "%g") |> String.concat ","))
  |> String.concat "; "

let show_cols (m: Matrix.t) = Array.to_list m.Matrix.col_names |> String.concat ","

(* A 2 x 3: two rows r1, r2 over three columns c1, c2, c3. *)
let m23 = matrix [ "r1"; "r2" ] [ "c1"; "c2"; "c3" ] [ [ 1.; 2.; 3. ]; [ 4.; 5.; 6. ] ]

let test_geometry () =
  Testing.section "Matrix geometry" (fun () ->
    Testing.check_int "the empty matrix has no rows"
      ~expected:0 (Array.length Matrix.empty.Matrix.row_names);
    Testing.check_int "and no columns"
      ~expected:0 (Array.length Matrix.empty.Matrix.col_names);
    Testing.check_string "a matrix reads back as it was built"
      ~expected:"r1:1,2,3; r2:4,5,6" (show m23);
    (* Transposition swaps the two name arrays along with the data. *)
    Testing.check_string "transposing swaps rows for columns"
      ~expected:"c1:1,4; c2:2,5; c3:3,6" (show (Matrix.transpose m23));
    Testing.check_string "the column names become the row names"
      ~expected:"r1,r2" (show_cols (Matrix.transpose m23));
    Testing.check_string "transposing twice is the identity"
      ~expected:(show m23) (show (Matrix.transpose (Matrix.transpose m23)));
    Testing.check_string "and restores the column names too"
      ~expected:(show_cols m23) (show_cols (Matrix.transpose (Matrix.transpose m23)));
    (* A single row and a single column are the degenerate shapes. *)
    Testing.check_string "a single-row matrix transposes to a single column"
      ~expected:"c1:1; c2:2"
      (show (Matrix.transpose (matrix [ "r1" ] [ "c1"; "c2" ] [ [ 1.; 2. ] ])));
    Testing.check_string "merging row-wise concatenates the rows"
      ~expected:"r1:1,2,3; r2:4,5,6; r3:7,8,9"
      (show (Matrix.merge_rowwise m23
               (matrix [ "r3" ] [ "c1"; "c2"; "c3" ] [ [ 7.; 8.; 9. ] ])));
    Testing.check_string "and leaves the columns alone"
      ~expected:"c1,c2,c3"
      (show_cols (Matrix.merge_rowwise m23
                    (matrix [ "r3" ] [ "c1"; "c2"; "c3" ] [ [ 7.; 8.; 9. ] ])));
    (* Merging matrices whose columns disagree is not meaningful. *)
    Testing.check_raises "merging incompatible geometries is refused"
      (fun () ->
        ignore (Matrix.merge_rowwise m23
                  (matrix [ "r3" ] [ "c1"; "c2" ] [ [ 7.; 8. ] ]))))

let test_products () =
  Testing.section "Matrix products" (fun () ->
    (* [1 2 3; 4 5 6] . [1 1 1] = [6; 15] *)
    Testing.check_string "a matrix times a vector"
      ~expected:"6,15"
      (Matrix.multiply_matrix_vector_single_threaded m23 (Float.Array.of_list [ 1.; 1.; 1. ])
       |> Float.Array.to_list |> List.map (Printf.sprintf "%g") |> String.concat ",");
    (* [1 2 3; 4 5 6] . [1 0 0] = [1; 4], i.e. the first column. *)
    Testing.check_string "a unit vector selects a column"
      ~expected:"1,4"
      (Matrix.multiply_matrix_vector_single_threaded m23 (Float.Array.of_list [ 1.; 0.; 0. ])
       |> Float.Array.to_list |> List.map (Printf.sprintf "%g") |> String.concat ",");
    Testing.check_string "a zero vector gives zero"
      ~expected:"0,0"
      (Matrix.multiply_matrix_vector_single_threaded m23 (Float.Array.of_list [ 0.; 0.; 0. ])
       |> Float.Array.to_list |> List.map (Printf.sprintf "%g") |> String.concat ",");
    (* The sparse form has to agree with the dense one on the same vector. *)
    Testing.check_string "the sparse product agrees with the dense one"
      ~expected:"1,4"
      (Matrix.multiply_matrix_sparse_vector_single_threaded m23
         { Matrix.length = 3; elements = IntMap.singleton 0 1. }
       |> Float.Array.to_list |> List.map (Printf.sprintf "%g") |> String.concat ",");
    Testing.check_string "and on a vector with two non-zero entries"
      ~expected:"3,9"
      (Matrix.multiply_matrix_sparse_vector_single_threaded m23
         { Matrix.length = 3; elements = IntMap.add 1 1. (IntMap.singleton 0 1.) }
       |> Float.Array.to_list |> List.map (Printf.sprintf "%g") |> String.concat ",");
    (* A vector of the wrong length cannot be multiplied. *)
    Testing.check_raises "a vector of the wrong length is refused"
      (fun () ->
        ignore (Matrix.multiply_matrix_vector_single_threaded m23
                  (Float.Array.of_list [ 1.; 1. ]))))

let test_quotes () =
  Testing.section "Matrix name quoting" (fun () ->
    let strip = Matrix.IO.strip_external_quotes_and_check in
    Testing.check_string "an unquoted name is left alone" ~expected:"abc" (strip "abc");
    Testing.check_string "a quoted name is stripped" ~expected:"abc" (strip "\"abc\"");
    Testing.check_string "the empty name survives" ~expected:"" (strip "");
    Testing.check_string "an empty quoted name becomes empty" ~expected:"" (strip "\"\""))

let run () =
  test_geometry ();
  test_products ();
  test_quotes ()
