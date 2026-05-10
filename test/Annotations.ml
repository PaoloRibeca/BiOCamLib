open BiOCamLib
open Better

module A = Annotations

let () =
  let path = "test/annotation_demo.gff3" in
  let ann = A.GFF3.of_file path in
  Printf.printf "Hierarchy: %s\n"
    (A.Hierarchy.to_string (A.Annotation.hierarchy ann));
  Printf.printf "Metadata:\n";
  StringMap.iter (fun k vs ->
    List.iter (fun v -> Printf.printf "  %s = %s\n" k v) vs
  ) (A.Annotation.all_metadata ann);
  Printf.printf "Features:\n";
  A.Annotation.iter_paths (fun ~path feature ->
    let lo, hi =
      match feature.A.Annotation.intervals with
      | (i : Sequences.Types.simple_interval_t) :: _ ->
        i.low + 1, i.low + i.length
      | _ -> 0, 0 in
    let strand =
      match feature.A.Annotation.strand with
      | Some Sequences.Types.Forward _ -> "+"
      | Some Sequences.Types.Reverse _ -> "-"
      | None -> "." in
    let id =
      match feature.A.Annotation.id with Some s -> s | None -> "(no ID)" in
    Printf.printf "  %s [%s:%d..%d %s] %s\n"
      (A.Annotation.path_to_string path)
      (A.Annotation.seq_name ann feature) lo hi strand id
  ) ann;
  Printf.printf "Tables: paths=%d seqs=%d attr_keys=%d\n"
    (A.Path.Table.cardinal (A.Annotation.paths ann))
    (A.Seq.Table.cardinal (A.Annotation.seqs ann))
    (A.AttrKey.Table.cardinal (A.Annotation.attr_keys ann));
  print_endline "---";
  print_endline "Round-trip:";
  print_string (A.GFF3.to_string ann)
