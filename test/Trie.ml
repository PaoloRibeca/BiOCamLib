open BiOCamLib
open Tools

let string_of_presence = function
  | Trie.Not_found n -> Printf.sprintf "not found (longest prefix = %d)" n
  | Trie.Partial n -> Printf.sprintf "partial match (length %d)" n
  | Trie.Ambiguous l -> Printf.sprintf "ambiguous %s" (List.map string_of_int l |> String.concat ",")
  | Trie.Contained (i, l) ->
    Printf.sprintf "contained (exact #%d; extensions %s)" i (List.map string_of_int l |> String.concat ",")
  | Trie.Unique i -> Printf.sprintf "unique #%d" i

let () =
  let t = ref (Trie.create ()) in
  List.iter (fun s ->
    t := Trie.add !t s;
    Array.iter (Printf.printf "%s\n%!") (Trie.all !t);
    Printf.printf "===\n%!") [ "A"; "C"; "G"; "T"; "AA"; "AAA"; "-k"; "--kk"; "-kakka" ];
  Array.iter (Printf.printf "%s\n%!") (Trie.all !t);
  List.iter
    (fun q -> Printf.printf "find %S: %s\n%!" q (string_of_presence (Trie.find !t q)))
    [ "-k"; "-kakka"; "AA"; "GG" ];
  ()
