open BiOCamLib
open Tools

let () =
  let t = ref Trie.empty in
  List.iter (fun s ->
    t := Trie.add !t s;
    List.iter (Printf.printf "%s\n%!") (Trie.find_all !t);
    Printf.printf "===\n%!") [ "A"; "C"; "G"; "T"; "AA"; "AAA"; "-k"; "--kk"; "-kakka" ];
  List.iter (Printf.printf "%s\n%!") (Trie.find_all !t);
  Printf.printf "%s\n%!" (Trie.find_string !t "-k");
  ()

