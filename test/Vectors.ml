open BiOCamLib
open Numbers

let () =
  let init = Stdlib.Float.Array.of_list [ 1.; 0.; 4.; 0.5; 0.; 3.5; 4. ] in
  let module FV = Frequencies.Vector(Float)(MakeComparableNumber) in
  let print_fvf fv = Printf.printf "["; FV.iter (Printf.printf " %g*%d") fv; Printf.printf " ]\n%!" in
  let print_fa fa = Printf.printf "["; Stdlib.Float.Array.iter (Printf.printf " %g") fa; Printf.printf " ]\n%!" in
  let a = FV.of_floatarray init in
  assert (FV.is_non_negative a);
  print_fvf a;
  FV.add a 0.5;
  print_fvf a;
  FV.median a |> Printf.printf "median=%g\n%!";
  FV.sum_abs a |> Printf.printf "sum_abs=%g\n%!";
  FV.threshold_accum_abs 0.5 a |> print_fvf;
  FV.to_floatarray a |> print_fa;
  let module FV = Frequencies.Vector(Float)(MakeRComparableNumber) in
  let print_fvf fv = Printf.printf "["; FV.iter (Printf.printf " %g*%d") fv; Printf.printf " ]\n%!" in
  let a = FV.of_floatarray init in
  assert (FV.is_non_negative a);
  print_fvf a;
  FV.add a 0.5;
  FV.add a 1.;
  print_fvf a;
  FV.median a |> Printf.printf "median=%g\n%!";
  FV.add a 0.5;
  print_fvf a;
  FV.median a |> Printf.printf "median=%g\n%!";
  FV.add a 0.5;
  print_fvf a;
  FV.median a |> Printf.printf "median=%g\n%!";
  FV.sum_abs a |> Printf.printf "sum_abs=%g\n%!";
  FV.threshold_accum_abs 0.5 a |> print_fvf;
  FV.to_floatarray a |> print_fa;
  let module IV = Frequencies.Vector(Int)(MakeComparableNumber) in
  let print_iv iv = Printf.printf "["; IV.iter (Printf.printf " %d*%d") iv; Printf.printf " ]\n%!" in
  let a = IV.of_floatarray init in
  assert (IV.is_non_negative a);
  print_iv a;
  IV.pow_abs 2 a |> print_iv
  
