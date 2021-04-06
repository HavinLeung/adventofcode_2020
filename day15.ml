open! Core
open! Re

let input = [16;12;1;0;15;7;11]
;;

let solve (data : int list) (turns: int) : int=
  let tbl = Array.create ~len:(Int.max (turns + 1) (List.max_elt data ~compare:Int.compare |> Option.value_exn)) (-1) in
  List.iteri data ~f:(fun i num -> Array.set tbl num (i+1));
  let rec recurse prev_num turn_num =
    if turns+1 = turn_num then prev_num
    else
      match Array.get tbl prev_num with
      | -1 -> Array.set tbl prev_num (turn_num - 1);
        recurse 0 (turn_num + 1)
      | prev_turn -> Array.set tbl prev_num (turn_num - 1);
        recurse (turn_num - prev_turn - 1) (turn_num + 1)
  in
  match List.nth data (turns-1) with
  | Some n -> n
  | None -> recurse (List.last_exn data) (List.length data + 1)
;;

let%expect_test _ = 
  print_s [%message (solve [0;3;6] 2020 : int)];
  print_s [%message (solve [1;3;2] 2020 : int)];
  print_s [%message (solve [2;1;3] 2020 : int)];
  print_s [%message (solve [1;2;3] 2020 : int)];
  print_s [%message (solve [2;3;1] 2020 : int)];
  print_s [%message (solve [3;2;1] 2020 : int)];
  print_s [%message (solve [3;1;2] 2020 : int)];
  print_s [%message (solve input 2020 : int)];
  print_s [%message (solve input 30000000 : int)];
  [%expect {|
    ("solve [0; 3; 6] 2020" 436)
    ("solve [1; 3; 2] 2020" 1)
    ("solve [2; 1; 3] 2020" 10)
    ("solve [1; 2; 3] 2020" 27)
    ("solve [2; 3; 1] 2020" 78)
    ("solve [3; 2; 1] 2020" 438)
    ("solve [3; 1; 2] 2020" 1836)
    ("solve input 2020" 403)
    ("solve input 30000000" 6823) |}]
;;
