#! /usr/bin/env utop -require core,re,ppx_jane
open! Core
open! Re

let input = [16;12;1;0;15;7;11]
;;

let part1 (data : int list) (turns: int) : int=
  let tbl = Int.Table.create () in
  List.iteri data ~f:(fun i num -> Hashtbl.set tbl ~key:num ~data:(i+1));
  let rec recurse prev_num turn_num =
    if turns+1 = turn_num then prev_num
    else
      match Hashtbl.find tbl prev_num with
      | None -> Hashtbl.set tbl ~key:prev_num ~data:(turn_num - 1);
        recurse 0 (turn_num + 1)
      | Some prev_turn -> Hashtbl.set tbl ~key:prev_num ~data:(turn_num - 1);
        recurse (turn_num - prev_turn - 1) (turn_num + 1)
  in
  match List.nth data (turns-1) with
  | Some n -> n
  | None -> recurse (List.last_exn data) (List.length data + 1)
;;

print_s [%message (part1 [0;3;6] 2020 : int)];;
print_s [%message (part1 [1;3;2] 2020 : int)];;
print_s [%message (part1 [2;1;3] 2020 : int)];;
print_s [%message (part1 [1;2;3] 2020 : int)];;
print_s [%message (part1 [2;3;1] 2020 : int)];;
print_s [%message (part1 [3;2;1] 2020 : int)];;
print_s [%message (part1 [3;1;2] 2020 : int)];;
print_s [%message (part1 input 2020 : int)];;

print_s [%message (part1 input 30000000 : int)];;
