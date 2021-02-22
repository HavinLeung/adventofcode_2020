#! /usr/bin/env utop -require core,re,ppx_jane
(* TIL that shebangs work in .ml files *)
open Core
open Re

let input =
  In_channel.create "in14.txt"
              |> In_channel.input_lines
              |> List.map ~f:(fun s ->
                  match String.prefix s 4 with
                  | "mask" ->
                    let rex = Re.Pcre.regexp {|mask = (.*)|} in
                    let mask = Re.Pcre.exec ~rex s
                               |> Fn.flip Group.get 1 in
                    `Mask mask
                  | "mem[" ->
                    let rex = Re.Pcre.regexp {|mem\[(\d+)\] = (\d+)|} in
                    let groups = Re.Pcre.exec ~rex s |> Group.all in
                    `Memset (groups.(1), groups.(2))
                  | _ -> failwith "unexpected input"
                )
;;

let ex1 = [
    `Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
  ; `Memset ("8", "11")
  ; `Memset ("7", "101")
  ; `Memset ("8", "0")
  ]

let ex2 = [
  `Mask "000000000000000000000000000000X1001X"
; `Memset ("42", "100")
; `Mask "00000000000000000000000000000000X0XX"
; `Memset ("26", "1")
]

let part1 (data : [`Mask of string | `Memset of (string * string)] list) =
  let do_mask (mask : string) (v : int) =
    let and_mask = String.map mask ~f:(fun x -> if Char.(x = 'X') then '1' else x)
                   |> String.(^) "0b"
                   |> Int.of_string in
    let or_mask = String.map mask ~f:(fun x -> if Char.(x = 'X') then '0' else x)
                  |> String.(^) "0b"
                  |> Int.of_string in
    let v = Int.(land) v and_mask |> Int.(lor) or_mask in
    v
  in
  let mem = Int.Table.create () in
  let _ = List.fold data ~init:None ~f:(fun mask instr ->
      match mask, instr with
      | _, `Mask mask -> Some mask
      | None, `Memset _ -> failwith "mask invariant violated"
      | Some mask, `Memset (addr, v) ->
        let addr = Int.of_string addr in
        let v = Int.of_string v |> do_mask mask in
        Int.Table.set mem ~key:addr ~data:v;
        Some mask
    ) in
  Hashtbl.fold mem ~init:0 ~f:(fun ~key:_ ~data accum -> accum + data)
;;

let part2 (data : [`Mask of string | `Memset of (string * string)] list) =
  let int_to_bin i =
    if i = 0 then "0"
    else (
      let rec bin acc i =
        if i = 0 then acc
        else if (i % 2 = 0) then bin ('0' :: acc) (i / 2)
        else bin ('1' :: acc) (i / 2)
      in
      bin [] i |> String.of_char_list
    )
  in
  let get_addrs mask addr =
    let rec fill_floating_bits bits =
      match List.findi bits ~f:(fun _ c -> Char.(c = 'X')) with
      | None -> ["0b" ^ (String.of_char_list bits) |> Int.of_string]
      | Some (i, _) ->
        let list_with i c =
          List.mapi bits ~f:(fun idx oc -> if i=idx then c else oc)
        in
        (list_with i '0' |> fill_floating_bits) @ (list_with i '1' |> fill_floating_bits)
    in
    let rec pad_list_front_until ~size ~pad_with l =
      if List.length l >= size then l
      else pad_list_front_until ~size ~pad_with (pad_with :: l)
    in
    let addr = int_to_bin addr in
    let addr = List.zip_exn (String.to_list mask) (String.to_list addr |> pad_list_front_until ~size:36 ~pad_with:'0')
               |> List.map ~f:(fun (mask, c) -> if Char.(mask = '0') then c else mask)
    in
    fill_floating_bits addr
  in
  let mem = Int.Table.create () in
  let _ = List.fold data ~init:None ~f:(fun mask instr ->
      match mask, instr with
      | _, `Mask mask -> Some mask
      | None, `Memset _ -> failwith "mask invariant violated"
      | Some mask, `Memset (addr, v) ->
        let addrs = Int.of_string addr |> get_addrs mask in
        let v = Int.of_string v in
        List.iter addrs ~f:(fun key -> Int.Table.set mem ~key ~data:v);
        Some mask
    ) in
  Hashtbl.fold mem ~init:0 ~f:(fun ~key:_ ~data accum -> accum + data)
;;

print_s [%message (part1 ex1 : int)];

print_s [%message (part1 input : int)];

print_s [%message (part2 ex2 : int)];

print_s [%message (part2 input : int)];
