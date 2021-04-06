open! Core
open! Re

type t = (((string * int * int * int * int) list) * (int list) * (int list list)) [@@deriving sexp]

let parse_strings input =
  let yt_idx,_ = List.findi input ~f:(fun _ s -> String.(=) s "your ticket:") |> Option.value_exn in
  let nb_idx,_ = List.findi input ~f:(fun _ s -> String.(=) s "nearby tickets:") |> Option.value_exn in
  let rules = List.slice input 0 (yt_idx - 1)
              |> List.map ~f:(fun s ->
                  let rex = Re.Pcre.regexp {|(.*?): (\d+)-(\d+) or (\d+)-(\d+)|} in
                  let arr = Re.Pcre.exec ~rex s |> Group.all in
                  let get_int idx =
                    arr.(idx) |> Int.of_string
                  in
                  (arr.(1),
                   get_int 2,
                   get_int 3,
                   get_int 4,
                   get_int 5)
                )
  in
  let ticket_to_list ticket =
    String.split ticket ~on:','
    |> List.map ~f:Int.of_string
  in
  let your_ticket = List.nth_exn input (yt_idx + 1) |> ticket_to_list
  in
  let nearby_tickets = List.slice input (nb_idx + 1) 0
                       |> List.map ~f:ticket_to_list
  in
  rules, your_ticket, nearby_tickets
;;


let input =
  In_channel.create "in16.txt"
  |> In_channel.input_lines
  |> parse_strings
;;

let example_input =
  {|class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12|} |> String.split_lines |> parse_strings
;;

let matches_rule (_,l1,u1,l2,u2) num =
  (l1 <= num && num <= u1) || (l2 <= num && num <= u2)
;;

let matches_any_rule rules num =
  List.find rules ~f:(fun rule -> matches_rule rule num) |> Option.is_some
;;

let part1 (input:t) =
  let rules, _, nearby_tickets = input in
  List.fold
    nearby_tickets
    ~init:0
    ~f:(fun accum (ticket : int list) ->
        List.fold ticket ~init:0 ~f:(fun accum num ->
            if matches_any_rule rules num then accum else accum + num
          ) + accum
      )
;;

let rules_that_work rules num =
  List.filter rules ~f:(fun rule -> matches_rule rule num)
;;

let part2 ((rules, your_ticket, nearby_tickets):t) =
  let nearby_tickets = List.filter nearby_tickets ~f:(fun ticket ->
      List.for_all ticket ~f:(matches_any_rule rules)) in
  let fields = List.range 0 (List.length your_ticket) in
  let fields = List.map fields ~f:(fun i ->
      List.fold nearby_tickets ~init:rules ~f:(fun possible_rules ticket ->
          let ticket_field = List.nth_exn ticket i in
          rules_that_work possible_rules ticket_field
        )
    ) |> List.mapi ~f:(fun i rules -> i,List.map rules ~f:(fun (s,_,_,_,_)->s))
  in
  (* sort by the number of rules that can possibly work, then deduce *)
  let fields = List.sort fields ~compare:(fun (_,rules1) (_,rules2) ->
                                                         Int.compare (List.length rules1) (List.length rules2)
    ) in
  let _, fields = List.fold_map fields ~init:String.Set.empty ~f:(
      fun accum (i,rulenames) ->
        match List.filter rulenames ~f:(Fn.non (Set.mem accum)) with
        | [hd] -> (Set.add accum hd), (i,hd)
        | _ -> failwith "invariant violated"
    ) in
  List.fold fields ~init:1 ~f:(fun accum (i,rule_name) ->
      match String.prefix rule_name 4 with
      | "depa" -> accum * (List.nth_exn your_ticket i)
      | _ -> accum)
;;

let%expect_test _ = 
  print_s [%message (part1 example_input : int)];
  print_s [%message (part1 input : int)];
  print_s [%message (part2 input : int)];
  [%expect {|
    ("part1 example_input" 71)
    ("part1 input" 25059)
    ("part2 input" 3253972369789) |}]
;;
