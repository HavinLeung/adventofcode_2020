#! /usr/bin/env utop -require core,re,ppx_jane
open! Core
open! Re

module Coordinate3D = struct
  module T = struct
    type t = (int * int * int) [@@deriving sexp, compare]
  end
  include T

  module Map = Map.Make(T)
  module Set = Set.Make(T)
end

let parse_strings (input : string list) =
  List.foldi input
    ~init:Coordinate3D.Set.empty
    ~f:(fun x set str ->
        String.foldi str ~init:set ~f:(fun y set c ->
            match c with
            | '.' -> set
            | '#' -> Set.add set (x,y,0)
            | _ -> failwith "invalid input"
          )
      );
;;


let input =
  In_channel.create "in17.txt"
  |> In_channel.input_lines
  |> parse_strings
;;

let example_input =
  {|.#.
..#
###|} |> String.split_lines
  |> parse_strings
;;

let cycle input_state neighbor_fun empty  =
  let only_did_actives = Set.fold input_state ~init:empty ~f:(fun new_state coord ->
      let neighbors = neighbor_fun coord in
      match List.count neighbors ~f:(Set.mem input_state) with
      | 2 | 3 -> Set.add new_state coord
      | _ -> new_state
    ) in
  let inactives_of_interest = Set.fold input_state ~init:empty ~f:(fun inactives coord ->
      let neighbors = neighbor_fun coord in
      List.fold neighbors ~init:inactives ~f:(fun inactives coord ->
          if Set.mem input_state coord then inactives else Set.add inactives coord
        )
    ) in
  let only_did_inactives = Set.fold inactives_of_interest ~init:empty ~f:(fun new_state coord ->
      let neighbors = neighbor_fun coord in
      match List.count neighbors ~f:(Set.mem input_state) with
      | 3 -> Set.add new_state coord
      | _ -> new_state
    ) in
  Set.union only_did_actives only_did_inactives
;;

let rec cycles input_state  turns neighbor_fun empty =
  if turns = 1 then cycle input_state neighbor_fun empty
  else cycles (cycle input_state neighbor_fun empty) (turns-1) neighbor_fun empty
;;

let neighbors1 (coord : Coordinate3D.t) : Coordinate3D.t list =
  let l = [-1;0;1] in
  List.fold l ~init:[] ~f:(fun neighbors x ->
      List.fold l ~init:neighbors ~f:(fun neighbors y ->
          List.fold l ~init:neighbors ~f:(fun neighbors z ->
              match x,y,z with
              | 0,0,0 -> neighbors
              | dx,dy,dz -> let x,y,z=coord in
                (x+dx, y+dy, z+dz) :: neighbors
            )
        )
    )
;;

print_s [%message (Set.length (cycles example_input 6 neighbors1 Coordinate3D.Set.empty) : int)];;
print_s [%message (Set.length (cycles input 6 neighbors1 Coordinate3D.Set.empty) : int)];;

module Coordinate4D = struct
  module T = struct
    type t = (int * int * int * int) [@@deriving sexp, compare]
  end
  include T

  module Map = Map.Make(T)
  module Set = Set.Make(T)
end


let parse_strings (input : string list) =
  List.foldi input
    ~init:Coordinate4D.Set.empty
    ~f:(fun x set str ->
        String.foldi str ~init:set ~f:(fun y set c ->
            match c with
            | '.' -> set
            | '#' -> Set.add set (x,y,0,0)
            | _ -> failwith "invalid input"
          )
      );
;;


let input =
  In_channel.create "in17.txt"
  |> In_channel.input_lines
  |> parse_strings
;;

let example_input =
  {|.#.
..#
###|} |> String.split_lines
  |> parse_strings
;;

let neighbors2 (coord : Coordinate4D.t) : Coordinate4D.t list =
  let l = [-1;0;1] in
  List.fold l ~init:[] ~f:(fun neighbors x ->
      List.fold l ~init:neighbors ~f:(fun neighbors y ->
          List.fold l ~init:neighbors ~f:(fun neighbors z ->
              List.fold l ~init:neighbors ~f:(fun neighbors w ->
                match x,y,z,w with
                | 0,0,0,0 -> neighbors
                | dx,dy,dz,dw -> let x,y,z,w=coord in
                  (x+dx, y+dy, z+dz, w+dw) :: neighbors
                )
            )
        )
    )
;;

print_s [%message (Set.length (cycles example_input 6 neighbors2 Coordinate4D.Set.empty) : int)];;
print_s [%message (Set.length (cycles input 6 neighbors2 Coordinate4D.Set.empty) : int)];;
