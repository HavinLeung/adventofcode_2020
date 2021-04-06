open Core

type t =
  | Add of (t * t)
  | Mul of (t * t)
  | Num of int
[@@deriving sexp]
;;
