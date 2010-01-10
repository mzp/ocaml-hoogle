open Base

type t =
    String of string
  | Bool   of bool
  | Table  of (string * t) list

let format _ = assert false
