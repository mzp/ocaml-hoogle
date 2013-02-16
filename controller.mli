type t =
    String of string
  | Bool   of bool
  | Table  of (string * t) list list

val format : Chconfig.t list -> Search.t -> (string * t) list
(** formatter for [Search.t] *)

val pagenation : offset:int -> window:int -> 'a list -> (string * t) list * 'a list
val available : Chconfig.t list -> t
