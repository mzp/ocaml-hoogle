type t =
    String of string
  | Bool   of bool
  | Table  of (string * t) list list

val format : Config.t list -> Search.t -> (string * t) list
val pagenation : offset:int -> window:int -> 'a list -> (string * t) list * 'a list
val available : Config.t list -> t
