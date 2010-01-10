type t =
    String of string
  | Bool   of bool

val format : Config.t list -> Search.t -> (string * t) list
