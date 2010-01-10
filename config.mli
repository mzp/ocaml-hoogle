type t = {
  name : string;
  modules : string list
}

val read : string -> t list
val find_package : string -> t list -> string
