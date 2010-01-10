type t = {
  name : string;
  modules : string list
}

val read : string -> t list

