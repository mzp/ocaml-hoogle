type t = {
  name : string;
  path:string option;
  modules : string list
}

val read : string -> t list

