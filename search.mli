type t = {
  module_: string;
  package : string;
  name   : string;
  type_  : string;
}

val search : string -> Config.t list -> t list

