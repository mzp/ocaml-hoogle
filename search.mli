type kind =
    Value of string
  | Type of string
  | Module
  | ModuleType
  | Class
  | ClassType
  | Other

type t = {
  module_ : string list;
  name    : string;
  kind    : kind
}

val search : string -> string list -> string list -> t list
