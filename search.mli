type kind =
    Value of string
  | Type of string
  | Module
  | ModuleType
  | Class
  | ClassType
  | Other

type t = {
  id : string list;
  kind    : kind
}

val search : string -> string list -> string list -> t list
