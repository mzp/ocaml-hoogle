type kind =
    Value of string (** type of the value *)
  | Type of string (** type def. "" means abstract. *)
  | Module
  | ModuleType
  | Class
  | ClassType
  | Other

type t = {
  id : string list; (** path of the object. ex. ["String"; "length"] for String.length. *)
  kind    : kind
}

val search : string -> string list -> string list -> t list
val raw_search : string -> string list -> string list -> (Longident.t * Searchid.pkind) list
