type desc =
    Value of string
  | Type of string
  | Module
  | ModuleType
  | Class
  | ClassType
  | Other

type t = {
  module_ : string;
  package : string;
  name    : string;
  desc    : desc
}

val search : string -> Config.t list -> t list
