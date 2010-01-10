type desc =
    Value of string
  | Type
  | Label
  | Constructor
  | Module
  | ModuleType
  | Class
  | ClassType

type t = {
  module_ : string;
  package : string;
  name    : string;
  desc    : desc
}

val search : string -> Config.t list -> t list
