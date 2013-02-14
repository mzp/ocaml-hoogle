external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** Haskell's ($) *)
 
external ( +> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
(** F#'s (|>) *)

val ( $ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
(** Haskell's (.) *)

val ( !$ ) : 'a Lazy.t -> 'a
(** Lazy.force *)

external id : 'a -> 'a = "%identity"
(** Identity *)

val uncurry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val curry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val const : 'a -> 'b -> 'a
val sure : ('a -> 'b) -> 'a option -> 'b option
val option : ('a -> 'b) -> 'a -> 'b option
val maybe : ('a -> 'b) -> 'a -> [> `Error of exn | `Val of 'b ]
val tee : ('a -> 'b) -> 'a -> 'a
type ('a, 'b) either = Left of 'a | Right of 'b

val failwithf : ('a, unit, string, unit -> 'b) format4 -> 'a
(** failwith with formatting *)

val assoc : 'a -> ('a * 'b) list -> 'b option
(** List.assoc with option *)

val string_of_list : string list -> string
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list
val range : int -> int -> int list
val interperse : 'a -> 'a list -> 'a list
val map_accum_left : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val map_accum_right : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val group_by : ('a -> 'a -> bool) -> 'a list -> 'a list list
val index : 'a -> 'a list -> int
val string_of_char : char -> string
val hex : int -> string
val open_out_with : string -> (out_channel -> 'a) -> 'a
val open_in_with : string -> (in_channel -> 'a) -> 'a
val undefined : 'a
val undef : 'a
