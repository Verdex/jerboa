
open Base

exception TestFailure of string

val test : string -> (unit -> unit) -> unit

val expect_eq : 'a -> 'a -> string -> unit

val meta : int -> int -> meta


