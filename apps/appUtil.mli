(* [split s] returns the list of strings separated by one or more spaces.
 *
 * Examples:
 *   split "" = []
 *   split "  " = []
 *   split "a" = ["a"]
 *   split "a b" = ["a"]
 *   split " a b   c " = ["a"; "b"; "c"] *)
val split : string -> string list
