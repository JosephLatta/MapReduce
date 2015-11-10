open Async.Std

type filename = string
type node     = string
type friends  = node list

(* [read filename] reads the contents of the graph file [filename] into a list
 * of nodes and their neighbors. For example, the following file:
 *
 *   A B C
 *   B C D
 *   E
 *   F G
 *
 * would be parsed into the following list:
 *
 *  [("A", ["B", "C"]);
 *   ("B", ["C", "D"]);
 *   ("E", []);
 *   ("F", ["G"])]
 *)
val read: filename -> (node * friends) list Deferred.t

(* [show common_friends] pretty prints [common_friends] to standard out. Node
 * pairs are printed in increasing lexicographic order. Friends are also
 * printed in increasing lexicographic order. All nodes are separated by a
 * single space. *)
val show: ((node * node) * friends) list -> unit
