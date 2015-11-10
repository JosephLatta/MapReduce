open Async.Std

module App        = struct
type input        = string * string list
type k            = (string * string)
type map_out      = string list
type output       = string list

let name = "CommonFriends.app"

  (* creates a list of pairs (key, friend list) tuples. A key is a pair
   * consisting of [name] and an element of [friendlist]. The order of
   * the pair values is alphabetized, e.g. ("B","A") becomes ("A","B").
   * Each key's friend list is the remaining [friendlist] which does not
   * contain any element found in the key. There is only one (key, friend list)
   * tuple for each element in [friendlist].
   * requires: an input type argument
   * returns: a (key * inter) list Deferred.t *)

  (*remove all such element's apperance in the list*)
  let rec remove a lst = match lst with
                         | [] -> []
                         | hd::tl -> if hd==a then (remove a tl) else hd::(remove a tl)

  (*order the list to its all possible and legal arrangement: tuple and following list
    all in alphabetical order*)
  let order name friendlist = List.fold_left (fun acc friend ->
                              if friend<name then ((friend,name),remove friend friendlist)::acc
                              else ((name,friend),remove friend friendlist)::acc) [] friendlist

  let map (name, friendlist) : (k * map_out) list Deferred.t =
    return (order name friendlist)

  let rec get_common lst1 lst2 = match lst1 with
                                 | [] -> []
                                 | hd::tl -> if List.mem hd lst2
                                             then hd::(get_common tl lst2)
                                             else get_common tl lst2

  (* creates a list of mutual friends of the elements given in the key.
   * [friendlists] should only contain two lists.
   * requires: a (key*inter list) type argument
   * returns: an output Deferred.t *)
  let reduce (_, friendlists : k * map_out list) : output Deferred.t =
    match friendlists with
    | lst1::lst2::[] -> return
        (get_common lst1 lst2)
    | _ -> raise (Failure "Reduce failure")

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
 *   ("F", ["G"])] *)
let read filename =
  Reader.file_lines filename >>| fun lines ->
  List.map AppUtil.split lines
  |> List.map (fun line ->
      match line with
      | node::friends -> (node, friends)
      | _ -> failwith ("Error: cannot read invalid graph file " ^ filename))


(* [show common_friends] pretty prints [common_friends] to standard out. Node
 * pairs are printed in increasing lexicographic order. Friends are also
 * printed in increasing lexicographic order. All nodes are separated by a
 * single space. *)
let show common_friends =
  let string_of_friends friends =
    Core.Std.String.concat ~sep:" " (List.sort compare friends)
  in

  List.sort (fun (uv, _) (uv', _) -> compare uv uv') common_friends
  |> List.iter (fun ((u, v), friends) -> printf "%s %s %s" u v (string_of_friends friends))

  let () = MapReduce.register_app(module App)



