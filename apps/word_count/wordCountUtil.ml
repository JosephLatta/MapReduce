open Async.Std

type filename = string
type word     = string
type line     = string

module App = struct
  type input = string
  type k = string
  type map_out = int
  type output = int

  let name = "WordCount.app"

  let read filename =
    Reader.file_lines filename

  let split (s : line) : line list =
   Core.Std.String.split ~on:' ' s
   |> List.map Core.Std.String.strip
   |> List.filter ((<>) "")

  let map (s : line) : (word * int) list =
   let count lst =
     match lst with
       | [] -> failwith ""
       | hd::_ -> (hd, List.length lst) in
     let rec part_hd lst =
       match lst with
         | [] -> []
         | hd::_ ->
            let (l1, l2) =
              (List.partition (fun x -> compare x hd == 0) lst) in
            l1::part_hd l2 in
     let split_s = split s in List.rev_map count (part_hd split_s)

  let reduce (s : (word * int list)) : (word * int) =
    let (w, l) = s in (w, List.fold_left (fun s c -> s+c) 0 l)

  let show word_counts =
   let word_counts = List.sort (fun (w1, _) (w2, _) -> compare w1 w2) word_counts in
     List.iter (fun (w, c) -> printf "%s %d" w c) word_counts
end

let () = MapReduce.register_app(module App)
