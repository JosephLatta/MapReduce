open Async.Std

(*Not sure what else to add to this, or if this will work...*)
module type App = sig
     type input
     type k
     type map_out
     type output

     val name : string
     val read : string -> input list Deferred.t
     val map : input -> (k*map_out) list Deferred.t
     val reduce : (k*map_out list) -> (k*output) Deferred.t
     val show : (k*output) list -> unit
(*
     type message = (input * (k*map_out) list * (k*map_out list) *
     ControllerMain.job_type)
     val send : Reader.input -> [`Ok of input | `Eof] Deferred.t
     val receive : Writer.input -> input -> unit*)
end

(*hash table of (string of app, module of app)*)
let apps_table = Hashtbl.create 10

let register_app app =
  let (module Application : App) = app in
  Hashtbl.add apps_table Application.name app

let string_to_module name =
  Hashtbl.find apps_table name

let combine (l : (('a * 'b) list) list) : ('a * 'b list) list =
  let fl = List.flatten l in
  let rec part_kv lst = (
    match lst with
    | [] -> []
    | hd::_ ->
      let (l1, l2) = (List.partition
        (fun x -> let (k, _) = x in let (hd_key, _) = hd in compare k hd_key == 0) lst) in
      l1::(part_kv l2)) in
      let pair_lst_lst = part_kv fl in
      List.map (fun pair_lst ->
        let (ks, vs) = List.split pair_lst in
        match ks with
        | [] -> failwith "fail"
        | hd::tl -> (hd, vs)) pair_lst_lst
