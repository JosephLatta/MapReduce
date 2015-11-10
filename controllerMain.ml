open Async.Std
open Protocol
(* [connect_to_workers app] first reads the file "addresses.txt" which should
 * contain a list of addresses of the form host:port. It then attempts to
 * connect to each address and send the string [app]. [connect_to_workers]
 * returns the list of (reader, writer) pairs formed from connections that were
 * successfully formed and over which [app] was successfully sent. *)
type job_type = Map | Reduce 

let connect_to_workers (app: string) : (Reader.t * Writer.t) list Deferred.t =
  let split_host_and_port s =
    match Str.split (Str.regexp_string ":") s with
      | [host; port] -> Some (host, int_of_string port)
      | _            -> (printf "Error: invalid host:port string %s" s; None)
  in

  let to_host_and_port s =
    match split_host_and_port s with
    | None -> None
    | Some (host, port) -> Some (Tcp.to_host_and_port host port)
  in

  let try_connect s : (Reader.t * Writer.t) option Deferred.t =
    match to_host_and_port s with
    | None -> return None
    | Some hostport -> begin
      try_with (fun () -> Tcp.connect hostport) >>| function
      | Core.Std.Ok (_, r, w) -> Some (r, w)
      | Core.Std.Error _ -> (printf "Failed to connect to %s" s; None)
    end
  in

  Reader.file_lines "addresses.txt" >>= fun addresses ->
  Deferred.List.map addresses ~f:try_connect >>| fun connections ->
  let connections = Core.Std.List.filter_map connections ~f:(fun x -> x) in
  List.iter (fun (_, w) -> Writer.send w app) connections;
  connections



let run_remote (app: string) (args: string list) : unit Deferred.t =
  failwith "see code below"
(*
  let module App_mod = (val (MapReduce.string_to_module app)) in
  App_mod.send 
  
   connect_to_workers app >>= fun workers -> (
     App_mod.read (List.hd args) >>= fun lines -> 
   )
 *)
(*currently only supports case where 1 file is passed (aka args length is 1)
 * *)
let run_local (app: string) (args: string list) : unit Deferred.t =
  (*use first class modules to make variable named App_mod*)
  let module App_mod = (val (MapReduce.string_to_module app)) in

  App_mod.read (List.hd args) >>= fun out ->
  Deferred.List.map ~how:`Parallel ~f:(App_mod.map) out >>= fun x ->
  let y = MapReduce.combine x in
  Deferred.List.map ~how:`Parallel ~f:(App_mod.reduce) y >>= fun z ->
  App_mod.show z; return ()

let () =
  Command.async
    ~summary:"Run the MapReduce controller"
    Command.Spec.(
      empty
      +> flag "-local" no_arg ~doc:" run the app locally"
      +> anon ("app" %: string)
      +> anon (sequence ("arg" %: string))
    )
    (fun local app args () -> (if local then run_local else run_remote) app args)
  |> Command.run
