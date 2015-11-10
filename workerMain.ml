open Async.Std

(* [serve app r w] services the app named [app] by using the reader and writer
 * connected to a controller. *)
let serve (app: string) (r: Reader.t) (w: Writer.t) : unit Deferred.t =
  failwith "see code below"
  (*let module App_mod = (val (MapReduce.string_to_module app)) in
  App_mod.receive r >>| function
    | `Eof -> failwith "a"
    | `Ok m -> (match m with
                | (mapinfo,_,_,Map) -> (App_mod.map mapinfo) >>=
                    fun x -> App_mod.send w x
                | (_,reduceinfo,_,Reduce) -> 
                    (App_mod.combine reduceinfo) >>=
                    fun y -> App_mod.send w y
    )*)
                
let () =
  let main (port: int) () : unit Deferred.t =
    let setup _ r w =
      Reader.recv r >>= function
      | `Eof -> (printf "Error: unable to read app name from controller."; return ())
      | `Ok app_name -> serve app_name r w
    in

    ignore (Tcp.Server.create (Tcp.on_port port) setup);
    never ()
  in

  Command.async
    ~summary:"Run a MapReduce worker"
    Command.Spec.(
      empty
      +> anon ("port" %: int)
    )
    main
  |> Command.run
