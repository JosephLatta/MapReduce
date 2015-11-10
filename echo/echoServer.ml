open Async.Std

let printer _ r w =
  EchoMessage.receive r >>| function
  | `Eof -> EchoMessage.send w (EchoMessage.Echo "`Eof")
  | `Ok m -> EchoMessage.send w m

let main () : unit Deferred.t =
  let port = 8080 in
  printf "listening on port %d\n" port;
  let where_to_listen = Tcp.on_port port in
  let _ = Tcp.Server.create where_to_listen printer in
  never ()

let () =
  Command.(run (async ~summary:"" Spec.empty main))
