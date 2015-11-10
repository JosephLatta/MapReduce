open Async.Std

let main () : unit Deferred.t =
  Tcp.connect (Tcp.to_host_and_port "localhost" 8080) >>= fun (_, r, w) ->
  EchoMessage.send w (EchoMessage.Echo "hello");
  EchoMessage.receive r >>| function
  | `Eof -> print_endline "Error: couldn't receive message from server"
  | `Ok (EchoMessage.Echo msg) -> print_endline msg

let () =
  Command.(run (async ~summary:"" Spec.empty main))
