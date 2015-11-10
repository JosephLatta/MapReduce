open Async.Std

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
     type message
     val send : Reader.t -> [`Ok of t | `Eof] Deferred.t
     val receive : Writer.t -> t -> unit*)
end

val register_app : (module App) -> unit

val string_to_module : string -> (module App)

val combine : (('a * 'b) list) list -> ('a * 'b list) list
