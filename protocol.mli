open Async.Std

(******************************************************************************)
(** {2 Sending and receiving messages}                                        *)
(******************************************************************************)

module type In = sig
  type t
end

(** Send or receive marshaled messages through Reader or Writer *)
module Make(T: In) : sig
  (** the type of a message *)
  type t = T.t

  (** [receive] and [send] receive and send messages. They will raise
      exceptions if the connection is broken or there was some kind of I/O
      failure (e.g. if the connection was unexpectedly terminated). *)
  val receive : Reader.t -> [`Ok of t | `Eof]  Deferred.t
  val send    : Writer.t -> t -> unit
end
