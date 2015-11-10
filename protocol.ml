open Async.Std

(******************************************************************************)
(** {2 Messaging}                                                             *)
(******************************************************************************)

module type In = sig
  type t
end

module Make(T: In) = struct
  type t = T.t
  let receive r = Reader.read_marshal r
  let send w v  = Writer.write_marshal w [] v
end
