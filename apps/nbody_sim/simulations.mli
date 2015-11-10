open Async.Std

type body = {
  (** mass in kg *)
  mass : float;

  (** position in meters *)
  position : Vector.t;

  (** velocity in meters / sec *)
  velocity : Vector.t;
}

val g : Vector.scalar

val read_data        : string -> body list Deferred.t
val write_transcript : string -> body list list -> unit Deferred.t

