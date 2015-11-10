(** Operations for 2D floating point vectors.

    Note: OCaml has a convenient syntax for working with modules like this:
    In general [M.(e)] evaluates [e] as if [M] has been opened.  This is useful
    because you can write expressions like {[
      Vector.(3. *. 5. * (x + y) / 8.)
    ]}
    
    where [x] is a vector.  [*] and [+] in this expression refer to
    [Vector.( * )] and [Vector.(+)] respectively. *)

type scalar = float
type vector = float * float
type t      = vector

(** vector addition and subtraction *)
val ( + ) : vector -> vector -> vector
val ( - ) : vector -> vector -> vector
val (~- ) : vector -> vector

(** scalar multiplication  *)
val ( * ) : scalar -> vector -> vector
(** [v / s] is shorthand for [(1 /. s) * v] *)
val ( / ) : vector -> scalar -> vector

val dot  : vector -> vector -> scalar
val norm : vector -> scalar

