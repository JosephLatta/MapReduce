type msg = Echo of string
include Protocol.Make(struct type t = msg end)
