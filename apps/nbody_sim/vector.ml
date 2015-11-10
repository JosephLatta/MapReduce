type scalar = float
type vector = float * float
type t      = vector

let ( + ) (ax,ay) (bx,by) = (ax +. bx, ay +. by)
let (~- ) (ax,ay)         = (-.ax, -.ay)
let ( - ) a b = a + (-b)
let ( * ) s (x,y) = (s *. x, s *. y)
let ( / ) (x,y) s = (x /. s, y /. s)

let dot (ax,ay) (bx,by) = ax *. bx +. ay *. by
let norm a = sqrt (dot a a)

