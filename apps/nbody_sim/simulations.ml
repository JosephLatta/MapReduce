open Async.Std

(* see .mli *)
type body = {
  mass : float;
  position : Vector.t;
  velocity : Vector.t;
}

let g = 6.7e-11

(****************************************************************************)
(** inputting simulation files                                              *)
(****************************************************************************)

(** input file format: each body is a line of the form
    "pos: (x,y); vel: (x,y); mass: m"
    and x, y, and m are floating point numbers.
    
    Lines starting with # are ignored. *)

let parse_line l =
  Scanf.sscanf l
    " pos: ( %f , %f ); vel: ( %f , %f ); mass: %f "
    (fun px py vx vy m -> {position = px,py;   velocity = vx, vy;  mass = m})


let read_data filename =
  let is_data line = not (line = "" || line.[0] = '#') in
  Reader.file_lines filename
    >>| List.filter is_data
    >>| List.map parse_line

(****************************************************************************)
(** outputting files for bouncy.jar                                         *)
(****************************************************************************)

(** output file format: a single line containing the number (n) of bodies,
    then a sequence of configs.  Each config is n lines, each line containing
    the position of a body in the format "x y 0" *)

let write_body w b =
  let x, y = b.position in
  Writer.writef w "%f %f 0\n" x y

let write_config w bs =
  List.iter (write_body w) bs

let write_transcript filename configs = match configs with
    | []   -> return ()
    | h::_ -> Writer.open_file filename >>= fun w ->
              Writer.writef w "%d\n" (List.length h);
              List.iter (write_config w) configs;
              Writer.close w

