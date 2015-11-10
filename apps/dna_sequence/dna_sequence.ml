open Async.Std
open Async_unix

(******************************************************************************)
(** input and output types                                                    *)
(******************************************************************************)

type id = int
type dna_type = Read | Ref

type sequence = {
  id   : id;
  kind : dna_type;
  data : string;
}

(** Indicates a matching subsequence of the given read and reference *)
type result = {
  length   : int;

  read     : id;
  read_off : int;

  ref      : id;
  ref_off  : int;
}

type infos = id * dna_type * int

let read_sequence line = match Str.split (Str.regexp "@") line with
  | [id; "READ"; seq] -> {id=int_of_string id; kind=Read; data=seq}
  | [id; "REF";  seq] -> {id=int_of_string id; kind=Ref;  data=seq}
  | _ -> failwith "malformed input"

let read_files filenames : sequence list Deferred.t =
  if filenames = [] then failwith "No files supplied"
  else
    Deferred.List.map filenames Reader.file_lines
      >>| List.flatten
      >>| List.map read_sequence

let print_result result =
  printf "read %i [%i-%i] matches reference %i [%i-%i]\n"
         result.read result.read_off (result.read_off + result.length - 1)
         result.ref  result.ref_off  (result.ref_off  + result.length - 1)

let print_results results : unit =
  List.iter print_result results

module DNA1    = struct
  type input   = sequence
  type k       = string
  type map_out = infos
  type output  = (infos * infos) list

  let name = "DNA1"

  (*first off will be 0, i d k are three fields of input*)
  let rec take_10 off i d k : (key * map_out) list =
         if String.length d < 10 then []
         else  ((String.sub d off 10),(i,k,off))::(take_10 (off+1) i (String.sub d (off+1) (String.length d-1)) k)

  let map input : (key * map_out) list Deferred.t =
    match input with
      | {id = i; kind = k; data = d} -> if String.length d < 10 then failwith "Too short"
                                        else return (take_10 0 i d k)

  let filt_ref inters = List.filter(fun a -> let (_,k,_) = a in
                                      if k == Ref then true else false) inters

  let filt_read inters = List.filter(fun a -> let (_,k,_) = a in
                                     if k == Read then true else false) inters

  let cartesian l1 l2 = List.concat (List.map (fun a1 -> List.map (fun a2 -> (a1,a2)) l1) l2)

  let reduce (key, map_outs) : output Deferred.t =
    return (cartesian (filt_ref inters) (filt_read inters))
end


let () = MapReduce.register_job (module DNA1)



module DNA2    = struct
  type input   = (infos * infos) list
  type k       = (int * int)
  type map_out = (int * int)
  type output  = result list

  let name = "DNA2"

  let sub_map input = match fst(input) with
                      | (id1,_,off1) -> (match snd(input) with
                         (id2,_,off2) -> ((id1,id2),(off1,off2)))

  let map input = return (List.map sub_map input)

  let rec sub_out lst ref_off read_off a = match lst with
                                          | [] -> a
                                          | (h1,h2)::tl -> if(h1=ref_off && h2=read_off)
                                                      then sub_out tl (ref_off+1) (read_off+1) (a@[(h1,h2)])
                                                      else sub_out tl ref_off read_off a


  let reduce (key, inters) : output Deferred.t =
    let ref_id = fst(key) in
    let read_id = snd(key) in
    let rec result lst a = match lst with
        | [] -> a
        | h::t -> (  let ref_off = fst(h) in
                     let read_off = snd(h) in
                     let ending = sub_out t (ref_off+1) (read_off+1) [h] in
                     let sub_result l = {length = 9 + List.length l;
                                         read = read_id;
                                         read_off = read_off;
                                         ref = ref_id;
                                         ref_off = ref_off} in
                     result(List.filter (fun a -> if List.mem a ending then false else true) t) (a@[sub_result ending])) in
        return(result (List.sort compare inters) [])

  let () = MapReduce.register_job (module DNA1)



