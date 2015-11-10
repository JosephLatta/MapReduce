open Async.Std

(******************************************************************************)
(** {2 URLs}                                                                  *)
(******************************************************************************)

module URL : sig
  type url
  val relative  : url -> string -> url option

  val to_string : url -> string
  val of_string : string -> url option
  val to_cohttp : url -> Uri.t
end = struct
  (** Invariant: all urls represent absolute urls. *)
  (** Note: we define urls as strings instead of Neturl.urls so that they can be
      marshalled. *)
  type url = string
  let relative base link =
    try
      let http = Hashtbl.find Neturl.common_url_syntax "http" in
      let url  = Neturl.url_of_string http link in
      let base = Neturl.url_of_string http base in
      Some (Neturl.string_of_url (Neturl.ensure_absolute_url ~base:base url))
    with
      | Neturl.Malformed_URL -> None

  let of_string s = relative "http://" s

  let to_string u = u
  let to_cohttp u = Uri.of_string u
end

type url = URL.url

let url_to_string = URL.to_string
let string_to_url = URL.of_string

(******************************************************************************)
(** reversed lists ************************************************************)
(******************************************************************************)

module TailList : sig
  type 'a t

  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val rev     : 'a t -> 'a list

  val (<::) : 'a t -> 'a -> 'a t
  val (@)   : 'a t -> 'a list -> 'a t
  val nil   : 'a t
end = struct
  type 'a t = 'a list

  let of_list = List.rev
  let to_list = List.rev

  let (<::) lst elem = elem::lst
  let (@)   lst tail = List.rev_append tail lst
  let nil   = []
  let rev l = l
end

(******************************************************************************)
(** working with strings ******************************************************)
(******************************************************************************)

let whitespace  = Str.regexp "\\([ \r\n\t,;/=!?()]\\|&.*;\\)+"

let rec join delim strings = match strings with
  | []    -> ""
  | [s]   -> s
  | s::ss -> s^delim^(join delim ss)

(******************************************************************************)
(** parsing html **************************************************************)
(******************************************************************************)

type page = {
  url   : url;

  title : string;
  links : url list;
  words : string list;
  descr : string list;
}

(** the accumulator type used to build pages *)
type _page = {
  _links : url TailList.t;
  _words : string TailList.t;
  _title : string option;
  _descr : string TailList.t
}

let empty = {
  _links = TailList.nil;
  _words = TailList.nil;
  _descr = TailList.nil;
  _title = None;
}

let to_page url p = {
  url   = url;
  links = TailList.to_list p._links;
  words = TailList.to_list p._words;
  descr = TailList.to_list p._descr;
  title = match p._title with | None -> "<untitled>" | Some t -> t
}


let word_split = Str.regexp "[/.?+:-$&,;=@]\\|%..\\|&.*;"
let url_words u =
  let str = URL.to_string u in
  Str.split word_split str

let rec parse_page url docs =
  let rec helper docs accum = match docs with
    | [] -> accum

    | (Nethtml.Data s) :: tl ->
        let words = Str.split whitespace s in
        let words = List.map String.lowercase words in
        helper tl {accum with _words=TailList.(accum._words @ words)}

    | Nethtml.Element ("title",_,children) :: tl ->
        (* recursively parse the title, and just pull out all the words *)
        let words = TailList.to_list (helper children empty)._words in
        let title = join " " words in
        helper (children@tl)
                  {accum with _title=Some title;
                              _descr=TailList.(accum._descr @ words);
                  }

    | Nethtml.Element ("meta",attrs,children) :: tl ->
        (* deal with meta tags *)
        if List.mem ("name","description") attrs && List.mem_assoc "content" attrs then
          let words = List.assoc "content" attrs in
          let words = Str.split whitespace words in
          helper (children@tl) {accum with
            _words=TailList.(accum._words @ words);
            _descr=TailList.(accum._descr @ words);
          }
        else
          helper (children@tl) accum

    | Nethtml.Element ("script",_,_) :: tl ->
        (* ignore scripts *)
        helper tl accum

    | Nethtml.Element ("a",attrs,children) :: tl ->
        (* pull out the href *)
        if List.mem_assoc "href" attrs then
          let href = List.assoc "href" attrs in
          match URL.relative url href with
            | None      -> helper (children@tl) accum
            | Some link -> helper (children@tl) {accum with _links=TailList.(accum._links <:: link)}
        else
          helper (children@tl) accum

    | Nethtml.Element (_,_,children) :: tl ->
        helper (children@tl) accum

  in
  let descr = url_words url in
  to_page url (helper docs {empty with _descr=TailList.of_list descr})

let html_to_docs html =
  Nethtml.parse_document (Lexing.from_string html)

let pdf = Str.regexp "\\.pdf"

let contains reg s =
  try ignore(Str.search_forward reg s 0); true with | Not_found -> false

let get url =
  let url_string = url_to_string url in
  if contains pdf url_string then return None else
  let uri = URL.to_cohttp url in
  print_endline ("fetching "^url_string);
  let get () =
    Cohttp_async.Client.get ~interrupt:(after (Core.Std.sec 10.)) uri
  in
  try_with (fun () ->
    with_timeout (Core.Std.sec 10.) (get ()) >>= function
     | `Result page -> print_endline ("fetched "^url_string); return (Some page)
     | `Timeout     -> print_endline ("timeout "^url_string); return None
  ) >>= function
     | Core.Std.Result.Error _ -> print_endline ("exception "^url_string);
                                  return None
     | Core.Std.Result.Ok resp -> return resp

let fetch_page url =
  get url >>= function
    | None          -> return None
    | Some (_,body) -> 
        Cohttp_async.Body.to_string body
          >>| html_to_docs
          >>| parse_page url
          >>= fun page ->
        return (Some page)

