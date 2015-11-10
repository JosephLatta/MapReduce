(** Module for fetching html pages from the web and parsing them *)

open Async.Std

(******************************************************************************)
(** {2 URLs}                                                                  *)
(******************************************************************************)

type url

val url_to_string : url    -> string
val string_to_url : string -> url option

(******************************************************************************)
(** {2 Pages}                                                                 *)
(******************************************************************************)

type page = {
  url : url;

  (** The document title *)
  title : string;

  (** The list of outgoing links *)
  links : url list;

  (** The list of words in the page *)
  words : string list;

  (** A list of ``descriptive'' words about the document (such as words from
      the document title and descsription meta tags *)
  descr : string list
}

(** Fetch and return a page. *)
val fetch_page : url -> page option Deferred.t

