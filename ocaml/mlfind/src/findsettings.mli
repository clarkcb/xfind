open Core.Std
(* module Regex = Re2.Regex *)

type t = {
  archivesonly : bool;
  colorize : bool;
  debug : bool;
  excludehidden : bool;
  in_archiveextensions : string list;
  in_archivefilepatterns : Re2.Regex.t list;
  in_dirpatterns : Re2.Regex.t list;
  in_extensions : string list;
  in_filepatterns : Re2.Regex.t list;
  in_filetypes : string list;
  includearchives : bool;
  listdirs : bool;
  listfiles : bool;
  out_archiveextensions : string list;
  out_archivefilepatterns : Re2.Regex.t list;
  out_dirpatterns : Re2.Regex.t list;
  out_extensions : string list;
  out_filepatterns : Re2.Regex.t list;
  out_filetypes : string list;
  paths : string list;
  printusage : bool;
  printversion : bool;
  recursive : bool;
  verbose : bool
}

val default_settings : t

val add_extensions : string -> string list -> string list

val add_filetypes : string -> string list -> string list

val to_string : t -> string
