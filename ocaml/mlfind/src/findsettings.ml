open Core.Std
open Common

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

let default_settings = {
  archivesonly = false;
  colorize = true;
  debug = false;
  excludehidden = true;
  in_archiveextensions = [];
  in_archivefilepatterns = [];
  in_dirpatterns = [];
  in_extensions = [];
  in_filepatterns = [];
  in_filetypes = [];
  in_linesafterpatterns = [];
  in_linesbeforepatterns = [];
  includearchives = false;
  listdirs = false;
  listfiles = false;
  out_archiveextensions = [];
  out_archivefilepatterns = [];
  out_dirpatterns = [];
  out_extensions = [];
  out_filepatterns = [];
  out_filetypes = [];
  paths = [];
  printusage = false;
  printversion = false;
  recursive = true;
  verbose = false
};;

let add_extensions (ext_string : string) (extensions : string list) = 
  let exts = String.split ext_string ~on:(char_of_int 44) in
  List.append extensions exts

let add_filetypes (ft_string : string) (filetypes : string list) = 
  let fts = String.split ft_string ~on:(char_of_int 44) in
  List.append filetypes fts

let set_archivesonly (ss : FindSettings.t) (archivesonly: bool) (ss : FindSettings.t) =
  let includearchives = if archivesonly then archivesonly else ss.includearchives
  { ss with archivesonly=archivesonly; includearchives=includearchives }

let set_debug (ss : FindSettings.t) (debug: bool) (ss : FindSettings.t) =
  let verbose = if debug then debug else ss.verbose
  { ss with debug=debug; verbose=verbose }

let to_string s = 
  String.concat [
    sprintf "{archivesonly=%b" s.archivesonly;
    sprintf "; colorize=%b" s.colorize;
    sprintf "; debug=%b" s.debug;
    sprintf "; excludehidden=%b" s.excludehidden;
    sprintf "; in_archiveextensions=%s" (list_to_string s.in_archiveextensions);
    sprintf "; in_archivefilepatterns=%s" (regexp_list_to_string s.in_archivefilepatterns);
    sprintf "; in_dirpatterns=%s" (regexp_list_to_string s.in_dirpatterns);
    sprintf "; in_extensions=%s" (list_to_string s.in_extensions);
    sprintf "; in_filepatterns=%s" (regexp_list_to_string s.in_filepatterns);
    sprintf "; in_filetypes=%s" (list_to_string s.in_filetypes);
    sprintf "; includearchives=%b" s.includearchives;
    sprintf "; listdirs=%b" s.listdirs;
    sprintf "; listfiles=%b" s.listfiles;
    sprintf "; out_archiveextensions=%s" (list_to_string s.out_archiveextensions);
    sprintf "; out_archivefilepatterns=%s" (regexp_list_to_string s.out_archivefilepatterns);
    sprintf "; out_dirpatterns=%s" (regexp_list_to_string s.out_dirpatterns);
    sprintf "; out_extensions=%s" (list_to_string s.out_extensions);
    sprintf "; out_filepatterns=%s" (regexp_list_to_string s.out_filepatterns);
    sprintf "; out_filetypes=%s" (list_to_string s.out_filetypes);
    sprintf "; printversion=%b" s.printversion;
    sprintf "; recursive=%b" s.recursive;
    sprintf "; paths=%s" (list_to_string s.paths);
    sprintf "; verbose=%b}" s.verbose];;
