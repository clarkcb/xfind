open Core.Std
open Common

module Regex = Re2.Regex

(* find :: Findsettings.t -> string list *)
let validate_settings (settings : Findsettings.t) =
  let errs = [
    if (List.is_empty settings.paths) then (Some "Startpath not defined") else None;
    if (Sys.file_exists_exn settings.startpath) then None else (Some "Startpath not found");
  ] in
  List.filter errs ~f:(fun e -> Option.is_some e)
  |> List.map ~f:(fun e -> Option.value ~default:"" e);;

let is_find_dir (settings : Findsettings.t) (dir : string) = 
  let tests : (string -> bool) list = [
    (fun d -> not (Fileutil.is_hidden d) || not settings.excludehidden);
    (fun d -> List.is_empty settings.in_dirpatterns ||
              List.exists settings.in_dirpatterns ~f:(fun p -> Regex.matches p d));
    (fun d -> List.is_empty settings.out_dirpatterns ||
              not (List.exists settings.out_dirpatterns ~f:(fun p -> Regex.matches p d)));
  ] in
  List.for_all tests ~f:(fun t -> t dir);;

let get_find_dirs (settings : Findsettings.t) (path : string) = 
  let rec rec_get_find_dirs dirs finddirs = 
    match dirs with
    | [] -> finddirs
    | h :: t -> 
        let subdirs =
          if (is_find_dir settings h) then
            Sys.ls_dir h
              |> List.filter ~f:(fun d -> Sys.is_directory_exn (Filename.concat h d))
              |> List.map ~f:(fun d -> Filename.concat h d)
          else [] in
        let alldirs = List.append subdirs t in
        if (is_find_dir settings h)
        then rec_get_find_dirs alldirs (List.append finddirs [h])
        else rec_get_find_dirs alldirs finddirs in
  rec_get_find_dirs [path] [];;

let is_archive_find_file (settings : Findsettings.t) (file : string) = 
  let tests : (string -> bool) list = [
    (* (fun f -> not (Fileutil.is_hidden f) || not settings.excludehidden); *)
    (fun f -> List.is_empty settings.in_archiveextensions ||
              List.mem settings.in_archiveextensions (Fileutil.get_extension f));
    (fun f -> List.is_empty settings.out_archiveextensions ||
              not (List.mem settings.out_archiveextensions (Fileutil.get_extension f)));
    (fun f -> List.is_empty settings.in_archivefilepatterns ||
              List.exists settings.in_archivefilepatterns ~f:(fun p -> Regex.matches p f));
    (fun f -> List.is_empty settings.out_archivefilepatterns ||
              not (List.exists settings.out_archivefilepatterns ~f:(fun p -> Regex.matches p f)));
  ] in
  List.for_all tests ~f:(fun t -> t file);;

let is_find_file (settings : Findsettings.t) (file : string) = 
  let tests : (string -> bool) list = [
    (fun f -> List.is_empty settings.in_extensions ||
              List.mem settings.in_extensions (Fileutil.get_extension f));
    (fun f -> List.is_empty settings.out_extensions ||
              not (List.mem settings.out_extensions (Fileutil.get_extension f)));
    (fun f -> List.is_empty settings.in_filepatterns ||
              List.exists settings.in_filepatterns ~f:(fun p -> Regex.matches p f));
    (fun f -> List.is_empty settings.out_filepatterns ||
              not (List.exists settings.out_filepatterns ~f:(fun p -> Regex.matches p f)));
  ] in
  List.for_all tests ~f:(fun t -> t file);;

let filter_file (settings : Findsettings.t) (sf : Findfile.t) = 
  let filename = (Findfile.to_string sf) in
  if not (Fileutil.is_hidden filename) || not settings.excludehidden
  then match sf.filetype with
       | Filetypes.Text -> is_find_file settings filename
       | Filetypes.Binary -> is_find_file settings filename
       | Filetypes.Archive -> settings.findarchives && is_archive_find_file settings filename
       | _ -> false
  else false

let get_find_files (settings : Findsettings.t) (filetypes : Filetypes.t) (finddirs : string list) : (Findfile.t list) = 
  let rec rec_get_find_files dirs (findfiles : Findfile.t list) = 
    match dirs with
    | [] -> findfiles
    | d :: ds -> 
      let newfindfiles = Sys.ls_dir d
        |> List.map ~f:(fun f -> Filename.concat d f)
        |> List.filter ~f:(fun f -> Sys.is_file_exn f)
        |> List.map ~f:(fun f -> Findfile.create f (Filetypes.get_filetype filetypes f))
        |> List.filter ~f:(fun sf -> filter_file settings sf)
      in
      rec_get_find_files ds (List.append findfiles newfindfiles) in
  rec_get_find_files finddirs [];;

let rec rec_get_pattern_matches (settings : Findsettings.t) (patterns : Regex.t list) (s : string) (p_matches : (Regex.t * Regex.Match.t) list) = 
  match patterns with
  | [] -> p_matches
  | p :: ps ->
    let next_matches =
      if settings.firstmatch
      then Regex.get_matches_exn ~max:1 p s
      else Regex.get_matches_exn p s in
    let next_p_matches = List.map next_matches ~f:(fun m -> (p, m)) in
    rec_get_pattern_matches settings ps s (List.append p_matches next_p_matches)

let get_linesbefore (settings : Findsettings.t) (s : string) (newline_indices : int list) (start_index : int) : (string list) = 
  if (settings.linesbefore > 0 && start_index > 0)
  then
    let rev_lt_indices =
      List.take_while newline_indices ~f:(fun n -> n < start_index)
      |> List.rev in
    let before_newline_indices = 
      List.take rev_lt_indices (settings.linesbefore + 1)
      |> List.rev in
    let rec rec_get_linesbefore nl_indices linesbefore = 
      match nl_indices with
      | x :: y :: rst -> 
        let next_line = String.sub s ~pos:(x + 1) ~len:(y - x - 1) in
        rec_get_linesbefore (y :: rst) (List.append linesbefore [next_line])
      | _ -> linesbefore in
    rec_get_linesbefore before_newline_indices []
  else []

let get_linesafter (settings : Findsettings.t) (s : string) (newline_indices : int list) (end_index : int) : (string list) = 
  if (settings.linesafter > 0 && end_index < (String.length s))
  then
    let after_newline_indices =
      List.take (List.drop_while newline_indices ~f:(fun n -> n < end_index)) (settings.linesafter + 1) in
    let rec rec_get_linesafter nl_indices linesafter = 
      match nl_indices with
      | x :: y :: rst -> 
        let next_line = String.sub s ~pos:(x + 1) ~len:(y - x - 1) in
        rec_get_linesafter (y :: rst) (List.append linesafter [next_line])
      | _ -> linesafter in
    rec_get_linesafter after_newline_indices []
  else []

let any_matches_any_pattern (lines : string list) (patterns : Regex.t list) = 
  List.exists lines ~f:(fun l -> List.exists patterns ~f:(fun p -> Regex.matches p l))

let lines_match (lines : string list) (in_patterns : Regex.t list) (out_patterns : Regex.t list) = 
  match lines with
  | [] -> if (List.is_empty in_patterns) then true else false
  | _  ->
     (((List.is_empty in_patterns) || any_matches_any_pattern lines in_patterns) &&
      (List.is_empty out_patterns) || not (any_matches_any_pattern lines out_patterns))

(* find_file :: Findsettings.t -> Filetypes.t -> Findresult.t list *)
let find_file (settings : Findsettings.t) (sf : Findfile.t) : Findresult.t list = 
  (* if settings.debug then log_msg (sprintf "Finding file %s" (Findfile.to_string sf)); *)
  let results : Findresult.t list =
    match sf.filetype with
    | Filetypes.Text -> find_text_file settings sf
    | Filetypes.Binary -> find_binary_file settings sf
    | Filetypes.Archive -> if settings.findarchives then find_archive_file settings sf else []
    | _ -> [] in
  results;;

(* find_path :: Findsettings.t -> Findresult.t list *)
let find_path (settings : Findsettings.t) (filetypes : Filetypes.t) (path : string) : Findresult.t list = 
  let finddirs = get_find_dirs settings path in
  if settings.verbose 
  then log_msg (sprintf "\nDirectories to be found (%d):\n%s" (List.length finddirs) (String.concat finddirs ~sep:"\n"));
  let findfiles = get_find_files settings filetypes finddirs in
  let rec rec_find_files files results : (Findresult.t list) = 
    match files with
    | [] -> results
    | f :: fs -> rec_find_files fs (List.append results (find_file settings f)) in
  rec_find_files findfiles [];;

(* do_find :: Findsettings.t -> Findresult.t list *)
let do_find (settings : Findsettings.t) : Findresult.t list = 
  let filetypes = Filetypes.get_filetypes in
  if Sys.is_directory_exn settings.startpath
  then (find_path settings filetypes settings.startpath)
  else
    let start_filetype = Filetypes.get_filetype filetypes settings.startpath in
    let start_findfile = Findfile.create settings.startpath start_filetype in
    find_file settings start_findfile;;

(* find :: Findsettings.t -> Result (Findresult.t list) *)
let find (settings : Findsettings.t) = 
  match (validate_settings settings) with
  | [] -> Ok (do_find settings)
  | err :: _ -> Error err;;
