open Core.Std
open Common

let print_usage findoptions = 
  log_msg (sprintf "\n%s\n" (Findoptions.get_usage findoptions));;

let print_error_with_usage (err : string) findoptions = 
  log_msg (sprintf "\nERROR: %s\n\n%s\n" err (Findoptions.get_usage findoptions));;

let sort_lines (lines : string list) : string list =
  let cmp_lines l1 l2 : int =
    if l1 < l2 then -1
    else if l1 > l2 then 1
    else 0 in
  List.sort lines ~cmp:cmp_lines

let get_matching_files (results : Findresult.t list) : string list =
  let rec rec_get_matching_files (res : Findresult.t list) (files : string list) : string list =
    match res with
    | [] -> files
    | r :: rs -> (
      let file = Findfile.to_string r.file in
      if List.exists files ~f:(fun f -> f = file)
      then rec_get_matching_files rs files
      else rec_get_matching_files rs (List.append files [file])) in
  rec_get_matching_files results []

let print_matching_files (results : Findresult.t list) : unit = 
  let matching_files = get_matching_files results in
  log_msg (sprintf "\nFiles with matches (%d):" (List.length matching_files));
  List.iter matching_files ~f:(fun f -> log_msg f);;

let get_matching_dirs (results : Findresult.t list) : string list =
  let rec rec_get_matching_dirs (res : Findresult.t list) (dirs : string list) : string list =
    match res with
    | [] -> dirs
    | r :: rs -> (
      let dir = r.file.path in
      if List.exists dirs ~f:(fun d -> d = dir)
      then rec_get_matching_dirs rs dirs
      else rec_get_matching_dirs rs (List.append dirs [dir])) in
  rec_get_matching_dirs results []

let print_matching_dirs (results : Findresult.t list) : unit = 
  let matching_dirs = get_matching_dirs results in
  log_msg (sprintf "\nDirectories with matches (%d):" (List.length matching_dirs));
  List.iter matching_dirs ~f:(fun d -> log_msg d);;

let find (settings : Findsettings.t) findoptions = 
  if settings.debug then log_msg (sprintf "settings: %s" (Findsettings.to_string settings));
  match Finder.find settings with
  | Ok (results : Findresult.t list) ->
      if settings.listdirs then print_matching_dirs results;
      if settings.listfiles then print_matching_files results;
  | Error msg -> print_error_with_usage msg findoptions;;

let () =
  let findoptions = Findoptions.get_findoptions in
  match (Array.to_list Sys.argv) with
  | []      -> print_error_with_usage "Startpath not defined" findoptions
  | [_]     -> print_error_with_usage "Startpath not defined" findoptions
  | _ :: tl -> (
      match (Findoptions.settings_from_args findoptions tl) with
      | Ok settings ->
        if settings.printusage
        then print_usage findoptions
        else find settings findoptions
      | Error msg -> print_error_with_usage msg findoptions
    );;
