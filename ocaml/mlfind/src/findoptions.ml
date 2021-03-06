open Core.Std
open Xml;;
(* 
    NOTE: xml parsing is done using 3rd-party library xml-light, which must be installed:

    $ opam install xml-light

    and a package reference must be added to the build line:

    -package xml-light
 *)
type findOption = { long : string; short : string; desc : string };;

let get_desc option_node = 
  match (List.hd (Xml.children option_node)) with
  | Some hd -> String.strip (Xml.pcdata hd)
  | None    -> "";;

let get_findoptions : findOption list = 
  let x = Xml.parse_file (Config.xfindpath ^ "/shared/findoptions.xml") in
  let option_nodes = Xml.children x in
  List.map option_nodes
    ~f:(fun o -> { short=(Xml.attrib o "short"); long=(Xml.attrib o "long"); desc=(get_desc o) });;

let cmp_findoptions so1 so2 = 
  let short_or_long so =
    match (so.short, so.long) with
    | ("", l) -> l
    | (s, l)  -> (String.lowercase s) ^ "@" ^ l in
  let sl1 = short_or_long so1 in
  let sl2 = short_or_long so2 in
  if sl1 < sl2 then -1
  else if sl1 > sl2 then 1
  else 0

let get_opt_strings findoptions = 
  let short_and_or_long so =
    match (so.short, so.long) with
    | ("", l) -> "--" ^ l
    | (s, l)  -> "-" ^ s ^ ",--" ^ l in
  List.map findoptions ~f:short_and_or_long

let pad_string s len =
  if String.length s < len then s ^ (String.make (len - (String.length s)) ' ')
  else s

let rec zip l1 l2 zipped = 
  match (l1, l2) with
  | ([], _) -> zipped
  | (_, []) -> zipped
  | ((h1 :: t1), (h2 :: t2)) -> zip t1 t2 (List.append zipped [(h1, h2)])

(* findoptions_to_string :: [FindOption] -> String *)
let findoptions_to_string findoptions = 
  let sorted = List.sort findoptions ~cmp:cmp_findoptions in
  let opt_strings = get_opt_strings sorted in
  let opt_descs = List.map sorted ~f:(fun so -> so.desc) in
  let longest = List.fold ~init:0 ~f:max (List.map opt_strings ~f:String.length) in
  let format_opt_line o d =
    " " ^ (pad_string o longest) ^ "  " ^ d in
  let zipped = zip opt_strings opt_descs [] in
  let lines = List.map zipped ~f:(fun (o, d) -> format_opt_line o d) in
  String.concat lines ~sep:"\n"

let get_usage findoptions = 
  "Usage:\n mlfind [options] -s <findpattern> <startpath>\n\nOptions:\n"
  ^ (findoptions_to_string findoptions)


type argAction = string -> Findsettings.t -> Findsettings.t;;

let arg_actions : (string * argAction) list = [
  ("in-archiveext", fun s ss -> { ss with in_archiveextensions=Findsettings.add_extensions s ss.in_archiveextensions });
  ("in-archivefilepattern", fun s ss -> { ss with in_archivefilepatterns=List.append ss.in_archivefilepatterns [Re2.Regex.create_exn s] });
  ("in-dirpattern", fun s ss -> { ss with in_dirpatterns=List.append ss.in_dirpatterns [Re2.Regex.create_exn s] });
  ("in-ext", fun s ss -> { ss with in_extensions=Findsettings.add_extensions s ss.in_extensions });
  ("in-filepattern", fun s ss -> { ss with in_filepatterns=List.append ss.in_filepatterns [Re2.Regex.create_exn s] });
  ("out-archiveext", fun s ss -> { ss with out_archiveextensions=Findsettings.add_extensions s ss.out_archiveextensions });
  ("out-archivefilepattern", fun s ss -> { ss with out_archivefilepatterns=List.append ss.out_archivefilepatterns [Re2.Regex.create_exn s] });
  ("out-dirpattern", fun s ss -> { ss with out_dirpatterns=List.append ss.out_dirpatterns [Re2.Regex.create_exn s] });
  ("out-ext", fun s ss -> { ss with out_extensions=Findsettings.add_extensions s ss.out_extensions });
  ("out-filepattern", fun s ss -> { ss with out_filepatterns=List.append ss.out_filepatterns [Re2.Regex.create_exn s] });
  ("path", fun s ss -> { ss with paths=List.append ss.paths s })
];;

type boolFlagAction = bool -> Findsettings.t -> Findsettings.t;;

let bool_flag_actions : (string * boolFlagAction) list = [
  ("archivesonly", fun b ss -> FindSettings.set_archivesonly ss b);
  ("colorize", fun b ss -> { ss with colorize=b });
  ("debug", fun b ss -> FindSettings.set_debug ss b);
  ("excludearchives", fun b ss -> { ss with includearchives=(not b) });
  ("excludehidden", fun b ss -> { ss with excludehidden=b });
  ("help", fun b ss -> { ss with printusage=b });
  ("includearchives", fun b ss -> { ss with includearchives=b });
  ("includehidden", fun b ss -> { ss with excludehidden=(not b) });
  ("listdirs", fun b ss -> { ss with listdirs=b });
  ("listfiles", fun b ss -> { ss with listfiles=b });
  ("nocolorize", fun b ss -> { ss with colorize=(not b) });
  ("noprintmatches", fun b ss -> { ss with printresults=(not b) });
  ("norecursive", fun b ss -> { ss with recursive=(not b) });
  ("printmatches", fun b ss -> { ss with printresults=b });
  ("recursive", fun b ss -> { ss with recursive=b });
  ("verbose", fun b ss -> { ss with verbose=b });
  ("version", fun b ss -> { ss with printversion=b })
];;

let rec arg_name arg = 
  if arg.[0] = '-' && (String.length arg) > 1 then arg_name (String.sub arg ~pos:1 ~len:((String.length arg) - 1))
  else arg

let get_long_arg findoptions arg = 
  match List.find findoptions ~f:(fun o -> o.long = arg || o.short = arg) with
  | Some opt -> Some opt.long
  | None     -> None

let settings_from_args findoptions args = 
  let rec rec_settings_from_args (settings : Findsettings.t) (args : string list) =
    match args with
    | [] -> Ok settings
    | hd :: tl when hd.[0] = '-' ->
        (let arg = arg_name hd in
         match get_long_arg findoptions arg with
         | Some "help" -> rec_settings_from_args { settings with printusage=true } []
         | Some long_arg ->
             (match List.find bool_flag_actions ~f:(fun (s, _) -> s = long_arg) with
              | Some (_, f) -> rec_settings_from_args (f true settings) tl
              | None ->
                (match List.find arg_actions ~f:(fun (s, _) -> s = long_arg) with
                 | Some (_, f) ->
                    (match tl with
                     | [] -> Error (sprintf "Missing value for option: %s" arg)
                     | h :: t  -> rec_settings_from_args (f h settings) t)
                 | None -> Error (sprintf "Invalid option: %s" arg)))
         | None -> Error (sprintf "Invalid option: %s" arg))
    | hd :: tl -> rec_settings_from_args { settings with paths=List.append settings.paths hd } tl in
  (* default listfiles to true since running as cli *)
  rec_settings_from_args { Findsettings.default_settings with listfiles=true } args;;

