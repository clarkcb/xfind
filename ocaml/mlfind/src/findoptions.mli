open Core.Std

type findOption = { long : string; short : string; desc : string }

val get_findoptions : findOption list

val get_usage : findOption list -> string

val settings_from_args : findOption list -> string list -> (Findsettings.t, string) Result.t
