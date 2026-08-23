namespace FsFindLib
open System.IO

type FindConfig() =
    member val FileTypesPath : string = "FsFindLib.Resources.filetypes.json" with get, set
    member val FindOptionsPath : string = "FsFindLib.Resources.findoptions.json" with get, set
    member val DefaultFindSettingsPath : string = Path.Join(FileUtil.GetHomePath(), ".config", "xfind", "settings.json") with get, set
