namespace FsFindLib
open System
open System.IO

type FindConfig() =
    let getXfindConfigDir () =
        let defaultXFindConfigDir = Path.Join(FileUtil.GetHomePath(), ".config", "xfind")
        let xFindConfigDir = Environment.GetEnvironmentVariable("XFIND_CONFIG_DIR")
        if String.IsNullOrEmpty(xFindConfigDir) then
            defaultXFindConfigDir
        else
            xFindConfigDir

    member val FileTypesPath : string = "FsFindLib.Resources.filetypes.json" with get, set
    member val FindOptionsPath : string = "FsFindLib.Resources.findoptions.json" with get, set
    member val DefaultFindSettingsPath : string = Path.Join(getXfindConfigDir(), "settings.json") with get, set
