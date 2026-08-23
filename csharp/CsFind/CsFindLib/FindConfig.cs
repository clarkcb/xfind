using System.IO;

namespace CsFindLib;

public class FindConfig
{
    public string FileTypesPath { get; private set; }
    public string FindOptionsPath { get; private set; }
    public string DefaultFindSettingsPath { get; private set; }

    public FindConfig()
    {
        FileTypesPath = "CsFindLib.Resources.filetypes.json";
        FindOptionsPath = "CsFindLib.Resources.findoptions.json";
		var homePath = FileUtil.GetHomePath();
        DefaultFindSettingsPath = Path.Join(homePath, ".config", "xfind", "settings.json");
    }
}