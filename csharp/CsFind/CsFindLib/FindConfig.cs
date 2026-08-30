using System;
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
        var defaultXFindConfigDir = Path.Join(homePath, ".config", "xfind");
        var xFindConfigDir = Environment.GetEnvironmentVariable("XFIND_CONFIG_DIR");
        if (string.IsNullOrEmpty(xFindConfigDir))
        {
            xFindConfigDir = defaultXFindConfigDir;
        }

        DefaultFindSettingsPath = Path.Join(xFindConfigDir, "settings.json");
    }
}