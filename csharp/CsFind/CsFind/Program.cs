using System;

namespace CsFind
{
    class Program
    {
        static void Main(string[] args)
        {
			var options = new FindOptions();
			try
			{
				var settings = options.SettingsFromArgs(args);

				if (settings.Debug)
				{
					Common.Log("settings: " + settings + "\n");
				}

				if (settings.PrintUsage)
				{
					options.Usage();
				}

				var finder = new Finder(settings);
				var files = finder.Find();

				if (settings.ListDirs)
				{
					finder.PrintMatchingDirs(files);
				}

				if (settings.ListFiles)
				{
					finder.PrintMatchingFiles(files);
				}
			}
			catch (FindException e)
			{
				Common.Log($"\nERROR: {e.Message}");
				options.Usage(1);
			}
        }
    }
}
