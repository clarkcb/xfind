using System.Linq;
using CsFindLib;

namespace CsFind;

static class Program
{
	public static void Main(string[] args)
	{
		var options = new FindOptions();
		try
		{
			var settings = options.SettingsFromArgs(args);

			if (settings.Debug)
			{
				Logger.Log(settings.ToString());
			}

			if (settings.PrintUsage)
			{
				options.Usage();
			}

			var finder = new Finder(settings);
			var fileResults = finder.Find().ToList();

			if (settings.PrintDirs)
			{
				finder.PrintMatchingDirs(fileResults);
			}

			if (settings.PrintFiles)
			{
				finder.PrintMatchingFiles(fileResults);
			}
		}
		catch (FindException e)
		{
			Logger.Log("");
			Logger.LogError(e.Message);
			options.Usage(1);
		}
	}
}
