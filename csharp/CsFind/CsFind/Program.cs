using CsFindLib;

namespace CsFind;

static class Program
{
	public static void Main(string[] args)
	{
		var colorize = true;
		FindOptions? options =  null;
		try
		{
			options = new FindOptions();
			var settings = options.SettingsFromArgs(args);
			colorize = settings.Colorize;

			if (settings.Debug)
			{
				Logger.Log(settings.ToString());
			}

			if (settings.PrintUsage)
			{
				options.Usage();
			}

			var finder = new Finder(settings);
			var fileResults = finder.Find();
			var formatter = new FileResultFormatter(settings);

			if (settings.PrintDirs)
			{
				Finder.PrintMatchingDirs(fileResults, formatter);
			}

			if (settings.PrintFiles)
			{
				Finder.PrintMatchingFiles(fileResults, formatter);
			}
		}
		catch (FindException e)
		{
			Logger.Log("");
			Logger.LogError(e.Message, colorize);
			options?.Usage(1);
		}
	}
}
