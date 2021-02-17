namespace CsFind
{
	static class FindMain
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
				finder.Find();

				if (settings.PrintResults)
				{
					Common.Log("");
					finder.PrintResults();
				}

				if (settings.ListDirs)
				{
					finder.PrintMatchingDirs();
				}

				if (settings.ListFiles)
				{
					finder.PrintMatchingFiles();
				}

				if (settings.ListLines)
				{
					finder.PrintMatchingLines();
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
