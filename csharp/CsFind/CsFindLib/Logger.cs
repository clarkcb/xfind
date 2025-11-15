using System;

namespace CsFindLib;

public static class Logger
{
	public static void Log(string message)
	{
		Console.WriteLine(message);
	}

	public static void LogError(string message, bool colorize = true)
	{
		var err = colorize
			? $"{ConsoleColor.BoldRed}ERROR: {message}{ConsoleColor.Reset}"
			: $"ERROR: {message}";
		Console.Error.WriteLine(err);
	}
}
