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
		var err = colorize ? $"{Color.BoldRed}ERROR: {message}{Color.Reset}" : $"ERROR: {message}";
		Console.Error.WriteLine(err);
	}
}
