using System;

namespace CsFindLib;

public static class Common
{
	public static void Log(string message)
	{
		Console.WriteLine(message);
	}

	public static void LogError(string message)
	{
		Console.Error.WriteLine($"ERROR: {message}");
	}
}
