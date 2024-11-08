using System;

namespace CsFindLib;

public class FindOption(string? shortArg, string longArg, string description)
{
	public string? ShortArg { get; } = shortArg;
	public string LongArg { get; } = longArg;

	public string SortArg
	{
		get
		{
			var longArg = LongArg.Replace("in-", "ina");
			if (!string.IsNullOrWhiteSpace(ShortArg))
				return ShortArg.ToLower() + "a" + longArg;
			return longArg;
		}
	}
	public string Description { get; } = description;
}

internal class FindArgOption(string? shortArg, string longArg, Action<string, FindSettings> action, string description)
	: FindOption(shortArg, longArg, description)
{
	public Action<string, FindSettings> Action { get; private set; } = action;
}

internal class FindFlagOption(string? shortArg, string longArg, Action<bool, FindSettings> action, string description)
	: FindOption(shortArg, longArg, description)
{
	public Action<bool, FindSettings> Action { get; private set; } = action;
}
