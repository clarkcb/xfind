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
