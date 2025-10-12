using System;

namespace CsFindLib;

public interface IOption
{
	string? ShortArg { get; }
	string LongArg { get; }
	string Description { get; }
	ArgTokenType ArgType { get; }

	string SortArg { get; }
}

public class FindOption(string? shortArg, string longArg, string description, ArgTokenType argType) : IOption
{
	public string? ShortArg { get; } = shortArg;
	public string LongArg { get; } = longArg;
	public string Description { get; } = description;
	public ArgTokenType ArgType { get; } =  argType;

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
}
