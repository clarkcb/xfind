﻿using System;

namespace CsFind
{
	public class FindOption
	{
		public string? ShortArg { get; }
		public string LongArg { get; }
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
		public string Description { get; }

		public FindOption(string? shortArg, string longArg, string description)
		{
			ShortArg = shortArg;
			LongArg = longArg;
			Description = description;
		}
	}

	internal class FindArgOption : FindOption
	{
		public Action<string, FindSettings> Action { get; private set; }
		public FindArgOption(string? shortArg, string longArg, Action<string, FindSettings> action, string description) :
			base(shortArg, longArg, description)
		{
			Action = action;
		}
	}

	internal class FindFlagOption : FindOption
	{
		public Action<bool, FindSettings> Action { get; private set; }
		public FindFlagOption(string? shortArg, string longArg, Action<bool, FindSettings> action, string description) :
			base(shortArg, longArg, description)
		{
			Action = action;
		}
	}
}
