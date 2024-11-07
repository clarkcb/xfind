using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using FindOptionsDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string, string>>>;

namespace CsFindLib;

public class FindOptions
{
	private readonly string _findOptionsResource;

	private static readonly Dictionary<string, Action<string, FindSettings>> ArgActionDictionary =
		new()
		{
			{ "in-archiveext", (s, settings) => settings.AddInArchiveExtension(s) },
			{ "in-archivefilepattern", (s, settings) => settings.AddInArchiveFilePattern(s) },
			{ "in-dirpattern", (s, settings) => settings.AddInDirPattern(s) },
			{ "in-ext", (s, settings) => settings.AddInExtension(s) },
			{ "in-filepattern", (s, settings) => settings.AddInFilePattern(s) },
			{ "in-filetype", (s, settings) => settings.AddInFileType(s) },
			{ "maxdepth", (s, settings) => settings.MaxDepth = int.Parse(s) },
			{ "maxlastmod", (s, settings) => {
					settings.MaxLastMod = DateTime.Parse(s);
				}
			},
			{ "maxsize", (s, settings) => settings.MaxSize = int.Parse(s) },
			{ "mindepth", (s, settings) => settings.MinDepth = int.Parse(s) },
			{ "minlastmod", (s, settings) => {
					settings.MinLastMod = DateTime.Parse(s);
				}
			},
			{ "minsize", (s, settings) => settings.MinSize = int.Parse(s) },
			{ "out-archiveext", (s, settings) => settings.AddOutArchiveExtension(s) },
			{ "out-archivefilepattern", (s, settings) => settings.AddOutArchiveFilePattern(s) },
			{ "out-dirpattern", (s, settings) => settings.AddOutDirPattern(s) },
			{ "out-ext", (s, settings) => settings.AddOutExtension(s) },
			{ "out-filepattern", (s, settings) => settings.AddOutFilePattern(s) },
			{ "out-filetype", (s, settings) => settings.AddOutFileType(s) },
			{ "path", (s, settings) => settings.Paths.Add(s) },
			{ "settings-file", SettingsFromFile },
			{ "sort-by", (s, settings) => settings.SetSortBy(s) },
		};

	private static readonly Dictionary<string, Action<bool, FindSettings>> BoolFlagActionDictionary =
		new()
		{
			{ "archivesonly", (b, settings) => settings.ArchivesOnly = b },
			{ "debug", (b, settings) => settings.Debug = b },
			{ "excludearchives", (b, settings) => settings.IncludeArchives = !b },
			{ "excludehidden", (b, settings) => settings.IncludeHidden = !b },
			{ "followsymlinks", (b, settings) => settings.FollowSymlinks = b },
			{ "help", (b, settings) => settings.PrintUsage = b },
			{ "includearchives", (b, settings) => settings.IncludeArchives = b },
			{ "includehidden", (b, settings) => settings.IncludeHidden = b },
			{ "nofollowsymlinks", (b, settings) => settings.FollowSymlinks = !b },
			{ "noprintdirs", (b, settings) => settings.PrintDirs = !b },
			{ "noprintfiles", (b, settings) => settings.PrintFiles = !b },
			{ "printdirs", (b, settings) => settings.PrintDirs = b },
			{ "printfiles", (b, settings) => settings.PrintFiles = b },
			{ "norecursive", (b, settings) => settings.Recursive = !b },
			{ "recursive", (b, settings) => settings.Recursive = b },
			{ "sort-ascending", (b, settings) => settings.SortDescending = !b },
			{ "sort-caseinsensitive", (b, settings) => settings.SortCaseInsensitive = b },
			{ "sort-casesensitive", (b, settings) => settings.SortCaseInsensitive = !b },
			{ "sort-descending", (b, settings) => settings.SortDescending = b },
			{ "verbose", (b, settings) => settings.Verbose = b },
			{ "version", (b, settings) => settings.PrintVersion = b },
		};

	public List<FindOption> Options { get; }
	public Dictionary<string, FindOption> ArgDictionary { get; }
	public Dictionary<string, FindOption> FlagDictionary { get; }

	public FindOptions()
	{
		_findOptionsResource = EmbeddedResource.GetResourceFileContents("CsFindLib.Resources.findoptions.json");
		Options = new List<FindOption>();
		ArgDictionary = new Dictionary<string, FindOption>();
		FlagDictionary = new Dictionary<string, FindOption>();
		SetOptionsFromJson();
	}

	private void SetOptionsFromJson()
	{
		var findOptionsDict = JsonSerializer.Deserialize<FindOptionsDictionary>(_findOptionsResource);
		var optionDicts = findOptionsDict!["findoptions"];
		foreach (var optionDict in optionDicts)
		{
			var longArg = optionDict["long"];
			var shortArg = optionDict.GetValueOrDefault("short");
			var desc = optionDict["desc"];
			if (ArgActionDictionary.TryGetValue(longArg, out var argActionValue))
			{
				var option = new FindArgOption(shortArg, longArg, argActionValue, desc);
				Options.Add(option);
				ArgDictionary.Add(longArg, option);
				if (!string.IsNullOrWhiteSpace(shortArg))
				{
					ArgDictionary.Add(shortArg, option);
				}
			}
			else if (BoolFlagActionDictionary.TryGetValue(longArg, out var boolFlagActionValue))
			{
				var option = new FindFlagOption(shortArg, longArg, boolFlagActionValue, desc);
				Options.Add(option);
				FlagDictionary.Add(longArg, option);
				if (!string.IsNullOrWhiteSpace(shortArg))
				{
					FlagDictionary.Add(shortArg, option);
				}
			}
		}
	}

	private static void SettingsFromFile(string filePath, FindSettings settings)
	{
		var fileInfo = new FileInfo(filePath);
		if (!fileInfo.Exists)
			throw new FindException($"Settings file not found: {filePath}");
		var contents = FileUtil.GetFileContents(filePath, Encoding.Default);
		SettingsFromJson(contents, settings);
	}

	public static void SettingsFromJson(string jsonString, FindSettings settings)
	{
		var settingsDict = JsonSerializer.Deserialize<Dictionary<string, object>>(jsonString);
		if (settingsDict != null) {
			foreach (var (key, value) in settingsDict)
			{
				var obj = (JsonElement)value;
				ApplySetting(key, obj, settings);
			}
		}
	}

	private static void ApplySetting(string arg, JsonElement obj, FindSettings settings)
	{
		switch (obj.ValueKind)
		{
			case JsonValueKind.String:
				string? s = obj.GetString();
				if (s != null) {
					ApplySetting(arg, s, settings);
				}
				break;
			case JsonValueKind.True:
				ApplySetting(arg, true, settings);
				break;
			case JsonValueKind.False:
				ApplySetting(arg, false, settings);
				break;
			case JsonValueKind.Number:
				ApplySetting(arg, obj.GetInt32().ToString(), settings);
				break;
			case JsonValueKind.Array:
				foreach (var arrVal in obj.EnumerateArray())
				{
					ApplySetting(arg, arrVal, settings);
				}
				break;
			case JsonValueKind.Undefined:
			case JsonValueKind.Object:
			case JsonValueKind.Null:
			default:
				break;
		}
	}

	private static void ApplySetting(string arg, string val, FindSettings settings)
	{
		if (ArgActionDictionary.ContainsKey(arg))
		{
			ArgActionDictionary[arg](val, settings);
		}
		else
		{
			throw new FindException($"Invalid option: {arg}");
		}
	}

	private static void ApplySetting(string arg, bool val, FindSettings settings)
	{
		if (BoolFlagActionDictionary.ContainsKey(arg))
		{
			BoolFlagActionDictionary[arg](val, settings);
		}
		else
		{
			throw new FindException($"Invalid option: {arg}");
		}
	}

	public FindSettings SettingsFromArgs(IEnumerable<string> args)
	{
		// default to PrintFiles = true since this is called from CLI
		var settings = new FindSettings {PrintFiles = true};
		var queue = new Queue<string>(args);

		while (queue.Count > 0)
		{
			var s = queue.Dequeue();
			if (s.StartsWith("-"))
			{
				try
				{
					while (s.StartsWith("-"))
					{
						s = s.Substring(1);
					}
				}
				catch (InvalidOperationException e)
				{
					throw new FindException(e.Message);
				}
				if (string.IsNullOrWhiteSpace(s))
				{
					throw new FindException("Invalid option: -");
				}
				if (ArgDictionary.ContainsKey(s))
				{
					try
					{
						((FindArgOption)ArgDictionary[s]).Action(queue.Dequeue(), settings);
					}
					catch (InvalidOperationException e)
					{
						throw new FindException(e.Message);
					}
				}
				else if (FlagDictionary.ContainsKey(s))
				{
					try
					{
						((FindFlagOption)FlagDictionary[s]).Action(true, settings);
					}
					catch (InvalidOperationException e)
					{
						throw new FindException(e.Message);
					}
				}
				else
				{
					throw new FindException($"Invalid option: {s}");
				}
			}
			else
			{
				settings.Paths.Add(s);
			}
		}
		return settings;
	}

	public void Usage(int exitCode = 0)
	{
		Console.WriteLine(GetUsageString());
		Environment.Exit(exitCode);
	}

	public string GetUsageString()
	{
		var sb = new StringBuilder();
		sb.AppendLine("\nUsage:");
		sb.AppendLine(" csfind [options] <path> [<path> ...]\n");
		sb.AppendLine("Options:");
		var optStrings = new List<string>();
		var optDescs = new List<string>();
		var longest = 0;
		foreach (var opt in Options.OrderBy(o => o.SortArg))
		{
			var optString = new StringBuilder();
			if (!string.IsNullOrWhiteSpace(opt.ShortArg))
			{
				optString.Append($"-{opt.ShortArg},");
			}
			optString.Append($"--{opt.LongArg}");
			if (optString.Length > longest)
			{
				longest = optString.Length;
			}
			optStrings.Add(optString.ToString());
			optDescs.Add(opt.Description);
		}
		var format = " {0,-" + longest + "}  {1}";
		for (var i = 0; i < optStrings.Count; i++)
		{
			sb.AppendLine(string.Format(format, optStrings[i], optDescs[i]));
		}
		return sb.ToString();
	}
}
