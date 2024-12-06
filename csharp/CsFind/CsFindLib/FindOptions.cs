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

	private static readonly Dictionary<string, Action<bool, FindSettings>> BoolActionDictionary =
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

	private static readonly Dictionary<string, Action<string, FindSettings>> StringActionDictionary =
		new()
		{
			{ "in-archiveext", (s, settings) => settings.AddInArchiveExtension(s) },
			{ "in-archivefilepattern", (s, settings) => settings.AddInArchiveFilePattern(s) },
			{ "in-dirpattern", (s, settings) => settings.AddInDirPattern(s) },
			{ "in-ext", (s, settings) => settings.AddInExtension(s) },
			{ "in-filepattern", (s, settings) => settings.AddInFilePattern(s) },
			{ "in-filetype", (s, settings) => settings.AddInFileType(s) },
			{ "maxlastmod", (s, settings) => {
					settings.MaxLastMod = DateTime.Parse(s);
				}
			},
			{ "minlastmod", (s, settings) => {
					settings.MinLastMod = DateTime.Parse(s);
				}
			},
			{ "out-archiveext", (s, settings) => settings.AddOutArchiveExtension(s) },
			{ "out-archivefilepattern", (s, settings) => settings.AddOutArchiveFilePattern(s) },
			{ "out-dirpattern", (s, settings) => settings.AddOutDirPattern(s) },
			{ "out-ext", (s, settings) => settings.AddOutExtension(s) },
			{ "out-filepattern", (s, settings) => settings.AddOutFilePattern(s) },
			{ "out-filetype", (s, settings) => settings.AddOutFileType(s) },
			{ "path", (s, settings) => settings.AddPath(s) },
			{ "settings-file", SettingsFromFile },
			{ "sort-by", (s, settings) => settings.SetSortBy(s) },
		};

	private static readonly Dictionary<string, Action<int, FindSettings>> IntActionDictionary =
		new()
		{
			{ "maxdepth", (i, settings) => settings.MaxDepth = i },
			{ "maxsize", (i, settings) => settings.MaxSize = i },
			{ "mindepth", (i, settings) => settings.MinDepth = i },
			{ "minsize", (i, settings) => settings.MinSize = i },
		};

	public List<FindOption> Options { get; }
	private Dictionary<string, string> LongArgDictionary { get; }

	public FindOptions()
	{
		_findOptionsResource = EmbeddedResource.GetResourceFileContents("CsFindLib.Resources.findoptions.json");
		Options = [];
		LongArgDictionary = new Dictionary<string, string>();
		SetOptionsFromJson();
	}

	private void SetOptionsFromJson()
	{
		var findOptionsDict = JsonSerializer.Deserialize<FindOptionsDictionary>(_findOptionsResource);
		if (findOptionsDict == null
		    || !findOptionsDict.TryGetValue("findoptions", out List<Dictionary<string, string>>? optionDicts))
		{
			throw new FindException("Missing or invalid search options resource");
		}

		foreach (var optionDict in optionDicts)
		{
			var longArg = optionDict["long"];
			LongArgDictionary.Add(longArg, longArg);
			string? shortArg = null;
			if (optionDict.TryGetValue("short", out var shortVal))
			{
				shortArg = shortVal;
				LongArgDictionary.Add(shortArg, longArg);
			}
			var desc = optionDict["desc"];
			var option = new FindOption(shortArg, longArg, desc);
			Options.Add(option);
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
		if (settingsDict == null) return;
		foreach (var (key, value) in settingsDict)
		{
			var obj = (JsonElement)value;
			ApplySetting(key, obj, settings);
		}
	}

	private static void ApplySetting(string arg, JsonElement obj, FindSettings settings)
	{
		switch (obj.ValueKind)
		{
			case JsonValueKind.String:
				var s = obj.GetString();
				if (s != null)
				{
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
				ApplySetting(arg, obj.GetInt32(), settings);
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

	private static void ApplySetting(string arg, bool val, FindSettings settings)
	{
		if (BoolActionDictionary.TryGetValue(arg, out var action))
		{
			action(val, settings);
		}
		else
		{
			throw new FindException($"Invalid option: {arg}");
		}
	}

	private static void ApplySetting(string arg, string val, FindSettings settings)
	{
		if (StringActionDictionary.TryGetValue(arg, out var action))
		{
			action(val, settings);
		}
		else
		{
			throw new FindException($"Invalid option: {arg}");
		}
	}

	private static void ApplySetting(string arg, int val, FindSettings settings)
	{
		if (IntActionDictionary.TryGetValue(arg, out var action))
		{
			action(val, settings);
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
			var arg = queue.Dequeue();
			if (arg.StartsWith('-'))
			{
				try
				{
					while (arg.StartsWith('-'))
					{
						arg = arg[1..];
					}
				}
				catch (InvalidOperationException e)
				{
					throw new FindException(e.Message);
				}
				if (string.IsNullOrWhiteSpace(arg))
				{
					throw new FindException("Invalid option: -");
				}

				var longArg = LongArgDictionary.GetValueOrDefault(arg, arg);
				if (BoolActionDictionary.TryGetValue(longArg, out var boolAction))
				{
					boolAction(true, settings);
				}
				else if (StringActionDictionary.TryGetValue(longArg, out var stringAction))
				{
					try
					{
						stringAction(queue.Dequeue(), settings);
					}
					catch (InvalidOperationException)
					{
						throw new FindException($"Missing value for option {arg}");
					}
				}
				else if (IntActionDictionary.TryGetValue(longArg, out var intAction))
				{
					try
					{
						intAction(int.Parse(queue.Dequeue()), settings);
					}
					catch (InvalidOperationException)
					{
						throw new FindException($"Missing value for option {arg}");
					}
				}
				else
				{
					throw new FindException($"Invalid option: {arg}");
				}
			}
			else
			{
				settings.AddPath(arg);
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
