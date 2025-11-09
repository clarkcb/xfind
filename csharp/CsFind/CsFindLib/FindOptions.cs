using System;
using System.Collections.Generic;
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
			{ "colorize", (b, settings) => settings.Colorize = b },
			{ "debug", (b, settings) => settings.Debug = b },
			{ "excludearchives", (b, settings) => settings.IncludeArchives = !b },
			{ "excludehidden", (b, settings) => settings.IncludeHidden = !b },
			{ "followsymlinks", (b, settings) => settings.FollowSymlinks = b },
			{ "help", (b, settings) => settings.PrintUsage = b },
			{ "includearchives", (b, settings) => settings.IncludeArchives = b },
			{ "includehidden", (b, settings) => settings.IncludeHidden = b },
			{ "nocolorize", (b, settings) => settings.Colorize = !b },
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

	public List<IOption> Options { get; }
	private Dictionary<string, string> BoolDictionary { get; }
	private Dictionary<string, string> StringDictionary { get; }
	private Dictionary<string, string> IntDictionary { get; }
	private ArgTokenizer ArgTokenizer { get; }


	public FindOptions()
	{
		_findOptionsResource = EmbeddedResource.GetResourceFileContents("CsFindLib.Resources.findoptions.json");
		Options = [];
		BoolDictionary = new Dictionary<string, string>();
		StringDictionary = new Dictionary<string, string> { { "path", "path" } };
		IntDictionary = new Dictionary<string, string>();
		SetOptionsFromJson();
		ArgTokenizer = new ArgTokenizer(Options);
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
			ArgTokenType argType;
			if (BoolActionDictionary.ContainsKey(longArg))
			{
				argType = ArgTokenType.Bool;
			}
			else if (StringActionDictionary.ContainsKey(longArg) || longArg.Equals("settings-file"))
			{
				argType = ArgTokenType.String;
			}
			else if (IntActionDictionary.ContainsKey(longArg))
			{
				argType = ArgTokenType.Int;
			}
			else
			{
				throw new FindException($"Invalid option: {longArg}");
			}
			string? shortArg = null;
			if (optionDict.TryGetValue("short", out var shortVal))
			{
				shortArg = shortVal;
			}
			var desc = optionDict["desc"];
			var option = new FindOption(shortArg, longArg, desc, argType);
			Options.Add(option);
		}
	}

	private void ApplyArgTokenToSettings(ArgToken argToken, FindSettings settings)
	{
		if (argToken.Type == ArgTokenType.Bool)
		{
			if (BoolActionDictionary.TryGetValue(argToken.Name, out var boolAction))
			{
				if (argToken.Value is bool b)
				{
					boolAction(b, settings);
				}
				else if (argToken.Value is JsonElement { ValueKind: JsonValueKind.False or JsonValueKind.True } jsonElem)
				{
					boolAction(jsonElem.GetBoolean(), settings);
				}
				else
				{
					throw new FindException($"Invalid value for option: {argToken.Name}");
				}
			}
			else
			{
				throw new FindException($"Invalid option: {argToken.Name}");
			}
		}
		else if (argToken.Type == ArgTokenType.String)
		{
			if (StringActionDictionary.TryGetValue(argToken.Name, out var stringAction))
			{
				if (argToken.Value is string s)
				{
					stringAction(s, settings);
				}
				else if (argToken.Value is IEnumerable<string> enumerable)
				{
					foreach (var enumVal in enumerable)
					{
						stringAction(enumVal, settings);
					}
				}
				else if (argToken.Value is JsonElement { ValueKind: JsonValueKind.String }  jsonElem)
				{
					var jsonString = jsonElem.GetString();
					if (jsonString is not null)
					{
						stringAction(jsonString, settings);
					}
					else
					{
						throw new FindException($"Invalid value for option: {argToken.Name}");
					}
				}
				else if (argToken.Value is JsonElement { ValueKind: JsonValueKind.Array }  jsonArr)
				{
					foreach (var arrVal in jsonArr.EnumerateArray())
					{
						if (arrVal is JsonElement { ValueKind: JsonValueKind.String }  jsonArrElem)
						{
							var jsonStr = jsonArrElem.GetString();
							if (jsonStr is not null)
							{
								stringAction(jsonStr, settings);
							}
							else
							{
								throw new FindException($"Invalid value for option: {argToken.Name}");
							}
						}
					}
				}
				else
				{
					throw new FindException($"Invalid value for option: {argToken.Name}");
				}
			}
			else if (argToken.Name.Equals("settings-file"))
			{
				if (argToken.Value is string settingsFilePath)
				{
					UpdateSettingsFromFile(settings, settingsFilePath);
				}
				else if (argToken.Value is IEnumerable<string> enumerable)
				{
					foreach (var enumVal in enumerable)
					{
						UpdateSettingsFromFile(settings, enumVal);
					}
				}
				else
				{
					throw new FindException($"Invalid value for option: {argToken.Name}");
				}
			}
			else
			{
				throw new FindException($"Invalid option: {argToken.Name}");
			}
		}
		else if (argToken.Type == ArgTokenType.Int)
		{
			if (IntActionDictionary.TryGetValue(argToken.Name, out var intAction))
			{
				if (argToken.Value is int i)
				{
					intAction(i, settings);
				}
				else if (argToken.Value is JsonElement { ValueKind: JsonValueKind.Number } jsonElem)
				{
					intAction(jsonElem.GetInt32(), settings);
				}
				else
				{
					throw new FindException($"Invalid value for option: {argToken.Name}");
				}
			}
		}
		else
		{
			throw new FindException($"Invalid option: {argToken.Name}");
		}
	}

	private void UpdateSettingsFromArgTokens(FindSettings settings, List<ArgToken> argTokens)
	{
		foreach (var argToken in argTokens)
		{
			ApplyArgTokenToSettings(argToken, settings);
		}
	}

	private void UpdateSettingsFromDictionary(FindSettings settings, Dictionary<string, object> dictionary)
	{
		var argTokens = ArgTokenizer.TokenizeDictionary(dictionary);
		UpdateSettingsFromArgTokens(settings, argTokens);
	}

	public void UpdateSettingsFromJson(FindSettings settings, string jsonString)
	{
		var argTokens = ArgTokenizer.TokenizeJson(jsonString);
		UpdateSettingsFromArgTokens(settings, argTokens);
	}

	public FindSettings SettingsFromJson(string jsonString)
	{
		var settings = new FindSettings();
		UpdateSettingsFromJson(settings, jsonString);
		return settings;
	}

	public void UpdateSettingsFromFile(FindSettings settings, string filePath)
	{
		var argTokens = ArgTokenizer.TokenizeFile(filePath);
		UpdateSettingsFromArgTokens(settings, argTokens);
	}

	public FindSettings SettingsFromFile(string filePath)
	{
		var settings = new FindSettings();
		UpdateSettingsFromFile(settings, filePath);
		return settings;
	}

	public void UpdateSettingsFromArgs(FindSettings settings, IEnumerable<string> args)
	{
		var argTokens = ArgTokenizer.TokenizeArgs(args);
		UpdateSettingsFromArgTokens(settings, argTokens);
	}

	public FindSettings SettingsFromArgs(IEnumerable<string> args)
	{
		// default to PrintFiles = true since this is called from CLI
		var settings = new FindSettings {PrintFiles = true};
		UpdateSettingsFromArgs(settings, args);
		return settings;
	}

	private string GetUsageString()
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

	public void Usage(int exitCode = 0)
	{
		Console.WriteLine(GetUsageString());
		Environment.Exit(exitCode);
	}
}
