using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Xml.Linq;

using FindOptionsDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string,string>>>;

namespace CsFind
{
	public class FindOptions
	{
		private readonly string _findOptionsResource;

		private static readonly Dictionary<string, Action<string, FindSettings>> ArgActionDictionary =
			new Dictionary<string, Action<string,FindSettings>>
				{
					{ "encoding", (s, settings) => settings.TextFileEncoding = s },
					{ "in-archiveext", (s, settings) => settings.AddInArchiveExtension(s) },
					{ "in-archivefilepattern", (s, settings) => settings.AddInArchiveFilePattern(s) },
					{ "in-dirpattern", (s, settings) => settings.AddInDirPattern(s) },
					{ "in-ext", (s, settings) => settings.AddInExtension(s) },
					{ "in-filepattern", (s, settings) => settings.AddInFilePattern(s) },
					{ "in-filetype", (s, settings) => settings.AddInFileType(s) },
					{ "in-linesafterpattern", (s, settings) => settings.AddInLinesAfterPattern(s) },
					{ "in-linesbeforepattern", (s, settings) => settings.AddInLinesBeforePattern(s) },
					{ "linesafter", (s, settings) => settings.LinesAfter = int.Parse(s) },
					{ "linesaftertopattern", (s, settings) => settings.AddLinesAfterToPattern(s) },
					{ "linesafteruntilpattern", (s, settings) => settings.AddLinesAfterUntilPattern(s) },
					{ "linesbefore", (s, settings) => settings.LinesBefore = int.Parse(s) },
					{ "maxlinelength", (s, settings) => settings.MaxLineLength = int.Parse(s) },
					{ "out-archiveext", (s, settings) => settings.AddOutArchiveExtension(s) },
					{ "out-archivefilepattern", (s, settings) => settings.AddOutArchiveFilePattern(s) },
					{ "out-dirpattern", (s, settings) => settings.AddOutDirPattern(s) },
					{ "out-ext", (s, settings) => settings.AddOutExtension(s) },
					{ "out-filepattern", (s, settings) => settings.AddOutFilePattern(s) },
					{ "out-filetype", (s, settings) => settings.AddOutFileType(s) },
					{ "out-linesafterpattern", (s, settings) => settings.AddOutLinesAfterPattern(s) },
					{ "out-linesbeforepattern", (s, settings) => settings.AddOutLinesBeforePattern(s) },
					{ "findpattern", (s, settings) => settings.AddFindPattern(s) },
					{ "settings-file", SettingsFromFile },
				};

		private static readonly Dictionary<string, Action<bool, FindSettings>> BoolFlagActionDictionary =
			new Dictionary<string, Action<bool, FindSettings>>
				{
					{ "allmatches", (b, settings) => settings.FirstMatch = !b },
					{ "archivesonly", (b, settings) => settings.ArchivesOnly = b },
					{ "colorize", (b, settings) => settings.Colorize = b },
					{ "debug", (b, settings) => settings.Debug = b },
					{ "excludehidden", (b, settings) => settings.ExcludeHidden = b },
					{ "firstmatch", (b, settings) => settings.FirstMatch = b },
					{ "help", (b, settings) => settings.PrintUsage = b },
					{ "includehidden", (b, settings) => settings.ExcludeHidden = !b },
					{ "listdirs", (b, settings) => settings.ListDirs = b },
					{ "listfiles", (b, settings) => settings.ListFiles = b },
					{ "listlines", (b, settings) => settings.ListLines = b },
					{ "multilineoption-REMOVE", (b, settings) => settings.MultiLineFind = b },
					{ "nocolorize", (b, settings) => settings.Colorize = !b },
					{ "noprintmatches", (b, settings) => settings.PrintResults = !b },
					{ "norecursive", (b, settings) => settings.Recursive = !b },
					{ "nofindarchives", (b, settings) => settings.FindArchives = !b },
					{ "printmatches", (b, settings) => settings.PrintResults = b },
					{ "recursive", (b, settings) => settings.Recursive = b },
					{ "findarchives", (b, settings) => settings.FindArchives = b },
					{ "uniquelines", (b, settings) => settings.UniqueLines = b },
					{ "verbose", (b, settings) => settings.Verbose = b },
					{ "version", (b, settings) => settings.PrintVersion = b },
				};

		public List<FindOption> Options { get; }
		public Dictionary<string, FindOption> ArgDictionary { get; }
		public Dictionary<string, FindOption> FlagDictionary { get; }

		public FindOptions()
		{
			// _findOptionsResource = EmbeddedResource.GetResourceFileContents("CsFind.Resources.findoptions.xml");
			_findOptionsResource = EmbeddedResource.GetResourceFileContents("CsFind.Resources.findoptions.json");
			Options = new List<FindOption>();
			ArgDictionary = new Dictionary<string, FindOption>();
			FlagDictionary = new Dictionary<string, FindOption>();
			// SetOptionsFromXml();
			SetOptionsFromJson();
		}

		private void SetOptionsFromJson()
		{
			var findOptionsDict = JsonSerializer.Deserialize<FindOptionsDictionary>(_findOptionsResource);
			var optionDicts = findOptionsDict["findoptions"];
			foreach (var optionDict in optionDicts)
			{
				var longArg = optionDict["long"];
				var shortArg = optionDict.ContainsKey("short") ? optionDict["short"] : null;
				var desc = optionDict["desc"];
				if (ArgActionDictionary.ContainsKey(longArg))
				{
					var option = new FindArgOption(shortArg, longArg, ArgActionDictionary[longArg], desc);
					Options.Add(option);
					ArgDictionary.Add(longArg, option);
					if (!string.IsNullOrWhiteSpace(shortArg))
					{
						ArgDictionary.Add(shortArg, option);
					}
				}
				else if (BoolFlagActionDictionary.ContainsKey(longArg))
				{
					var option = new FindFlagOption(shortArg, longArg, BoolFlagActionDictionary[longArg], desc);
					Options.Add(option);
					FlagDictionary.Add(longArg, option);
					if (!string.IsNullOrWhiteSpace(shortArg))
					{
						FlagDictionary.Add(shortArg, option);
					}
				}
			}
		}

		private void SetOptionsFromXml()
		{
			var doc = XDocument.Parse(_findOptionsResource);
			foreach (var f in doc.Descendants("findoption"))
			{
				var longArg = f.Attributes("long").First().Value;
				var shortArg = f.Attributes("short").First().Value;
				var desc = f.Value.Trim();
				if (ArgActionDictionary.ContainsKey(longArg))
				{
					var option = new FindArgOption(shortArg, longArg, ArgActionDictionary[longArg], desc);
					Options.Add(option);
					ArgDictionary.Add(longArg, option);
					if (!string.IsNullOrWhiteSpace(shortArg))
					{
						ArgDictionary.Add(shortArg, option);
					}
				}
				else if (BoolFlagActionDictionary.ContainsKey(longArg))
				{
					var option = new FindFlagOption(shortArg, longArg, BoolFlagActionDictionary[longArg], desc);
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
				throw new FindException("Settings fie not found: " + filePath);
			var contents = FileUtil.GetFileContents(filePath, Encoding.Default);
			SettingsFromJson(contents, settings);
		}

		public static void SettingsFromJson(string jsonString, FindSettings settings)
		{
			var settingsDict = JsonSerializer.Deserialize<Dictionary<string, object>>(jsonString);
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
					ApplySetting(arg, obj.GetString(), settings);
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
			else if (arg.Equals("startpath"))
			{
				settings.StartPath = val;
			}
			else
			{
				throw new FindException("Invalid option: " + arg);
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
				throw new FindException("Invalid option: " + arg);
			}
		}

		public FindSettings SettingsFromArgs(IEnumerable<string> args)
		{
			// default to PrintResults = true since this is called from CLI functionality
			var settings = new FindSettings {PrintResults = true};
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
						throw new FindException("Invalid option: " + s);
					}
				}
				else
				{
					settings.StartPath = s;
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
			sb.AppendLine(" csfind [options] -s <findpattern> <startpath>\n");
			sb.AppendLine("Options:");
			var optStrings = new List<string>();
			var optDescs = new List<string>();
			var longest = 0;
			foreach (var opt in Options.OrderBy(o => o.SortArg))
			{
				var optString = new StringBuilder();
				if (!string.IsNullOrWhiteSpace(opt.ShortArg))
				{
					optString.Append("-" + opt.ShortArg + ",");
				}
				optString.Append("--" + opt.LongArg);
				if (optString.Length > longest)
				{
					longest = optString.Length;
				}
				optStrings.Add(optString.ToString());
				optDescs.Add(opt.Description);
			}
			var format = " {0,-"+longest+"}  {1}";
			for (var i = 0; i < optStrings.Count; i++)
			{
				sb.AppendLine(string.Format(format, optStrings[i], optDescs[i]));
			}
			return sb.ToString();
		}
	}
}
