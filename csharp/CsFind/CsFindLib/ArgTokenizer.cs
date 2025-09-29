using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace CsFindLib;

public class ArgTokenizer(
	Dictionary<string, string> boolDict,
	Dictionary<string, string> stringDict,
	Dictionary<string, string> intDict)
{
    private Dictionary<string, string> BoolDictionary { get; } = boolDict;
    private Dictionary<string, string> StringDictionary { get; } = stringDict;
    private Dictionary<string, string> IntDictionary { get; } = intDict;

    public List<ArgToken> TokenizeArgs(IEnumerable<string> args)
	{
		var argTokens = new List<ArgToken>();
		var queue = new Queue<string>(args);

		while (queue.Count > 0)
		{
			var arg = queue.Dequeue();
			if (arg.StartsWith('-'))
			{
				var argNames = new List<string>();
				if (arg.StartsWith("--") && arg.Length > 2)
				{
					// Process long arg
					arg = arg[2..];
					// TODO: if = in arg, split
					if (arg.Contains('='))
					{
						var parts = arg.Split('=');
						if (parts.Length > 0 && !string.IsNullOrEmpty(parts[0]))
						{
							arg = parts[0];
						}
						if (parts.Length > 1 && !string.IsNullOrEmpty(parts[1]))
						{
							queue = new Queue<string>(queue.Prepend(parts[1]));
						}
					}
					argNames.Add(arg);
				}
				else if (arg.Length > 1)
				{
					// Process short arg(s)
					arg = arg[1..];
					foreach (var c in arg)
					{
						if (BoolDictionary.TryGetValue(c.ToString(), out var boolArg))
						{
							argNames.Add(boolArg);
						}
						else if (StringDictionary.TryGetValue(c.ToString(), out var stringArg))
						{
							argNames.Add(stringArg);
						}
						else if (IntDictionary.TryGetValue(c.ToString(), out var intArg))
						{
							argNames.Add(intArg);
						}
						else
						{
							throw new FindException($"Invalid option: {c}");
						}
					}
				}

				foreach (var argName in argNames)
				{
					if (BoolDictionary.ContainsKey(argName))
					{
						argTokens.Add(new ArgToken(argName, ArgTokenType.Bool, true));
					}
					else if (StringDictionary.ContainsKey(argName)
					         || IntDictionary.ContainsKey(argName) ||
					         argName.Equals("settings-file"))
					{
						try
						{
							var argVal = queue.Dequeue();
							if (StringDictionary.ContainsKey(argName) || argName.Equals("settings-file"))
							{
								argTokens.Add(new ArgToken(argName, ArgTokenType.String, argVal));
							}
							else if (IntDictionary.ContainsKey(argName))
							{
								argTokens.Add(new ArgToken(argName, ArgTokenType.Int, int.Parse(argVal)));
							}
							else
							{
								throw new FindException($"Invalid option: {arg}");
							}
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
			}
			else
			{
				argTokens.Add(new ArgToken("path", ArgTokenType.String, arg));
			}
		}
		return argTokens;
	}

	public List<ArgToken> TokenizeDictionary(Dictionary<string, object> dictionary)
	{
		var keys = dictionary.Keys.ToList();
		// keys are sorted so that output is consistent across all versions
		keys.Sort();
		var argTokens = new List<ArgToken>();
		foreach (var key in keys)
		{
			var val = dictionary[key];
			if (BoolDictionary.ContainsKey(key))
			{
				if (val is bool b)
				{
					argTokens.Add(new ArgToken(key, ArgTokenType.Bool, b));
				}
				else if (val is JsonElement { ValueKind: JsonValueKind.False or JsonValueKind.True } jsonBool)
				{
					argTokens.Add(new ArgToken(key, ArgTokenType.Bool, jsonBool.GetBoolean()));
				}
				else
				{
					throw new FindException($"Invalid value for option: {key}");
				}
			}
			else if (StringDictionary.ContainsKey(key))
			{
				if (val is string s)
				{
					argTokens.Add(new ArgToken(key, ArgTokenType.String, s));
				}
				else if (val is IEnumerable<string> enumerable)
				{
					foreach (var enumStr in enumerable)
					{
						argTokens.Add(new ArgToken(key, ArgTokenType.String, enumStr));
					}
				}
				else if (val is JsonElement { ValueKind: JsonValueKind.String }  jsonElem)
				{
					var jsonStr = jsonElem.GetString();
					if (jsonStr is not null)
					{
						argTokens.Add(new ArgToken(key, ArgTokenType.String, jsonStr));
					}
					else
					{
						throw new FindException($"Invalid value for option: {key}");
					}
				}
				else if (val is JsonElement { ValueKind: JsonValueKind.Array }  jsonArr)
				{
					foreach (var arrVal in jsonArr.EnumerateArray())
					{
						if (arrVal is JsonElement { ValueKind: JsonValueKind.String }  arrStr)
						{
							var jsonStr = arrStr.GetString();
							if (jsonStr is not null)
							{
								argTokens.Add(new ArgToken(key, ArgTokenType.String, jsonStr));
							}
							else
							{
								throw new FindException($"Invalid value for option: {key}");
							}
						}
					}
				}
				else
				{
					throw new FindException($"Invalid value for option: {key}");
				}
			}
			else if (IntDictionary.ContainsKey(key))
			{
				if (val is int i)
				{
					argTokens.Add(new ArgToken(key, ArgTokenType.Int, i));
				}
				else if (val is JsonElement { ValueKind: JsonValueKind.Number } jsonNum)
				{
					argTokens.Add(new ArgToken(key, ArgTokenType.Int, jsonNum.GetInt32()));
				}
				else
				{
					throw new FindException($"Invalid value for option: {key}");
				}
			}
			else
			{
				throw new FindException($"Invalid option: {key}");
			}
		}
		return argTokens;
	}

	public List<ArgToken> TokenizeJson(string jsonString)
	{
		try
		{
			var settingsDict = JsonSerializer.Deserialize<Dictionary<string, object>>(jsonString);
			if (settingsDict == null)
			{
				throw new FindException($"Unable to parse JSON");
			}
			return TokenizeDictionary(settingsDict);
		}
		catch (JsonException e)
		{
			throw new FindException($"Invalid JSON: {e.Message}");
		}
	}

	public List<ArgToken> TokenizeFile(string filePath)
	{
		var expandedPath = FileUtil.ExpandPath(filePath);
		var fileInfo = new FileInfo(expandedPath);
		if (!fileInfo.Exists)
			throw new FindException($"Settings file not found: {filePath}");
		if (!fileInfo.Extension.Equals(".json"))
			throw new FindException($"Invalid settings file (must be JSON): {filePath}");
		var contents = FileUtil.GetFileContents(expandedPath, Encoding.Default);
		return TokenizeJson(contents);
	}
}
