using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;

namespace CsFindLib;

public class ArgTokenizer
{
    private Dictionary<string, string> BoolDictionary { get; } =  new();
    private Dictionary<string, string> StringDictionary { get; } = new();
    private Dictionary<string, string> IntDictionary { get; }  = new();
    private Dictionary<string, string> LongDictionary { get; }  = new();
    
    private static readonly Regex NumModifierPattern = new(@"^\d+[ckmgtp]$", RegexOptions.Compiled | RegexOptions.IgnoreCase);


    public ArgTokenizer(List<IOption> options)
    {
	    foreach (var option in options)
	    {
		    if (option.ArgType == ArgTokenType.Bool)
		    {
			    BoolDictionary[option.LongArg] = option.LongArg;
			    if (option.ShortArg != null)
			    {
				    BoolDictionary[option.ShortArg] = option.LongArg;
			    }
		    }
		    else if (option.ArgType == ArgTokenType.String)
		    {
			    StringDictionary[option.LongArg] = option.LongArg;
			    if (option.ShortArg != null)
			    {
				    StringDictionary[option.ShortArg] = option.LongArg;
			    }
		    }
		    else if (option.ArgType == ArgTokenType.Int) 
		    {
			    IntDictionary[option.LongArg] = option.LongArg;
			    if (option.ShortArg != null)
			    {
				    IntDictionary[option.ShortArg] = option.LongArg;
			    }
		    }
		    else if (option.ArgType == ArgTokenType.Long)
		    {
			    LongDictionary[option.LongArg] = option.LongArg;
			    if (option.ShortArg != null)
			    {
				    LongDictionary[option.ShortArg] = option.LongArg;
			    }
		    }
	    }
    }

    public ArgToken TokenizeSizeArg(string argName, string argVal)
    {
	    long lng;
	    try
	    {
		    lng = long.Parse(argVal);
	    }
	    catch (FormatException)
	    {
		    if (NumModifierPattern.IsMatch(argVal))
		    {
			    lng = argVal[..^1] switch
			    {
				    var s when int.TryParse(s, out var num) => num,
				    _ => throw new FindException($"Invalid value for option {argName}: {argVal}")
			    };
			    var modifier = argVal.ToLowerInvariant()[^1];
			    lng *= modifier switch
			    {
				    'c' => 1,
				    'k' => 1024,
				    'm' => 1024 * 1024,
				    'g' => 1024 * 1024 * 1024,
				    't' => 1024L * 1024 * 1024 * 1024,
				    'p' => 1024L * 1024 * 1024 * 1024 * 1024,
				    _ => throw new FindException($"Invalid value for option {argName}: {argVal}")
			    };
		    }
		    else
		    {
			    throw new FindException($"Invalid value for option {argName}: {argVal}");
		    }
	    }
	    return new ArgToken(argName, ArgTokenType.Long, lng);
    }

    public ArgToken TokenizeLongArg(string argName, string argVal)
    {
	    if (argName is "maxsize" or "minsize")
	    {
		    return TokenizeSizeArg(argName, argVal);
	    }
	    long lng;
	    try
	    {
		    lng = long.Parse(argVal);
	    }
	    catch (FormatException)
	    {
		    throw new FindException($"Invalid value for option {argName}: {argVal}");
	    }
	    return new ArgToken(argName, ArgTokenType.Long, lng);
    }
    
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
						else if (LongDictionary.TryGetValue(c.ToString(), out var longArg))
						{
							argNames.Add(longArg);
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
					         || IntDictionary.ContainsKey(argName)
					         || LongDictionary.ContainsKey(argName)
					         || argName.Equals("settings-file"))
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
							else if (LongDictionary.ContainsKey(argName))
							{
								argTokens.Add(TokenizeLongArg(argName, argVal));
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
					throw new FindException($"Invalid value for option {key}: {val}");
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
						throw new FindException($"Invalid value for option {key}: {val}");
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
								throw new FindException($"Invalid value for option {key}: {val}");
							}
						}
					}
				}
				else
				{
					throw new FindException($"Invalid value for option {key}: {val}");
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
					throw new FindException($"Invalid value for option {key}: {val}");
				}
			}
			else if (LongDictionary.ContainsKey(key))
			{
				if (val is long lng)
				{
					argTokens.Add(new ArgToken(key, ArgTokenType.Long, lng));
				}
				else if (val is JsonElement { ValueKind: JsonValueKind.Number } jsonNum)
				{
					argTokens.Add(new ArgToken(key, ArgTokenType.Long, jsonNum.GetInt64()));
				}
				else
				{
					throw new FindException($"Invalid value for option {key}: {val}");
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
