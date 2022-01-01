using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using FileTypesDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string, object>>>;

namespace CsFindGen;

[Generator]
public class FileTypesGenerator : ISourceGenerator
{
	public void Initialize(GeneratorInitializationContext context)
	{
	}

	public void Execute(GeneratorExecutionContext context)
	{
		try
		{
			var fileTypesFile = context.AdditionalFiles.First(a => a.Path.EndsWith("filetypes.json"));
			GenerateFileTypes(fileTypesFile, context);
		}
		catch (Exception e)
		{
			context.ReportDiagnostic(
				Diagnostic.Create(
					new DiagnosticDescriptor("CSFGEN", "GeneratorError", "Error when generating source: {0}", "CsFindGen.Execute", DiagnosticSeverity.Error, true),
					Location.Create("filetypes.json", new TextSpan(), new LinePositionSpan()),
					e.Message));
		}
	}

	private void GenerateFileTypes(AdditionalText fileTypesFile, GeneratorExecutionContext context)
	{
		var filetypesDict = JsonSerializer.Deserialize<FileTypesDictionary>(fileTypesFile.GetText()!.ToString());
		IDictionary<string, ISet<string>> fileTypesDictionary = new Dictionary<string, ISet<string>>();
		if (filetypesDict!.ContainsKey("filetypes"))
		{
			var filetypeDicts = filetypesDict["filetypes"];
			foreach (var filetypeDict in filetypeDicts)
			{
				if (filetypeDict.ContainsKey("type") && filetypeDict.ContainsKey("extensions"))
				{
					var name = ((JsonElement)filetypeDict["type"]).GetString();
					var extensions = ((JsonElement)filetypeDict["extensions"]).EnumerateArray()
						.Select(x => "." + x.GetString());
					var extensionSet = new HashSet<string>(extensions);
					fileTypesDictionary[name!] = extensionSet;
				}
			}
		}

		// source opener
		var sourceBuilder = new StringBuilder(@"
using System;
using System.Collections.Generic;

namespace CsFindLib;

public partial class FileTypes
{
	partial void PopulateFileTypes() 
	{
");
		var depth = 2;
		var indent = new string('\t', depth);

		foreach (var typeName in fileTypesDictionary.Keys)
		{
			var extensionsString = string.Join("\",\"", fileTypesDictionary[typeName]);
			sourceBuilder.AppendLine($@"{indent}_fileTypesDictionary[""{typeName}""] = new HashSet<string> {{ ""{extensionsString}"" }};");
		}

		// source close
		sourceBuilder.Append(@"    }
}");

		Console.WriteLine($"sourceBuilder:\n${sourceBuilder}");
		// inject the created source into the users compilation
		context.AddSource("FileTypes.types.cs", SourceText.From(sourceBuilder.ToString(), Encoding.UTF8));

	}
}
