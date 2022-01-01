using System;
using System.Linq;
using System.Text;
using System.Text.Json;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using FindOptionsDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string, string>>>;

namespace CsFindGen;

[Generator]
public class OptionsGenerator : ISourceGenerator
{
	public void Initialize(GeneratorInitializationContext context)
	{
	}

	public void Execute(GeneratorExecutionContext context)
	{
		try
		{
			var optionsFile = context.AdditionalFiles.First(a => a.Path.EndsWith("findoptions.json"));
			GenerateOptions(optionsFile, context);
		}
		catch (Exception e)
		{
			context.ReportDiagnostic(
				Diagnostic.Create(
					new DiagnosticDescriptor("CSFGEN", "GeneratorError", "Error when generating source: {0}", "CsFindGen.Execute", DiagnosticSeverity.Error, true),
					Location.Create("findoptions.json", new TextSpan(), new LinePositionSpan()),
					e.Message));
		}
	}

	private void GenerateOptions(AdditionalText optionsFile, GeneratorExecutionContext context)
	{
		var findOptionsDict = JsonSerializer.Deserialize<FindOptionsDictionary>(optionsFile.GetText()!.ToString());
		var optionDicts = findOptionsDict!["findoptions"];

		// source opener
		var sourceBuilder = new StringBuilder(@"
using System;
namespace CsFindLib;

public partial class FindOptions
{
    partial void SetOptions() 
    {
");
		var depth = 2;
		var indent = new string('\t', depth);
		foreach (var optionDict in optionDicts)
		{
			var longArg = optionDict["long"];
			var shortArg = optionDict.ContainsKey("short") ? optionDict["short"] : null;
			var desc = optionDict["desc"];
			sourceBuilder.AppendLine(shortArg == null
				? $@"{indent}Options.Add(new FindOption(null, ""{longArg}"", ""{desc}""));"
				: $@"{indent}Options.Add(new FindOption(""{shortArg}"", ""{longArg}"", ""{desc}""));");
		}

		// source close
		sourceBuilder.Append(@"    }
}");

		Console.WriteLine($"sourceBuilder:\n${sourceBuilder}");
		// inject source into compilation
		context.AddSource("FindOptions.options.cs", SourceText.From(sourceBuilder.ToString(), Encoding.UTF8));
	}
}
