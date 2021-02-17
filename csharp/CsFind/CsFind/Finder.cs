using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace CsFind
{
	public class Finder
	{
		private readonly FileTypes _fileTypes;
		private FindSettings Settings { get; set; }
		private ConcurrentBag<FindResult> Results { get; set; }
		private Encoding TextFileEncoding { get; set; } = Encoding.Default;

		// TODO: move this to FindSettings
		private const int FileBatchSize = 255;
		// use single-byte encoding for reading binary files (will break if UTF8)
		private readonly Encoding _binaryEncoding = Encoding.GetEncoding("ISO-8859-1");

		public Finder(FindSettings settings)
		{
			Settings = settings;
			ValidateSettings();
			_fileTypes = new FileTypes();
			Results = new ConcurrentBag<FindResult>();
		}

		private void ValidateSettings()
		{
			if (string.IsNullOrEmpty(Settings.StartPath))
				throw new FindException("Startpath not defined");
			var expandedPath = FileUtil.ExpandPath(Settings.StartPath);
			if (!Directory.Exists(expandedPath) && !File.Exists(expandedPath))
			{
				throw new FindException("Startpath not found");
			}
			if (Settings.FindPatterns.Count < 1)
				throw new FindException("No find patterns defined");
			try
			{
				TextFileEncoding = Encoding.GetEncoding(Settings.TextFileEncoding);
			}
			catch (ArgumentException)
			{
				throw new FindException("Invalid encoding");
			}
			if (Settings.LinesBefore < 0)
				throw new FindException("Invalid linesbefore");
			if (Settings.LinesAfter < 0)
				throw new FindException("Invalid linesafter");
			if (Settings.MaxLineLength < 0)
				throw new FindException("Invalid maxlinelength");
		}

		public bool IsFindDirectory(DirectoryInfo d)
		{
			if (FileUtil.IsDotDir(d.Name))
				return true;
			if (Settings.ExcludeHidden && FileUtil.IsHidden(d))
				return false;
			return (Settings.InDirPatterns.Count == 0 ||
			        Settings.InDirPatterns.Any(p => p.Matches(d.FullName).Count > 0)) &&
			       (Settings.OutDirPatterns.Count == 0 ||
			        !Settings.OutDirPatterns.Any(p => p.Matches(d.FullName).Count > 0));
		}

		public bool IsFindFile(FindFile sf)
		{
			return (Settings.InExtensions.Count == 0 ||
			        Settings.InExtensions.Contains(sf.File.Extension)) &&
			       (Settings.OutExtensions.Count == 0 ||
			        !Settings.OutExtensions.Contains(sf.File.Extension)) &&
			       (Settings.InFilePatterns.Count == 0 ||
			        Settings.InFilePatterns.Any(p => p.Match(sf.File.Name).Success)) &&
			       (Settings.OutFilePatterns.Count == 0 ||
			        !Settings.OutFilePatterns.Any(p => p.Match(sf.File.Name).Success));
		}

		public bool IsArchiveFindFile(FindFile sf)
		{
			return (Settings.InArchiveExtensions.Count == 0 ||
			        Settings.InArchiveExtensions.Contains(sf.File.Extension)) &&
			       (Settings.OutArchiveExtensions.Count == 0 ||
			        !Settings.OutArchiveExtensions.Contains(sf.File.Extension)) &&
			       (Settings.InArchiveFilePatterns.Count == 0 ||
			        Settings.InArchiveFilePatterns.Any(p => p.Match(sf.File.Name).Success)) &&
			       (Settings.OutArchiveFilePatterns.Count == 0 ||
			        !Settings.OutArchiveFilePatterns.Any(p => p.Match(sf.File.Name).Success));
		}

		public bool FilterFile(FindFile sf)
		{
			if (FileUtil.IsHidden(sf.File) && Settings.ExcludeHidden)
				return false;
			if (sf.Type.Equals(FileType.Archive))
			{
				return (Settings.FindArchives && IsArchiveFindFile(sf));
			}
			return (!Settings.ArchivesOnly && IsFindFile(sf));
		}

		private IEnumerable<FindFile> GetFindFiles()
		{
			var expandedPath = FileUtil.ExpandPath(Settings.StartPath!);
			var findOption = Settings.Recursive ? System.IO.FindOption.AllDirectories :
				System.IO.FindOption.TopDirectoryOnly;
			return new DirectoryInfo(expandedPath).
				EnumerateFiles("*", findOption).
				Where(f => IsFindDirectory(f.Directory)).
				Select(f => new FindFile(f, _fileTypes.GetFileType(f))).
				Where(FilterFile).
				Select(sf => sf);
		}

		public void Find()
		{
			var expandedPath = FileUtil.ExpandPath(Settings.StartPath!);
			if (Directory.Exists(expandedPath))
			{
				var startDir = new DirectoryInfo(expandedPath);
				if (IsFindDirectory(startDir))
				{
					FindPath(startDir);
				}
				else
				{
					throw new FindException("Startpath does not match find settings");
				}
			}
			else
			{
				var f = new FileInfo(expandedPath);
				var sf = new FindFile(f, _fileTypes.GetFileType(f));
				if (FilterFile(sf))
				{
					FindFile(sf);
				}
				else
				{
					throw new FindException("Startpath does not match find settings");
				}
			}
		}

		public void FindPath(DirectoryInfo path)
		{
			var findFiles = GetFindFiles().ToList();
			if (Settings.Verbose)
			{
				findFiles.Sort(new FindFilesComparer());

				var findDirs = findFiles
					.Where(sf => sf.File.Directory != null)
					.Select(sf => sf.File.Directory.ToString())
					.Distinct()
					.OrderBy(d => d).ToArray();
				Common.Log($"Directories to be found ({findDirs.Length}):");
				foreach (var d in findDirs)
				{
					Common.Log(FileUtil.ContractOrRelativePath(d, Settings.StartPath!));
				}
				
				Common.Log($"\nFiles to be found ({findFiles.Count}):");
				foreach (var f in findFiles)
				{
					Common.Log(FileUtil.ContractOrRelativePath(f.FullName, Settings.StartPath!));
				}
			}

			var found = 0;
			while (findFiles.Count - found > FileBatchSize)
			{
				FindBatch(findFiles.Skip(found).Take(FileBatchSize).ToArray());
				found += FileBatchSize;
			}
			if (findFiles.Count > found)
			{
				FindBatch(findFiles.Skip(found).ToArray());
			}
		}

		private void FindBatch(IReadOnlyList<FindFile> findFiles)
		{
			if (findFiles.Count > 100)
			{
				FindBatchConcurrent(findFiles);
			}
			else
			{
				foreach (var f in findFiles)
				{
					FindFile(f);
				}
			}
		}

		private void FindBatchConcurrent(IReadOnlyList<FindFile> findFiles)
		{
			var findTasks = new Task[findFiles.Count];
			for (var i = 0; i < findFiles.Count; i++)
			{
				var findFile = findFiles[i];
				findTasks[i] = Task.Factory.StartNew(() => FindFile(findFile));
			}
			Task.WaitAll(findTasks);
		}

		public void FindFile(FindFile f)
		{
			switch (f.Type)
			{
				case FileType.Code:
				case FileType.Text:
				case FileType.Xml:
					FindTextFile(f);
					break;
				case FileType.Binary:
					FindBinaryFile(f);
					break;
				case FileType.Archive:
					Common.Log($"Skipping archive file {FileUtil.ContractPath(f.FullName)}");
					break;
				default:
				{
					if (Settings.Verbose)
					{
						Common.Log($"Skipping file {FileUtil.ContractPath(f.FullName)}");
					}

					break;
				}
			}
		}

		private void FindTextFile(FindFile f)
		{
			if (Settings.Debug)
				Common.Log($"Finding text file {FileUtil.ContractOrRelativePath(f.FullName, Settings.StartPath!)}");
			if (Settings.MultiLineFind)
				FindTextFileContents(f);
			else
				FindTextFileLines(f);
		}

		private void FindTextFileContents(FindFile f)
		{
			try
			{
				var contents = FileUtil.GetFileContents(f, TextFileEncoding);
				var results = FindContents(contents);
				foreach (var r in results)
				{
					r.File = f;
					AddFindResult(r);
				}
			}
			catch (IOException e)
			{
				Common.Log(e.Message);
			}
		}

		private static IEnumerable<int> GetNewLineIndices(string text)
		{
			// to keep it simple, only get indices of '\n' and deal with '\r' later
			IList<int> newLineIndices = new List<int>();
			for (var i = 0; i < text.Length; i++)
			{
				if (text[i] == '\n')
					newLineIndices.Add(i);
			}
			return newLineIndices;
		}

		private static IEnumerable<int> GetStartLineIndices(IEnumerable<int> newLineIndices)
		{
			var startLineIndices = new List<int> {0};
			startLineIndices.AddRange(newLineIndices.Select(i => i + 1));
			return startLineIndices;
		}

		private static IEnumerable<int> GetEndLineIndices(string text,
			IEnumerable<int> newLineIndices)
		{
			var endLineIndices = new List<int>(newLineIndices) {text.Length - 1};
			return endLineIndices;
		}

		private IEnumerable<string> GetLinesBeforeFromContents(string content,
			IEnumerable<int> beforeStartIndices, IEnumerable<int> beforeEndIndices)
		{
			var linesBefore = new List<string>();
			if (Settings.LinesBefore == 0) return linesBefore;
			var starts = beforeStartIndices.Reverse().Take(Settings.LinesBefore).Reverse().ToList();
			var ends = beforeEndIndices.Reverse().Take(Settings.LinesBefore + 1).Reverse().Skip(1).ToList();
			linesBefore.AddRange(starts.Select(
				(t, i) => content.Substring(t, ends[i] - t)));
			return linesBefore;
		}

		private IEnumerable<string> GetLinesAfterFromContents(string content,
			IEnumerable<int> afterStartIndices, IEnumerable<int> afterEndIndices)
		{
			var linesAfter = new List<string>();
			if (Settings.LinesAfter == 0) return linesAfter;
			var starts = afterStartIndices.Take(Settings.LinesAfter).ToList();
			var ends = afterEndIndices.Skip(1).Take(Settings.LinesAfter).ToList();
			linesAfter.AddRange(starts.Select(
				(t, i) => content.Substring(t, ends[i] - t)));
			return linesAfter;
		}

		public IEnumerable<FindResult> FindContents(string contents)
		{
			var patternMatches = new Dictionary<Regex, int>();
			var results = new List<FindResult>();
			var newLineIndices = GetNewLineIndices(contents);
			var startLineIndices = GetStartLineIndices(newLineIndices).ToList();
			var endLineIndices = GetEndLineIndices(contents, newLineIndices).ToList();

			foreach (var p in Settings.FindPatterns)
			{
				var match = p.Match(contents);
				while (match.Success)
				{
					if (Settings.FirstMatch && patternMatches.ContainsKey(p))
					{
						break;
					}
					var matchIndex = match.Index;
					// get the start and end indices before the match index
					var beforeStartIndices = startLineIndices.TakeWhile(i => i <= matchIndex).ToList();
					var beforeEndIndices = endLineIndices.TakeWhile(i => i < matchIndex).ToList();
					// add another end line index if it exists or the last index of the string
					if (endLineIndices.Count > beforeEndIndices.Count)
						beforeEndIndices.ToList().Add(endLineIndices[beforeEndIndices.Count]);
					else
						beforeEndIndices.ToList().Add(contents.Length - 1);
					var afterStartIndices = startLineIndices.SkipWhile(i => i <= matchIndex).ToList();
					var afterEndIndices = endLineIndices.SkipWhile(i => i <= matchIndex).ToList();
					var lineNum = beforeStartIndices.ToList().Count;
					var startLineIndex = beforeStartIndices.Max();
					var endLineIndex = afterStartIndices.Min() - 1;
					var line = contents.Substring(startLineIndex, endLineIndex - startLineIndex);
					var linesBefore = GetLinesBeforeFromContents(contents,
						beforeStartIndices.AsEnumerable().Reverse().Skip(1).Reverse(),
						beforeEndIndices);
					var linesAfter = GetLinesAfterFromContents(contents, afterStartIndices,
						afterEndIndices);
					if ((linesBefore.ToList().Count == 0 || LinesBeforeMatch(linesBefore))
						&&
						(linesAfter.ToList().Count == 0 || LinesAfterMatch(linesAfter)))
						results.Add(new FindResult(
						p,
						null,
						lineNum,
						match.Index - startLineIndex + 1,
						match.Index - startLineIndex + match.Length + 1,
						line,
						new List<string>(linesBefore),
						new List<string>(linesAfter)));
					patternMatches[p] = 1;

					match = match.NextMatch();
				}
			}
			return results;
		}

		private void FindTextFileLines(FindFile f)
		{
			try
			{
				var enumerableLines = FileUtil.EnumerableStringFromFile(f, TextFileEncoding);
				var results = FindLines(enumerableLines);

				foreach (var r in results)
				{
					r.File = f;
					AddFindResult(r);
				}
			}
			catch (IOException e)
			{
				Common.Log(e.Message);
			}
		}

		private static bool AnyMatchesAnyPattern(IEnumerable<string> strings,
			IEnumerable<Regex> patterns)
		{
			return strings.Any(s => MatchesAnyPattern(s, patterns));
		}

		private static bool MatchesAnyPattern(string s, IEnumerable<Regex> patterns)
		{
			return !string.IsNullOrEmpty(s) && patterns.Any(p => p.Match(s).Success);
		}

		private static bool LinesMatch(IEnumerable<string> lines,
			ICollection<Regex> inPatterns, ICollection<Regex> outPatterns)
		{
			return ((inPatterns.Count == 0 || AnyMatchesAnyPattern(lines, inPatterns))
				&& (outPatterns.Count == 0 || !AnyMatchesAnyPattern(lines, outPatterns)));
		}

		private bool LinesBeforeMatch(IEnumerable<string> linesBefore)
		{
			return LinesMatch(linesBefore, Settings.InLinesBeforePatterns,
				Settings.OutLinesBeforePatterns);
		}

		private bool LinesAfterMatch(IEnumerable<string> linesAfter)
		{
			return LinesMatch(linesAfter, Settings.InLinesAfterPatterns,
				Settings.OutLinesAfterPatterns);
		}

		public IEnumerable<FindResult> FindLines(IEnumerable<string> lines)
		{
			var patternMatches = new Dictionary<Regex, int>();
			var results = new List<FindResult>();
			var lineNum = 0;
			var linesBefore = new Queue<string>();
			var linesAfter = new Queue<string>();

			using var lineEnumerator = lines.GetEnumerator();
			var stop = false;
			while ((lineEnumerator.MoveNext() || linesAfter.Count > 0) && !stop)
			{
				lineNum++;
				var line = linesAfter.Count > 0 ? linesAfter.Dequeue() : lineEnumerator.Current;
				if (Settings.LinesAfter > 0)
				{
					while (linesAfter.Count < Settings.LinesAfter && lineEnumerator.MoveNext())
					{
						linesAfter.Enqueue(lineEnumerator.Current);
					}
				}

				if ((Settings.LinesBefore == 0 || linesBefore.Count == 0 || LinesBeforeMatch(linesBefore))
				    &&
				    (Settings.LinesAfter == 0 || linesAfter.Count == 0 || LinesAfterMatch(linesAfter)))
				{
					foreach (var p in Settings.FindPatterns)
					{
						foreach (Match match in p.Matches(line).Where(m => m != null))
						{
							if (Settings.FirstMatch && patternMatches.ContainsKey(p))
							{
								stop = true;
								break;
							}
							results.Add(new FindResult(p,
								null,
								lineNum,
								match.Index + 1,
								match.Index + match.Length + 1,
								line,
								new List<string>(linesBefore),
								new List<string>(linesAfter)));
							patternMatches[p] = 1;
						}
					}
				}

				if (Settings.LinesBefore == 0) continue;
				if (linesBefore.Count == Settings.LinesBefore)
				{
					linesBefore.Dequeue();
				}
				if (linesBefore.Count < Settings.LinesBefore)
				{
					linesBefore.Enqueue(line);
				}
			}

			return results;
		}

		private void FindBinaryFile(FindFile sf)
		{
			if (Settings.Verbose)
				Common.Log($"Finding binary file {FileUtil.ContractPath(sf.FullName)}");
			try
			{
				using var sr = new StreamReader(sf.FullName, _binaryEncoding);
				var contents = sr.ReadToEnd();
				foreach (var p in Settings.FindPatterns)
				{
					var matches = p.Matches(contents).Cast<Match>();
					if (Settings.FirstMatch)
					{
						matches = matches.Take(1);
					}
					foreach (var m in matches)
					{
						AddFindResult(new FindResult(
							p,
							sf,
							0,
							m.Index + 1,
							m.Index + m.Length + 1,
							null));
					}
				}
			}
			catch (IOException e)
			{
				Common.Log(e.Message);
			}
		}

		private void AddFindResult(FindResult findResult)
		{
			Results.Add(findResult);
		}

		private IList<FindResult> GetSortedFindResults()
		{
			var sorted = Results.ToList();
			sorted.Sort(new FindResultsComparer());
			return sorted;
		}

		public void PrintResults()
		{
			var sortedResults = GetSortedFindResults();
			var formatter = new FindResultFormatter(Settings);
			Common.Log($"Find results ({sortedResults.Count}):");
			foreach (var findResult in sortedResults)
			{
				Common.Log(formatter.Format(findResult));
			}
		}

		private IEnumerable<DirectoryInfo> GetMatchingDirs()
		{
			return new List<DirectoryInfo>(
				Results.Where(r => r.File != null)
					.Select(r => r.File!.File.Directory)
					.Distinct()
					.OrderBy(d => d.FullName));
		}

		public void PrintMatchingDirs()
		{
			var matchingDirs = GetMatchingDirs()
				.Select(d => FileUtil.GetRelativePath(d.FullName, Settings.StartPath!))
				.Distinct()
				.OrderBy(d => d);
			Common.Log($"\nDirectories with matches ({matchingDirs.Count()}):");
			foreach (var d in matchingDirs)
			{
				Common.Log(d);
			}
		}

		private IEnumerable<FileInfo> GetMatchingFiles()
		{
			return new List<FileInfo>(
				Results.Where(r => r.File != null)
					.Select(r => r.File!.PathAndName)
					.Distinct().Select(f => new FileInfo(f))
					.OrderBy(d => d.FullName));
		}

		public void PrintMatchingFiles()
		{
			var matchingFiles = GetMatchingFiles()
				.Select(f => FileUtil.GetRelativePath(f.FullName, Settings.StartPath!))
				.Distinct()
				.OrderBy(f => f);
			Common.Log($"\nFiles with matches ({matchingFiles.Count()}):");
			foreach (var f in matchingFiles)
			{
				Common.Log(f);
			}
		}

		private IEnumerable<string> GetMatchingLines()
		{
			var lines = Results.Where(r => r.Line != null)
				.Select(r => r.Line!.Trim()).ToList();
			if (Settings.UniqueLines)
			{
				lines = new HashSet<string>(lines).ToList();
			}
			lines.Sort(new CaseInsensitiveComparer());
			return lines;
		}

		public void PrintMatchingLines()
		{
			var matchingLines = GetMatchingLines();
			var hdrText = Settings.UniqueLines ? "Unique lines with matches" : "Lines with matches";
			Common.Log($"\n{hdrText} ({matchingLines.Count()}):");
			foreach (var m in matchingLines)
			{
				Common.Log(m);
			}
		}
	}

	internal class CaseInsensitiveComparer : IComparer<string>
	{
		public int Compare(string? a, string? b)
		{
			if (a == null && b == null)
				return 0;
			if (a == null)
				return -1;
			if (b == null)
				return 1;
			return string.Compare(a.ToUpper(), b.ToUpper(), StringComparison.Ordinal);
		}
	}
}
