using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace CsFind
{
	public class FindSettings
	{
		private bool _archivesOnly;

		public bool ArchivesOnly
		{
			get => _archivesOnly;
			set
			{
				_archivesOnly = value;
				if (_archivesOnly)
					IncludeArchives = true;
			}
		}

		public bool Colorize { get; set; }

		private bool _debug;
		public bool Debug
		{
			get => _debug;
			set
			{
				_debug = value;
				if (_debug)
					Verbose = true;
			}
		}

		public bool ExcludeHidden { get; set; }
		public ISet<string> InArchiveExtensions { get; private set; }
		public ISet<Regex> InArchiveFilePatterns { get; private set; }
		public ISet<Regex> InDirPatterns { get; private set; }
		public ISet<string> InExtensions { get; private set; }
		public ISet<Regex> InFilePatterns { get; private set; }
		public ISet<FileType> InFileTypes { get; private set; }
		public bool IncludeArchives { get; set; }
		public bool ListDirs { get; set; }
		public bool ListFiles { get; set; }
		public ISet<string> OutArchiveExtensions { get; private set; }
		public ISet<Regex> OutArchiveFilePatterns { get; private set; }
		public ISet<Regex> OutDirPatterns { get; private set; }
		public ISet<string> OutExtensions { get; private set; }
		public ISet<Regex> OutFilePatterns { get; private set; }
		public ISet<FileType> OutFileTypes { get; private set; }
		public bool PrintUsage { get; set; }
		public bool PrintVersion { get; set; }
		public bool Recursive { get; set; }
		public ISet<string> Paths { get; private set; }
		public bool Verbose { get; set; }

		public FindSettings()
		{
			ArchivesOnly = false;
			Colorize = true;
			Debug = false;
			ExcludeHidden = true;
			InArchiveExtensions = new HashSet<string>();
			InArchiveFilePatterns = new HashSet<Regex>();
			InDirPatterns = new HashSet<Regex>();
			InExtensions = new HashSet<string>();
			InFilePatterns = new HashSet<Regex>();
			InFileTypes = new HashSet<FileType>();
			IncludeArchives = false;
			ListDirs = false;
			ListFiles = false;
			OutArchiveExtensions = new HashSet<string>();
			OutArchiveFilePatterns = new HashSet<Regex>();
			OutDirPatterns = new HashSet<Regex>();
			OutExtensions = new HashSet<string>();
			OutFilePatterns = new HashSet<Regex>();
			OutFileTypes = new HashSet<FileType>();
			Paths = new HashSet<string>();
			PrintUsage = false;
			PrintVersion = false;
			Recursive = true;
			Verbose = false;
		}

		private static void AddExtension(ISet<string> set, string extList)
		{
			var exts = extList.Split(new[] { ',' });
			foreach (var x in exts)
			{
				var ext = x;
				if (!ext.StartsWith("."))
					ext = "." + ext;
				set.Add(ext.ToLowerInvariant());
			}
		}

		public void AddInExtension(string ext)
		{
			AddExtension(InExtensions, ext);
		}

		public void AddOutExtension(string ext)
		{
			AddExtension(OutExtensions, ext);
		}

		private static void AddPattern(ISet<Regex> set, string pattern)
		{
			set.Add(new Regex(pattern, RegexOptions.Compiled));
		}

		public void AddInDirPattern(string pattern)
		{
			AddPattern(InDirPatterns, pattern);
		}

		public void AddOutDirPattern(string pattern)
		{
			AddPattern(OutDirPatterns, pattern);
		}

		public void AddInFilePattern(string pattern)
		{
			AddPattern(InFilePatterns, pattern);
		}

		public void AddOutFilePattern(string pattern)
		{
			AddPattern(OutFilePatterns, pattern);
		}

		public void AddInArchiveExtension(string ext)
		{
			AddExtension(InArchiveExtensions, ext);
		}

		public void AddOutArchiveExtension(string ext)
		{
			AddExtension(OutArchiveExtensions, ext);
		}

		public void AddInArchiveFilePattern(string pattern)
		{
			AddPattern(InArchiveFilePatterns, pattern);
		}

		public void AddOutArchiveFilePattern(string pattern)
		{
			AddPattern(OutArchiveFilePatterns, pattern);
		}

		private static void AddFileType(ISet<FileType> set, string typeNameList)
		{
			var typeNames = typeNameList.Split(new[] { ',' });
			foreach (var t in typeNames)
			{
				set.Add(FileTypes.FromName(t));
			}
		}

		public void AddInFileType(string typeName)
		{
			AddFileType(InFileTypes, typeName);
		}

		public void AddOutFileType(string typeName)
		{
			AddFileType(OutFileTypes, typeName);
		}

		private static string EnumerableToString<T>(IEnumerable<T> enumerable)
		{
			var sb = new StringBuilder("[");
			var elemCount = 0;
			foreach (var x in enumerable)
			{
				var t = x!.GetType();
				if (elemCount > 0)
					sb.Append(", ");
				if (t == typeof(string))
					sb.Append("\"");
				sb.Append(x);
				if (t == typeof(string))
					sb.Append("\"");
				elemCount++;
			}
			sb.Append("]");
			return sb.ToString();
		}

		public override string ToString()
		{
			return "FindSettings(" +
				"ArchivesOnly: " + ArchivesOnly +
				", Colorize: " + Colorize +
				", Debug: " + Debug +
				", ExcludeHidden: " + ExcludeHidden +
				", InArchiveExtensions: " + EnumerableToString(InArchiveExtensions) +
				", InArchiveFilePatterns: " + EnumerableToString(InArchiveFilePatterns) +
				", InDirPatterns: " + EnumerableToString(InDirPatterns) +
				", InExtensions: " + EnumerableToString(InExtensions) +
				", InFilePatterns: " + EnumerableToString(InFilePatterns) +
				", InFileTypes: " + EnumerableToString(InFileTypes) +
				", IncludeArchives: " + IncludeArchives +
				", ListDirs: " + ListDirs +
				", ListFiles: " + ListFiles +
				", OutArchiveExtensions: " + EnumerableToString(OutArchiveExtensions) +
				", OutArchiveFilePatterns: " + EnumerableToString(OutArchiveFilePatterns) +
				", OutDirPatterns: " + EnumerableToString(OutDirPatterns) +
				", OutExtensions: " + EnumerableToString(OutExtensions) +
				", OutFilePatterns: " + EnumerableToString(OutFilePatterns) +
				", OutFileTypes: " + EnumerableToString(OutFileTypes) +
				", Paths: " + EnumerableToString(Paths) +
				", PrintUsage: " + PrintUsage +
				", PrintVersion: " + PrintVersion +
				", Recursive: " + Recursive +
				", Verbose: " + Verbose +
				")";
		}
	}
}
