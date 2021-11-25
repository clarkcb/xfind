using NUnit.Framework;
using CsFindLib;

namespace CsFindTests
{
	[TestFixture]
	class FindFileTests
	{
		private const string CsFindPath = "~/src/xfind/csharp/CsFind/CsFind";
		private const string WinCsFindPath = @"C:\src\git\xfind\csharp\CsFind\CsFind";

		[Test]
		public void FindFile_ToString_EqualsExpected()
		{
			var findFile = new FindFile(CsFindPath, "Finder.cs", FileType.Text);
			Assert.AreEqual(CsFindPath + "/Finder.cs", findFile.ToString());
		}

		[Test]
		public void FindFileTrailingSlash_ToString_EqualsExpected()
		{
			var findFile = new FindFile(CsFindPath + "/", "Finder.cs", FileType.Text);
			Assert.AreEqual(CsFindPath + "/Finder.cs", findFile.ToString());
		}

		[Test]
		public void FindFileBackSlashes_ToString_EqualsExpected()
		{
			var findFile = new FindFile(WinCsFindPath, "Finder.cs", FileType.Text);
			Assert.AreEqual(WinCsFindPath + @"\Finder.cs", findFile.ToString());
		}

		[Test]
		public void FindFileBackSlashesTrailingSlash_ToString_EqualsExpected()
		{
			var findFile = new FindFile(WinCsFindPath + @"\", "Finder.cs", FileType.Text);
			Assert.AreEqual(WinCsFindPath + @"\Finder.cs", findFile.ToString());
		}
	}
}
