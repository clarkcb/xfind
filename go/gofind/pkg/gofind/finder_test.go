package gofind

import (
	"os"
	"testing"
)

func getSettings() *FindSettings {
	settings := GetDefaultFindSettings()
	settings.AddPath(".")
	return settings
}

func getFinder() *Finder {
	settings := getSettings()
	return NewFinder(settings)
}

func getBinPath() string {
	xfindPath := os.Getenv("XFIND_PATH")
	if xfindPath == "" {
		xfindPath = os.Getenv("HOME") + "/src/xfind"
	}
	return xfindPath + "/bin"
}

/*************************************************************
 * isMatchingDir tests
 *************************************************************/
func TestIsFindDir_SingleDot_True(t *testing.T) {
	finder := getFinder()
	d := "."
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_DoubleDot_True(t *testing.T) {
	finder := getFinder()
	d := ".."
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_IsHidden_False(t *testing.T) {
	finder := getFinder()
	d := ".git"
	if finder.isMatchingDir(d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_IsHiddenIncludeHidden_True(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeHidden(true)
	finder := NewFinder(settings)
	d := ".git"
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_NoPatterns_True(t *testing.T) {
	finder := getFinder()
	d := "/Users"
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInDirPattern("Find")
	finder := NewFinder(settings)
	d := "CsFind"
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("Find")
	finder := NewFinder(settings)
	d := "CsFind"
	if finder.isMatchingDir(d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInDirPattern("FindFiles")
	finder := NewFinder(settings)
	d := "CsFind"
	if finder.isMatchingDir(d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("FindFiles")
	finder := NewFinder(settings)
	var d = "CsFind"
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

/*************************************************************
 * filterToFileResult tests
*************************************************************/
func TestIsFindFile_NoExtensionsNoPatterns_True(t *testing.T) {
	finder := getFinder()
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_MatchesInExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("cs")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("java")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("cs")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("java")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder := NewFinder(settings)
	f := "Finder.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder := NewFinder(settings)
	f := "Finder.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

/*************************************************************
 * filterToFileResult (archive files) tests
*************************************************************/
func TestIsArchiveFindFile_NoExtensionsNoPatterns_True(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeArchives(true)
	finder := NewFinder(settings)
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsArchiveFindFile_MatchesInExtension_True(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeArchives(true)
	settings.AddInArchiveExtension("zip")
	finder := NewFinder(settings)
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsArchiveFindFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeArchives(true)
	settings.AddInArchiveExtension("gz")
	finder := NewFinder(settings)
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsArchiveFindFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeArchives(true)
	settings.AddOutArchiveExtension("zip")
	finder := NewFinder(settings)
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsArchiveFindFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeArchives(true)
	settings.AddOutArchiveExtension("gz")
	finder := NewFinder(settings)
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsArchiveFindFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeArchives(true)
	settings.AddInArchiveFilePattern("arch")
	finder := NewFinder(settings)
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsArchiveFindFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeArchives(true)
	settings.AddInArchiveFilePattern("archives")
	finder := NewFinder(settings)
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsArchiveFindFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeArchives(true)
	settings.AddOutArchiveFilePattern("arch")
	finder := NewFinder(settings)
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsArchiveFindFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeArchives(true)
	settings.AddOutArchiveFilePattern("archives")
	finder := NewFinder(settings)
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

/*************************************************************
 * followSymlinks tests
*************************************************************/
func TestFollowSymlinks_DefaultSettings_Excluded(t *testing.T) {
	settings := GetDefaultFindSettings()
	settings.AddPath(getBinPath())
	finder := NewFinder(settings)
	fileResults, err := finder.Find()
	if err == nil {
		if fileResults.Len() > 3 {
			t.Errorf("expected less than 4 files")
		}
	} else {
		t.Errorf("expected no error")
	}
}

func TestFollowSymlinks_FollowSymlinks_Included(t *testing.T) {
	settings := GetDefaultFindSettings()
	settings.AddPath(getBinPath())
	settings.SetFollowSymlinks(true)
	finder := NewFinder(settings)
	fileResults, err := finder.Find()
	if err == nil {
		if !fileResults.IsEmpty() && fileResults.Len() < 3 {
			t.Errorf("expected more than 2 files")
		}
	} else {
		t.Errorf("expected no error")
	}
}

func TestFollowSymlinks_NoFollowSymlinks_Excluded(t *testing.T) {
	settings := GetDefaultFindSettings()
	settings.AddPath(getBinPath())
	settings.SetFollowSymlinks(false)
	finder := NewFinder(settings)
	fileResults, err := finder.Find()
	if err == nil {
		if fileResults.Len() > 3 {
			t.Errorf("expected less than 4 files")
		}
	} else {
		t.Errorf("expected no error")
	}
}
