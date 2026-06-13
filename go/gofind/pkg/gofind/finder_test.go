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

func getFinder(settings *FindSettings) *Finder {
	// assumes no error with finder creation
	finder, _ := NewFinder(settings)
	return finder
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
	finder := getFinder(getSettings())
	d := "."
	if !finder.isMatchingDirPath(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_DoubleDot_True(t *testing.T) {
	finder := getFinder(getSettings())
	d := ".."
	if !finder.isMatchingDirPath(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_IsHidden_False(t *testing.T) {
	finder := getFinder(getSettings())
	d := ".git"
	if finder.isMatchingDirPath(d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_IsHiddenIncludeHidden_True(t *testing.T) {
	settings := getSettings()
	settings.SetIncludeHidden(true)
	finder := getFinder(settings)
	d := ".git"
	if !finder.isMatchingDirPath(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_NoPatterns_True(t *testing.T) {
	finder := getFinder(getSettings())
	d := "/Users"
	if !finder.isMatchingDirPath(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInDirPattern("Find")
	finder := getFinder(settings)
	d := "CsFind"
	if !finder.isMatchingDirPath(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("Find")
	finder := getFinder(settings)
	d := "CsFind"
	if finder.isMatchingDirPath(d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInDirPattern("FindFiles")
	finder := getFinder(settings)
	d := "CsFind"
	if finder.isMatchingDirPath(d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("FindFiles")
	finder := getFinder(settings)
	var d = "CsFind"
	if !finder.isMatchingDirPath(d) {
		t.Errorf("expected true")
	}
}

/*************************************************************
 * filterToFileResult tests
*************************************************************/
func TestIsFindFile_NoExtensionsNoPatterns_True(t *testing.T) {
	finder := getFinder(getSettings())
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_MatchesInExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("cs")
	finder := getFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("java")
	finder := getFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("cs")
	finder := getFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("java")
	finder := getFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder := getFinder(settings)
	f := "Finder.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder := getFinder(settings)
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder := getFinder(settings)
	f := "Finder.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
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
	finder := getFinder(settings)
	fileResults, err := finder.Find()
	if err == nil {
		if fileResults.Len() > 3 {
			t.Errorf("expected less than 4 files")
		}
	} else {
		t.Errorf("expected no error")
	}
}
