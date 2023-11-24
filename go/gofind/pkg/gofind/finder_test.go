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
	finder, err := NewFinder(settings)
	if err != nil {
		return nil
	}
	return finder
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	d := "CsFind"
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("Find")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	d := "CsFind"
	if finder.isMatchingDir(d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInDirPattern("FindFiles")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	d := "CsFind"
	if finder.isMatchingDir(d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("FindFiles")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("java")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("cs")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("java")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	f := "Finder.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	f := "FileUtil.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	f := "Finder.cs"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
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
	finder, err := NewFinder(settings)
	if err != nil {
		t.Errorf("failed to create new Finder")
	}
	f := "archive.zip"
	var fi os.FileInfo
	if finder.filterToFileResult(f, fi) == nil {
		t.Errorf("expected match")
	}
}
