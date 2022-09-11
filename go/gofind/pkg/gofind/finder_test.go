package gofind

import (
	"fmt"
	"io/ioutil"
	"os"
	"testing"
)

func getTestFileContents() (string, error) {
	config := NewConfig()
	testFile := fmt.Sprintf("%s/testFiles/testFile2.txt", config.SHAREDPATH)
	r, err1 := os.Open(testFile)
	if err1 != nil {
		return "", err1
	}
	bytes, err2 := ioutil.ReadAll(r)
	if err2 != nil {
		return "", err2
	}
	return string(bytes), err2
}

func getSettings() *FindSettings {
	settings := GetDefaultFindSettings()
	settings.AddPath(".")
	return settings
}

func getFinder() *Finder {
	settings := getSettings()
	return NewFinder(settings)
}

/*************************************************************
 * isMatchingDir tests
 *************************************************************/
func TestIsFindDir_SingleDot_True(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	d := "."
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_DoubleDot_True(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	d := ".."
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_IsHidden_False(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	d := ".git"
	if finder.isMatchingDir(d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_IsHiddenIncludeHidden_True(t *testing.T) {
	settings := getSettings()
	settings.ExcludeHidden = false
	finder := NewFinder(settings)
	d := ".git"
	if !finder.isMatchingDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_NoPatterns_True(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
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
	settings := getSettings()
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_MatchesInExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("cs")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("java")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.filterToFileResult(f) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("cs")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.filterToFileResult(f) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("java")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder := NewFinder(settings)
	f := "Finder.cs"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}

func TestIsFindFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.filterToFileResult(f) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder := NewFinder(settings)
	f := "Finder.cs"
	if finder.filterToFileResult(f) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsFindFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}

/*************************************************************
 * filterToFileResult (archive files) tests
*************************************************************/
func TestIsArchiveFindFile_NoExtensionsNoPatterns_True(t *testing.T) {
	settings := getSettings()
	settings.IncludeArchives = true
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}

func TestIsArchiveFindFile_MatchesInExtension_True(t *testing.T) {
	settings := getSettings()
	settings.IncludeArchives = true
	settings.AddInArchiveExtension("zip")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}

func TestIsArchiveFindFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.IncludeArchives = true
	settings.AddInArchiveExtension("gz")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterToFileResult(f) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsArchiveFindFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.IncludeArchives = true
	settings.AddOutArchiveExtension("zip")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterToFileResult(f) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsArchiveFindFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.IncludeArchives = true
	settings.AddOutArchiveExtension("gz")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}

func TestIsArchiveFindFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.IncludeArchives = true
	settings.AddInArchiveFilePattern("arch")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}

func TestIsArchiveFindFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.IncludeArchives = true
	settings.AddInArchiveFilePattern("archives")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterToFileResult(f) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsArchiveFindFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.IncludeArchives = true
	settings.AddOutArchiveFilePattern("arch")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterToFileResult(f) != nil {
		t.Errorf("expected no match")
	}
}

func TestIsArchiveFindFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.IncludeArchives = true
	settings.AddOutArchiveFilePattern("archives")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterToFileResult(f) == nil {
		t.Errorf("expected match")
	}
}
