package gofind

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"testing"
)

func getTestFileContents() (string, error) {
	testFile := fmt.Sprintf("%s/shared/testFiles/testFile2.txt", XFINDPATH)
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
	settings.AddFindPattern("Finder")
	settings.StartPath = "."
	return settings
}

func getFinder() *Finder {
	settings := getSettings()
	return NewFinder(settings)
}

/*************************************************************
 * isFindDir tests
 *************************************************************/

func TestIsFindDir_SingleDot_True(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	d := "."
	if !finder.isFindDir(&d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_DoubleDot_True(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	d := ".."
	if !finder.isFindDir(&d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_IsHidden_False(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	d := ".git"
	if finder.isFindDir(&d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_IsHiddenIncludeHidden_True(t *testing.T) {
	settings := getSettings()
	settings.ExcludeHidden = false
	finder := NewFinder(settings)
	d := ".git"
	if !finder.isFindDir(&d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_NoPatterns_True(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	d := "/Users"
	if !finder.isFindDir(&d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInDirPattern("Find")
	finder := NewFinder(settings)
	d := "CsFind"
	if !finder.isFindDir(&d) {
		t.Errorf("expected true")
	}
}

func TestIsFindDir_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("Find")
	finder := NewFinder(settings)
	d := "CsFind"
	if finder.isFindDir(&d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInDirPattern("FindFiles")
	finder := NewFinder(settings)
	d := "CsFind"
	if finder.isFindDir(&d) {
		t.Errorf("expected false")
	}
}

func TestIsFindDir_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("FindFiles")
	finder := NewFinder(settings)
	var d = "CsFind"
	if !finder.isFindDir(&d) {
		t.Errorf("expected true")
	}
}

/*************************************************************
 * isFindFile tests
*************************************************************/

func TestIsFindFile_NoExtensionsNoPatterns_True(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if !finder.isFindFile(&f) {
		t.Errorf("expected true")
	}
}

func TestIsFindFile_MatchesInExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("cs")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if !finder.isFindFile(&f) {
		t.Errorf("expected true")
	}
}

func TestIsFindFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("java")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.isFindFile(&f) {
		t.Errorf("expected false")
	}
}

func TestIsFindFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("cs")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.isFindFile(&f) {
		t.Errorf("expected false")
	}
}

func TestIsFindFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("java")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if !finder.isFindFile(&f) {
		t.Errorf("expected true")
	}
}

func TestIsFindFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder := NewFinder(settings)
	f := "Finder.cs"
	if !finder.isFindFile(&f) {
		t.Errorf("expected true")
	}
}

func TestIsFindFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Find")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.isFindFile(&f) {
		t.Errorf("expected false")
	}
}

func TestIsFindFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder := NewFinder(settings)
	f := "Finder.cs"
	if finder.isFindFile(&f) {
		t.Errorf("expected false")
	}
}

func TestIsFindFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Find")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if !finder.isFindFile(&f) {
		t.Errorf("expected true")
	}
}

/*************************************************************
 * isArchiveFindFile tests
*************************************************************/

func TestIsArchiveFindFile_NoExtensionsNoPatterns_True(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	f := "archive.zip"
	if !finder.isArchiveFindFile(&f) {
		t.Errorf("expected true")
	}
}

func TestIsArchiveFindFile_MatchesInExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddInArchiveExtension("zip")
	finder := NewFinder(settings)
	f := "archive.zip"
	if !finder.isArchiveFindFile(&f) {
		t.Errorf("expected true")
	}
}

func TestIsArchiveFindFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddInArchiveExtension("gz")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.isArchiveFindFile(&f) {
		t.Errorf("expected false")
	}
}

func TestIsArchiveFindFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutArchiveExtension("zip")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.isArchiveFindFile(&f) {
		t.Errorf("expected false")
	}
}

func TestIsArchiveFindFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutArchiveExtension("gz")
	finder := NewFinder(settings)
	f := "archive.zip"
	if !finder.isArchiveFindFile(&f) {
		t.Errorf("expected true")
	}
}

func TestIsArchiveFindFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInArchiveFilePattern("arch")
	finder := NewFinder(settings)
	f := "archive.zip"
	if !finder.isArchiveFindFile(&f) {
		t.Errorf("expected true")
	}
}

func TestIsArchiveFindFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInArchiveFilePattern("archives")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.isArchiveFindFile(&f) {
		t.Errorf("expected false")
	}
}

func TestIsArchiveFindFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutArchiveFilePattern("arch")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.isArchiveFindFile(&f) {
		t.Errorf("expected false")
	}
}

func TestIsArchiveFindFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutArchiveFilePattern("archives")
	finder := NewFinder(settings)
	f := "archive.zip"
	if !finder.isArchiveFindFile(&f) {
		t.Errorf("expected true")
	}
}

/*************************************************************
 * filterFile tests
*************************************************************/

func TestFilterFile_IsHidden_False(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	f := ".gitignore"
	if finder.filterFile(&f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_IsHiddenIncludeHidden_True(t *testing.T) {
	settings := getSettings()
	settings.ExcludeHidden = false
	finder := NewFinder(settings)
	f := ".gitignore"
	// fmt.Printf("isHidden(\"%s\"): %v\n", f, isHidden(f))
	// fmt.Printf("isFindFile(\"%s\"): %v\n", f, finder.isFindFile(&f))
	if !finder.filterFile(&f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_ArchiveNoFindArchives_False(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterFile(&f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_ArchiveFindArchives_True(t *testing.T) {
	settings := getSettings()
	settings.FindArchives = true
	finder := NewFinder(settings)
	f := "archive.zip"
	if !finder.filterFile(&f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_IsArchiveFindFile_True(t *testing.T) {
	settings := getSettings()
	settings.FindArchives = true
	settings.AddInArchiveExtension("zip")
	finder := NewFinder(settings)
	f := "archive.zip"
	if !finder.filterFile(&f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_NotIsArchiveFindFile_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("zip")
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterFile(&f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_ArchiveFileArchivesOnly_True(t *testing.T) {
	settings := getSettings()
	settings.ArchivesOnly = true
	finder := NewFinder(settings)
	f := "archive.zip"
	if finder.filterFile(&f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_NoExtensionsNoPatterns_True(t *testing.T) {
	settings := getSettings()
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if !finder.filterFile(&f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_isFindFile_True(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("cs")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if !finder.filterFile(&f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_NotisFindFile_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("cs")
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.filterFile(&f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_NonArchiveFileArchivesOnly_False(t *testing.T) {
	settings := getSettings()
	settings.ArchivesOnly = true
	finder := NewFinder(settings)
	f := "FileUtil.cs"
	if finder.filterFile(&f) {
		t.Errorf("expected false")
	}
}

/*************************************************************
 * FindTextReaderLines test
 *************************************************************/
func TestFindTextReaderLines(t *testing.T) {
	contents, err := getTestFileContents()
	if err != nil {
		t.Errorf("error from getTestFileContents()")
		panic(err)
	}
	settings := getSettings()
	finder := NewFinder(settings)
	results := finder.FindTextReaderLines(strings.NewReader(contents))

	if len(results) != 2 {
		t.Errorf("len(results)=%d, expected=2", len(results))
	}

	firstResult := results[0]
	expectedFirstLineNum := 29
	if firstResult.LineNum != expectedFirstLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *firstResult,
			expectedFirstLineNum)
	}
	expectedFirstMatchStartIndex := 3
	if firstResult.MatchStartIndex != expectedFirstMatchStartIndex {
		t.Errorf("firstResult=%v, expected MatchStartIndex=%d", *firstResult,
			expectedFirstMatchStartIndex)
	}
	expectedFirstMatchEndIndex := 11
	if firstResult.MatchEndIndex != expectedFirstMatchEndIndex {
		t.Errorf("firstResult=%v, expected MatchEndIndex=%d", *firstResult,
			expectedFirstMatchEndIndex)
	}

	secondResult := results[1]
	expectedSecondLineNum := 35
	if secondResult.LineNum != expectedSecondLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *secondResult,
			expectedSecondLineNum)
	}
	expectedSecondMatchStartIndex := 24
	if secondResult.MatchStartIndex != expectedSecondMatchStartIndex {
		t.Errorf("firstResult=%v, expected MatchStartIndex=%d", *secondResult,
			expectedSecondMatchStartIndex)
	}
	expectedSecondMatchEndIndex := 32
	if secondResult.MatchEndIndex != expectedSecondMatchEndIndex {
		t.Errorf("secondResult=%v, expected MatchEndIndex=%d", *secondResult,
			expectedSecondMatchEndIndex)
	}
}

/*************************************************************
 * FindMultiLineString test
 *************************************************************/
func TestFindMultiLineString(t *testing.T) {
	contents, err := getTestFileContents()
	if err != nil {
		t.Errorf("error from getTestFileContents()")
	}
	settings := getSettings()
	finder := NewFinder(settings)
	results := finder.FindMultiLineString(contents)

	if len(results) != 2 {
		t.Errorf("len(results)=%d, expected=2", len(results))
	}

	firstResult := results[0]
	expectedFirstLineNum := 29
	if firstResult.LineNum != expectedFirstLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *firstResult,
			expectedFirstLineNum)
	}
	expectedFirstMatchStartIndex := 3
	if firstResult.MatchStartIndex != expectedFirstMatchStartIndex {
		t.Errorf("firstResult=%v, expected MatchStartIndex=%d", *firstResult,
			expectedFirstMatchStartIndex)
	}
	expectedFirstMatchEndIndex := 11
	if firstResult.MatchEndIndex != expectedFirstMatchEndIndex {
		t.Errorf("firstResult=%v, expected MatchEndIndex=%d", *firstResult,
			expectedFirstMatchEndIndex)
	}

	secondResult := results[1]
	expectedSecondLineNum := 35
	if secondResult.LineNum != expectedSecondLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *secondResult,
			expectedSecondLineNum)
	}
	expectedSecondMatchStartIndex := 24
	if secondResult.MatchStartIndex != expectedSecondMatchStartIndex {
		t.Errorf("firstResult=%v, expected MatchStartIndex=%d", *secondResult,
			expectedSecondMatchStartIndex)
	}
	expectedSecondMatchEndIndex := 32
	if secondResult.MatchEndIndex != expectedSecondMatchEndIndex {
		t.Errorf("secondResult=%v, expected MatchEndIndex=%d", *secondResult,
			expectedSecondMatchEndIndex)
	}
}
