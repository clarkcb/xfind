package gofind

import "testing"

func TestDefaultFindSettings(t *testing.T) {
	settings := GetDefaultFindSettings()
	if settings.ArchivesOnly ||
		settings.Debug ||
		!settings.ExcludeHidden ||
		settings.IncludeArchives ||
		settings.ListDirs ||
		settings.ListFiles ||
		settings.PrintUsage ||
		settings.PrintVersion ||
		!settings.Recursive ||
		settings.Verbose {
		t.Errorf("settings did not match defaults")
	}
}

func TestAddExtensions(t *testing.T) {
	settings := GetDefaultFindSettings()
	settings.AddInExtension("go,hs")
	if len(settings.InExtensions) != 2 {
		t.Errorf("InExtensions should have two elements")
	}
}

func TestSetArchivesOnly(t *testing.T) {
	settings := GetDefaultFindSettings()
	settings.SetArchivesOnly(true)
	if !settings.ArchivesOnly {
		t.Errorf("ArchivesOnly should be true")
	}
	if !settings.IncludeArchives {
		t.Errorf("IncludeArchives should be true")
	}
}

func TestSetDebug(t *testing.T) {
	settings := GetDefaultFindSettings()
	settings.SetDebug(true)
	if !settings.Debug {
		t.Errorf("Debug should be true")
	}
	if !settings.Verbose {
		t.Errorf("Verbose should be true")
	}
}
