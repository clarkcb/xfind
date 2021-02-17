package gofind

import "testing"

func TestFindSettingsFromNoArgs(t *testing.T) {
	findOptions := NewFindOptions()

	args := []string{}

	settings, err := findOptions.FindSettingsFromArgs(args)
	if err != nil {
		t.Errorf("FindSettingsFromArgs: err: %v", err)
	}

	if settings.ArchivesOnly ||
		settings.Debug ||
		!settings.ExcludeHidden ||
		settings.FirstMatch ||
		settings.ListDirs ||
		settings.ListFiles ||
		settings.ListLines ||
		settings.MultiLineFind ||
		!settings.PrintResults ||
		settings.PrintUsage ||
		settings.PrintVersion ||
		!settings.Recursive ||
		settings.FindArchives ||
		settings.UniqueLines ||
		settings.Verbose {
		t.Errorf("settings did not match defaults")
	}
}

func TestFindSettingsFromValidArgs(t *testing.T) {
	findOptions := NewFindOptions()

	args := []string{
		"-x", "go", "-s", "Finder", ".",
	}

	settings, err := findOptions.FindSettingsFromArgs(args)
	if err != nil {
		t.Errorf("FindSettingsFromArgs: err: %v", err)
	}

	if settings.StartPath != "." {
		t.Errorf("settings.StartPath (%s) != \".\"", settings.StartPath)
	}

	if len(settings.InExtensions) != 1 {
		t.Errorf("len(settings.InExtensions) = %d, expected 1",
			len(settings.InExtensions))
	}
	expectedExt := "go"
	if *settings.InExtensions[0] != expectedExt {
		t.Errorf("settings.InExtensions[0] (\"%s\") != \"%s\"",
			*settings.InExtensions[0], expectedExt)
	}
}

func TestFindSettingsFromJson(t *testing.T) {
	findOptions := NewFindOptions()

	jsonSettings := []byte(`{
  "startpath": "~/src/xfind/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "findpattern": "Finder",
  "linesbefore": 2,
  "linesafter": 2,
  "debug": true,
  "allmatches": false,
  "includehidden": true
}`)

	settings := GetDefaultFindSettings()
	var err error
	err = findOptions.SettingsFromJson(jsonSettings, settings)
	if err != nil {
		t.Errorf("TestFindSettingsFromJson: err: %v", err)
	}

	if settings.StartPath != "~/src/xfind/" {
		t.Errorf("settings.StartPath (%s) != \"~/src/xfind/\"", settings.StartPath)
	}

	if len(settings.InExtensions) != 2 {
		t.Errorf("len(settings.InExtensions) = %d, expected 2",
			len(settings.InExtensions))
	}
	expectedInExts := []string{"js", "ts"}
	for i, _ := range expectedInExts {
		if *settings.InExtensions[i] != expectedInExts[i] {
			t.Errorf("settings.InExtensions[%d] (\"%s\") != \"%s\"",
				i, *settings.InExtensions[i], expectedInExts[i])
		}
	}

	if len(settings.OutDirPatterns.patterns) != 1 {
		t.Errorf("len(settings.OutDirPatterns.patterns) = %d, expected 1",
			len(settings.OutDirPatterns.patterns))
	}
	if settings.OutDirPatterns.patterns[0].String() != "node_module" {
		t.Errorf("settings.OutDirPatterns.patterns[0].String() (\"%s\") != \"node_module\"",
			settings.OutDirPatterns.patterns[0].String())
	}

	if len(settings.OutFilePatterns.patterns) != 1 {
		t.Errorf("len(settings.OutFilePatterns.patterns) = %d, expected 1",
			len(settings.OutFilePatterns.patterns))
	}
	if settings.OutFilePatterns.patterns[0].String() != "temp" {
		t.Errorf("settings.OutFilePatterns.patterns[0].String() (\"%s\") != \"temp\"",
			settings.OutFilePatterns.patterns[0].String())
	}

	if len(settings.FindPatterns.patterns) != 1 {
		t.Errorf("len(settings.FindPatterns.patterns) = %d, expected 1",
			len(settings.FindPatterns.patterns))
	}
	if settings.FindPatterns.patterns[0].String() != "Finder" {
		t.Errorf("settings.FindPatterns.patterns[0].String() (\"%s\") != \"Finder\"",
			settings.FindPatterns.patterns[0].String())
	}

	if settings.LinesBefore != 2 {
		t.Errorf("settings.LinesBefore (%d) != 2", settings.LinesBefore)
	}

	if settings.LinesAfter != 2 {
		t.Errorf("settings.LinesAfter (%d) != 2", settings.LinesAfter)
	}

	if !settings.Debug {
		t.Errorf("settings.Debug (%t) != true", settings.Debug)
	}

	if !settings.FirstMatch {
		t.Errorf("settings.FirstMatch (%t) != true", settings.FirstMatch)
	}

	if settings.ExcludeHidden {
		t.Errorf("settings.ExcludeHidden (%t) != false", settings.ExcludeHidden)
	}
}
