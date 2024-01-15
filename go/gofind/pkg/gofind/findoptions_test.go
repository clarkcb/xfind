package gofind

import "testing"

func TestFindSettingsFromNoArgs(t *testing.T) {
	findOptions := NewFindOptions()

	args := []string{}

	settings, err := findOptions.FindSettingsFromArgs(args)
	if err != nil {
		t.Errorf("FindSettingsFromArgs: err: %v", err)
	}

	if settings.ArchivesOnly() ||
		settings.Debug() ||
		settings.IncludeArchives() ||
		settings.IncludeHidden() ||
		settings.PrintDirs() ||
		!settings.PrintFiles() ||
		settings.PrintUsage() ||
		settings.PrintVersion() ||
		!settings.Recursive() ||
		settings.Verbose() {
		t.Errorf("settings did not match defaults")
	}
}

func TestFindSettingsFromValidArgs(t *testing.T) {
	findOptions := NewFindOptions()

	args := []string{
		"-x", "go", ".",
	}

	settings, err := findOptions.FindSettingsFromArgs(args)
	if err != nil {
		t.Errorf("FindSettingsFromArgs: err: %v", err)
	}

	if settings.Paths()[0] != "." {
		t.Errorf("settings.Paths[0] != \".\"")
	}

	if len(settings.InExtensions()) != 1 {
		t.Errorf("len(settings.InExtensions) = %d, expected 1",
			len(settings.InExtensions()))
	}
	expectedExt := "go"
	if settings.InExtensions()[0] != expectedExt {
		t.Errorf("settings.InExtensions[0] (\"%s\") != \"%s\"",
			settings.InExtensions()[0], expectedExt)
	}
}

func TestFindSettingsFromJson(t *testing.T) {
	findOptions := NewFindOptions()

	jsonSettings := []byte(`{
  "path": "~/src/xfind/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "debug": true,
  "includehidden": true
}`)

	settings := GetDefaultFindSettings()
	var err error
	err = findOptions.SettingsFromJson(jsonSettings, settings)
	if err != nil {
		t.Errorf("TestFindSettingsFromJson: err: %v", err)
	}

	if len(settings.Paths()) != 1 {
		t.Errorf("len(settings.Paths) = %d, expected 1",
			len(settings.Paths()))
	}
	if settings.Paths()[0] != "~/src/xfind/" {
		t.Errorf("settings.Paths[0] != \"~/src/xfind/\"")
	}

	if len(settings.InExtensions()) != 2 {
		t.Errorf("len(settings.InExtensions) = %d, expected 2",
			len(settings.InExtensions()))
	}
	expectedInExts := []string{"js", "ts"}
	for i, _ := range expectedInExts {
		if settings.InExtensions()[i] != expectedInExts[i] {
			t.Errorf("settings.InExtensions[%d] (\"%s\") != \"%s\"",
				i, settings.InExtensions()[i], expectedInExts[i])
		}
	}

	if len(settings.OutDirPatterns().patterns) != 1 {
		t.Errorf("len(settings.OutDirPatterns.patterns) = %d, expected 1",
			len(settings.OutDirPatterns().patterns))
	}
	if settings.OutDirPatterns().patterns[0].String() != "node_module" {
		t.Errorf("settings.OutDirPatterns.patterns[0].String() (\"%s\") != \"node_module\"",
			settings.OutDirPatterns().patterns[0].String())
	}

	if len(settings.OutFilePatterns().patterns) != 1 {
		t.Errorf("len(settings.OutFilePatterns.patterns) = %d, expected 1",
			len(settings.OutFilePatterns().patterns))
	}
	if settings.OutFilePatterns().patterns[0].String() != "temp" {
		t.Errorf("settings.OutFilePatterns.patterns[0].String() (\"%s\") != \"temp\"",
			settings.OutFilePatterns().patterns[0].String())
	}

	if !settings.Debug() {
		t.Errorf("settings.Debug (%t) != true", settings.Debug())
	}

	if !settings.IncludeHidden() {
		t.Errorf("settings.IncludeHidden (%t) != true", settings.IncludeHidden())
	}
}
