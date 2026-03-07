package gofind

import (
	"os"
	"path/filepath"
)

type FindConfig struct {
	XFINDPATH           string
	SHAREDPATH          string
	FILETYPESPATH       string
	FINDOPTIONSPATH     string
	DEFAULTSETTINGSPATH string
	VERSION             string
}

func NewFindConfig() *FindConfig {
	home := os.Getenv("HOME")
	xFindPath := os.Getenv("XFIND_PATH")
	if xFindPath == "" {
		xFindPath = filepath.Join(home, "src/xfind")
	}
	sharedPath := filepath.Join(xFindPath, "shared")
	defaultSettingsPath := filepath.Join(home, ".config/xfind/settings.json")

	return &FindConfig{
		xFindPath,
		sharedPath,
		filepath.Join(sharedPath, "filetypes.json"),
		filepath.Join(sharedPath, "findoptions.json"),
		defaultSettingsPath,
		"0.1.0",
	}
}
