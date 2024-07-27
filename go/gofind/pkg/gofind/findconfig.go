package gofind

import (
	"os"
	"path/filepath"
)

type FindConfig struct {
	XFINDPATH               string
	SHAREDPATH              string
	FILETYPESPATH           string
	FINDOPTIONSPATH         string
	DEFAULTFINDSETTINGSPATH string
	XFINDDB                 string
	VERSION                 string
}

func NewFindConfig() *FindConfig {
	home := os.Getenv("HOME")
	xFindPath := os.Getenv("XFIND_PATH")
	if xFindPath == "" {
		xFindPath = filepath.Join(home, "src/xfind")
	}
	sharedPath := filepath.Join(xFindPath, "shared")
	defaultFindSettingsPath := filepath.Join(home, ".config/xfind/settings.json")

	return &FindConfig{
		xFindPath,
		sharedPath,
		filepath.Join(sharedPath, "filetypes.json"),
		filepath.Join(sharedPath, "findoptions.json"),
		defaultFindSettingsPath,
		filepath.Join(sharedPath, "xfind.db"),
		"0.1.0",
	}
}
