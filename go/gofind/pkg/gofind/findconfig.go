package gofind

import (
	"os"
	"path/filepath"
)

type FindConfig struct {
	XFindPath               string
	FileTypesPath           string
	FindOptionsPath         string
	DefaultFindSettingsPath string
	Version                 string
}

func NewFindConfig() *FindConfig {
	home := os.Getenv("HOME")
	defaultXFindConfigDir := filepath.Join(home, ".config", "xfind")
	xFindConfigDir := os.Getenv("XFIND_CONFIG_DIR")
	if xFindConfigDir == "" {
		xFindConfigDir = defaultXFindConfigDir
	}
	defaultXFindPath := filepath.Join(home, "src", "xfind")
	xFindPath := os.Getenv("XFIND_PATH")
	if xFindPath == "" {
		xFindPath = defaultXFindPath
	}
	sharedPath := filepath.Join(xFindPath, "shared")
	defaultFindSettingsPath := filepath.Join(xFindConfigDir, "settings.json")

	return &FindConfig{
		xFindPath,
		filepath.Join(sharedPath, "filetypes.json"),
		filepath.Join(sharedPath, "findoptions.json"),
		defaultFindSettingsPath,
		"0.1.0",
	}
}
