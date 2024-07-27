package gofind

import (
	"os"
	"path/filepath"
)

type FindConfig struct {
	XFINDPATH       string
	SHAREDPATH      string
	FILETYPESPATH   string
	FINDOPTIONSPATH string
	XFINDDB         string
	VERSION         string
}

func NewFindConfig() *FindConfig {
	xFindPath := os.Getenv("XFIND_PATH")
	if xFindPath == "" {
		xFindPath = filepath.Join(os.Getenv("HOME"), "src/xfind")
	}
	sharedPath := filepath.Join(xFindPath, "shared")

	return &FindConfig{
		xFindPath,
		sharedPath,
		filepath.Join(sharedPath, "filetypes.json"),
		filepath.Join(sharedPath, "findoptions.json"),
		filepath.Join(sharedPath, "xfind.db"),
		"0.1.0",
	}
}
