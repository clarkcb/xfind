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
		"0.1.0",
	}
}
