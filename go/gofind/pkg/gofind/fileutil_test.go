package gofind

import (
	"fmt"
	"os/user"
	"path/filepath"
	"testing"
)

func TestExpandPath(t *testing.T) {
	expected := map[string]string{
		"hello.txt":        "hello.txt",
		"/a/path/to/where": "/a/path/to/where",
	}
	for k, v := range expected {
		if path := ExpandPath(k); path != v {
			t.Errorf("ExpandPath(\"%s\")=\"%s\", expected=\"%s\"", k, path, v)
		}
	}
	usr, err := user.Current()
	if err != nil || usr == nil {
		t.Errorf("Unable to get current user")
	}
	userPath := usr.HomeDir
	// test tilde
	tilde := "~"
	expandedTilde := ExpandPath(tilde)
	if expandedTilde != userPath {
		t.Errorf("ExpandPath(\"%s\")=\"%s\", expected expanded", tilde, userPath)
	}
	// test path with tilde
	tildePath := "~/src/xfind"
	expandedTildePath := ExpandPath(tildePath)
	expectedTildePath := filepath.Join(userPath, "src", "xfind")
	if expandedTildePath != expectedTildePath {
		t.Errorf("ExpandPath(\"%s\")=\"%s\", expected expanded", tildePath, expectedTildePath)
	}
	// test path with tilde and name
	tildeNamePath := "~cary/src/xfind"
	expandedTildeNamePath := ExpandPath(tildeNamePath)
	if expandedTildeNamePath != expectedTildePath {
		t.Errorf("ExpandPath(\"%s\")=\"%s\", expected expanded", tildeNamePath, expectedTildePath)
	}
}

func TestGetExtension(t *testing.T) {
	expected := map[string]string{
		"hello.txt":      "txt",
		"lib.a":          "a",
		"noext":          "",
		"archive.tar.gz": "gz",
	}

	for k, v := range expected {
		if ext := GetExtension(k); ext != v {
			t.Errorf("GetExtension(\"%s\")=\"%s\", expected=\"%s\"", k, ext, v)
		}
	}
}

func TestGetHome(t *testing.T) {
	homePath := getHome()
	Log(fmt.Sprintf("homePath: %s\n", homePath))
	if homePath := getHome(); homePath == "" {
		t.Errorf("getHome()=\"%s\", expected non-blank", homePath)
	}
}

func TestIsDotDir(t *testing.T) {
	expected := map[string]bool{
		".":     true,
		"..":    true,
		"lib.a": false,
		"noext": false,
	}

	for k, v := range expected {
		if d := isDotDir(k); d != v {
			t.Errorf("isDotDir(\"%s\")=%v, expected=%v", k, d, v)
		}
	}
}

func TestIsHiddenName(t *testing.T) {
	expected := map[string]bool{
		".":          false,
		"..":         false,
		"lib.a":      false,
		"noext":      false,
		".git":       true,
		".gitignore": true,
	}

	for k, v := range expected {
		if h := IsHiddenName(k); h != v {
			t.Errorf("IsHiddenName(\"%s\")=%v, expected=%v", k, h, v)
		}
	}
}

func TestIsHiddenPath(t *testing.T) {
	expected := map[string]bool{
		"./":           false,
		"../":          false,
		"./lib.a":      false,
		"./noext":      false,
		"./.git":       true,
		"./.gitignore": true,
	}

	for k, v := range expected {
		if h := IsHiddenPath(k); h != v {
			t.Errorf("IsHiddenPath(\"%s\")=%v, expected=%v", k, h, v)
		}
	}
}

func TestNormalizePath(t *testing.T) {
	expected := map[string]string{
		".":          ".",
		"./":         ".",
		"..":         "..",
		"../":        "..",
		"path":       "path",
		"path/":      "path",
		"long/path":  "long/path",
		"long/path/": "long/path",
	}

	for k, v := range expected {
		if p := normalizePath(k); p != v {
			t.Errorf("normalizePath(\"%s\")=\"%s\", expected=\"%s\"", k, p, v)
		}
	}
}

func TestRelativePath(t *testing.T) {
	homePath := getHome()
	startPath := "."
	expected := map[string]string{
		".":                     ".",
		"./":                    "./",
		"..":                    "..",
		"../":                   "../",
		"path":                  "path",
		"path/":                 "path/",
		"long/path":             "long/path",
		"long/path/":            "long/path/",
		homePath:                ".",
		homePath + "/long/path": "./long/path",
	}

	for k, v := range expected {
		if p := relativePath(k, startPath); p != v {
			t.Errorf("relativePath(\"%s\")=\"%s\", expected=\"%s\"", k, p, v)
		}
	}
}
