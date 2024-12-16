package gofind

import (
	"fmt"
	"os"
	"os/user"
	"path/filepath"
	"strings"
)

func ExpandPath(filePath string) string {
	if strings.HasPrefix(filePath, "~") {
		userPath := getHome()
		tildeSlash := fmt.Sprintf("~%c", os.PathSeparator)
		if filePath == "~" || filePath == tildeSlash {
			return userPath
		}
		if strings.HasPrefix(filePath, tildeSlash) {
			return filepath.Join(userPath, filePath[2:])
		}
		// Another user's home directory
		homePath := filepath.Dir(userPath)
		return filepath.Join(homePath, filePath[1:])
	}
	return filePath
}

func GetExtension(file string) string {
	ext := filepath.Ext(filepath.Base(file))
	return strings.ToLower(strings.TrimLeft(ext, "."))
}

func getHome() string {
	usr, err := user.Current()
	if err != nil || usr == nil {
		if err != nil {
			LogError(err.Error())
		}
		return ""
	}
	return usr.HomeDir
}

func getPathSeparatorString() string {
	return fmt.Sprintf("%c", os.PathSeparator)
}

func isDotDir(file string) bool {
	dotDirs := []string{".", ".."}
	return containsV(dotDirs, file)
}

func IsHidden(file string) bool {
	for _, d := range strings.Split(file, getPathSeparatorString()) {
		if len(d) > 1 && strings.HasPrefix(d, ".") && !isDotDir(d) {
			return true
		}
	}
	return false
}

func normalizePath(path string) string {
	return strings.TrimRight(path, "/\\")
}

func relativePath(path string, startPath string) string {
	homePath := getHome()
	Log(fmt.Sprintf("homePath:%s", homePath))
	relativePath := path
	if startPath == "." && strings.HasPrefix(path, homePath) {
		Log("path starts with homePath")
		relativePath = "." + strings.TrimPrefix(path, homePath)
	}
	return relativePath
}
