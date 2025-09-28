package gofind

import (
	"fmt"
	"os"
	"os/user"
	"path/filepath"
	"strings"
)

const (
	Dot    = "."
	DotDot = ".."
	Tilde  = "~"
)

func ExpandPath(filePath string) string {
	if strings.HasPrefix(filePath, Tilde) {
		userPath := getHome()
		tildeSlash := fmt.Sprintf("~%c", os.PathSeparator)
		if filePath == Tilde || filePath == tildeSlash {
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
	return strings.ToLower(strings.TrimLeft(ext, Dot))
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
	dotDirs := []string{Dot, DotDot}
	return containsV(dotDirs, file)
}

func IsHiddenName(name string) bool {
	return len(name) > 1 && strings.HasPrefix(name, Dot) && !isDotDir(name)
}

func IsHiddenPath(path string) bool {
	for _, elem := range strings.Split(path, getPathSeparatorString()) {
		if IsHiddenName(elem) {
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
	if startPath == Dot && strings.HasPrefix(path, homePath) {
		Log("path starts with homePath")
		relativePath = Dot + strings.TrimPrefix(path, homePath)
	}
	return relativePath
}
