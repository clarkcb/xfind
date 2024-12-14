package gofind

import (
	"fmt"
	"os"
	"os/user"
	"path/filepath"
	"runtime"
	"strings"
)

func expandPath(filePath string) string {
	if strings.HasPrefix(filePath, "~") {
		usr, err := user.Current()
		if err != nil || usr == nil {
			// TODO: handle error
			return filePath
		}
		userPath := usr.HomeDir
		sepIndex := strings.Index(filePath, string(os.PathSeparator))
		if filePath != "~" && sepIndex != 1 {
			// Another user's home directory
			homePath := filepath.Dir(userPath)
			userName := ""
			if sepIndex == -1 {
				userName = filePath[1:]
			} else {
				userName = filePath[1:sepIndex]
			}
			userPath = filepath.Join(homePath, userName)
		}
		return filepath.Join(userPath, filePath[sepIndex+1:])
	}
	return filePath
}

func GetExtension(file string) string {
	ext := filepath.Ext(filepath.Base(file))
	return strings.ToLower(strings.TrimLeft(ext, "."))
}

func getHome() string {
	//home := ""
	homeName := "HOME"
	if runtime.GOOS == "windows" {
		homeName = "USERPROFILE"
	}
	//env := os.Environ()
	//for _, x := range env {
	//	if strings.HasPrefix(x, homeName+"=") {
	//		home = strings.TrimPrefix(x, homeName+"=")
	//		break
	//	}
	//}
	//return home
	return os.Getenv(homeName)
}

func getPathSeparator() string {
	if runtime.GOOS == "windows" {
		return "\\"
	}
	return "/"
}

func isDotDir(file string) bool {
	dotDirs := []string{".", ".."}
	return containsV(dotDirs, file)
}

func isHidden(file string) bool {
	for _, d := range strings.Split(file, getPathSeparator()) {
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
