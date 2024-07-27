package gofind

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

func Test_FileResult_AbsPath(t *testing.T) {
	fileTypes := NewFileTypes()
	home := os.Getenv("HOME")
	path := home + "/src/xfind/go/gofind/pkg/gofind/fileresults.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.GetFileType(file)
	fileResult := NewFileResult(dir, file, fileType, 0, time.Time{})
	if fileResult.String() != path {
		t.Errorf("fileResult.String() (%s) != path (%s)", fileResult.String(), path)
	}
}

func Test_FileResult_TildePath(t *testing.T) {
	fileTypes := NewFileTypes()
	path := "~/src/xfind/go/gofind/pkg/gofind/fileresults.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.GetFileType(file)
	fileResult := NewFileResult(dir, file, fileType, 0, time.Time{})
	if fileResult.String() != path {
		t.Errorf("fileResult.String() (%s) != path (%s)", fileResult.String(), path)
	}
}

func Test_FileResult_RelPath1(t *testing.T) {
	fileTypes := NewFileTypes()
	path := "./fileresults.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.GetFileType(file)
	fileResult := NewFileResult(dir, file, fileType, 0, time.Time{})
	fileResultString := fileResult.String()
	if fileResultString != path {
		t.Errorf("fileResult.String() (%s) != path (%s)", fileResult.String(), path)
	}
}

func Test_FileResult_RelPath2(t *testing.T) {
	fileTypes := NewFileTypes()
	path := "./fileresults.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.GetFileType(file)
	fileResult := NewFileResult(dir, file, fileType, 0, time.Time{})
	if fileResult.String() != path {
		t.Errorf("fileResult.String() (%s) != path (%s)", fileResult.String(), path)
	}
}
