package gofind

import (
	"os"
	"testing"
	"time"
)

func Test_FileResult_AbsPath(t *testing.T) {
	fileTypes := NewFileTypes()
	home := os.Getenv("HOME")
	filePath := home + "/src/xfind/go/gofind/pkg/gofind/fileresults.go"
	fileType := fileTypes.GetFileType(filePath)
	fileResult := NewFileResult(filePath, fileType, 0, time.Time{})
	if fileResult.String() != filePath {
		t.Errorf("fileResult.String() (%s) != path (%s)", fileResult.String(), filePath)
	}
}

func Test_FileResult_TildePath(t *testing.T) {
	fileTypes := NewFileTypes()
	filePath := "~/src/xfind/go/gofind/pkg/gofind/fileresults.go"
	fileType := fileTypes.GetFileType(filePath)
	fileResult := NewFileResult(filePath, fileType, 0, time.Time{})
	if fileResult.String() != filePath {
		t.Errorf("fileResult.String() (%s) != path (%s)", fileResult.String(), filePath)
	}
}

func Test_FileResult_RelPath1(t *testing.T) {
	fileTypes := NewFileTypes()
	filePath := "./fileresults.go"
	fileType := fileTypes.GetFileType(filePath)
	fileResult := NewFileResult(filePath, fileType, 0, time.Time{})
	fileResultString := fileResult.String()
	if fileResultString != filePath {
		t.Errorf("fileResult.String() (%s) != path (%s)", fileResult.String(), filePath)
	}
}

func Test_FileResult_RelPath2(t *testing.T) {
	fileTypes := NewFileTypes()
	filePath := "./fileresults.go"
	fileType := fileTypes.GetFileType(filePath)
	fileResult := NewFileResult(filePath, fileType, 0, time.Time{})
	if fileResult.String() != filePath {
		t.Errorf("fileResult.String() (%s) != path (%s)", fileResult.String(), filePath)
	}
}
