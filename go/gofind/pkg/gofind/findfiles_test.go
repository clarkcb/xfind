package gofind

import (
	"fmt"
	"path/filepath"
	"testing"
)

func Test_FindItem_AbsPath(t *testing.T) {
	fileTypes := FileTypesFromJson()
	path := "/Users/cary/src/xfind/go/gofind/pkg/gofind/findfiles.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.getFileType(file)
	findItem := NewFindItem(&dir, &file, fileType)
	if findItem.String() != path {
		t.Errorf(fmt.Sprintf("findItem.String() (%s) != path (%s)", findItem.String(), path))
	}
}

func Test_FindItem_TildePath(t *testing.T) {
	fileTypes := FileTypesFromJson()
	path := "~/src/xfind/go/gofind/pkg/gofind/findfiles.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.getFileType(file)
	findItem := NewFindItem(&dir, &file, fileType)
	if findItem.String() != path {
		t.Errorf(fmt.Sprintf("findItem.String() (%s) != path (%s)", findItem.String(), path))
	}
}

func Test_FindItem_RelPath1(t *testing.T) {
	fileTypes := FileTypesFromJson()
	path := "./findfiles.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.getFileType(file)
	findItem := NewFindItem(&dir, &file, fileType)
	findItemString := findItem.String()
	if findItemString != path {
		t.Errorf(fmt.Sprintf("findItem.String() (%s) != path (%s)", findItem.String(), path))
	}
}

func Test_FindItem_RelPath2(t *testing.T) {
	fileTypes := FileTypesFromJson()
	path := "./findfiles.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.getFileType(file)
	findItem := NewFindItem(&dir, &file, fileType)
	if findItem.String() != path {
		t.Errorf(fmt.Sprintf("findItem.String() (%s) != path (%s)", findItem.String(), path))
	}
}
