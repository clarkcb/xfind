package xsearch

import (
	"path/filepath"
	"strings"
)

type FileType int

const (
	FILETYPE_UNKNOWN    FileType = iota
	FILETYPE_BINARY     FileType = iota
	FILETYPE_COMPRESSED FileType = iota
	FILETYPE_TEXT       FileType = iota
)

type FileTypes struct {
	fileTypeMap map[string]set
}

func getExtension(file string) string {
	ext := filepath.Ext(file)
	return strings.ToLower(strings.TrimLeft(ext, "."))
}

func (f *FileTypes) getFileType(file string) FileType {
	if f.IsTextFile(file) {
		return FILETYPE_TEXT
	}
	if f.IsBinaryFile(file) {
		return FILETYPE_BINARY
	}
	if f.IsCompressedFile(file) {
		return FILETYPE_COMPRESSED
	}
	return FILETYPE_UNKNOWN
}

func (f *FileTypes) isFileType(filetype string, file string) bool {
	return f.fileTypeMap[filetype][getExtension(file)]
}

// going to assume file is binary if it has no extension
func (f *FileTypes) IsBinaryFile(file string) bool {
	return f.isFileType("binary", file) || getExtension(file) == ""
}

func (f *FileTypes) IsCompressedFile(file string) bool {
	return f.isFileType("compressed", file)
}

func (f *FileTypes) IsTextFile(file string) bool {
	textTypes := []string{"text", "code", "xml"}
	for _, t := range textTypes {
		if f.isFileType(t, file) {
			return true
		}
	}
	return false
}

func (f *FileTypes) IsSearchableFile(file string) bool {
	return f.IsTextFile(file) || f.IsBinaryFile(file) || f.IsCompressedFile(file)
}