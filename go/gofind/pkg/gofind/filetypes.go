package gofind

import (
	"encoding/json"
	"io/ioutil"
	"strings"
)

type FileType int

const (
	FiletypeUnknown FileType = iota
	FiletypeArchive FileType = iota
	FiletypeBinary  FileType = iota
	FiletypeCode    FileType = iota
	FiletypeText    FileType = iota
	FiletypeXml     FileType = iota
)

type FileTypes struct {
	fileTypeExtMap  map[string]set
	fileTypeNameMap map[string]set
}

// used for unmarshalling
type JsonFileType struct {
	Type       string
	Extensions []string
	Names      []string
}

type JsonFileTypes struct {
	FileTypes []*JsonFileType
}

func FileTypesFromJson() *FileTypes {
	config := NewConfig()

	var fileTypes FileTypes
	fileTypes.fileTypeExtMap = make(map[string]set)
	fileTypes.fileTypeNameMap = make(map[string]set)
	data, err := ioutil.ReadFile(config.FILETYPESPATH)
	if err != nil {
		return &fileTypes
	}
	var jsonFileTypes JsonFileTypes
	if err = json.Unmarshal(data, &jsonFileTypes); err != nil {
		return &fileTypes
	}
	for _, ft := range jsonFileTypes.FileTypes {
		fileTypes.fileTypeExtMap[ft.Type] = makeSet(ft.Extensions)
		fileTypes.fileTypeNameMap[ft.Type] = makeSet(ft.Names)
	}
	return &fileTypes
}

func (ft *FileTypes) GetFileType(file string) FileType {
	if ft.IsCodeFile(file) {
		return FiletypeCode
	}
	if ft.IsXmlFile(file) {
		return FiletypeXml
	}
	if ft.IsTextFile(file) {
		return FiletypeText
	}
	if ft.IsBinaryFile(file) {
		return FiletypeBinary
	}
	if ft.IsArchiveFile(file) {
		return FiletypeArchive
	}
	return FiletypeUnknown
}

func GetFileTypeForName(name string) FileType {
	if strings.ToUpper(name) == "TEXT" {
		return FiletypeText
	}
	if strings.ToUpper(name) == "BINARY" {
		return FiletypeBinary
	}
	if strings.ToUpper(name) == "CODE" {
		return FiletypeCode
	}
	if strings.ToUpper(name) == "ARCHIVE" {
		return FiletypeArchive
	}
	if strings.ToUpper(name) == "XML" {
		return FiletypeXml
	}
	return FiletypeUnknown
}

func GetNameForFileType(fileType FileType) string {
	if fileType == FiletypeText {
		return "TEXT"
	}
	if fileType == FiletypeBinary {
		return "BINARY"
	}
	if fileType == FiletypeCode {
		return "CODE"
	}
	if fileType == FiletypeArchive {
		return "ARCHIVE"
	}
	if fileType == FiletypeXml {
		return "XML"
	}
	return "UNKNOWN"
}

func (ft *FileTypes) isFileType(filetype string, file string) bool {
	return ft.fileTypeNameMap[filetype][file] || ft.fileTypeExtMap[filetype][getExtension(file)]
}

func (ft *FileTypes) IsArchiveFile(file string) bool {
	return ft.isFileType("archive", file)
}

// IsBinaryFile going to assume file is binary if it has no extension (for now)
func (ft *FileTypes) IsBinaryFile(file string) bool {
	return ft.isFileType("binary", file)
}

func (ft *FileTypes) IsCodeFile(file string) bool {
	return ft.isFileType("code", file)
}

func (ft *FileTypes) IsTextFile(file string) bool {
	textTypes := [...]string{"code", "text", "xml"}
	for _, t := range textTypes {
		if ft.isFileType(t, file) {
			return true
		}
	}
	return false
}

func (ft *FileTypes) IsXmlFile(file string) bool {
	return ft.isFileType("xml", file)
}

func (ft *FileTypes) IsUnknownFile(file string) bool {
	return ft.GetFileType(file) == FiletypeUnknown
}

func ContainsFileType(fileTypes []FileType, fileType FileType) bool {
	for _, ft := range fileTypes {
		if fileType == ft {
			return true
		}
	}
	return false
}
