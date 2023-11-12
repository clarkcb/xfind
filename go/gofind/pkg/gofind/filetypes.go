package gofind

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

type FileType int

const (
	FiletypeUnknown FileType = iota
	FiletypeArchive FileType = iota
	FiletypeAudio   FileType = iota
	FiletypeBinary  FileType = iota
	FiletypeCode    FileType = iota
	FiletypeFont    FileType = iota
	FiletypeImage   FileType = iota
	FiletypeText    FileType = iota
	FiletypeVideo   FileType = iota
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
	config := NewFindConfig()

	var fileTypes FileTypes
	fileTypes.fileTypeExtMap = make(map[string]set)
	fileTypes.fileTypeNameMap = make(map[string]set)
	data, err := os.ReadFile(config.FILETYPESPATH)
	if err != nil {
		return &fileTypes
	}
	var jsonFileTypes JsonFileTypes
	if err = json.Unmarshal(data, &jsonFileTypes); err != nil {
		return &fileTypes
	}
	for _, ft := range jsonFileTypes.FileTypes {
		fileTypes.fileTypeExtMap[ft.Type] = MakeStringSet(ft.Extensions)
		fileTypes.fileTypeNameMap[ft.Type] = MakeStringSet(ft.Names)
	}

	// TEMPORARY
	//fileTypes.generateCodeFile("/Users/cary/src/xfind/go/gofind/pkg/gofind/filetypesgen.go")

	return &fileTypes
}

func (ft *FileTypes) GetFileType(file string) FileType {
	// more specific first
	if ft.IsCodeFile(file) {
		return FiletypeCode
	}
	if ft.IsArchiveFile(file) {
		return FiletypeArchive
	}
	if ft.IsAudioFile(file) {
		return FiletypeAudio
	}
	if ft.IsFontFile(file) {
		return FiletypeFont
	}
	if ft.IsImageFile(file) {
		return FiletypeImage
	}
	if ft.IsVideoFile(file) {
		return FiletypeVideo
	}

	// more general last
	if ft.IsXmlFile(file) {
		return FiletypeXml
	}
	if ft.IsTextFile(file) {
		return FiletypeText
	}
	if ft.IsBinaryFile(file) {
		return FiletypeBinary
	}
	return FiletypeUnknown
}

func GetFileTypeForName(name string) FileType {
	if strings.ToUpper(name) == "ARCHIVE" {
		return FiletypeArchive
	}
	if strings.ToUpper(name) == "AUDIO" {
		return FiletypeAudio
	}
	if strings.ToUpper(name) == "BINARY" {
		return FiletypeBinary
	}
	if strings.ToUpper(name) == "CODE" {
		return FiletypeCode
	}
	if strings.ToUpper(name) == "FONT" {
		return FiletypeFont
	}
	if strings.ToUpper(name) == "IMAGE" {
		return FiletypeImage
	}
	if strings.ToUpper(name) == "TEXT" {
		return FiletypeText
	}
	if strings.ToUpper(name) == "VIDEO" {
		return FiletypeVideo
	}
	if strings.ToUpper(name) == "XML" {
		return FiletypeXml
	}
	return FiletypeUnknown
}

func GetNameForFileType(fileType FileType) string {
	if fileType == FiletypeArchive {
		return "ARCHIVE"
	}
	if fileType == FiletypeAudio {
		return "AUDIO"
	}
	if fileType == FiletypeBinary {
		return "BINARY"
	}
	if fileType == FiletypeCode {
		return "CODE"
	}
	if fileType == FiletypeFont {
		return "FONT"
	}
	if fileType == FiletypeImage {
		return "IMAGE"
	}
	if fileType == FiletypeText {
		return "TEXT"
	}
	if fileType == FiletypeVideo {
		return "VIDEO"
	}
	if fileType == FiletypeXml {
		return "XML"
	}
	return "UNKNOWN"
}

func (ft *FileTypes) isFileType(fileType string, file string) bool {
	return ft.fileTypeNameMap[fileType][file] || ft.fileTypeExtMap[fileType][GetExtension(file)]
}

func (ft *FileTypes) IsArchiveFile(file string) bool {
	return ft.isFileType("archive", file)
}

func (ft *FileTypes) IsAudioFile(file string) bool {
	return ft.isFileType("audio", file)
}

// IsBinaryFile going to assume file is binary if it has no extension (for now)
func (ft *FileTypes) IsBinaryFile(file string) bool {
	return ft.isFileType("binary", file)
}

func (ft *FileTypes) IsCodeFile(file string) bool {
	return ft.isFileType("code", file)
}

func (ft *FileTypes) IsFontFile(file string) bool {
	return ft.isFileType("font", file)
}

func (ft *FileTypes) IsImageFile(file string) bool {
	return ft.isFileType("image", file)
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

func (ft *FileTypes) IsVideoFile(file string) bool {
	return ft.isFileType("video", file)
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

func (ft *FileTypes) generateCodeFile(filePath string) {
	var buffer bytes.Buffer
	depth := 0
	buffer.WriteString("package gofind\n\n")
	buffer.WriteString("func GetFileTypes() *FileTypes {\n")
	depth++
	buffer.WriteString(fmt.Sprintf("%sreturn &FileTypes{\n", strings.Repeat("\t", depth)))
	depth++
	buffer.WriteString(fmt.Sprintf("%smap[string]set{\n", strings.Repeat("\t", depth)))
	depth++
	for k, exts := range ft.fileTypeExtMap {
		buffer.WriteString(fmt.Sprintf("%s\"%s\": MakeStringSet([]string{\"%s\"}),\n",
			strings.Repeat("\t", depth), k, strings.Join(getSortedSetValues(exts), "\", \"")))
	}
	depth--
	buffer.WriteString(fmt.Sprintf("%s},\n", strings.Repeat("\t", depth)))
	depth--
	depth++
	buffer.WriteString(fmt.Sprintf("%smap[string]set{\n", strings.Repeat("\t", depth)))
	depth++
	for k, names := range ft.fileTypeNameMap {
		buffer.WriteString(fmt.Sprintf("%s\"%s\": MakeStringSet([]string{\"%s\"}),\n",
			strings.Repeat("\t", depth), k, strings.Join(getSortedSetValues(names), "\", \"")))
	}
	depth--
	buffer.WriteString(fmt.Sprintf("%s},\n", strings.Repeat("\t", depth)))
	depth--
	buffer.WriteString(fmt.Sprintf("%s}\n}\n", strings.Repeat("\t", depth)))
	os.WriteFile(filePath, buffer.Bytes(), 0644)
}
