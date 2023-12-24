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
	FileTypeNameUnknown = "unknown"
	FileTypeNameArchive = "archive"
	FileTypeNameAudio   = "audio"
	FileTypeNameBinary  = "binary"
	FileTypeNameCode    = "code"
	FileTypeNameFont    = "font"
	FileTypeNameImage   = "image"
	FileTypeNameText    = "text"
	FileTypeNameVideo   = "video"
	FileTypeNameXml     = "xml"
)

const (
	FileTypeUnknown FileType = iota
	FileTypeArchive FileType = iota
	FileTypeAudio   FileType = iota
	FileTypeBinary  FileType = iota
	FileTypeCode    FileType = iota
	FileTypeFont    FileType = iota
	FileTypeImage   FileType = iota
	FileTypeText    FileType = iota
	FileTypeVideo   FileType = iota
	FileTypeXml     FileType = iota
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
		return FileTypeCode
	}
	if ft.IsArchiveFile(file) {
		return FileTypeArchive
	}
	if ft.IsAudioFile(file) {
		return FileTypeAudio
	}
	if ft.IsFontFile(file) {
		return FileTypeFont
	}
	if ft.IsImageFile(file) {
		return FileTypeImage
	}
	if ft.IsVideoFile(file) {
		return FileTypeVideo
	}

	// more general last
	if ft.IsXmlFile(file) {
		return FileTypeXml
	}
	if ft.IsTextFile(file) {
		return FileTypeText
	}
	if ft.IsBinaryFile(file) {
		return FileTypeBinary
	}
	return FileTypeUnknown
}

func GetFileTypeForName(name string) FileType {
	lname := strings.ToLower(name)
	if lname == FileTypeNameArchive {
		return FileTypeArchive
	}
	if lname == FileTypeNameAudio {
		return FileTypeAudio
	}
	if lname == FileTypeNameBinary {
		return FileTypeBinary
	}
	if lname == FileTypeNameCode {
		return FileTypeCode
	}
	if lname == FileTypeNameFont {
		return FileTypeFont
	}
	if lname == FileTypeNameImage {
		return FileTypeImage
	}
	if lname == FileTypeNameText {
		return FileTypeText
	}
	if lname == FileTypeNameVideo {
		return FileTypeVideo
	}
	if lname == FileTypeNameXml {
		return FileTypeXml
	}
	return FileTypeUnknown
}

func GetNameForFileType(fileType FileType) string {
	if fileType == FileTypeArchive {
		return FileTypeNameArchive
	}
	if fileType == FileTypeAudio {
		return FileTypeNameAudio
	}
	if fileType == FileTypeBinary {
		return FileTypeNameBinary
	}
	if fileType == FileTypeCode {
		return FileTypeNameCode
	}
	if fileType == FileTypeFont {
		return FileTypeNameFont
	}
	if fileType == FileTypeImage {
		return FileTypeNameImage
	}
	if fileType == FileTypeText {
		return FileTypeNameText
	}
	if fileType == FileTypeVideo {
		return FileTypeNameVideo
	}
	if fileType == FileTypeXml {
		return FileTypeNameXml
	}
	return FileTypeNameUnknown
}

func (ft *FileTypes) isFileType(fileType string, file string) bool {
	return ft.fileTypeNameMap[fileType][file] || ft.fileTypeExtMap[fileType][GetExtension(file)]
}

func (ft *FileTypes) IsArchiveFile(file string) bool {
	return ft.isFileType(FileTypeNameArchive, file)
}

func (ft *FileTypes) IsAudioFile(file string) bool {
	return ft.isFileType(FileTypeNameAudio, file)
}

// IsBinaryFile going to assume file is binary if it has no extension (for now)
func (ft *FileTypes) IsBinaryFile(file string) bool {
	return ft.isFileType(FileTypeNameBinary, file)
}

func (ft *FileTypes) IsCodeFile(file string) bool {
	return ft.isFileType(FileTypeNameCode, file)
}

func (ft *FileTypes) IsFontFile(file string) bool {
	return ft.isFileType(FileTypeNameFont, file)
}

func (ft *FileTypes) IsImageFile(file string) bool {
	return ft.isFileType(FileTypeNameImage, file)
}

func (ft *FileTypes) IsTextFile(file string) bool {
	textTypes := [...]string{FileTypeNameCode, FileTypeNameText, FileTypeNameXml}
	for _, t := range textTypes {
		if ft.isFileType(t, file) {
			return true
		}
	}
	return false
}

func (ft *FileTypes) IsVideoFile(file string) bool {
	return ft.isFileType(FileTypeNameVideo, file)
}

func (ft *FileTypes) IsXmlFile(file string) bool {
	return ft.isFileType(FileTypeNameXml, file)
}

func (ft *FileTypes) IsUnknownFile(file string) bool {
	return ft.GetFileType(file) == FileTypeUnknown
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
