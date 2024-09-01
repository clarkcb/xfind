package gofind

import (
	"database/sql"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
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
	db                  *sql.DB
	extTypeCache        map[string]FileType
	nameTypeCache       map[string]FileType
	nameTypeCacheLoaded bool
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

func NewFileTypes() *FileTypes {
	config := NewFindConfig()
	var fileTypes FileTypes
	db, err := sql.Open("sqlite3", fmt.Sprintf("file:%v?mode=ro", config.XFINDDB))

	if err != nil {
		//log.Fatal(err)
		return &fileTypes
	}
	fileTypes.db = db
	fileTypes.extTypeCache = make(map[string]FileType)
	return &fileTypes
}

func (ft *FileTypes) GetFileTypesForQueryAndParams(query string, params []any) map[string]FileType {
	results := make(map[string]FileType)
	rows, err := ft.db.Query(query, params...)
	if err != nil {
		return results
	}
	defer rows.Close()
	for rows.Next() {
		var key string
		var fileTypeId int
		if err := rows.Scan(&key, &fileTypeId); err != nil {
			return results
		}
		results[key] = FileType(fileTypeId - 1)
	}
	return results
}

func (ft *FileTypes) loadNameTypeCache() {
	ft.nameTypeCache = ft.GetFileTypesForQueryAndParams("SELECT name, file_type_id FROM file_name", []any{})
	ft.nameTypeCacheLoaded = true
}

func (ft *FileTypes) GetFileTypeForQueryAndParams(query string, params []any) FileType {
	stmt, err := ft.db.Prepare(query)
	if err != nil {
		return FileTypeUnknown
	}
	defer stmt.Close()
	var fileTypeId int
	err = stmt.QueryRow(params...).Scan(&fileTypeId)
	if err != nil {
		return FileTypeUnknown
	}
	return FileType(fileTypeId - 1)
}

func (ft *FileTypes) GetFileTypeForFileName(fileName string) FileType {
	if !ft.nameTypeCacheLoaded {
		ft.loadNameTypeCache()
	}
	if fileType, found := ft.nameTypeCache[fileName]; found {
		return fileType
	}
	//query := "select file_type_id from file_name where name = ?"
	//return ft.GetFileTypeForQueryAndParams(query, []any{fileName})
	return FileTypeUnknown
}

func (ft *FileTypes) GetFileTypeForExtension(ext string) FileType {
	if ext == "" {
		return FileTypeUnknown
	}
	if fileType, found := ft.extTypeCache[ext]; found {
		return fileType
	}
	query := "select file_type_id from file_extension where extension = ?"
	fileType := ft.GetFileTypeForQueryAndParams(query, []any{ext})
	ft.extTypeCache[ext] = fileType
	return fileType
}

func (ft *FileTypes) GetFileType(file string) FileType {
	fileTypeForFileName := ft.GetFileTypeForFileName(file)
	if fileTypeForFileName != FileTypeUnknown {
		return fileTypeForFileName
	}
	return ft.GetFileTypeForExtension(GetExtension(file))
}

func GetFileTypeForName(name string) FileType {
	lname := strings.ToLower(name)
	switch lname {
	case FileTypeNameArchive:
		return FileTypeArchive
	case FileTypeNameAudio:
		return FileTypeAudio
	case FileTypeNameBinary:
		return FileTypeBinary
	case FileTypeNameCode:
		return FileTypeCode
	case FileTypeNameFont:
		return FileTypeFont
	case FileTypeNameImage:
		return FileTypeImage
	case FileTypeNameText:
		return FileTypeText
	case FileTypeNameVideo:
		return FileTypeVideo
	case FileTypeNameXml:
		return FileTypeXml
	default:
		return FileTypeUnknown
	}
}

func GetNameForFileType(fileType FileType) string {
	switch fileType {
	case FileTypeArchive:
		return FileTypeNameArchive
	case FileTypeAudio:
		return FileTypeNameAudio
	case FileTypeBinary:
		return FileTypeNameBinary
	case FileTypeCode:
		return FileTypeNameCode
	case FileTypeFont:
		return FileTypeNameFont
	case FileTypeImage:
		return FileTypeNameImage
	case FileTypeText:
		return FileTypeNameText
	case FileTypeVideo:
		return FileTypeNameVideo
	case FileTypeXml:
		return FileTypeNameXml
	default:
		return FileTypeNameUnknown
	}
}

func (ft *FileTypes) IsArchiveFile(file string) bool {
	return ft.GetFileType(file) == FileTypeArchive
}

func (ft *FileTypes) IsAudioFile(file string) bool {
	return ft.GetFileType(file) == FileTypeAudio
}

func (ft *FileTypes) IsBinaryFile(file string) bool {
	return ft.GetFileType(file) == FileTypeBinary
}

func (ft *FileTypes) IsCodeFile(file string) bool {
	return ft.GetFileType(file) == FileTypeCode
}

func (ft *FileTypes) IsFontFile(file string) bool {
	return ft.GetFileType(file) == FileTypeFont
}

func (ft *FileTypes) IsImageFile(file string) bool {
	return ft.GetFileType(file) == FileTypeImage
}

func (ft *FileTypes) IsTextFile(file string) bool {
	fileType := ft.GetFileType(file)
	return fileType == FileTypeText || fileType == FileTypeCode || fileType == FileTypeXml
}

func (ft *FileTypes) IsVideoFile(file string) bool {
	return ft.GetFileType(file) == FileTypeVideo
}

func (ft *FileTypes) IsXmlFile(file string) bool {
	return ft.GetFileType(file) == FileTypeXml
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
