package gofind

import "testing"

func TestGetFileType(t *testing.T) {
	expected := map[string]FileType{
		"archive.tar.gz":        FileTypeArchive,
		"music.mp3":             FileTypeAudio,
		"lib.a":                 FileTypeBinary,
		"filetypes.go":          FileTypeCode,
		"font.ttf":              FileTypeFont,
		"image.png":             FileTypeImage,
		"hello.txt":             FileTypeText,
		"movie.mp4":             FileTypeVideo,
		"markup.xml":            FileTypeXml,
		"noext":                 FileTypeUnknown,
		"nonsense.zippitydooda": FileTypeUnknown,
	}

	fileTypes := FileTypesFromJson()

	for k, v := range expected {
		if ft := fileTypes.GetFileType(k); ft != v {
			t.Errorf("GetFileType(\"%s\")=\"%v\", expected=\"%v\"", k, ft, v)
		}
	}
}
