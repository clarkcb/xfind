package gofind

import "testing"

func TestGetFileType(t *testing.T) {
	expected := map[string]FileType{
		"archive.tar.gz":        FiletypeArchive,
		"music.mp3":             FiletypeAudio,
		"lib.a":                 FiletypeBinary,
		"filetypes.go":          FiletypeCode,
		"font.ttf":              FiletypeFont,
		"image.png":             FiletypeImage,
		"hello.txt":             FiletypeText,
		"movie.mp4":             FiletypeVideo,
		"markup.xml":            FiletypeXml,
		"noext":                 FiletypeUnknown,
		"nonsense.zippitydooda": FiletypeUnknown,
	}

	fileTypes := FileTypesFromJson()

	for k, v := range expected {
		if ft := fileTypes.GetFileType(k); ft != v {
			t.Errorf("GetFileType(\"%s\")=\"%v\", expected=\"%v\"", k, ft, v)
		}
	}
}
