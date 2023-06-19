package gofind

import "testing"

func TestGetFileType(t *testing.T) {
	expected := map[string]FileType{
		"hello.txt":             FiletypeText,
		"filetypes.go":          FiletypeCode,
		"markup.xml":            FiletypeXml,
		"lib.a":                 FiletypeBinary,
		"noext":                 FiletypeUnknown,
		"archive.tar.gz":        FiletypeArchive,
		"nonsense.zippitydooda": FiletypeUnknown,
	}

	fileTypes := FileTypesFromJson()

	for k, v := range expected {
		if ft := fileTypes.GetFileType(k); ft != v {
			t.Errorf("GetFileType(\"%s\")=\"%v\", expected=\"%v\"", k, ft, v)
		}
	}
}
