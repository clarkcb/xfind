package gofind

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type FindItemsIterator struct {
	idx   int
	items *FindItems
}

func NewFindItemsIterator(fi *FindItems) *FindItemsIterator {
	return &FindItemsIterator{
		-1,
		fi,
	}
}

func (it *FindItemsIterator) Next() bool {
	it.idx++
	if it.idx >= len(it.items.items) {
		return false
	}
	return true
}

func (it *FindItemsIterator) Value() *FindItem {
	return it.items.items[it.idx]
}

type FindItems struct {
	items     []*FindItem
	strPtrMap map[string]*string
}

func NewFindItems() *FindItems {
	return &FindItems{
		[]*FindItem{},
		make(map[string]*string),
	}
}

// limits string pointers to one per distinct string (memory management)
func (fi *FindItems) getStrPtr(s *string) *string {
	strPtr := s
	if sp, ok := fi.strPtrMap[*s]; ok {
		strPtr = sp
	} else {
		fi.strPtrMap[*s] = s
	}
	return strPtr
}

func (fi *FindItems) AddItem(i *FindItem) {
	fi.items = append(fi.items, &FindItem{
		i.Containers,
		// fi.getStrPtr(i.Path),
		// fi.getStrPtr(i.Name),
		i.Path,
		i.Name,
		i.fileType,
	})
}

func (fi *FindItems) Count() int {
	return len(fi.items)
}

func (fi *FindItems) IsEmpty() bool {
	return len(fi.items) == 0
}

func (fi *FindItems) Iterator() *FindItemsIterator {
	return NewFindItemsIterator(fi)
}

func (fi *FindItems) GetPathCountMap() map[string]int {
	pathCountMap := make(map[string]int)
	for _, i := range fi.items {
		pathCountMap[i.Path]++
	}
	return pathCountMap
}

func (fi *FindItems) GetMatchingDirs() []string {
	pathCountMap := fi.GetPathCountMap()
	paths := getSortedCountKeys(pathCountMap)
	return paths
}

func (fi *FindItems) PrintMatchingDirs() {
	paths := fi.GetMatchingDirs()
	if len(paths) > 0 {
		log(fmt.Sprintf("\nMatching directories (%d):", len(paths)))
		for _, p := range paths {
			log(p)
		}
	} else {
		log("\nMatching directories: 0")
	}
}

func (fi *FindItems) GetFileCountMap() map[string]int {
	fileCountMap := make(map[string]int)
	for _, i := range fi.items {
		fileCountMap[i.String()]++
	}
	return fileCountMap
}

func (fi *FindItems) GetMatchingFiles() []string {
	fileCountMap := fi.GetFileCountMap()
	files := getSortedCountKeys(fileCountMap)
	return files
}

func (fi *FindItems) PrintMatchingFiles() {
	files := fi.GetMatchingFiles()
	if len(files) > 0 {
		log(fmt.Sprintf("\nMatching files (%d):", len(files)))
		for _, p := range files {
			log(p)
		}
	} else {
		log("\nMatching files: 0")
	}
}

type FindItem struct {
	Containers []string
	Path       string
	Name       string
	fileType   FileType
}

func NewFindItem(path string, name string, fileType FileType) *FindItem {
	return &FindItem{
		[]string{},
		path,
		name,
		fileType,
	}
}

func (fi *FindItem) AddContainer(c string) {
	fi.Containers = append(fi.Containers, c)
}

const containerSeparator = "!"

func (fi *FindItem) String() string {
	var buffer bytes.Buffer
	if len(fi.Containers) > 0 {
		buffer.WriteString(strings.Join(fi.Containers, containerSeparator))
		buffer.WriteString(containerSeparator)
	}
	path := normalizePath(fi.Path)
	if isDotDir(path) {
		buffer.WriteString(fmt.Sprintf("%s%c%s", path, os.PathSeparator, fi.Name))
	} else {
		buffer.WriteString(filepath.Join(fi.Path, fi.Name))
	}
	return buffer.String()
}
