package gofind

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type FileResultsIterator struct {
	idx     int
	results *FileResults
}

func NewFileResultsIterator(fr *FileResults) *FileResultsIterator {
	return &FileResultsIterator{
		-1,
		fr,
	}
}

func (it *FileResultsIterator) Next() bool {
	it.idx++
	if it.idx >= len(it.results.results) {
		return false
	}
	return true
}

func (it *FileResultsIterator) Value() *FileResult {
	return it.results.results[it.idx]
}

type FileResults struct {
	results   []*FileResult
	strPtrMap map[string]*string
}

func NewFileResults() *FileResults {
	return &FileResults{
		[]*FileResult{},
		make(map[string]*string),
	}
}

// limits string pointers to one per distinct string (memory management)
func (fr *FileResults) getStrPtr(s *string) *string {
	strPtr := s
	if sp, ok := fr.strPtrMap[*s]; ok {
		strPtr = sp
	} else {
		fr.strPtrMap[*s] = s
	}
	return strPtr
}

func (fr *FileResults) AddResult(r *FileResult) {
	fr.results = append(fr.results, &FileResult{
		r.Containers,
		r.Path,
		r.Name,
		r.fileType,
	})
}

func (fr *FileResults) Len() int {
	return len(fr.results)
}

func (fr *FileResults) IsEmpty() bool {
	return len(fr.results) == 0
}

func (fr *FileResults) Iterator() *FileResultsIterator {
	return NewFileResultsIterator(fr)
}

func (fr *FileResults) GetPathCountMap() map[string]int {
	pathCountMap := make(map[string]int)
	for _, i := range fr.results {
		pathCountMap[i.Path]++
	}
	return pathCountMap
}

func (fr *FileResults) GetMatchingDirs() []string {
	pathCountMap := fr.GetPathCountMap()
	paths := getSortedCountKeys(pathCountMap)
	return paths
}

func (fr *FileResults) PrintMatchingDirs() {
	paths := fr.GetMatchingDirs()
	if len(paths) > 0 {
		log(fmt.Sprintf("\nMatching directories (%d):", len(paths)))
		for _, p := range paths {
			log(p)
		}
	} else {
		log("\nMatching directories: 0")
	}
}

func (fr *FileResults) GetFileCountMap() map[string]int {
	fileCountMap := make(map[string]int)
	for _, r := range fr.results {
		fileCountMap[r.String()]++
	}
	return fileCountMap
}

func (fr *FileResults) GetMatchingFiles() []string {
	fileCountMap := fr.GetFileCountMap()
	files := getSortedCountKeys(fileCountMap)
	return files
}

func (fr *FileResults) PrintMatchingFiles() {
	files := fr.GetMatchingFiles()
	if len(files) > 0 {
		log(fmt.Sprintf("\nMatching files (%d):", len(files)))
		for _, f := range files {
			log(f)
		}
	} else {
		log("\nMatching files: 0")
	}
}

type FileResult struct {
	Containers []string
	Path       string
	Name       string
	fileType   FileType
}

func NewFileResult(path string, name string, fileType FileType) *FileResult {
	return &FileResult{
		[]string{},
		path,
		name,
		fileType,
	}
}

func (fr *FileResult) AddContainer(c string) {
	fr.Containers = append(fr.Containers, c)
}

const containerSeparator = "!"

func (fr *FileResult) String() string {
	var buffer bytes.Buffer
	if len(fr.Containers) > 0 {
		buffer.WriteString(strings.Join(fr.Containers, containerSeparator))
		buffer.WriteString(containerSeparator)
	}
	path := normalizePath(fr.Path)
	if isDotDir(path) {
		buffer.WriteString(fmt.Sprintf("%s%c%s", path, os.PathSeparator, fr.Name))
	} else {
		buffer.WriteString(filepath.Join(fr.Path, fr.Name))
	}
	return buffer.String()
}
