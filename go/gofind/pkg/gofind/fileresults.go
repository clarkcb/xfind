package gofind

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
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
		r.FileType,
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

func (fr *FileResults) GetMatchingDirs() []string {
	var dirs []string
	for _, r := range fr.results {
		dirs = append(dirs, r.Path)
	}
	return dirs
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

func (fr *FileResults) GetMatchingFiles() []string {
	var files []string
	for _, r := range fr.results {
		files = append(files, r.String())
	}
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

func (fr *FileResults) sortByPath(i, j int) bool {
	if fr.results[i].Path == fr.results[j].Path {
		return fr.results[i].Name < fr.results[j].Name
	}
	return fr.results[i].Path < fr.results[j].Path
}

func (fr *FileResults) sortByName(i, j int) bool {
	if fr.results[i].Name == fr.results[j].Name {
		return fr.results[i].Path < fr.results[j].Path
	}
	return fr.results[i].Name < fr.results[j].Name
}

func (fr *FileResults) sortByType(i, j int) bool {
	if fr.results[i].FileType == fr.results[j].FileType {
		return fr.sortByPath(i, j)
	}
	return fr.results[i].FileType < fr.results[j].FileType
}

func (fr *FileResults) Sort(sortBy SortBy, sortDescending bool) {
	switch sortBy {
	case SortByFilename:
		sort.Slice(fr.results, fr.sortByName)
	case SortByFiletype:
		sort.Slice(fr.results, fr.sortByType)
	default:
		sort.Slice(fr.results, fr.sortByPath)
	}
	if sortDescending {
		fr.reverse()
	}
}

func (fr *FileResults) reverse() {
	for i, j := 0, len(fr.results)-1; i < j; i, j = i+1, j-1 {
		fr.results[i], fr.results[j] = fr.results[j], fr.results[i]
	}
}

type FileResult struct {
	Containers []string
	Path       string
	Name       string
	FileType   FileType
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
