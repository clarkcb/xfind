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
	keys := make(map[string]bool)
	var dirs []string
	for _, r := range fr.results {
		if _, value := keys[r.Path]; !value {
			keys[r.Path] = true
			dirs = append(dirs, r.Path)
		}
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

func compareStrings(str1, str2 string, sortCaseInsensitive bool) int {
	s1 := str1
	if sortCaseInsensitive {
		s1 = strings.ToLower(s1)
	}
	s2 := str2
	if sortCaseInsensitive {
		s2 = strings.ToLower(s2)
	}
	if s1 == s2 {
		return 0
	}
	if s1 < s2 {
		return -1
	}
	return 1
}

func (fr *FileResults) getSortByPath(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		pres := compareStrings(fr.results[i].Path, fr.results[j].Path, sortCaseInsensitive)
		if pres == 0 {
			fres := compareStrings(fr.results[i].Name, fr.results[j].Name, sortCaseInsensitive)
			return fres < 0
		}
		return pres < 0
	}
}

func (fr *FileResults) getSortByName(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		fres := compareStrings(fr.results[i].Name, fr.results[j].Name, sortCaseInsensitive)
		if fres == 0 {
			pres := compareStrings(fr.results[i].Path, fr.results[j].Path, sortCaseInsensitive)
			return pres < 0
		}
		return fres < 0
	}
}

func (fr *FileResults) getSortByType(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		if fr.results[i].FileType == fr.results[j].FileType {
			sortByPath := fr.getSortByPath(sortCaseInsensitive)
			return sortByPath(i, j)
		}
		return fr.results[i].FileType < fr.results[j].FileType
	}
}

func (fr *FileResults) Sort(settings *FindSettings) {
	switch settings.SortBy {
	case SortByFilename:
		sort.Slice(fr.results, fr.getSortByName(settings.SortCaseInsensitive))
	case SortByFiletype:
		sort.Slice(fr.results, fr.getSortByType(settings.SortCaseInsensitive))
	default:
		sort.Slice(fr.results, fr.getSortByPath(settings.SortCaseInsensitive))
	}
	if settings.SortDescending {
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
