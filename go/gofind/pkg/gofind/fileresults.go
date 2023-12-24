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
	if it.idx >= len(it.results.FileResults) {
		return false
	}
	return true
}

func (it *FileResultsIterator) Value() *FileResult {
	return it.results.FileResults[it.idx]
}

func (it *FileResultsIterator) Take(count int) []*FileResult {
	if it.idx < 0 {
		it.idx = 0
	}
	maxCount := GetMinInt(count, len(it.results.FileResults)-it.idx)
	fileResults := it.results.FileResults[it.idx : maxCount+it.idx]
	it.idx += maxCount
	return fileResults
}

type FileResults struct {
	FileResults []*FileResult
	strPtrMap   map[string]*string
}

func NewFileResults() *FileResults {
	return &FileResults{
		[]*FileResult{},
		make(map[string]*string),
	}
}

// limits string pointers to one per distinct string (memory management)
func (frs *FileResults) getStrPtr(s *string) *string {
	strPtr := s
	if sp, ok := frs.strPtrMap[*s]; ok {
		strPtr = sp
	} else {
		frs.strPtrMap[*s] = s
	}
	return strPtr
}

func (frs *FileResults) AddResult(r *FileResult) {
	frs.FileResults = append(frs.FileResults, &FileResult{
		r.Containers,
		r.Path,
		r.Name,
		r.FileType,
		r.FileInfo,
	})
}

func (frs *FileResults) Len() int {
	return len(frs.FileResults)
}

func (frs *FileResults) IsEmpty() bool {
	return len(frs.FileResults) == 0
}

func (frs *FileResults) Index(fr *FileResult) int {
	for i, _ := range frs.FileResults {
		if fr.Path == frs.FileResults[i].Path && fr.Name == frs.FileResults[i].Name {
			return i
		}
	}
	return -1
}

func (frs *FileResults) Iterator() *FileResultsIterator {
	return NewFileResultsIterator(frs)
}

func (frs *FileResults) GetMatchingDirs() []string {
	keys := make(map[string]bool)
	var dirs []string
	for _, r := range frs.FileResults {
		if _, value := keys[r.Path]; !value {
			keys[r.Path] = true
			dirs = append(dirs, r.Path)
		}
	}
	return dirs
}

func (frs *FileResults) PrintMatchingDirs() {
	paths := frs.GetMatchingDirs()
	if len(paths) > 0 {
		Log(fmt.Sprintf("\nMatching directories (%d):", len(paths)))
		for _, p := range paths {
			Log(p)
		}
	} else {
		Log("\nMatching directories: 0")
	}
}

func (frs *FileResults) GetMatchingFiles() []string {
	var files []string
	for _, r := range frs.FileResults {
		files = append(files, r.String())
	}
	return files
}

func (frs *FileResults) PrintMatchingFiles() {
	files := frs.GetMatchingFiles()
	if len(files) > 0 {
		Log(fmt.Sprintf("\nMatching files (%d):", len(files)))
		for _, f := range files {
			Log(f)
		}
	} else {
		Log("\nMatching files: 0")
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

func (frs *FileResults) getSortByPath(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		pres := compareStrings(frs.FileResults[i].Path, frs.FileResults[j].Path, sortCaseInsensitive)
		if pres == 0 {
			fres := compareStrings(frs.FileResults[i].Name, frs.FileResults[j].Name, sortCaseInsensitive)
			return fres < 0
		}
		return pres < 0
	}
}

func (frs *FileResults) getSortByName(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		fres := compareStrings(frs.FileResults[i].Name, frs.FileResults[j].Name, sortCaseInsensitive)
		if fres == 0 {
			pres := compareStrings(frs.FileResults[i].Path, frs.FileResults[j].Path, sortCaseInsensitive)
			return pres < 0
		}
		return fres < 0
	}
}

func (frs *FileResults) getSortBySize(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		if frs.FileResults[i].FileInfo.Size() == frs.FileResults[j].FileInfo.Size() {
			sortByPath := frs.getSortByPath(sortCaseInsensitive)
			return sortByPath(i, j)
		}
		return frs.FileResults[i].FileInfo.Size() < frs.FileResults[j].FileInfo.Size()
	}
}

func (frs *FileResults) getSortByType(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		if frs.FileResults[i].FileType == frs.FileResults[j].FileType {
			sortByPath := frs.getSortByPath(sortCaseInsensitive)
			return sortByPath(i, j)
		}
		return frs.FileResults[i].FileType < frs.FileResults[j].FileType
	}
}

func (frs *FileResults) getSortByLastMod(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		if frs.FileResults[i].FileInfo.ModTime().Equal(frs.FileResults[j].FileInfo.ModTime()) {
			sortByPath := frs.getSortByPath(sortCaseInsensitive)
			return sortByPath(i, j)
		}
		return frs.FileResults[i].FileInfo.ModTime().Before(frs.FileResults[j].FileInfo.ModTime())
	}
}

func (frs *FileResults) Sort(settings *FindSettings) {
	switch settings.SortBy() {
	case SortByFileName:
		sort.Slice(frs.FileResults, frs.getSortByName(settings.SortCaseInsensitive()))
	case SortByFileSize:
		sort.Slice(frs.FileResults, frs.getSortBySize(settings.SortCaseInsensitive()))
	case SortByFileType:
		sort.Slice(frs.FileResults, frs.getSortByType(settings.SortCaseInsensitive()))
	case SortByLastMod:
		sort.Slice(frs.FileResults, frs.getSortByLastMod(settings.SortCaseInsensitive()))
	default:
		sort.Slice(frs.FileResults, frs.getSortByPath(settings.SortCaseInsensitive()))
	}
	if settings.SortDescending() {
		frs.reverse()
	}
}

func (frs *FileResults) reverse() {
	for i, j := 0, len(frs.FileResults)-1; i < j; i, j = i+1, j-1 {
		frs.FileResults[i], frs.FileResults[j] = frs.FileResults[j], frs.FileResults[i]
	}
}

type FileResult struct {
	Containers []string
	Path       string
	Name       string
	FileType   FileType
	FileInfo   os.FileInfo
}

func NewFileResult(path string, name string, fileType FileType, fi os.FileInfo) *FileResult {
	return &FileResult{
		[]string{},
		path,
		name,
		fileType,
		fi,
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
