package gofind

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"
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
	frs.FileResults = append(frs.FileResults, r)
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
	dirMap := make(map[string]bool)
	var dirs []string
	for _, r := range frs.FileResults {
		if _, value := dirMap[r.Path]; !value {
			dirMap[r.Path] = true
			dirs = append(dirs, r.Path)
		}
	}
	return dirs
}

func (frs *FileResults) PrintMatchingDirs(formatter *FileResultFormatter) {
	paths := frs.GetMatchingDirs()
	if len(paths) > 0 {
		Log(fmt.Sprintf("\nMatching directories (%d):", len(paths)))
		for _, p := range paths {
			Log(formatter.FormatPath(p))
		}
	} else {
		Log("\nMatching directories: 0")
	}
}

func (frs *FileResults) PrintMatchingFiles(formatter *FileResultFormatter) {
	if frs.IsEmpty() {
		Log("\nMatching files: 0")
	} else {
		Log(fmt.Sprintf("\nMatching files (%d):", frs.Len()))
		for _, r := range frs.FileResults {
			Log(formatter.FormatFileResult(r))
		}
	}
}

func (frs *FileResults) Sort(settings *FindSettings) {
	fileResultSorter := NewFileResultSorter(settings)
	fileResultSorter.Sort(frs.FileResults)
}

type FileResult struct {
	Containers []string
	Path       string
	Name       string
	FileType   FileType
	FileSize   int64
	LastMod    time.Time
}

func NewFileResult(path string, name string, fileType FileType, fileSize int64, lastMod time.Time) *FileResult {
	return &FileResult{
		[]string{},
		path,
		name,
		fileType,
		fileSize,
		lastMod,
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

type StringFormatter func(string) string

type FileResultFormatter struct {
	Settings       *FindSettings
	FormatPath     StringFormatter
	FormatFileName StringFormatter
}

func NewFileResultFormatter(settings *FindSettings) *FileResultFormatter {
	formatPath := func(path string) string { return path }
	formatFileName := func(fileName string) string { return fileName }
	f := &FileResultFormatter{
		settings,
		formatPath,
		formatFileName,
	}
	if settings.Colorize() {
		if !settings.InDirPatterns().IsEmpty() {
			formatPath = func(path string) string { return f.formatPathWithColor(path) }
		}
		if len(settings.InExtensions()) > 0 || !settings.InFilePatterns().IsEmpty() {
			formatFileName = func(fileName string) string { return f.formatFileNameWithColor(fileName) }
		}
		f = &FileResultFormatter{
			settings,
			formatPath,
			formatFileName,
		}
	}
	return f
}

func Colorize(s string, matchStartIndex int, matchEndIndex int) string {
	prefix := ""
	if matchStartIndex > 0 {
		prefix = s[0:matchStartIndex]
	}
	suffix := ""
	if matchEndIndex < len(s) {
		suffix = s[matchEndIndex:]
	}
	return prefix +
		ColorGreen +
		s[matchStartIndex:matchEndIndex] +
		ColorReset +
		suffix
}

func (f *FileResultFormatter) formatPathWithColor(path string) string {
	formattedPath := "."
	if path != "" {
		formattedPath = path
		it := f.Settings.InDirPatterns().Iterator()
		for it.Next() {
			p := it.Value()
			if match := p.FindStringIndex(formattedPath); match != nil {
				formattedPath = Colorize(formattedPath, match[0], match[1])
				break
			}
		}
	}
	return formattedPath
}

func (f *FileResultFormatter) formatFileNameWithColor(fileName string) string {
	formattedFileName := fileName
	it := f.Settings.InFilePatterns().Iterator()
	for it.Next() {
		p := it.Value()
		if match := p.FindStringIndex(formattedFileName); match != nil {
			formattedFileName = Colorize(formattedFileName, match[0], match[1])
			break
		}
	}
	if len(f.Settings.InExtensions()) > 0 {
		idx := strings.LastIndex(formattedFileName, ".")
		if idx > 0 && idx < len(formattedFileName)-1 {
			formattedFileName = Colorize(formattedFileName, idx+1, len(formattedFileName))
		}
	}
	return formattedFileName
}

func (f *FileResultFormatter) FormatFileResult(r *FileResult) string {
	path := f.FormatPath(r.Path)
	fileName := f.FormatFileName(r.Name)
	return filepath.Join(path, fileName)
}

// *****************************************************************************
// FileResultSorter
// *****************************************************************************

type FileResultSorter struct {
	Settings *FindSettings
}

func NewFileResultSorter(settings *FindSettings) *FileResultSorter {
	f := &FileResultSorter{
		settings,
	}
	return f
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

func (frs *FileResultSorter) CompareByPath(fr1, fr2 *FileResult) int {
	cmp := compareStrings(fr1.Path, fr2.Path, frs.Settings.sortCaseInsensitive)
	if cmp == 0 {
		return compareStrings(fr1.Name, fr2.Name, frs.Settings.sortCaseInsensitive)
	}
	return cmp
}

func (frs *FileResultSorter) getCompareByPath() func(fr1, fr2 *FileResult) int {
	if frs.Settings.sortDescending {
		return func(fr1, fr2 *FileResult) int {
			return frs.CompareByPath(fr2, fr1)
		}
	}
	return func(fr1, fr2 *FileResult) int {
		return frs.CompareByPath(fr1, fr2)
	}
}

func (frs *FileResultSorter) CompareByName(fr1, fr2 *FileResult) int {
	cmp := compareStrings(fr1.Name, fr2.Name, frs.Settings.sortCaseInsensitive)
	if cmp == 0 {
		return compareStrings(fr1.Path, fr2.Path, frs.Settings.sortCaseInsensitive)
	}
	return cmp
}

func (frs *FileResultSorter) getCompareByName() func(fr1, fr2 *FileResult) int {
	if frs.Settings.sortDescending {
		return func(fr1, fr2 *FileResult) int {
			return frs.CompareByName(fr2, fr1)
		}
	}
	return func(fr1, fr2 *FileResult) int {
		return frs.CompareByName(fr1, fr2)
	}
}

func (frs *FileResultSorter) CompareBySize(fr1, fr2 *FileResult) int {
	if fr1.FileSize == fr2.FileSize {
		return frs.CompareByPath(fr1, fr2)
	}
	if fr1.FileSize < fr2.FileSize {
		return -1
	}
	return 1
}

func (frs *FileResultSorter) getCompareBySize() func(fr1, fr2 *FileResult) int {
	if frs.Settings.sortDescending {
		return func(fr1, fr2 *FileResult) int {
			return frs.CompareBySize(fr2, fr1)
		}
	}
	return func(fr1, fr2 *FileResult) int {
		return frs.CompareBySize(fr1, fr2)
	}
}

func (frs *FileResultSorter) CompareByType(fr1, fr2 *FileResult) int {
	if fr1.FileType == fr2.FileType {
		return frs.CompareByPath(fr1, fr2)
	}
	if fr1.FileType < fr2.FileType {
		return -1
	}
	return 1
}

func (frs *FileResultSorter) getCompareByType() func(fr1, fr2 *FileResult) int {
	if frs.Settings.sortDescending {
		return func(fr1, fr2 *FileResult) int {
			return frs.CompareByType(fr2, fr1)
		}
	}
	return func(fr1, fr2 *FileResult) int {
		return frs.CompareByType(fr1, fr2)
	}
}

func (frs *FileResultSorter) CompareByLastMod(fr1, fr2 *FileResult) int {
	if fr1.LastMod.Equal(fr2.LastMod) {
		return frs.CompareByPath(fr1, fr2)
	}
	if fr1.LastMod.Before(fr2.LastMod) {
		return -1
	}
	return 1
}

func (frs *FileResultSorter) getCompareByLastMod() func(fr1, fr2 *FileResult) int {
	if frs.Settings.sortDescending {
		return func(fr1, fr2 *FileResult) int {
			return frs.CompareByLastMod(fr2, fr1)
		}
	}
	return func(fr1, fr2 *FileResult) int {
		return frs.CompareByLastMod(fr1, fr2)
	}
}

func (frs *FileResultSorter) getSortFunc() func(fr1, fr2 *FileResult) int {
	settings := frs.Settings
	switch settings.SortBy() {
	case SortByFileName:
		return frs.getCompareByName()
	case SortByFileSize:
		return frs.getCompareBySize()
	case SortByFileType:
		return frs.getCompareByType()
	case SortByLastMod:
		return frs.getCompareByLastMod()
	default:
		return frs.getCompareByPath()
	}
}

func (frs *FileResultSorter) Sort(fileResults []*FileResult) {
	sortFunc := frs.getSortFunc()
	slices.SortFunc(fileResults, sortFunc)
}
