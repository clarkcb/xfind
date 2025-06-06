package gofind

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
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

func (frs *FileResults) CompareByPath(fr1, fr2 *FileResult, sortCaseInsensitive bool) int {
	pres := compareStrings(fr1.Path, fr2.Path, sortCaseInsensitive)
	if pres == 0 {
		return compareStrings(fr1.Name, fr2.Name, sortCaseInsensitive)
	}
	return pres
}

func (frs *FileResults) getSortByPath(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return frs.CompareByPath(frs.FileResults[j], frs.FileResults[i], sortCaseInsensitive) < 0
		}
	}
	return func(i, j int) bool {
		return frs.CompareByPath(frs.FileResults[i], frs.FileResults[j], sortCaseInsensitive) < 0
	}
}

func (frs *FileResults) CompareByName(fr1, fr2 *FileResult, sortCaseInsensitive bool) int {
	nres := compareStrings(fr1.Name, fr2.Name, sortCaseInsensitive)
	if nres == 0 {
		return compareStrings(fr1.Path, fr2.Path, sortCaseInsensitive)
	}
	return nres
}

func (frs *FileResults) getSortByName(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return frs.CompareByName(frs.FileResults[j], frs.FileResults[i], sortCaseInsensitive) < 0
		}
	}
	return func(i, j int) bool {
		return frs.CompareByName(frs.FileResults[i], frs.FileResults[j], sortCaseInsensitive) < 0
	}
}

func (frs *FileResults) CompareBySize(fr1, fr2 *FileResult, sortCaseInsensitive bool) int {
	if fr1.FileSize == fr2.FileSize {
		return frs.CompareByPath(fr1, fr2, sortCaseInsensitive)
	}
	if fr1.FileSize < fr2.FileSize {
		return -1
	}
	return 1
}

func (frs *FileResults) getSortBySize(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return frs.CompareBySize(frs.FileResults[j], frs.FileResults[i], sortCaseInsensitive) < 0
		}
	}
	return func(i, j int) bool {
		return frs.CompareBySize(frs.FileResults[i], frs.FileResults[j], sortCaseInsensitive) < 0
	}
}

func (frs *FileResults) CompareByType(fr1, fr2 *FileResult, sortCaseInsensitive bool) int {
	if fr1.FileType == fr2.FileType {
		return frs.CompareByPath(fr1, fr2, sortCaseInsensitive)
	}
	if fr1.FileType < fr2.FileType {
		return -1
	}
	return 1
}

func (frs *FileResults) getSortByType(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return frs.CompareByType(frs.FileResults[j], frs.FileResults[i], sortCaseInsensitive) < 0
		}
	}
	return func(i, j int) bool {
		return frs.CompareByType(frs.FileResults[i], frs.FileResults[j], sortCaseInsensitive) < 0
	}
}

func (frs *FileResults) CompareByLastMod(fr1, fr2 *FileResult, sortCaseInsensitive bool) int {
	if fr1.LastMod.Equal(fr2.LastMod) {
		return frs.CompareByPath(fr1, fr2, sortCaseInsensitive)
	}
	if fr1.LastMod.Before(fr2.LastMod) {
		return -1
	}
	return 1
}

func (frs *FileResults) getSortByLastMod(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return frs.CompareByLastMod(frs.FileResults[j], frs.FileResults[i], sortCaseInsensitive) < 0
		}
	}
	return func(i, j int) bool {
		return frs.CompareByLastMod(frs.FileResults[i], frs.FileResults[j], sortCaseInsensitive) < 0
	}
}

func (frs *FileResults) getSortComparator(settings *FindSettings) func(i, j int) bool {
	switch settings.SortBy() {
	case SortByFileName:
		return frs.getSortByName(settings.SortCaseInsensitive(), settings.SortDescending())
	case SortByFileSize:
		return frs.getSortBySize(settings.SortCaseInsensitive(), settings.SortDescending())
	case SortByFileType:
		return frs.getSortByType(settings.SortCaseInsensitive(), settings.SortDescending())
	case SortByLastMod:
		return frs.getSortByLastMod(settings.SortCaseInsensitive(), settings.SortDescending())
	default:
		return frs.getSortByPath(settings.SortCaseInsensitive(), settings.SortDescending())
	}
}

func (frs *FileResults) Sort(settings *FindSettings) {
	sortComparator := frs.getSortComparator(settings)
	sort.Slice(frs.FileResults, sortComparator)
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
			formatPath = func(path string) string { return f.FormatPathWithColor(path) }
		}
		if len(settings.InExtensions()) > 0 || !settings.InFilePatterns().IsEmpty() {
			formatFileName = func(fileName string) string { return f.FormatFileNameWithColor(fileName) }
		}
		return &FileResultFormatter{
			settings,
			formatPath,
			formatFileName,
		}
	}
	return f
}

func (f *FileResultFormatter) getFormatPath() StringFormatter {
	if f.Settings.Colorize() && !f.Settings.InDirPatterns().IsEmpty() {
		return func(p string) string {
			return f.FormatPathWithColor(p)
		}
	}
	return func(p string) string {
		return p
	}
}

func colorize(s string, matchStartIndex int, matchEndIndex int) string {
	prefix := ""
	if matchStartIndex > 0 {
		prefix = s[0:matchStartIndex]
	}
	suffix := ""
	if matchEndIndex < len(s) {
		suffix = s[matchEndIndex:]
	}
	return prefix +
		COLOR_GREEN +
		s[matchStartIndex:matchEndIndex] +
		COLOR_RESET +
		suffix
}

func (f *FileResultFormatter) FormatPathWithColor(path string) string {
	formattedPath := "."
	if path != "" {
		formattedPath = path
		it := f.Settings.InDirPatterns().Iterator()
		for it.Next() {
			p := it.Value()
			if match := p.FindStringIndex(formattedPath); match != nil {
				formattedPath = colorize(formattedPath, match[0], match[1])
				break
			}
		}
	}
	return formattedPath
}

func (f *FileResultFormatter) FormatFileNameWithColor(fileName string) string {
	formattedFileName := fileName
	it := f.Settings.InFilePatterns().Iterator()
	for it.Next() {
		p := it.Value()
		if match := p.FindStringIndex(formattedFileName); match != nil {
			formattedFileName = colorize(formattedFileName, match[0], match[1])
			break
		}
	}
	if len(f.Settings.InExtensions()) > 0 {
		idx := strings.Index(formattedFileName, ".")
		if idx > 0 && idx < len(formattedFileName)-1 {
			formattedFileName = colorize(formattedFileName, idx+1, len(formattedFileName))
		}
	}
	return formattedFileName
}

func (f *FileResultFormatter) FormatFileResult(r *FileResult) string {
	path := f.FormatPath(r.Path)
	fileName := f.FormatFileName(r.Name)
	return filepath.Join(path, fileName)
}
