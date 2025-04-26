package gofind

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

type SortBy int

const (
	SortByFilePathName = "filepath"
	SortByFileNameName = "filename"
	SortByNameName     = "name"
	SortByFileSizeName = "filesize"
	SortBySizeName     = "size"
	SortByFileTypeName = "filetype"
	SortByTypeName     = "type"
	SortByLastModName  = "lastmod"
)

const (
	SortByFilePath SortBy = iota
	SortByFileName SortBy = iota
	SortByFileSize SortBy = iota
	SortByFileType SortBy = iota
	SortByLastMod  SortBy = iota
)

func SortByForName(name string) SortBy {
	lname := strings.ToLower(name)
	if lname == SortByFileNameName || lname == SortByNameName {
		return SortByFileName
	}
	if lname == SortByFileSizeName || lname == SortBySizeName {
		return SortByFileSize
	}
	if lname == SortByFileTypeName || lname == SortByTypeName {
		return SortByFileType
	}
	if lname == SortByLastModName {
		return SortByLastMod
	}
	return SortByFilePath
}

func NameForSortBy(sortBy SortBy) string {
	if sortBy == SortByFileName {
		return SortByFileNameName
	}
	if sortBy == SortByFileSize {
		return SortByFileSizeName
	}
	if sortBy == SortByFileType {
		return SortByFileTypeName
	}
	if sortBy == SortByLastMod {
		return SortByLastModName
	}
	return SortByFilePathName
}

// FindSettings - the settings for the find session
type FindSettings struct {
	archivesOnly           bool
	debug                  bool
	followSymlinks         bool
	inArchiveExtensions    []string
	inArchiveFilePatterns  *Patterns
	inDirPatterns          *Patterns
	inExtensions           []string
	inFilePatterns         *Patterns
	inFileTypes            []FileType
	includeArchives        bool
	includeHidden          bool
	maxDepth               int
	maxLastMod             time.Time
	maxSize                int64
	minDepth               int
	minLastMod             time.Time
	minSize                int64
	outArchiveExtensions   []string
	outArchiveFilePatterns *Patterns
	outDirPatterns         *Patterns
	outExtensions          []string
	outFilePatterns        *Patterns
	outFileTypes           []FileType
	paths                  []string
	printDirs              bool
	printFiles             bool
	printUsage             bool
	printVersion           bool
	recursive              bool
	sortBy                 SortBy
	sortCaseInsensitive    bool
	sortDescending         bool
	verbose                bool
}

func GetDefaultFindSettings() *FindSettings {
	return &FindSettings{
		false,          // ArchivesOnly
		false,          // Debug
		false,          // FollowSymlinks
		[]string{},     // InArchiveExtensions
		NewPatterns(),  // InArchiveFilePatterns
		NewPatterns(),  // InDirPatterns
		[]string{},     // InExtensions
		NewPatterns(),  // InFilePatterns
		[]FileType{},   // InFileTypes
		false,          // IncludeArchives
		false,          // IncludeHidden
		-1,             // MaxDepth
		time.Time{},    // MaxLastMod
		0,              // MaxSize
		-1,             // MinDepth
		time.Time{},    // MinLastMod
		0,              // MinSize
		[]string{},     // OutArchiveExtensions
		NewPatterns(),  // OutArchiveFilePatterns
		NewPatterns(),  // OutDirPatterns
		[]string{},     // OutExtensions
		NewPatterns(),  // OutFilePatterns
		[]FileType{},   // OutFileTypes
		[]string{},     // Paths
		false,          // PrintDirs
		false,          // PrintFiles
		false,          // PrintUsage
		false,          // PrintVersion
		true,           // Recursive
		SortByFilePath, // SortBy
		false,          // SortCaseInsensitive
		false,          // SortDescending
		false,          // Verbose
	}
}

const (
	startPathNotDefined                    = "Startpath not defined"
	startPathNotFound                      = "Startpath not found"
	startPathNotReadable                   = "Startpath not readable"
	invalidRangeForMinDepthAndMaxDepth     = "Invalid range for mindepth and maxdepth"
	invalidRangeForMinLastModAndMaxLastMod = "Invalid range for minlastmod and maxlastmod"
	invalidRangeForMinSizeAndMaxSize       = "Invalid range for minsize and maxsize"
)

func (f *FindSettings) Validate() error {
	if len(f.Paths()) < 1 {
		return fmt.Errorf(startPathNotDefined)
	}

	for _, p := range f.Paths() {
		_, err := os.Stat(ExpandPath(p))
		if err != nil {
			if os.IsNotExist(err) {
				return fmt.Errorf(startPathNotFound)
			}
			if os.IsPermission(err) {
				return fmt.Errorf(startPathNotReadable)
			}
			return err
		}
	}

	if f.maxDepth > -1 && f.maxDepth < f.minDepth {
		return fmt.Errorf(invalidRangeForMinDepthAndMaxDepth)
	}
	if !f.maxLastMod.IsZero() && f.minLastMod.After(f.maxLastMod) {
		return fmt.Errorf(invalidRangeForMinLastModAndMaxLastMod)
	}
	if f.maxSize > 0 && f.maxSize < f.minSize {
		return fmt.Errorf(invalidRangeForMinSizeAndMaxSize)
	}

	return nil
}

func (f *FindSettings) ArchivesOnly() bool {
	return f.archivesOnly
}

func (f *FindSettings) SetArchivesOnly(archivesOnly bool) {
	f.archivesOnly = archivesOnly
	if archivesOnly {
		f.includeArchives = true
	}
}

func (f *FindSettings) Debug() bool {
	return f.debug
}

func (f *FindSettings) SetDebug(debug bool) {
	f.debug = debug
	if debug {
		f.verbose = true
	}
}

func (f *FindSettings) FollowSymlinks() bool {
	return f.followSymlinks
}

func (f *FindSettings) SetFollowSymlinks(b bool) {
	f.followSymlinks = b
}

func (f *FindSettings) InArchiveExtensions() []string {
	return f.inArchiveExtensions
}

func (f *FindSettings) AddInArchiveExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			f.inArchiveExtensions = append(f.inArchiveExtensions, ext)
		}
	}
}

func (f *FindSettings) InArchiveFilePatterns() *Patterns {
	return f.inArchiveFilePatterns
}

func (f *FindSettings) AddInArchiveFilePattern(p string) {
	addPattern(p, f.inArchiveFilePatterns)
}

func (f *FindSettings) InDirPatterns() *Patterns {
	return f.inDirPatterns
}

func (f *FindSettings) AddInDirPattern(p string) {
	addPattern(p, f.inDirPatterns)
}

func (f *FindSettings) InExtensions() []string {
	return f.inExtensions
}

func (f *FindSettings) AddInExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			f.inExtensions = append(f.inExtensions, ext)
		}
	}
}

func (f *FindSettings) InFilePatterns() *Patterns {
	return f.inFilePatterns
}

func (f *FindSettings) AddInFilePattern(p string) {
	addPattern(p, f.inFilePatterns)
}

func (f *FindSettings) InFileTypes() []FileType {
	return f.inFileTypes
}

func (f *FindSettings) AddInFileType(t FileType) {
	f.inFileTypes = append(f.inFileTypes, t)
}

func (f *FindSettings) IncludeArchives() bool {
	return f.includeArchives
}

func (f *FindSettings) SetIncludeArchives(b bool) {
	f.includeArchives = b
}

func (f *FindSettings) IncludeHidden() bool {
	return f.includeHidden
}

func (f *FindSettings) SetIncludeHidden(b bool) {
	f.includeHidden = b
}

func (f *FindSettings) MaxDepth() int {
	return f.maxDepth
}

func (f *FindSettings) SetMaxDepth(i int) {
	f.maxDepth = i
}

func (f *FindSettings) SetMaxDepthFromString(depthStr string) {
	depth, err := strconv.Atoi(depthStr)
	if err != nil {
		depth = 0
	}
	f.maxDepth = depth
}

func (f *FindSettings) MaxLastMod() time.Time {
	return f.maxLastMod
}

func (f *FindSettings) SetMaxLastMod(t time.Time) {
	f.maxLastMod = t
}

func (f *FindSettings) SetMaxLastModFromString(timeStr string) {
	f.maxLastMod = f.getLastMod(timeStr)
}

func (f *FindSettings) MaxSize() int64 {
	return f.maxSize
}

func (f *FindSettings) SetMaxSize(i int64) {
	f.maxSize = i
}

func (f *FindSettings) SetMaxSizeFromString(sizeStr string) {
	f.maxSize = f.getSize(sizeStr)
}

func (f *FindSettings) MinDepth() int {
	return f.minDepth
}

func (f *FindSettings) SetMinDepth(i int) {
	f.minDepth = i
}

func (f *FindSettings) SetMinDepthFromString(depthStr string) {
	depth, err := strconv.Atoi(depthStr)
	if err != nil {
		depth = 0
	}
	f.minDepth = depth
}

func (f *FindSettings) MinLastMod() time.Time {
	return f.minLastMod
}

func (f *FindSettings) SetMinLastMod(t time.Time) {
	f.minLastMod = t
}

func (f *FindSettings) SetMinLastModFromString(timeStr string) {
	f.minLastMod = f.getLastMod(timeStr)
}

func (f *FindSettings) MinSize() int64 {
	return f.minSize
}

func (f *FindSettings) SetMinSize(i int64) {
	f.minSize = i
}

func (f *FindSettings) SetMinSizeFromString(sizeStr string) {
	f.minSize = f.getSize(sizeStr)
}

func (f *FindSettings) OutArchiveExtensions() []string {
	return f.outArchiveExtensions
}

func (f *FindSettings) AddOutArchiveExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			f.outArchiveExtensions = append(f.outArchiveExtensions, ext)
		}
	}
}

func (f *FindSettings) OutArchiveFilePatterns() *Patterns {
	return f.outArchiveFilePatterns
}

func (f *FindSettings) AddOutArchiveFilePattern(p string) {
	addPattern(p, f.outArchiveFilePatterns)
}

func (f *FindSettings) OutDirPatterns() *Patterns {
	return f.outDirPatterns
}

func (f *FindSettings) AddOutDirPattern(p string) {
	addPattern(p, f.outDirPatterns)
}

func (f *FindSettings) OutExtensions() []string {
	return f.outExtensions
}

func (f *FindSettings) AddOutExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			f.outExtensions = append(f.outExtensions, ext)
		}
	}
}

func (f *FindSettings) OutFilePatterns() *Patterns {
	return f.outFilePatterns
}

func (f *FindSettings) AddOutFilePattern(p string) {
	addPattern(p, f.outFilePatterns)
}

func (f *FindSettings) OutFileTypes() []FileType {
	return f.outFileTypes
}

func (f *FindSettings) AddOutFileType(t FileType) {
	f.outFileTypes = append(f.outFileTypes, t)
}

func (f *FindSettings) Paths() []string {
	return f.paths
}

func (f *FindSettings) AddPath(p string) {
	f.paths = append(f.paths, p)
}

func (f *FindSettings) PrintDirs() bool {
	return f.printDirs
}

func (f *FindSettings) SetPrintDirs(b bool) {
	f.printDirs = b
}

func (f *FindSettings) PrintFiles() bool {
	return f.printFiles
}

func (f *FindSettings) SetPrintFiles(b bool) {
	f.printFiles = b
}

func (f *FindSettings) PrintUsage() bool {
	return f.printUsage
}

func (f *FindSettings) SetPrintUsage(b bool) {
	f.printUsage = b
}

func (f *FindSettings) PrintVersion() bool {
	return f.printVersion
}

func (f *FindSettings) SetPrintVersion(b bool) {
	f.printVersion = b
}

func (f *FindSettings) Recursive() bool {
	return f.recursive
}

func (f *FindSettings) SetRecursive(b bool) {
	f.recursive = b
}

func (f *FindSettings) SortBy() SortBy {
	return f.sortBy
}

func (f *FindSettings) SetSortBy(sortBy SortBy) {
	f.sortBy = sortBy
}

func (f *FindSettings) SetSortByFromString(sortByStr string) {
	f.sortBy = SortByForName(sortByStr)
}

func (f *FindSettings) SortCaseInsensitive() bool {
	return f.sortCaseInsensitive
}

func (f *FindSettings) SetSortCaseInsensitive(b bool) {
	f.sortCaseInsensitive = b
}

func (f *FindSettings) SortDescending() bool {
	return f.sortDescending
}

func (f *FindSettings) SetSortDescending(b bool) {
	f.sortDescending = b
}

func (f *FindSettings) Verbose() bool {
	return f.verbose
}

func (f *FindSettings) SetVerbose(b bool) {
	f.verbose = b
}

func (f *FindSettings) getLastMod(timeStr string) time.Time {
	t, err := time.Parse(time.RFC3339, timeStr)
	if err != nil {
		t, err = time.Parse("2006-01-02", timeStr)
	}
	if err != nil {
		//fmt.Print(fmt.Sprintf("error: %s", err))
		return time.Time{}
	}
	return t
}

func (f *FindSettings) getSize(sizeStr string) int64 {
	size, err := strconv.ParseInt(sizeStr, 0, 64)
	if err != nil {
		//fmt.Print(fmt.Sprintf("error: %s", err))
		return 0
	}
	return size
}

func LastModToString(t time.Time) string {
	if t.IsZero() {
		return "0"
	}
	return fmt.Sprintf("\"%s\"", t.String())
}

func addPattern(p string, sp *Patterns) {
	sp.AddPatternString(p)
}

func (f *FindSettings) String() string {
	const template = "FindSettings(" +
		"ArchivesOnly=%t" +
		", Debug=%t" +
		", FollowSymlinks=%t" +
		", InArchiveExtensions=%s" +
		", InArchiveFilePatterns=%s" +
		", InDirPatterns=%s" +
		", InExtensions=%s" +
		", InFilePatterns=%s" +
		", InFileTypes=%s" +
		", IncludeArchives=%t" +
		", IncludeHidden=%t" +
		", MaxDepth=%d" +
		", MaxLastMod=%s" +
		", MaxSize=%d" +
		", MinDepth=%d" +
		", MinLastMod=%s" +
		", MinSize=%d" +
		", OutArchiveExtensions=%s" +
		", OutArchiveFilePatterns=%s" +
		", OutDirPatterns=%s" +
		", OutExtensions=%s" +
		", OutFilePatterns=%s" +
		", OutFileTypes=%s" +
		", Paths=%s" +
		", PrintDirs=%t" +
		", PrintFiles=%t" +
		", PrintUsage=%t" +
		", PrintVersion=%t" +
		", Recursive=%t" +
		", SortBy=%s" +
		", SortCaseInsensitive=%t" +
		", SortDescending=%t" +
		", Verbose=%t)"
	return fmt.Sprintf(template,
		f.ArchivesOnly(),
		f.Debug(),
		f.FollowSymlinks(),
		StringListToString(f.InArchiveExtensions()),
		PatternsToString(f.InArchiveFilePatterns()),
		PatternsToString(f.InDirPatterns()),
		StringListToString(f.InExtensions()),
		PatternsToString(f.InFilePatterns()),
		FileTypeListToString(f.InFileTypes()),
		f.IncludeArchives(),
		f.IncludeHidden(),
		f.MaxDepth(),
		LastModToString(f.MaxLastMod()),
		f.MaxSize(),
		f.MinDepth(),
		LastModToString(f.MinLastMod()),
		f.MinSize(),
		StringListToString(f.OutArchiveExtensions()),
		PatternsToString(f.OutArchiveFilePatterns()),
		PatternsToString(f.OutDirPatterns()),
		StringListToString(f.OutExtensions()),
		PatternsToString(f.OutFilePatterns()),
		FileTypeListToString(f.OutFileTypes()),
		StringListToString(f.Paths()),
		f.PrintDirs(),
		f.PrintFiles(),
		f.PrintUsage(),
		f.PrintVersion(),
		f.Recursive(),
		NameForSortBy(f.SortBy()),
		f.SortCaseInsensitive(),
		f.SortDescending(),
		f.Verbose(),
	)
}
