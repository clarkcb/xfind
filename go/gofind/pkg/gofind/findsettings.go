package gofind

import (
	"fmt"
	"strconv"
	"strings"
	"time"
)

type SortBy int

const (
	SortByFilepath SortBy = iota
	SortByFilename SortBy = iota
	SortByFilesize SortBy = iota
	SortByFiletype SortBy = iota
	SortByLastmod  SortBy = iota
)

func getSortByForName(name string) SortBy {
	if strings.ToUpper(name) == "NAME" {
		return SortByFilename
	}
	if strings.ToUpper(name) == "SIZE" {
		return SortByFilesize
	}
	if strings.ToUpper(name) == "TYPE" {
		return SortByFiletype
	}
	if strings.ToUpper(name) == "LASTMOD" {
		return SortByLastmod
	}
	return SortByFilepath
}

func getNameForSortBy(sortBy SortBy) string {
	if sortBy == SortByFilename {
		return "NAME"
	}
	if sortBy == SortByFilesize {
		return "SIZE"
	}
	if sortBy == SortByFiletype {
		return "TYPE"
	}
	if sortBy == SortByLastmod {
		return "LASTMOD"
	}
	return "PATH"
}

// FindSettings - the settings for the find session
type FindSettings struct {
	ArchivesOnly           bool
	Debug                  bool
	ExcludeHidden          bool
	InArchiveExtensions    []string
	InArchiveFilePatterns  *FindPatterns
	InDirPatterns          *FindPatterns
	InExtensions           []string
	InFilePatterns         *FindPatterns
	InFileTypes            []FileType
	IncludeArchives        bool
	ListDirs               bool
	ListFiles              bool
	MaxLastMod             time.Time
	MaxSize                int64
	MinLastMod             time.Time
	MinSize                int64
	OutArchiveExtensions   []string
	OutArchiveFilePatterns *FindPatterns
	OutDirPatterns         *FindPatterns
	OutExtensions          []string
	OutFilePatterns        *FindPatterns
	OutFileTypes           []FileType
	Paths                  []string
	PrintUsage             bool
	PrintVersion           bool
	Recursive              bool
	SortBy                 SortBy
	SortCaseInsensitive    bool
	SortDescending         bool
	Verbose                bool
}

func GetDefaultFindSettings() *FindSettings {
	return &FindSettings{
		false,             // ArchivesOnly
		false,             // Debug
		true,              // ExcludeHidden
		[]string{},        // InArchiveExtensions
		NewFindPatterns(), // InArchiveFilePatterns
		NewFindPatterns(), // InDirPatterns
		[]string{},        // InExtensions
		NewFindPatterns(), // InFilePatterns
		[]FileType{},      // InFileTypes
		false,             // IncludeArchives
		false,             // ListDirs
		false,             // ListFiles
		time.Time{},       // MaxLastMod
		0,                 // MaxSize
		time.Time{},       // MinLastMod
		0,                 // MinSize
		[]string{},        // OutArchiveExtensions
		NewFindPatterns(), // OutArchiveFilePatterns
		NewFindPatterns(), // OutDirPatterns
		[]string{},        // OutExtensions
		NewFindPatterns(), // OutFilePatterns
		[]FileType{},      // OutFileTypes
		[]string{},        // Paths
		false,             // PrintUsage
		false,             // PrintVersion
		true,              // Recursive
		SortByFilepath,    // SortBy
		false,             // SortCaseInsensitive
		false,             // SortDescending
		false,             // Verbose
	}
}

func (f *FindSettings) AddInExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			f.InExtensions = append(f.InExtensions, ext)
		}
	}
}

func (f *FindSettings) AddOutExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			f.OutExtensions = append(f.OutExtensions, ext)
		}
	}
}

func addPattern(p string, sp *FindPatterns) {
	sp.AddPatternString(p)
}

func (f *FindSettings) AddInDirPattern(p string) {
	addPattern(p, f.InDirPatterns)
}

func (f *FindSettings) AddOutDirPattern(p string) {
	addPattern(p, f.OutDirPatterns)
}

func (f *FindSettings) AddInFilePattern(p string) {
	addPattern(p, f.InFilePatterns)
}

func (f *FindSettings) AddOutFilePattern(p string) {
	addPattern(p, f.OutFilePatterns)
}

func (f *FindSettings) AddInFileType(t FileType) {
	f.InFileTypes = append(f.InFileTypes, t)
}

func (f *FindSettings) AddOutFileType(t FileType) {
	f.OutFileTypes = append(f.OutFileTypes, t)
}

func (f *FindSettings) AddInArchiveExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		ext := strings.ToLower(x)
		f.InArchiveExtensions = append(f.InArchiveExtensions, ext)
	}
}

func (f *FindSettings) AddOutArchiveExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		ext := strings.ToLower(x)
		f.OutArchiveExtensions = append(f.OutArchiveExtensions, ext)
	}
}

func (f *FindSettings) AddInArchiveFilePattern(p string) {
	addPattern(p, f.InArchiveFilePatterns)
}

func (f *FindSettings) AddOutArchiveFilePattern(p string) {
	addPattern(p, f.OutArchiveFilePatterns)
}

func (f *FindSettings) AddPath(p string) {
	f.Paths = append(f.Paths, p)
}

func (f *FindSettings) SetArchivesOnly(archivesOnly bool) {
	f.ArchivesOnly = archivesOnly
	if archivesOnly {
		f.IncludeArchives = true
	}
}

func (f *FindSettings) SetDebug(debug bool) {
	f.Debug = debug
	if debug {
		f.Verbose = true
	}
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

func (f *FindSettings) SetMaxLastMod(timeStr string) {
	f.MaxLastMod = f.getLastMod(timeStr)
}

func (f *FindSettings) SetMinLastMod(timeStr string) {
	f.MinLastMod = f.getLastMod(timeStr)
}

func (f *FindSettings) getSize(sizeStr string) int64 {
	size, err := strconv.ParseInt(sizeStr, 0, 64)
	if err != nil {
		//fmt.Print(fmt.Sprintf("error: %s", err))
		return 0
	}
	return size
}

func (f *FindSettings) SetMaxSize(sizeStr string) {
	f.MaxSize = f.getSize(sizeStr)
}

func (f *FindSettings) SetMinSize(sizeStr string) {
	f.MinSize = f.getSize(sizeStr)
}

func (f *FindSettings) SetSortBy(sortBy string) {
	f.SortBy = getSortByForName(sortBy)
}

func lastModToString(t time.Time) string {
	if t.IsZero() {
		return "0"
	}
	return fmt.Sprintf("\"%s\"", t.String())
}

func (f *FindSettings) String() string {
	const template = "SearchSettings{" +
		"ArchivesOnly: %t" +
		", Debug: %t" +
		", ExcludeHidden: %t" +
		", InArchiveExtensions: %s" +
		", InArchiveFilePatterns: %s" +
		", InDirPatterns: %s" +
		", InExtensions: %s" +
		", InFilePatterns: %s" +
		", InFileTypes: %s" +
		", IncludeArchives: %t" +
		", ListDirs: %t" +
		", ListFiles: %t" +
		", MaxLastMod: %s" +
		", MaxSize: %d" +
		", MinLastMod: %s" +
		", MinSize: %d" +
		", OutArchiveExtensions: %s" +
		", OutArchiveFilePatterns: %s" +
		", OutDirPatterns: %s" +
		", OutExtensions: %s" +
		", OutFilePatterns: %s" +
		", OutFileTypes: %s" +
		", Paths: %s" +
		", PrintUsage: %t" +
		", PrintVersion: %t" +
		", Recursive: %t" +
		", SortBy: %s" +
		", SortCaseInsensitive: %t" +
		", SortDescending: %t" +
		", Verbose: %t}"
	return fmt.Sprintf(template,
		f.ArchivesOnly,
		f.Debug,
		f.ExcludeHidden,
		stringListToString(f.InArchiveExtensions),
		findPatternsToString(f.InArchiveFilePatterns),
		findPatternsToString(f.InDirPatterns),
		stringListToString(f.InExtensions),
		findPatternsToString(f.InFilePatterns),
		fileTypeListToString(f.InFileTypes),
		f.IncludeArchives,
		f.ListDirs,
		f.ListFiles,
		lastModToString(f.MaxLastMod),
		f.MaxSize,
		lastModToString(f.MinLastMod),
		f.MinSize,
		stringListToString(f.OutArchiveExtensions),
		findPatternsToString(f.OutArchiveFilePatterns),
		findPatternsToString(f.OutDirPatterns),
		stringListToString(f.OutExtensions),
		findPatternsToString(f.OutFilePatterns),
		fileTypeListToString(f.OutFileTypes),
		stringListToString(f.Paths),
		f.PrintUsage,
		f.PrintVersion,
		f.Recursive,
		getNameForSortBy(f.SortBy),
		f.SortCaseInsensitive,
		f.SortDescending,
		f.Verbose,
	)
}
