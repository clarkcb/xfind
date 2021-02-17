package gofind

import (
	"bytes"
	"fmt"
	"strings"
)

type FindSettings struct {
	StartPath               string
	TextFileEncoding        string
	InExtensions            []*string
	OutExtensions           []*string
	InDirPatterns           *FindPatterns
	OutDirPatterns          *FindPatterns
	InFilePatterns          *FindPatterns
	OutFilePatterns         *FindPatterns
	InFileTypes             []FileType
	OutFileTypes            []FileType
	InArchiveExtensions     []*string
	OutArchiveExtensions    []*string
	InArchiveFilePatterns   *FindPatterns
	OutArchiveFilePatterns  *FindPatterns
	InLinesAfterPatterns    *FindPatterns
	OutLinesAfterPatterns   *FindPatterns
	InLinesBeforePatterns   *FindPatterns
	OutLinesBeforePatterns  *FindPatterns
	LinesAfterToPatterns    *FindPatterns
	LinesAfterUntilPatterns *FindPatterns
	FindPatterns          *FindPatterns
	ArchivesOnly            bool
	Colorize                bool
	Debug                   bool
	ExcludeHidden           bool
	FirstMatch              bool
	LinesAfter              int
	LinesBefore             int
	ListDirs                bool
	ListFiles               bool
	ListLines               bool
	MaxLineLength           int
	MultiLineFind         bool
	PrintResults            bool
	PrintUsage              bool
	PrintVersion            bool
	Recursive               bool
	FindArchives          bool
	UniqueLines             bool
	Verbose                 bool
}

func GetDefaultFindSettings() *FindSettings {
	return &FindSettings{
		"",                  // StartPath
		"utf-8",             // StartPath
		[]*string{},         // InExtensions
		[]*string{},         // OutExtensions
		NewFindPatterns(), // InDirPatterns
		NewFindPatterns(), // OutDirPatterns
		NewFindPatterns(), // InFilePatterns
		NewFindPatterns(), // OutFilePatterns
		[]FileType{},        // InArchiveExtensions
		[]FileType{},        // OutArchiveExtensions
		[]*string{},         // InArchiveExtensions
		[]*string{},         // OutArchiveExtensions
		NewFindPatterns(), // InArchiveFilePatterns
		NewFindPatterns(), // OutArchiveFilePatterns
		NewFindPatterns(), // InLinesAfterPatterns
		NewFindPatterns(), // OutLinesAfterPatterns
		NewFindPatterns(), // InLinesBeforePatterns
		NewFindPatterns(), // OutLinesBeforePatterns
		NewFindPatterns(), // LinesAfterToPatterns
		NewFindPatterns(), // LinesAfterUntilPatterns
		NewFindPatterns(), // FindPatterns
		false,               // ArchivesOnly
		true,                // Debug
		false,               // Debug
		true,                // ExcludeHidden
		false,               // FirstMatch
		0,                   // LinesAfter
		0,                   // LinesBefore
		false,               // ListDirs
		false,               // ListFiles
		false,               // ListLines
		150,                 // MaxLineLength
		false,               // MultiLineFind
		true,                // PrintResults
		false,               // PrintUsage
		false,               // PrintVersion
		true,                // Recursive
		false,               // FindArchives
		false,               // UniqueLines
		false,               // Verbose
	}
}

func (s *FindSettings) AddInExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			s.InExtensions = append(s.InExtensions, &ext)
		}
	}
}

func (s *FindSettings) AddOutExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			s.OutExtensions = append(s.OutExtensions, &ext)
		}
	}
}

func addPattern(p *string, sp *FindPatterns) {
	sp.AddPattern(p)
}

func (s *FindSettings) AddInDirPattern(p string) {
	addPattern(&p, s.InDirPatterns)
}

func (s *FindSettings) AddOutDirPattern(p string) {
	addPattern(&p, s.OutDirPatterns)
}

func (s *FindSettings) AddInFilePattern(p string) {
	addPattern(&p, s.InFilePatterns)
}

func (s *FindSettings) AddOutFilePattern(p string) {
	addPattern(&p, s.OutFilePatterns)
}

func (s *FindSettings) AddInFileType(t FileType) {
	s.InFileTypes = append(s.InFileTypes, t)
}

func (s *FindSettings) AddOutFileType(t FileType) {
	s.OutFileTypes = append(s.OutFileTypes, t)
}

func (s *FindSettings) AddInArchiveExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		ext := strings.ToLower(x)
		s.InArchiveExtensions = append(s.InArchiveExtensions, &ext)
	}
}

func (s *FindSettings) AddOutArchiveExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		ext := strings.ToLower(x)
		s.OutArchiveExtensions = append(s.OutArchiveExtensions, &ext)
	}
}

func (s *FindSettings) AddInArchiveFilePattern(p string) {
	addPattern(&p, s.InArchiveFilePatterns)
}

func (s *FindSettings) AddOutArchiveFilePattern(p string) {
	addPattern(&p, s.OutArchiveFilePatterns)
}

func (s *FindSettings) AddInLinesBeforePattern(p string) {
	addPattern(&p, s.InLinesBeforePatterns)
}

func (s *FindSettings) AddOutLinesBeforePattern(p string) {
	addPattern(&p, s.OutLinesBeforePatterns)
}

func (s *FindSettings) AddInLinesAfterPattern(p string) {
	addPattern(&p, s.InLinesAfterPatterns)
}

func (s *FindSettings) AddOutLinesAfterPattern(p string) {
	addPattern(&p, s.OutLinesAfterPatterns)
}

func (s *FindSettings) AddLinesAfterToPattern(p string) {
	addPattern(&p, s.LinesAfterToPatterns)
}

func (s *FindSettings) AddLinesAfterUntilPattern(p string) {
	addPattern(&p, s.LinesAfterUntilPatterns)
}

func (s *FindSettings) AddFindPattern(p string) {
	addPattern(&p, s.FindPatterns)
}

func (s *FindSettings) SetArchivesOnly(archivesOnly bool) {
	s.ArchivesOnly = archivesOnly
	if archivesOnly {
		s.FindArchives = true
	}
}

func (s *FindSettings) SetDebug(debug bool) {
	s.Debug = debug
	if debug {
		s.Verbose = true
	}
}

func addFindPatternsToBuffer(name string, sp *FindPatterns, buffer *bytes.Buffer) {
	buffer.WriteString(fmt.Sprintf("%s: [", name))
	for i, r := range sp.patterns {
		if i > 0 {
			buffer.WriteString(",")
		}
		buffer.WriteString(fmt.Sprintf("\"%s\"", r.String()))
	}
	buffer.WriteString("]")
}

func addStringListToBuffer(name string, list []*string, buffer *bytes.Buffer) {
	buffer.WriteString(fmt.Sprintf("%s: [", name))
	elems := []string{}
	for _, l := range list {
		elems = append(elems, fmt.Sprintf("\"%s\"", *l))
	}
	buffer.WriteString(strings.Join(elems, ","))
	buffer.WriteString("]")
}

func addFileTypeListToBuffer(name string, list []FileType, buffer *bytes.Buffer) {
	buffer.WriteString(fmt.Sprintf("%s: [", name))
	elems := []string{}
	for _, ft := range list {
		elems = append(elems, fmt.Sprintf("\"%s\"", getNameForFileType(ft)))
	}
	buffer.WriteString(strings.Join(elems, ","))
	buffer.WriteString("]")
}

func (s *FindSettings) String() string {
	var buffer bytes.Buffer
	buffer.WriteString("FindSettings{")
	buffer.WriteString(fmt.Sprintf("ArchivesOnly: %t", s.ArchivesOnly))
	buffer.WriteString(fmt.Sprintf(", Colorize: %t", s.Colorize))
	buffer.WriteString(fmt.Sprintf(", Debug: %t", s.Debug))
	buffer.WriteString(fmt.Sprintf(", ExcludeHidden: %t", s.ExcludeHidden))
	buffer.WriteString(fmt.Sprintf(", FirstMatch: %t", s.FirstMatch))
	buffer.WriteString(", ")
	addStringListToBuffer("InArchiveExtensions", s.InArchiveExtensions, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("InArchiveFilePatterns", s.InArchiveFilePatterns, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("InDirPatterns", s.InDirPatterns, &buffer)
	buffer.WriteString(", ")
	addStringListToBuffer("InExtensions", s.InExtensions, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("InFilePatterns", s.InFilePatterns, &buffer)
	buffer.WriteString(", ")
	addFileTypeListToBuffer("InFileTypes", s.InFileTypes, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("InLinesAfterPatterns", s.InLinesAfterPatterns, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("InLinesBeforePatterns", s.InLinesBeforePatterns, &buffer)
	buffer.WriteString(fmt.Sprintf(", LinesAfter: %d", s.LinesAfter))
	buffer.WriteString(", ")
	addFindPatternsToBuffer("LinesAfterToPatterns", s.LinesAfterToPatterns, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("LinesAfterUntilPatterns", s.LinesAfterUntilPatterns, &buffer)
	buffer.WriteString(fmt.Sprintf(", LinesBefore: %d", s.LinesBefore))
	buffer.WriteString(fmt.Sprintf(", ListDirs: %t", s.ListDirs))
	buffer.WriteString(fmt.Sprintf(", ListFiles: %t", s.ListFiles))
	buffer.WriteString(fmt.Sprintf(", ListLines: %t", s.ListLines))
	buffer.WriteString(fmt.Sprintf(", MaxLineLength: %d", s.MaxLineLength))
	buffer.WriteString(fmt.Sprintf(", MultiLineFind: %t", s.MultiLineFind))
	buffer.WriteString(", ")
	addStringListToBuffer("OutArchiveExtensions", s.OutArchiveExtensions, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("OutArchiveFilePatterns", s.OutArchiveFilePatterns, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("OutDirPatterns", s.OutDirPatterns, &buffer)
	buffer.WriteString(", ")
	addStringListToBuffer("OutExtensions", s.OutExtensions, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("OutFilePatterns", s.OutFilePatterns, &buffer)
	buffer.WriteString(", ")
	addFileTypeListToBuffer("OutFileTypes", s.OutFileTypes, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("OutLinesAfterPatterns", s.OutLinesAfterPatterns, &buffer)
	buffer.WriteString(", ")
	addFindPatternsToBuffer("OutLinesBeforePatterns", s.OutLinesBeforePatterns, &buffer)
	buffer.WriteString(fmt.Sprintf(", PrintResults: %t", s.PrintResults))
	buffer.WriteString(fmt.Sprintf(", PrintUsage: %t", s.PrintUsage))
	buffer.WriteString(fmt.Sprintf(", PrintVersion: %t", s.PrintVersion))
	buffer.WriteString(fmt.Sprintf(", Recursive: %t", s.Recursive))
	buffer.WriteString(fmt.Sprintf(", FindArchives: %t", s.FindArchives))
	buffer.WriteString(", ")
	addFindPatternsToBuffer("FindPatterns", s.FindPatterns, &buffer)
	buffer.WriteString(fmt.Sprintf(", StartPath: \"%s\"", s.StartPath))
	buffer.WriteString(fmt.Sprintf(", TextFileEncoding: \"%s\"", s.TextFileEncoding))
	buffer.WriteString(fmt.Sprintf(", UniqueLines: %t", s.UniqueLines))
	buffer.WriteString(fmt.Sprintf(", Verbose: %t", s.Verbose))
	buffer.WriteString("}")
	return buffer.String()
}
