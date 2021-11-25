package gofind

import (
	"bytes"
	"fmt"
	"strings"
)

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
		false,             // Verbose
	}
}

func (s *FindSettings) AddInExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			s.InExtensions = append(s.InExtensions, ext)
		}
	}
}

func (s *FindSettings) AddOutExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			s.OutExtensions = append(s.OutExtensions, ext)
		}
	}
}

func addPattern(p string, sp *FindPatterns) {
	sp.AddPattern(p)
}

func (s *FindSettings) AddInDirPattern(p string) {
	addPattern(p, s.InDirPatterns)
}

func (s *FindSettings) AddOutDirPattern(p string) {
	addPattern(p, s.OutDirPatterns)
}

func (s *FindSettings) AddInFilePattern(p string) {
	addPattern(p, s.InFilePatterns)
}

func (s *FindSettings) AddOutFilePattern(p string) {
	addPattern(p, s.OutFilePatterns)
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
		s.InArchiveExtensions = append(s.InArchiveExtensions, ext)
	}
}

func (s *FindSettings) AddOutArchiveExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		ext := strings.ToLower(x)
		s.OutArchiveExtensions = append(s.OutArchiveExtensions, ext)
	}
}

func (s *FindSettings) AddInArchiveFilePattern(p string) {
	addPattern(p, s.InArchiveFilePatterns)
}

func (s *FindSettings) AddOutArchiveFilePattern(p string) {
	addPattern(p, s.OutArchiveFilePatterns)
}

func (s *FindSettings) AddPath(p string) {
	s.Paths = append(s.Paths, p)
}

func (s *FindSettings) SetArchivesOnly(archivesOnly bool) {
	s.ArchivesOnly = archivesOnly
	if archivesOnly {
		s.IncludeArchives = true
	}
}

func (s *FindSettings) SetDebug(debug bool) {
	s.Debug = debug
	if debug {
		s.Verbose = true
	}
}

func (s *FindSettings) addFindPatternsToBuffer(name string, sp *FindPatterns, buffer *bytes.Buffer) {
	buffer.WriteString(fmt.Sprintf("%s: [", name))
	elems := []string{}
	for _, r := range sp.patterns {
		elems = append(elems, fmt.Sprintf("\"%s\"", r.String()))
	}
	buffer.WriteString(strings.Join(elems, ","))
	buffer.WriteString("]")
}

func (s *FindSettings) addStringListToBuffer(name string, list []string, buffer *bytes.Buffer) {
	buffer.WriteString(fmt.Sprintf("%s: [", name))
	elems := []string{}
	for _, l := range list {
		elems = append(elems, fmt.Sprintf("\"%s\"", l))
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

func (s *FindSettings) addBoolToBuffer(name string, b bool, buffer *bytes.Buffer) {
	buffer.WriteString(fmt.Sprintf("%s: ", name))
	buffer.WriteString(fmt.Sprintf("%t", b))
}

func (s *FindSettings) String() string {
	var buffer bytes.Buffer
	buffer.WriteString("FindSettings{")
	s.addBoolToBuffer("ArchivesOnly", s.ArchivesOnly, &buffer)
	buffer.WriteString(", ")
	s.addBoolToBuffer("Debug", s.Debug, &buffer)
	buffer.WriteString(", ")
	s.addBoolToBuffer("ExcludeHidden", s.ExcludeHidden, &buffer)
	buffer.WriteString(", ")
	s.addStringListToBuffer("InArchiveExtensions", s.InArchiveExtensions, &buffer)
	buffer.WriteString(", ")
	s.addFindPatternsToBuffer("InArchiveFilePatterns", s.InArchiveFilePatterns, &buffer)
	buffer.WriteString(", ")
	s.addFindPatternsToBuffer("InDirPatterns", s.InDirPatterns, &buffer)
	buffer.WriteString(", ")
	s.addStringListToBuffer("InExtensions", s.InExtensions, &buffer)
	buffer.WriteString(", ")
	s.addFindPatternsToBuffer("InFilePatterns", s.InFilePatterns, &buffer)
	buffer.WriteString(", ")
	addFileTypeListToBuffer("InFileTypes", s.InFileTypes, &buffer)
	buffer.WriteString(", ")
	s.addBoolToBuffer("IncludeArchives", s.IncludeArchives, &buffer)
	buffer.WriteString(", ")
	s.addBoolToBuffer("ListDirs", s.ListDirs, &buffer)
	buffer.WriteString(", ")
	s.addBoolToBuffer("ListFiles", s.ListFiles, &buffer)
	buffer.WriteString(", ")
	s.addStringListToBuffer("OutArchiveExtensions", s.OutArchiveExtensions, &buffer)
	buffer.WriteString(", ")
	s.addFindPatternsToBuffer("OutArchiveFilePatterns", s.OutArchiveFilePatterns, &buffer)
	buffer.WriteString(", ")
	s.addFindPatternsToBuffer("OutDirPatterns", s.OutDirPatterns, &buffer)
	buffer.WriteString(", ")
	s.addStringListToBuffer("OutExtensions", s.OutExtensions, &buffer)
	buffer.WriteString(", ")
	s.addFindPatternsToBuffer("OutFilePatterns", s.OutFilePatterns, &buffer)
	buffer.WriteString(", ")
	addFileTypeListToBuffer("OutFileTypes", s.OutFileTypes, &buffer)
	buffer.WriteString(", ")
	s.addBoolToBuffer("PrintUsage", s.PrintUsage, &buffer)
	buffer.WriteString(", ")
	s.addBoolToBuffer("PrintVersion", s.PrintVersion, &buffer)
	buffer.WriteString(", ")
	s.addBoolToBuffer("Recursive", s.Recursive, &buffer)
	buffer.WriteString(", ")
	s.addStringListToBuffer("Paths", s.Paths, &buffer)
	buffer.WriteString(", ")
	s.addBoolToBuffer("Verbose", s.Verbose, &buffer)
	buffer.WriteString("}")
	return buffer.String()
}
