/*
Package xfind provides functionality to find specific files in specific
directories for content that matches any number of regular expressions.

The Finder class is the main class that provides the file finding
functionality. It takes a FindSettings instance argument on instantiation
that defines the various find options (what files extension, what directory
and/or file name patterns, what content find patterns, etc.).

The two main methods of Finder are:

* Find - this performs the find based on the FindSettings, starting in
           StartPath. It has three main phases:

    a) Find matching directories - get the list of directories to find
    b) Find matching files - get the list of files to find under the directories
    c) Find matching files - find the matching files

* FindFile - this performs a find of a single file. Its use is less common
               but provided for cases where this is needed.
*/
package gofind

import (
	"archive/tar"
	"archive/zip"
	"bufio"
	"compress/bzip2"
	"compress/gzip"
	"fmt"
	"golang.org/x/text/encoding"
	"golang.org/x/text/encoding/ianaindex"
	"golang.org/x/text/transform"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
)

type Finder struct {
	Settings         *FindSettings
	fileTypes        *FileTypes
	findDirs       []*string
	findItems      *FindItems
	errors           []error
	addItemChan      chan *FindItem
	addItemsDoneChan chan bool
	doneChan         chan *string
	errChan          chan error
	findResults    *FindResults
	resultChan       chan *FindResult
	textDecoder      *encoding.Decoder
}

func NewFinder(settings *FindSettings) *Finder {
	return &Finder{
		settings,                   // Settings
		FileTypesFromJson(),        // fileTypes
		[]*string{},                // findDirs
		NewFindItems(),           // findItems
		[]error{},                  // errors
		make(chan *FindItem),     // addItemChan
		make(chan bool),            // addItemsDoneChan
		make(chan *string, 10),     // doneChan
		make(chan error, 1),        // errChan
		NewFindResults(settings), // findResults
		make(chan *FindResult),   // resultChan
		nil,
	}
}

func (s *Finder) ClearFindResults() {
	s.findResults = nil
}

func (s *Finder) GetFindResults() *FindResults {
	return s.findResults
}

func (s *Finder) validateSettings() error {
	if s.Settings.StartPath == "" {
		return fmt.Errorf("Startpath not defined")
	}
	fi, err := os.Stat(s.Settings.StartPath)
	if err != nil {
		if os.IsNotExist(err) {
			return fmt.Errorf("Startpath not found")
		}
		if os.IsPermission(err) {
			return fmt.Errorf("Startpath not readable")
		}
		return err
	}
	if fi.IsDir() && !s.isFindDir(&s.Settings.StartPath) {
		return fmt.Errorf("Startpath does not match find settings")
	} else if fi.Mode().IsRegular() {
		dir, file := filepath.Split(s.Settings.StartPath)
		if !s.isFindDir(&dir) || !s.isFindFile(&file) {
			return fmt.Errorf("Startpath does not match find settings")
		}
	}
	if s.Settings.FindPatterns.IsEmpty() {
		return fmt.Errorf("No find patterns defined")
	}
	if s.Settings.LinesAfter < 0 {
		return fmt.Errorf("Invalid linesafter")
	}
	if s.Settings.LinesBefore < 0 {
		return fmt.Errorf("Invalid linesbefore")
	}
	if s.Settings.MaxLineLength < 0 {
		return fmt.Errorf("Invalid maxlinelength")
	}
	enc, err := ianaindex.IANA.Encoding(s.Settings.TextFileEncoding)
	if err != nil {
		return fmt.Errorf("Invalid or unsupported text file encoding")
	}
	s.textDecoder = enc.NewDecoder()
	return nil
}

func filterInByFindPatterns(s *string, inPatterns *FindPatterns,
	outPatterns *FindPatterns) bool {
	if !inPatterns.IsEmpty() && !inPatterns.MatchesAny(s) {
		return false
	}
	if !outPatterns.IsEmpty() && outPatterns.MatchesAny(s) {
		return false
	}
	return true
}

func (s *Finder) isFindDir(d *string) bool {
	if isHidden(*d) && s.Settings.ExcludeHidden {
		return false
	}
	return filterInByFindPatterns(d, s.Settings.InDirPatterns,
		s.Settings.OutDirPatterns)
}

func (s *Finder) isArchiveFindFile(filename *string) bool {
	if s.fileTypes.IsArchiveFile(*filename) {
		if isHidden(*filename) && s.Settings.ExcludeHidden {
			return false
		}
		ext := getExtension(*filename)
		if len(s.Settings.InArchiveExtensions) > 0 && !contains(s.Settings.InArchiveExtensions, ext) {
			return false
		}
		if len(s.Settings.OutArchiveExtensions) > 0 && contains(s.Settings.OutArchiveExtensions, ext) {
			return false
		}
		return filterInByFindPatterns(filename, s.Settings.InArchiveFilePatterns,
			s.Settings.OutArchiveFilePatterns)
	}
	return false
}

func (s *Finder) isArchiveFindItem(si *FindItem) bool {
	if si.fileType == FiletypeArchive {
		if (isHidden(*si.Path) || isHidden(*si.Name)) && s.Settings.ExcludeHidden {
			return false
		}
		ext := getExtension(*si.Name)
		if len(s.Settings.InArchiveExtensions) > 0 && !contains(s.Settings.InArchiveExtensions, ext) {
			return false
		}
		if len(s.Settings.OutArchiveExtensions) > 0 && contains(s.Settings.OutArchiveExtensions, ext) {
			return false
		}
		return filterInByFindPatterns(si.Name, s.Settings.InArchiveFilePatterns,
			s.Settings.OutArchiveFilePatterns)
	}
	return false
}

func (s *Finder) isFindFile(filename *string) bool {
	if isHidden(*filename) && s.Settings.ExcludeHidden {
		return false
	}
	ext := getExtension(*filename)
	if len(s.Settings.InExtensions) > 0 && !contains(s.Settings.InExtensions, ext) {
		return false
	}
	if len(s.Settings.OutExtensions) > 0 && contains(s.Settings.OutExtensions, ext) {
		return false
	}
	fileType := s.fileTypes.getFileType(*filename)
	if len(s.Settings.InFileTypes) > 0 && !containsFileType(s.Settings.InFileTypes, fileType) {
		return false
	}
	if len(s.Settings.OutFileTypes) > 0 && containsFileType(s.Settings.OutFileTypes, fileType) {
		return false
	}
	return filterInByFindPatterns(filename, s.Settings.InFilePatterns,
		s.Settings.OutFilePatterns)
}

func (s *Finder) isFindItem(si *FindItem) bool {
	if (isHidden(*si.Path) || isHidden(*si.Name)) && s.Settings.ExcludeHidden {
		return false
	}
	ext := getExtension(*si.Name)
	if len(s.Settings.InExtensions) > 0 && !contains(s.Settings.InExtensions, ext) {
		return false
	}
	if len(s.Settings.OutExtensions) > 0 && contains(s.Settings.OutExtensions, ext) {
		return false
	}
	if len(s.Settings.InFileTypes) > 0 && !containsFileType(s.Settings.InFileTypes, si.fileType) {
		return false
	}
	if len(s.Settings.OutFileTypes) > 0 && containsFileType(s.Settings.OutFileTypes, si.fileType) {
		return false
	}
	return filterInByFindPatterns(si.Name, s.Settings.InFilePatterns,
		s.Settings.OutFilePatterns)
}

func (s *Finder) filterFile(f *string) bool {
	if s.fileTypes.IsArchiveFile(*f) {
		return s.Settings.FindArchives && s.isArchiveFindFile(f)
	}
	return !s.Settings.ArchivesOnly && s.isFindFile(f)
}

func (s *Finder) filterFindItem(si *FindItem) bool {
	if si.fileType == FiletypeArchive {
		return s.Settings.FindArchives && s.isArchiveFindItem(si)
	}
	return !s.Settings.ArchivesOnly && s.isFindItem(si)
}

func (s *Finder) fileToFindItem(f string) *FindItem {
	dir, file := filepath.Split(f)
	if dir == "" {
		dir = "."
	} else {
		dir = normalizePath(dir)
	}
	t := s.fileTypes.getFileType(file)
	return NewFindItem(&dir, &file, t)
}

func (s *Finder) checkAddFindFile(f string) {
	findItem := s.fileToFindItem(f)
	if s.filterFindItem(findItem) {
		s.addItemChan <- findItem
	}
}

// this method passed to the filepath.Walk method, it must have this signature
func (s *Finder) checkAddFindWalkFile(f string, fi os.FileInfo, err error) error {
	if fi.Mode().IsRegular() {
		s.checkAddFindFile(f)
	}
	return nil
}

func (s *Finder) setFindFiles() error {
	if s.Settings.Verbose {
		log("\nBuilding file find list")
	}

	startPath := normalizePath(s.Settings.StartPath)
	fi, err := os.Stat(startPath)
	if err != nil {
		return err
	}
	if fi.IsDir() {
		if s.Settings.Recursive {
			err := filepath.Walk(startPath, s.checkAddFindWalkFile)
			if err != nil {
				return err
			}
		} else {
			files, err := ioutil.ReadDir(startPath)
			if err != nil {
				return err
			}

			for _, file := range files {
				s.checkAddFindFile(file.Name())
			}
		}

	} else if fi.Mode().IsRegular() {
		s.checkAddFindFile(s.Settings.StartPath)
	}

	s.addItemsDoneChan <- true

	return nil
}

func (s *Finder) addFindResult(r *FindResult) {
	s.findResults.AddFindResult(r)
}

func linesMatch(lines []*string, inPatterns *FindPatterns,
	outPatterns *FindPatterns) bool {
	inLinesMatch := inPatterns.IsEmpty() || inPatterns.AnyMatchesAny(lines)
	outLinesMatch := !outPatterns.IsEmpty() && outPatterns.AnyMatchesAny(lines)
	return inLinesMatch && !outLinesMatch
}

func (s *Finder) linesAfterMatch(linesAfter []*string) bool {
	return linesMatch(linesAfter, s.Settings.InLinesAfterPatterns,
		s.Settings.OutLinesAfterPatterns)
}

func (s *Finder) linesBeforeMatch(linesBefore []*string) bool {
	return linesMatch(linesBefore, s.Settings.InLinesBeforePatterns,
		s.Settings.OutLinesBeforePatterns)
}

func hasNewLine(bytes []byte) bool {
	for _, b := range bytes {
		if b == '\n' {
			return true
		}
	}
	return false
}

func getNewLineCount(bytes []byte) int {
	count := 0
	for _, b := range bytes {
		if b == '\n' {
			count++
		}
	}
	return count
}

func newlineIndices(bytes []byte) []int {
	var newlineidxs []int
	for i, b := range bytes {
		if b == '\n' {
			newlineidxs = append(newlineidxs, i)
		}
	}
	return newlineidxs
}

func lineStartEndIndicesForIndex(idx int, bytes []byte) (int, int) {
	startidx, endidx := idx, idx
	for startidx > 0 && bytes[startidx] != '\n' {
		startidx--
	}
	if bytes[startidx] == '\n' {
		startidx++
	}
	for endidx < len(bytes) && bytes[endidx] != '\n' {
		endidx++
	}
	if endidx < len(bytes) && bytes[endidx] == '\n' {
		endidx++
	}
	if startidx == endidx && startidx > 0 {
		startidx--
	}
	return startidx, endidx
}

func splitIntoLines(bytes []byte) []*string {
	newlineidxs := newlineIndices(bytes)
	emptyStr := ""
	var lines []*string
	startidx, endidx := 0, 0
	for _, n := range newlineidxs {
		endidx = n
		if startidx == endidx {
			lines = append(lines, &emptyStr)
		} else if startidx < endidx {
			nextline := string(bytes[startidx:endidx])
			lines = append(lines, &nextline)
		}
		startidx = endidx + 1
	}
	endidx = len(bytes) - 1
	if bytes[endidx] == '\n' {
		lines = append(lines, &emptyStr)
	}
	return lines
}

func linesBeforeIndex(bytes []byte, idx int, lineCount int) []*string {
	var lines []*string
	if idx < 1 {
		return lines
	}
	newlines := 0
	beforeidx := idx
	for beforeidx > 0 && newlines < lineCount {
		if bytes[beforeidx] == '\n' {
			newlines++
		}
		beforeidx--
	}
	beforestartlineidx, _ := lineStartEndIndicesForIndex(beforeidx, bytes)
	lines = splitIntoLines(bytes[beforestartlineidx : idx-1])
	return lines
}

func linesAfterIndex(bytes []byte, idx int, lineCount int) []*string {
	var lines []*string
	newlines := 0
	afteridx := idx
	for afteridx < len(bytes)-1 && newlines < lineCount {
		if bytes[afteridx] == '\n' {
			newlines++
		}
		afteridx++
	}
	_, afterendlineidx := lineStartEndIndicesForIndex(afteridx-1, bytes)
	lines = splitIntoLines(bytes[idx:afterendlineidx])
	return lines[:lineCount]
}

func (s *Finder) findTextFileReaderContents(r io.Reader, si *FindItem) {
	bytes, err := ioutil.ReadAll(r)
	if err != nil {
		s.errChan <- err
		return
	}
	results := s.findTextBytes(bytes)
	for _, sr := range results {
		sr.File = si
		s.resultChan <- sr
	}
}

// public method to find a multi-line string
func (s *Finder) FindMultiLineString(str string) []*FindResult {
	return s.findTextBytes([]byte(str))
}

func (s *Finder) findTextBytes(bytes []byte) []*FindResult {
	var results []*FindResult
	var linesBefore []*string
	var linesAfter []*string
	findLimit := -1
	if s.Settings.FirstMatch {
		findLimit = 1
	}
	spi := s.Settings.FindPatterns.Iterator()
	for spi.Next() {
		p := spi.Value()
		if allIndices := p.FindAllIndex(bytes, findLimit); allIndices != nil {
			for _, idx := range allIndices {
				// get the start and end indices of the current line
				startidx, endidx := lineStartEndIndicesForIndex(idx[0], bytes)
				// grab the contents in that range as the line
				line := bytes[startidx:endidx]
				linenum, beforeLineCount, afterLineCount := 1, 0, 0
				if hasNewLine(bytes[0:startidx]) {
					beforeLineCount = getNewLineCount(bytes[0:startidx])
					linenum = beforeLineCount + 1
				}
				if hasNewLine(bytes[endidx:]) {
					afterLineCount = getNewLineCount(bytes[endidx:])
				}
				if s.Settings.LinesBefore > 0 && beforeLineCount > 0 {
					linesBefore = linesBeforeIndex(bytes, startidx, s.Settings.LinesBefore)
				}
				if s.Settings.LinesAfter > 0 && afterLineCount > 0 {
					linesAfter = linesAfterIndex(bytes, endidx, s.Settings.LinesAfter)
				}

				if len(linesBefore) > 0 && !s.linesBeforeMatch(linesBefore) {
					continue
				} else if len(linesAfter) > 0 && !s.linesAfterMatch(linesAfter) {
					continue
				}

				lineStr := strings.TrimRight(string(line), "\r\n")
				sr := &FindResult{
					p,
					nil,
					linenum,
					idx[0] - startidx + 1,
					idx[1] - startidx + 1,
					&lineStr,
					linesBefore,
					linesAfter,
				}
				results = append(results, sr)

				// reset linesBefore and LinesAfter
				linesBefore, linesAfter = []*string{}, []*string{}
			}
		}
	}
	return results
}

func (s *Finder) findTextFileReaderLines(r io.Reader, si *FindItem) {
	results := s.FindTextReaderLines(r)
	for _, sr := range results {
		sr.File = si
		s.resultChan <- sr
	}
}

func (s *Finder) FindTextReaderLines(r io.Reader) []*FindResult {
	var results []*FindResult
	scanner := bufio.NewScanner(r)
	linenum := 0
	var linesBefore []*string
	var linesAfter []*string
	linesAfterIdx := 0
	patternMatches := map[*regexp.Regexp]int{}
ReadLines:
	for {
		linenum++
		var line *string
		if len(linesAfter) > 0 {
			line, linesAfter = linesAfter[0], linesAfter[1:]
		} else if scanner.Scan() {
			text := scanner.Text()
			line = &text
		} else {
			break ReadLines
		}
		for len(linesAfter) < s.Settings.LinesAfter && scanner.Scan() {
			lineAfter := scanner.Text()
			linesAfter = append(linesAfter, &lineAfter)
		}
		spi := s.Settings.FindPatterns.Iterator()
		for spi.Next() {
			p := spi.Value()
			if matchIndices := p.FindAllStringIndex(*line, -1); matchIndices != nil {
				if len(linesBefore) > 0 && !s.linesBeforeMatch(linesBefore) {
					continue
				} else if len(linesAfter) > 0 && !s.linesAfterMatch(linesAfter) {
					continue
				}
				linesAfterToMatch := false
				linesAfterUntilMatch := false
				if !s.Settings.LinesAfterToPatterns.IsEmpty() ||
					!s.Settings.LinesAfterUntilPatterns.IsEmpty() {

					if !s.Settings.LinesAfterToPatterns.IsEmpty() &&
						s.Settings.LinesAfterToPatterns.AnyMatchesAny(linesAfter) {
						linesAfterToMatch = true
					}
					if !s.Settings.LinesAfterUntilPatterns.IsEmpty() &&
						s.Settings.LinesAfterUntilPatterns.AnyMatchesAny(linesAfter) {
						linesAfterUntilMatch = true
					}

					for !linesAfterToMatch && !linesAfterUntilMatch {
						if len(linesAfter) < linesAfterIdx+1 {
							if !scanner.Scan() {
								break
							}
							lineAfter := scanner.Text()
							linesAfter = append(linesAfter, &lineAfter)
						}
						nextLine := linesAfter[linesAfterIdx]
						if !s.Settings.LinesAfterToPatterns.IsEmpty() &&
							s.Settings.LinesAfterToPatterns.MatchesAny(nextLine) {
							linesAfterToMatch = true
						}
						if !s.Settings.LinesAfterUntilPatterns.IsEmpty() &&
							s.Settings.LinesAfterUntilPatterns.MatchesAny(nextLine) {
							linesAfterUntilMatch = true
						}
						linesAfterIdx++
					}
				}
				var srLinesAfter []*string
				if linesAfterIdx > 0 {
					lastIdx := linesAfterIdx + 1
					if lastIdx > len(linesAfter) {
						lastIdx = len(linesAfter)
					}
					if !s.Settings.LinesAfterToPatterns.IsEmpty() {
						srLinesAfter = linesAfter[:lastIdx]
					} else if !s.Settings.LinesAfterUntilPatterns.IsEmpty() {
						srLinesAfter = linesAfter[:lastIdx-1]
					}
					linesAfterIdx = 0
				} else {
					srLinesAfter = linesAfter
				}
				// iterate through matchIndices
				for _, m := range matchIndices {
					// check for FirstMatch setting and stop if file+pattern match exists
					_, patternMatched := patternMatches[p]
					if s.Settings.FirstMatch && patternMatched {
						continue
					} else {
						sr := &FindResult{
							p,
							nil,
							linenum,
							m[0] + 1,
							m[1] + 1,
							line,
							linesBefore,
							srLinesAfter,
						}
						results = append(results, sr)
						patternMatches[p] = 1
					}
				}
			}
		}
		if s.Settings.LinesBefore > 0 {
			if len(linesBefore) == s.Settings.LinesBefore {
				linesBefore = linesBefore[1:]
			}
			if len(linesBefore) < s.Settings.LinesBefore {
				linesBefore = append(linesBefore, line)
			}
		}
	}
	if err := scanner.Err(); err != nil {
		s.errChan <- err
	}
	return results
}

func (s *Finder) findTextFileReader(r io.Reader, si *FindItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Finding text file %s", si.String()))
	}
	if s.Settings.MultiLineFind {
		s.findTextFileReaderContents(r, si)
	} else {
		s.findTextFileReaderLines(r, si)
	}
}

func (s *Finder) findBinaryFileReader(r io.Reader, si *FindItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Finding binary file %s", si.String()))
	}
	bytes, err := ioutil.ReadAll(r)
	if err != nil {
		s.errChan <- err
		return
	}
	findLimit := -1
	if s.Settings.FirstMatch {
		findLimit = 1
	}
	spi := s.Settings.FindPatterns.Iterator()
	for spi.Next() {
		p := spi.Value()
		if matchIndices := p.FindAllIndex(bytes, findLimit); matchIndices != nil {
			for _, m := range matchIndices {
				emptyStr := ""
				sr := &FindResult{
					p,
					si,
					0,
					m[0] + 1,
					m[1] + 1,
					&emptyStr,
					[]*string{},
					[]*string{},
				}
				s.resultChan <- sr
			}
		}
	}
}

func notR(c rune) bool {
	return c != 'r'
}

func (s *Finder) findTarFileReader(r io.Reader, si *FindItem) {
	if s.Settings.Verbose {
		tarName := strings.TrimRightFunc(si.String(), notR)
		log(fmt.Sprintf("Finding tar file %s", tarName))
	}
	tr := tar.NewReader(r)
	for {
		hdr, err := tr.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			if err == io.ErrUnexpectedEOF {
				if s.Settings.Debug {
					log(fmt.Sprintf("Encountered unexpected EOF in tar file %s",
						si.String()))
				}
				break
			}
			if s.Settings.Debug {
				log(fmt.Sprintf("Error encountered in findTarFileReader: %s",
					err))
			}
			s.errChan <- err
		}
		dir, file := filepath.Split(hdr.Name)
		if !strings.HasSuffix(hdr.Name, "/") {
			if s.isFindFile(&file) {
				t := s.fileTypes.getFileType(file)
				newFindItem := NewFindItem(&dir, &file, t)
				for _, c := range si.Containers {
					newFindItem.AddContainer(c)
				}
				newFindItem.AddContainer(filepath.Join(*si.Path, *si.Name))
				s.findFileReader(tr, newFindItem)
			} else if s.isArchiveFindFile(&file) {
				t := s.fileTypes.getFileType(file)
				newFindItem := NewFindItem(&dir, &file, t)
				for _, c := range si.Containers {
					newFindItem.AddContainer(c)
				}
				newFindItem.AddContainer(filepath.Join(*si.Path, *si.Name))
				s.findArchiveFileReader(tr, newFindItem)
			}
		}
	}
}

func (s *Finder) findGzipFileReader(r io.Reader, si *FindItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Finding gzip file %s", si.String()))
	}
	gr, err := gzip.NewReader(r)
	if err != nil {
		if s.Settings.Debug {
			log(fmt.Sprintf("Error encountered in findGzipFileReader: %s",
				err))
		}
		s.errChan <- err
		return
	}
	defer func() {
		gr.Close()
	}()
	if strings.HasSuffix(*si.Name, "tar.gz") || strings.HasSuffix(*si.Name, "tgz") {
		s.findTarFileReader(gr, si)
	} else {
		name := gr.Name
		if s.isFindFile(&name) {
			emptyStr := ""
			t := s.fileTypes.getFileType(name)
			newFindItem := NewFindItem(&emptyStr, &name, t)
			for _, c := range si.Containers {
				newFindItem.AddContainer(c)
			}
			newFindItem.AddContainer(filepath.Join(*si.Path, *si.Name))
			s.findFileReader(gr, newFindItem)
		}
	}
}

func (s *Finder) findBzip2FileReader(r io.Reader, si *FindItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Finding bzip2 file %s", si.String()))
	}
	br := bzip2.NewReader(r)
	if strings.HasSuffix(*si.Name, "tar.bz2") {
		s.findTarFileReader(br, si)
	} else {
		containedFileName := strings.TrimSuffix(*si.Name, ".bz2")
		if s.isFindFile(&containedFileName) {
			emptyStr := ""
			t := s.fileTypes.getFileType(containedFileName)
			newFindItem := NewFindItem(&emptyStr, &containedFileName, t)
			for _, c := range si.Containers {
				newFindItem.AddContainer(c)
			}
			newFindItem.AddContainer(filepath.Join(*si.Path, *si.Name))
			s.findFileReader(br, newFindItem)
		}
	}
}

func (s *Finder) findZipFileReader(r io.Reader, si *FindItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Finding zip file %s", si.String()))
	}
	// zip.OpenReader returns a *zip.ReaderCloser struct type that extends Reader
	zr, err := zip.OpenReader(filepath.Join(*si.Path, *si.Name))
	if err != nil {
		s.errChan <- err
		return
	}
	defer zr.Close()
	// f is a zip.File struct type
	for _, f := range zr.File {
		dir, file := filepath.Split(f.Name)
		if f.FileHeader.Flags != 0 && f.FileHeader.Flags != 2 {
			log(fmt.Sprintf("%s is an UNKNOWN file type", file))
		}
		// f.FileHeader.Flags == 2 seems to mean it's a file (not a dir, etc.)
		if f.FileHeader.Flags == 2 && s.isFindFile(&file) {
			cr, err := f.Open()
			if err != nil {
				s.errChan <- err
				return
			}
			t := s.fileTypes.getFileType(file)
			newFindItem := NewFindItem(&dir, &file, t)
			for _, c := range si.Containers {
				newFindItem.AddContainer(c)
			}
			newFindItem.AddContainer(filepath.Join(*si.Path, *si.Name))
			s.findFileReader(cr, newFindItem)
			cr.Close()
		}
	}
}

func (s *Finder) findArchiveFileReader(r io.Reader, si *FindItem) {
	if !s.isArchiveFindFile(si.Name) {
		return
	}
	ext := getExtension(*si.Name)
	switch ext {
	case "zip", "jar", "war", "ear":
		s.findZipFileReader(r, si)
	case "gz", "tgz":
		s.findGzipFileReader(r, si)
	case "bz2":
		s.findBzip2FileReader(r, si)
	default:
		log(fmt.Sprintf("Finding not currently supported for %s files", ext))
	}
}

func (s *Finder) findFileReader(r io.Reader, si *FindItem) {
	switch si.fileType {
	case FiletypeCode, FiletypeXml, FiletypeText:
		s.findTextFileReader(transform.NewReader(r, s.textDecoder), si)
	case FiletypeBinary:
		s.findBinaryFileReader(r, si)
	case FiletypeArchive:
		if s.Settings.FindArchives {
			s.findArchiveFileReader(r, si)
		} else {
			if s.Settings.Verbose {
				log(fmt.Sprintf("Skipping archive file: %s", si.String()))
			}
		}
	default:
		log(fmt.Sprintf("Skipping unknown file type: %s", si.String()))
	}
}

func (s *Finder) findFindItem(si *FindItem) {
	if !s.fileTypes.IsFindableItem(si) {
		if contains(s.Settings.InExtensions, getExtension(*si.Name)) {
			if s.Settings.Debug {
				log(fmt.Sprintf("File made findable by passing in-ext: %s",
					si.String()))
			}
		} else {
			if s.Settings.Verbose {
				log(fmt.Sprintf("Skipping unfindable file: %s", si.String()))
			}
			return
		}
	}
	// make sure it doesn't have any containers (not sure how this could happen)
	if len(si.Containers) > 0 {
		log(fmt.Sprintf("Has containers: %s", si.String()))
	} else {
		// create an io.Reader
		fullName := filepath.Join(*si.Path, *si.Name)
		r, err := os.Open(fullName)
		if err != nil {
			s.errChan <- err
			return
		}
		defer r.Close()
		s.findFileReader(r, si)
		s.doneChan <- &fullName
	}
}

// initiates goroutines to find each file in the batch, waiting for all
// to finish before returning
func (s *Finder) doBatchFileFind(findItems []*FindItem) {
	wg := &sync.WaitGroup{}
	wg.Add(len(findItems)) // set the WaitGroup counter to findItem length
	for _, si := range findItems {
		go func(wg *sync.WaitGroup, si *FindItem) {
			s.findFindItem(si)
			wg.Done() // decrement the counter
		}(wg, si)
	}
	wg.Wait()
}

func (s *Finder) doFileFind() error {
	const batchSize = 240 // max files to find at one time
	var findItems []*FindItem
	sii := s.findItems.Iterator()
	for sii.Next() {
		findItems = append(findItems, sii.Value())
	}
	// start the processFindChannels goroutine
	go s.processFindChannels()

	// batch find (to avoid too many files open at once)
	for len(findItems) > batchSize && len(s.errors) == 0 {
		s.doBatchFileFind(findItems[:batchSize])
		findItems = findItems[batchSize:]
	}
	if len(findItems) > 0 && len(s.errors) == 0 {
		s.doBatchFileFind(findItems)
	}
	if len(s.errors) > 0 {
		return s.errors[0]
	}
	return nil
}

func (s *Finder) processFindChannels() {
	//get the results from the results channel
	doneFiles := map[string]bool{}
	for len(doneFiles) < s.findItems.Count() && len(s.errors) == 0 {
		select {
		case r := <-s.resultChan:
			s.addFindResult(r)
		case f := <-s.doneChan:
			doneFiles[*f] = true
		case e := <-s.errChan:
			s.errors = append(s.errors, e)
		}
	}
}

// the public-facing method takes a string and converts to a FindItem type
func (s *Finder) FindFile(fp string) {
	dir, file := filepath.Split(fp)
	t := s.fileTypes.getFileType(file)
	findItem := NewFindItem(&dir, &file, t)
	s.findFindItem(findItem)
}

//get the find items (files) from the file channel
func (s *Finder) processFindItemChannels() {
	addItemsDone := false
	for !addItemsDone {
		select {
		case b := <-s.addItemsDoneChan:
			addItemsDone = b
		case i := <-s.addItemChan:
			s.findItems.AddItem(i)
		}
	}
}

func (s *Finder) printToBeFinded() {
	dirs := []string{}
	files := []string{}

	sfi := s.findItems.Iterator()
	for sfi.Next() {
		dirs = append(dirs, *sfi.Value().Path)
		files = append(files, sfi.Value().String())
	}

	dirMap := makeMap(dirs)
	dirs = getSortedKeys(dirMap)

	log(fmt.Sprintf("\nDirectories to be found (%d):", len(dirs)))
	for _, d := range dirs {
		log(d)
	}

	fileMap := makeMap(files)
	files = getSortedKeys(fileMap)

	log(fmt.Sprintf("\nFiles to be found (%d):", len(files)))
	for _, f := range files {
		log(f)
	}
}

func (s *Finder) Find() error {
	if err := s.validateSettings(); err != nil {
		return err
	}

	// get find file list
	// first start the processFindItemChannels goroutine
	go s.processFindItemChannels()

	// now fill the findItem channels
	if err := s.setFindFiles(); err != nil {
		return err
	}

	if s.Settings.Verbose {
		s.printToBeFinded()
	}

	// find the files
	if s.Settings.Verbose {
		log("\nStarting file find...\n")
	}
	if err := s.doFileFind(); err != nil {
		return err
	}
	if s.Settings.Verbose {
		log("\nFile find complete.\n")
	}

	return nil
}

func (s *Finder) PrintFindResults() {
	s.findResults.PrintFindResults()
}

func (s *Finder) PrintDirCounts() {
	s.findResults.PrintDirCounts()
}

func (s *Finder) PrintFileCounts() {
	s.findResults.PrintFileCounts()
}

func (s *Finder) PrintLineCounts() {
	s.findResults.PrintLineCounts()
}

func (s *Finder) PrintUniqueLineCounts() {
	s.findResults.PrintUniqueLineCounts()
}
