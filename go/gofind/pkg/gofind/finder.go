package gofind

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"

	"golang.org/x/text/encoding"
)

// Finder - the find executor
type Finder struct {
	Settings           *FindSettings
	fileTypes          *FileTypes
	fileResults        *FileResults
	errors             []error
	addResultChan      chan *FileResult
	addResultsDoneChan chan bool
	findDoneChan       chan bool
	errChan            chan error
	textDecoder        *encoding.Decoder
}

func NewFinder(settings *FindSettings) *Finder {
	return &Finder{
		settings,               // Settings
		FileTypesFromJson(),    // fileTypes
		NewFileResults(),       // fileResults
		[]error{},              // errors
		make(chan *FileResult), // addResultChan
		make(chan bool),        // addResultsDoneChan
		make(chan bool, 1),     // findDoneChan
		make(chan error, 1),    // errChan
		nil,
	}
}

func (f *Finder) isMatchingDir(d string) bool {
	if !f.Settings.IncludeHidden() && isHidden(d) {
		return false
	}
	return (f.Settings.InDirPatterns().IsEmpty() || f.Settings.InDirPatterns().MatchesAny(d)) &&
		(f.Settings.OutDirPatterns().IsEmpty() || !f.Settings.OutDirPatterns().MatchesAny(d))
}

func (f *Finder) isMatchingArchiveExtension(ext string) bool {
	return (len(f.Settings.InArchiveExtensions()) == 0 || Contains(f.Settings.InArchiveExtensions(), ext)) &&
		(len(f.Settings.OutArchiveExtensions()) == 0 || !Contains(f.Settings.OutArchiveExtensions(), ext))
}

func (f *Finder) hasMatchingArchiveExtension(fr *FileResult) bool {
	if len(f.Settings.InArchiveExtensions()) > 0 || len(f.Settings.OutArchiveExtensions()) > 0 {
		ext := GetExtension(fr.Name)
		return f.isMatchingArchiveExtension(ext)
	}
	return true
}

func (f *Finder) isMatchingExtension(ext string) bool {
	return (len(f.Settings.InExtensions()) == 0 || Contains(f.Settings.InExtensions(), ext)) &&
		(len(f.Settings.OutExtensions()) == 0 || !Contains(f.Settings.OutExtensions(), ext))
}

func (f *Finder) hasMatchingExtension(fr *FileResult) bool {
	if len(f.Settings.InExtensions()) > 0 || len(f.Settings.OutExtensions()) > 0 {
		ext := GetExtension(fr.Name)
		return f.isMatchingExtension(ext)
	}
	return true
}

func (f *Finder) isMatchingArchiveFileName(fileName string) bool {
	return (f.Settings.InArchiveFilePatterns().IsEmpty() || f.Settings.InArchiveFilePatterns().MatchesAny(fileName)) &&
		(f.Settings.OutArchiveFilePatterns().IsEmpty() || !f.Settings.OutArchiveFilePatterns().MatchesAny(fileName))
}

func (f *Finder) isMatchingFileName(fileName string) bool {
	return (f.Settings.InFilePatterns().IsEmpty() || f.Settings.InFilePatterns().MatchesAny(fileName)) &&
		(f.Settings.OutFilePatterns().IsEmpty() || !f.Settings.OutFilePatterns().MatchesAny(fileName))
}

func (f *Finder) isMatchingFileType(fileType FileType) bool {
	return (len(f.Settings.InFileTypes()) == 0 || ContainsFileType(f.Settings.InFileTypes(), fileType)) &&
		(len(f.Settings.OutFileTypes()) == 0 || !ContainsFileType(f.Settings.OutFileTypes(), fileType))
}

func (f *Finder) isMatchingFileSize(fileSize int64) bool {
	return (f.Settings.maxSize == 0 || fileSize <= f.Settings.maxSize) &&
		(f.Settings.minSize == 0 || fileSize >= f.Settings.minSize)
}

func (f *Finder) isMatchingLastMod(lastMod time.Time) bool {
	return (f.Settings.maxLastMod.IsZero() || lastMod.Compare(f.Settings.maxLastMod) <= 0) &&
		(f.Settings.minLastMod.IsZero() || lastMod.Compare(f.Settings.minLastMod) >= 0)
}

func (f *Finder) isMatchingArchiveFileResult(fr *FileResult) bool {
	return f.hasMatchingArchiveExtension(fr) &&
		f.isMatchingArchiveFileName(fr.Name)
}

func (f *Finder) isMatchingFileResult(fr *FileResult) bool {
	return f.hasMatchingExtension(fr) &&
		f.isMatchingFileName(fr.Name) &&
		f.isMatchingFileType(fr.FileType) &&
		f.isMatchingFileSize(fr.FileSize) &&
		f.isMatchingLastMod(fr.LastMod)
}

func (f *Finder) filterToFileResult(filePath string, fi os.FileInfo) *FileResult {
	if !f.Settings.IncludeHidden() && isHidden(filePath) {
		return nil
	}
	dir, file := filepath.Split(filePath)
	if dir == "" {
		dir = "."
	} else {
		dir = normalizePath(dir)
	}
	fileType := f.fileTypes.GetFileType(file)
	if fileType == FileTypeArchive && !f.Settings.includeArchives && !f.Settings.archivesOnly {
		return nil
	}
	var fileSize int64 = 0
	lastMod := time.Time{}
	if fi != nil {
		fileSize = fi.Size()
		lastMod = fi.ModTime()
	}
	fr := NewFileResult(dir, file, fileType, fileSize, lastMod)
	if fr.FileType == FileTypeArchive {
		if f.isMatchingArchiveFileResult(fr) {
			return fr
		}
		return nil
	}
	if !f.Settings.ArchivesOnly() && f.isMatchingFileResult(fr) {
		return fr
	}
	return nil
}

func (f *Finder) checkAddFileResult(filePath string, fi os.FileInfo) {
	path, _ := filepath.Split(filePath)
	// TODO: I think the dir has already been checked at this point
	if f.isMatchingDir(path) {
		fileResult := f.filterToFileResult(filePath, fi)
		if fileResult != nil {
			f.addResultChan <- fileResult
		}
	}
}

// this method passed to the filepath.WalkDir method, it must have this signature
func (f *Finder) checkAddFindWalkFile(filePath string, entry fs.DirEntry, err error) error {
	if err != nil {
		fmt.Printf("an error occurred accessing path %q: %v\n", filePath, err)
		return err
	}
	if entry.IsDir() && !f.isMatchingDir(entry.Name()) {
		return filepath.SkipDir
	} else if entry.Type().IsRegular() {
		fi, err := entry.Info()
		if err != nil {
			return err
		}
		f.checkAddFileResult(filePath, fi)
	}
	return nil
}

// if we get true from addResultsDoneChan, set addResultsDone to true
// if we get a result from addResultChan channel, add to fileResults
// if we get an error from errChan channel, add to errors
func (f *Finder) activateFindChannels() {
	addResultsDone := false
	for !addResultsDone {
		select {
		case b := <-f.addResultsDoneChan:
			addResultsDone = b
			f.findDoneChan <- b
		case r := <-f.addResultChan:
			f.fileResults.AddResult(r)
		case e := <-f.errChan:
			f.errors = append(f.errors, e)
		}
	}
}

func (f *Finder) recSetFileResultsForPath(path string, minDepth int, maxDepth int, currentDepth int) error {
	recurse := true
	if currentDepth == maxDepth {
		recurse = false
	} else if maxDepth > -1 && currentDepth > maxDepth {
		return nil
	}
	var dirs []string
	entries, err := os.ReadDir(path)
	if err != nil {
		return err
	}
	for _, entry := range entries {
		if entry.IsDir() && recurse && f.isMatchingDir(entry.Name()) {
			dirs = append(dirs, filepath.Join(path, entry.Name()))
		} else if entry.Type().IsRegular() && (minDepth < 0 || currentDepth >= minDepth) {
			fi, _ := entry.Info()
			f.checkAddFileResult(filepath.Join(path, fi.Name()), fi)
		}
	}
	for _, dir := range dirs {
		err = f.recSetFileResultsForPath(dir, minDepth, maxDepth, currentDepth+1)
		if err != nil {
			return err
		}
	}
	return nil
}

func (f *Finder) setFileResultsForPath(path string) error {
	fi, err := os.Stat(path)
	if err != nil {
		return err
	}
	if fi.IsDir() {
		// if MaxDepth is zero, we can skip since a directory cannot be a result
		if f.Settings.MaxDepth() == 0 {
			return nil
		}
		if f.isMatchingDir(path) {
			maxDepth := f.Settings.MaxDepth()
			if !f.Settings.Recursive() {
				maxDepth = 1
			}
			return f.recSetFileResultsForPath(path, f.Settings.MinDepth(), maxDepth, 1)
		} else if fi.Mode().Type().IsRegular() {
			if f.Settings.minDepth > 0 {
				return nil
			}
			f.checkAddFileResult(filepath.Join(path, fi.Name()), fi)
		}
	} else if fi.Mode().IsRegular() {
		// if MinDepth > zero, we can skip since the file is at depth zero
		if f.Settings.MinDepth() <= 0 {
			f.checkAddFileResult(path, fi)
		}
	}
	return nil
}

func (f *Finder) setFileResults() error {
	if f.Settings.Verbose() {
		Log("\nBuilding file result list")
	}

	for _, p := range f.Settings.Paths() {
		if err := f.setFileResultsForPath(p); err != nil {
			return err
		}
	}

	f.addResultsDoneChan <- true
	return nil
}

func (f *Finder) Find() (*FileResults, error) {
	if err := f.Settings.Validate(); err != nil {
		return nil, err
	}

	// start the find channels with goroutine
	go f.activateFindChannels()

	// send to the find channels
	if err := f.setFileResults(); err != nil {
		return nil, err
	}

	findDone := false
	for !findDone {
		select {
		case b := <-f.findDoneChan:
			findDone = b
		}
	}

	// sort the FileResults
	f.fileResults.Sort(f.Settings)

	return f.fileResults, nil
}
