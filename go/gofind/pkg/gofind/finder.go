package gofind

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
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

func (f *Finder) isMatchingArchiveFileResult(fr *FileResult) bool {
	if len(f.Settings.InArchiveExtensions()) > 0 || len(f.Settings.OutArchiveExtensions()) > 0 {
		ext := GetExtension(fr.Name)
		if (len(f.Settings.InArchiveExtensions()) > 0 && !Contains(f.Settings.InArchiveExtensions(), ext)) ||
			(len(f.Settings.OutArchiveExtensions()) > 0 && Contains(f.Settings.OutArchiveExtensions(), ext)) {
			return false
		}
	}
	return (f.Settings.InArchiveFilePatterns().IsEmpty() || f.Settings.InArchiveFilePatterns().MatchesAny(fr.Name)) &&
		(f.Settings.OutArchiveFilePatterns().IsEmpty() || !f.Settings.OutArchiveFilePatterns().MatchesAny(fr.Name))
}

func (f *Finder) hasMatchingExtension(fr *FileResult) bool {
	if len(f.Settings.InExtensions()) > 0 || len(f.Settings.OutExtensions()) > 0 {
		ext := GetExtension(fr.Name)
		return (len(f.Settings.InExtensions()) == 0 || Contains(f.Settings.InExtensions(), ext)) &&
			(len(f.Settings.OutExtensions()) == 0 || !Contains(f.Settings.OutExtensions(), ext))
	}
	return true
}

func (f *Finder) hasMatchingFileName(fr *FileResult) bool {
	return (f.Settings.InFilePatterns().IsEmpty() || f.Settings.InFilePatterns().MatchesAny(fr.Name)) &&
		(f.Settings.OutFilePatterns().IsEmpty() || !f.Settings.OutFilePatterns().MatchesAny(fr.Name))
}

func (f *Finder) hasMatchingFileType(fr *FileResult) bool {
	return (len(f.Settings.InFileTypes()) == 0 || ContainsFileType(f.Settings.InFileTypes(), fr.FileType)) &&
		(len(f.Settings.OutFileTypes()) == 0 || !ContainsFileType(f.Settings.OutFileTypes(), fr.FileType))
}

func (f *Finder) hasMatchingFileSize(fr *FileResult) bool {
	return (f.Settings.maxSize == 0 || fr.FileSize <= f.Settings.maxSize) &&
		(f.Settings.minSize == 0 || fr.FileSize >= f.Settings.minSize)
}

func (f *Finder) hasMatchingLastMod(fr *FileResult) bool {
	return (f.Settings.maxLastMod.IsZero() || fr.LastMod.Compare(f.Settings.maxLastMod) <= 0) &&
		(f.Settings.minLastMod.IsZero() || fr.LastMod.Compare(f.Settings.minLastMod) >= 0)
}

func (f *Finder) isMatchingFileResult(fr *FileResult) bool {
	return f.hasMatchingExtension(fr) &&
		f.hasMatchingFileName(fr) &&
		f.hasMatchingFileSize(fr) &&
		f.hasMatchingFileType(fr) &&
		f.hasMatchingFileSize(fr) &&
		f.hasMatchingLastMod(fr)
}

func (f *Finder) FilePathToFileResult(filePath string, fi os.FileInfo) *FileResult {
	dir, file := filepath.Split(filePath)
	if dir == "" {
		dir = "."
	} else {
		dir = normalizePath(dir)
	}
	t := f.fileTypes.GetFileType(file)
	var fileSize int64 = 0
	lastMod := time.Time{}
	if fi != nil {
		fileSize = fi.Size()
		lastMod = fi.ModTime()
	}
	return NewFileResult(dir, file, t, fileSize, lastMod)
}

func (f *Finder) filterToFileResult(filePath string, fi os.FileInfo) *FileResult {
	if !f.Settings.IncludeHidden() && isHidden(filePath) {
		return nil
	}
	fr := f.FilePathToFileResult(filePath, fi)
	if fr.FileType == FileTypeArchive {
		if f.Settings.IncludeArchives() && f.isMatchingArchiveFileResult(fr) {
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

func (f *Finder) setFileResults() error {
	if f.Settings.Verbose() {
		Log("\nBuilding file result list")
	}

	for _, p := range f.Settings.Paths() {
		normPath := normalizePath(p)
		fi, err := os.Stat(normPath)
		if err != nil {
			return err
		}
		if fi.IsDir() {
			// if MaxDepth is zero, we can skip since a directory cannot be a result
			if f.Settings.MaxDepth() == 0 {
				continue
			}
			if f.Settings.Recursive() {
				err := filepath.WalkDir(normPath, func(path string, entry fs.DirEntry, err error) error {
					if err != nil {
						fmt.Printf("an error occurred accessing path %q: %v\n", path, err)
						return err
					}
					startPathSepCount := strings.Count(normPath, string(os.PathSeparator))
					pathSepCount := strings.Count(path, string(os.PathSeparator))
					depth := pathSepCount - startPathSepCount
					if entry.IsDir() {
						if f.Settings.MaxDepth() > -1 && depth > f.Settings.MaxDepth() {
							return filepath.SkipDir
						}
						if !f.isMatchingDir(entry.Name()) {
							return filepath.SkipDir
						}
					} else if entry.Type().IsRegular() {
						if depth >= f.Settings.MinDepth() && (f.Settings.MaxDepth() < 1 || depth <= f.Settings.MaxDepth()) {
							fi, _ := entry.Info()
							f.checkAddFileResult(path, fi)
						}
					}
					return nil
				})
				if err != nil {
					return err
				}
			} else {
				entries, err := os.ReadDir(normPath)
				if err != nil {
					return err
				}

				for _, entry := range entries {
					fi, _ := entry.Info()
					f.checkAddFileResult(filepath.Join(p, entry.Name()), fi)
				}
			}
		} else if fi.Mode().IsRegular() {
			// if MinDepth > zero, we can skip since the file is at depth zero
			if f.Settings.MinDepth() <= 0 {
				f.checkAddFileResult(p, fi)
			}
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
