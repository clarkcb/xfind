package gofind

import (
	"fmt"
	"golang.org/x/text/encoding"
	"os"
	"path/filepath"
)

// Finder - the find executor
type Finder struct {
	Settings           *FindSettings
	fileTypes          *FileTypes
	fileResults        *FileResults
	errors             []error
	addResultChan      chan *FileResult
	addResultsDoneChan chan bool
	doneChan           chan string
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
		make(chan string, 1),   // doneChan
		make(chan error, 1),    // errChan
		nil,
	}
}

func (f *Finder) validateSettings() error {
	if len(f.Settings.Paths) < 1 {
		return fmt.Errorf("Startpath not defined")
	}

	for _, p := range f.Settings.Paths {
		_, err := os.Stat(p)
		if err != nil {
			if os.IsNotExist(err) {
				return fmt.Errorf("Startpath not found")
			}
			if os.IsPermission(err) {
				return fmt.Errorf("Startpath not readable")
			}
			return err
		}
	}

	return nil
}

func (f *Finder) isMatchingDir(d string) bool {
	if f.Settings.ExcludeHidden && isHidden(d) {
		return false
	}
	return (f.Settings.InDirPatterns.IsEmpty() || f.Settings.InDirPatterns.MatchesAny(d)) &&
		(f.Settings.OutDirPatterns.IsEmpty() || !f.Settings.OutDirPatterns.MatchesAny(d))
}

func (f *Finder) isMatchingArchiveFileResult(fr *FileResult) bool {
	ext := getExtension(fr.Name)
	return (len(f.Settings.InArchiveExtensions) == 0 || contains(f.Settings.InArchiveExtensions, ext)) &&
		(len(f.Settings.OutArchiveExtensions) == 0 || !contains(f.Settings.OutArchiveExtensions, ext)) &&
		(f.Settings.InArchiveFilePatterns.IsEmpty() || f.Settings.InArchiveFilePatterns.MatchesAny(fr.Name)) &&
		(f.Settings.OutArchiveFilePatterns.IsEmpty() || !f.Settings.OutArchiveFilePatterns.MatchesAny(fr.Name))
}

func (f *Finder) isMatchingFileResult(fr *FileResult) bool {
	ext := getExtension(fr.Name)
	return (len(f.Settings.InExtensions) == 0 || contains(f.Settings.InExtensions, ext)) &&
		(len(f.Settings.OutExtensions) == 0 || !contains(f.Settings.OutExtensions, ext)) &&
		(len(f.Settings.InFileTypes) == 0 || containsFileType(f.Settings.InFileTypes, fr.FileType)) &&
		(len(f.Settings.OutFileTypes) == 0 || !containsFileType(f.Settings.OutFileTypes, fr.FileType)) &&
		(f.Settings.InFilePatterns.IsEmpty() || f.Settings.InFilePatterns.MatchesAny(fr.Name)) &&
		(f.Settings.OutFilePatterns.IsEmpty() || !f.Settings.OutFilePatterns.MatchesAny(fr.Name))
}

func (f *Finder) FilePathToFileResult(filePath string) *FileResult {
	dir, file := filepath.Split(filePath)
	if dir == "" {
		dir = "."
	} else {
		dir = normalizePath(dir)
	}
	t := f.fileTypes.getFileType(file)
	return NewFileResult(dir, file, t)
}

func (f *Finder) filterToFileResult(filePath string) *FileResult {
	if f.Settings.ExcludeHidden && isHidden(filePath) {
		return nil
	}
	fr := f.FilePathToFileResult(filePath)
	if fr.FileType == FiletypeArchive {
		if f.Settings.IncludeArchives && f.isMatchingArchiveFileResult(fr) {
			return fr
		}
		return nil
	}
	if !f.Settings.ArchivesOnly && f.isMatchingFileResult(fr) {
		return fr
	}
	return nil
}

func (f *Finder) checkAddFileResult(filePath string) {
	path, _ := filepath.Split(filePath)
	if f.isMatchingDir(path) {
		fileResult := f.filterToFileResult(filePath)
		if fileResult != nil {
			f.addResultChan <- fileResult
		}
	}
}

// this method passed to the filepath.Walk method, it must have this signature
func (f *Finder) checkAddFindWalkFile(filePath string, fi os.FileInfo, err error) error {
	if err != nil {
		fmt.Printf("an error occurred accessing path %q: %v\n", filePath, err)
		return err
	}
	if fi.IsDir() && !f.isMatchingDir(fi.Name()) {
		return filepath.SkipDir
	} else if fi.Mode().IsRegular() {
		f.checkAddFileResult(filePath)
	}
	return nil
}

func (f *Finder) setFileResults() error {
	if f.Settings.Verbose {
		log("\nBuilding file result list")
	}

	for _, p := range f.Settings.Paths {
		normPath := normalizePath(p)
		fi, err := os.Stat(normPath)
		if err != nil {
			return err
		}
		if fi.IsDir() {
			if f.Settings.Recursive {
				err := filepath.Walk(normPath, f.checkAddFindWalkFile)
				if err != nil {
					return err
				}
			} else {
				entries, err := os.ReadDir(normPath)
				if err != nil {
					return err
				}

				for _, entry := range entries {
					f.checkAddFileResult(filepath.Join(p, entry.Name()))
				}
			}
		} else if fi.Mode().IsRegular() {
			f.checkAddFileResult(p)
		}
	}

	f.addResultsDoneChan <- true
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
		case r := <-f.addResultChan:
			f.fileResults.AddResult(r)
		case e := <-f.errChan:
			f.errors = append(f.errors, e)
		}
	}
}

func (f *Finder) Find() (*FileResults, error) {
	// validate the settings
	if err := f.validateSettings(); err != nil {
		return nil, err
	}

	// start the find channels with goroutine
	go f.activateFindChannels()

	// send to the find channels
	if err := f.setFileResults(); err != nil {
		return nil, err
	}

	// sort the results
	f.fileResults.Sort(f.Settings.SortBy, f.Settings.SortDescending)

	return f.fileResults, nil
}
