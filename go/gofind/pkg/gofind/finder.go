// Package gofind /*
package gofind

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"sync"

	"golang.org/x/text/encoding"
)

// Finder - the find executor
type Finder struct {
	Settings         *FindSettings
	fileTypes        *FileTypes
	findItems        *FindItems
	errors           []error
	addItemChan      chan *FindItem
	addItemsDoneChan chan bool
	doneChan         chan string
	errChan          chan error
	textDecoder      *encoding.Decoder
}

func NewFinder(settings *FindSettings) *Finder {
	return &Finder{
		settings,             // Settings
		FileTypesFromJson(),  // fileTypes
		NewFindItems(),       // findItems
		[]error{},            // errors
		make(chan *FindItem), // addItemChan
		make(chan bool),      // addItemsDoneChan
		make(chan string, 1), // doneChan
		make(chan error, 1),  // errChan
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

func (f *Finder) isFindDir(d string) bool {
	if f.Settings.ExcludeHidden && isHidden(d) {
		return false
	}
	return (f.Settings.InDirPatterns.IsEmpty() || f.Settings.InDirPatterns.MatchesAny(d)) &&
		(f.Settings.OutDirPatterns.IsEmpty() || !f.Settings.OutDirPatterns.MatchesAny(d))
}

func (f *Finder) isArchiveFindItem(fi *FindItem) bool {
	ext := getExtension(fi.Name)
	return (len(f.Settings.InArchiveExtensions) == 0 || contains(f.Settings.InArchiveExtensions, ext)) &&
		(len(f.Settings.OutArchiveExtensions) == 0 || !contains(f.Settings.OutArchiveExtensions, ext)) &&
		(f.Settings.InArchiveFilePatterns.IsEmpty() || f.Settings.InArchiveFilePatterns.MatchesAny(fi.Name)) &&
		(f.Settings.OutArchiveFilePatterns.IsEmpty() || !f.Settings.OutArchiveFilePatterns.MatchesAny(fi.Name))
}

func (f *Finder) isFindItem(fi *FindItem) bool {
	ext := getExtension(fi.Name)
	return (len(f.Settings.InExtensions) == 0 || contains(f.Settings.InExtensions, ext)) &&
		(len(f.Settings.OutExtensions) == 0 || !contains(f.Settings.OutExtensions, ext)) &&
		(len(f.Settings.InFileTypes) == 0 || containsFileType(f.Settings.InFileTypes, fi.fileType)) &&
		(len(f.Settings.OutFileTypes) == 0 || !containsFileType(f.Settings.OutFileTypes, fi.fileType)) &&
		(f.Settings.InFilePatterns.IsEmpty() || f.Settings.InFilePatterns.MatchesAny(fi.Name)) &&
		(f.Settings.OutFilePatterns.IsEmpty() || !f.Settings.OutFilePatterns.MatchesAny(fi.Name))
}

func (f *Finder) FilePathToFindItem(filePath string) *FindItem {
	dir, file := filepath.Split(filePath)
	if dir == "" {
		dir = "."
	} else {
		dir = normalizePath(dir)
	}
	t := f.fileTypes.getFileType(file)
	return NewFindItem(dir, file, t)
}

func (f *Finder) filterToFindItem(filePath string) *FindItem {
	if f.Settings.ExcludeHidden && isHidden(filePath) {
		return nil
	}
	fi := f.FilePathToFindItem(filePath)
	if fi.fileType == FiletypeArchive {
		if f.Settings.IncludeArchives && f.isArchiveFindItem(fi) {
			return fi
		}
		return nil
	}
	if !f.Settings.ArchivesOnly && f.isFindItem(fi) {
		return fi
	}
	return nil
}

func (f *Finder) checkAddFindFile(filePath string) {
	path, _ := filepath.Split(filePath)
	if f.isFindDir(path) {
		findItem := f.filterToFindItem(filePath)
		if findItem != nil {
			f.addItemChan <- findItem
		}
	}
}

// this method passed to the filepath.Walk method, it must have this signature
func (f *Finder) checkAddFindWalkFile(filePath string, fi os.FileInfo, err error) error {
	if err != nil {
		fmt.Printf("an error occurred accessing path %q: %v\n", filePath, err)
		return err
	}
	if fi.IsDir() && !f.isFindDir(fi.Name()) {
		return filepath.SkipDir
	} else if fi.Mode().IsRegular() {
		f.checkAddFindFile(filePath)
	}
	return nil
}

func (f *Finder) setFindFiles() error {
	if f.Settings.Verbose {
		log("\nBuilding file find list")
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
					f.checkAddFindFile(filepath.Join(p, entry.Name()))
				}
			}
		} else if fi.Mode().IsRegular() {
			f.checkAddFindFile(p)
		}
	}

	f.addItemsDoneChan <- true
	return nil
}

func (f *Finder) setFindFilesGoRoutines() error {
	if f.Settings.Verbose {
		log("\nBuilding file find list")
	}

	wg := &sync.WaitGroup{}
	wg.Add(len(f.Settings.Paths)) // set the WaitGroup counter to Paths length

	for _, p := range f.Settings.Paths {
		go func(wg *sync.WaitGroup, p string) {
			startPath := normalizePath(p)
			fi, err := os.Stat(startPath)
			if err != nil {
				f.errChan <- err
				return
			}
			if fi.IsDir() {
				if f.Settings.Recursive {
					err := filepath.Walk(startPath, f.checkAddFindWalkFile)
					if err != nil {
						f.errChan <- err
						return
					}
				} else {
					files, err := ioutil.ReadDir(startPath)
					if err != nil {
						f.errChan <- err
						return
					}

					for _, file := range files {
						f.checkAddFindFile(file.Name())
					}
				}

			} else if fi.Mode().IsRegular() {
				f.checkAddFindFile(p)
			}
			f.doneChan <- p
			wg.Done()
		}(wg, p)

		if len(f.errors) > 0 {
			break
		}
	}

	if len(f.errors) > 0 {
		return f.errors[0]
	}

	return nil
}

//get the find items (files) from the file channel
func (f *Finder) activateFindItemChannels() {
	addItemsDone := false
	for !addItemsDone {
		select {
		case b := <-f.addItemsDoneChan:
			addItemsDone = b
		case i := <-f.addItemChan:
			f.findItems.AddItem(i)
		case e := <-f.errChan:
			f.errors = append(f.errors, e)
		}
	}
}

func (f *Finder) Find() (*FindItems, error) {
	if err := f.validateSettings(); err != nil {
		return nil, err
	}

	// first start the activateFindItemChannels goroutine
	go f.activateFindItemChannels()

	// now fill the findItem channels
	if err := f.setFindFiles(); err != nil {
		return nil, err
	}

	return f.findItems, nil
}
