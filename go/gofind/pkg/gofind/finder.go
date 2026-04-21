package gofind

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"
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
	}
}

func (f *Finder) validateSettings() error {
	if len(f.Settings.Paths()) < 1 {
		return fmt.Errorf(StartPathNotDefined)
	}

	for _, p := range f.Settings.Paths() {
		fi, err := os.Lstat(ExpandPath(p))
		if err != nil {
			if os.IsNotExist(err) {
				return fmt.Errorf(StartPathNotFound)
			}
			if os.IsPermission(err) {
				return fmt.Errorf(StartPathNotReadable)
			}
			return err
		}

		if fi.Mode()&os.ModeSymlink == os.ModeSymlink {
			if !f.Settings.FollowSymlinks() {
				return fmt.Errorf(StartPathNotMatchFindSettings)
			}
		} else if fi.IsDir() {
			// This means that this method should be in Finder
			if !f.filterDirByHidden(p) || !f.filterDirByOutPatterns(p) {
				return fmt.Errorf(StartPathNotMatchFindSettings)
			}

		} else if fi.Mode().Type().IsRegular() {
			fileResult := f.filterToFileResult(p, fi)
			if fileResult == nil {
				return fmt.Errorf(StartPathNotMatchFindSettings)
			}

		} else {
			// TODO: handle symlinks, etc.?
			return fmt.Errorf(StartPathNotMatchFindSettings)
		}
	}

	if f.Settings.maxDepth > -1 && f.Settings.maxDepth < f.Settings.minDepth {
		return fmt.Errorf(InvalidRangeForMinDepthAndMaxDepth)
	}
	if !f.Settings.maxLastMod.IsZero() && f.Settings.minLastMod.After(f.Settings.maxLastMod) {
		return fmt.Errorf(InvalidRangeForMinLastModAndMaxLastMod)
	}
	if f.Settings.maxSize > 0 && f.Settings.maxSize < f.Settings.minSize {
		return fmt.Errorf(InvalidRangeForMinSizeAndMaxSize)
	}

	return nil
}

func (f *Finder) filterDirByHidden(d string) bool {
	return f.Settings.IncludeHidden() || !IsHiddenPath(d)
}

func (f *Finder) filterDirByInPatterns(d string) bool {
	return f.Settings.InDirPatterns().IsEmpty() || f.Settings.InDirPatterns().MatchesAny(d)
}

func (f *Finder) filterDirByOutPatterns(d string) bool {
	return f.Settings.OutDirPatterns().IsEmpty() || !f.Settings.OutDirPatterns().MatchesAny(d)
}

func (f *Finder) isMatchingDir(d string) bool {
	return f.filterDirByHidden(d) && f.filterDirByInPatterns(d) && f.filterDirByOutPatterns(d)
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

func (f *Finder) filterArchiveFileToFileResult(filePath string, fi os.FileInfo) *FileResult {
	if !f.Settings.includeArchives && !f.Settings.archivesOnly {
		return nil
	}

	dir, fileName := filepath.Split(filePath)
	if dir == "" {
		dir = Dot
	} else {
		dir = normalizePath(dir)
	}
	if !f.isMatchingDir(dir) {
		return nil
	}
	if !f.Settings.IncludeHidden() && IsHiddenName(fileName) {
		return nil
	}

	var fileSize int64 = 0
	lastMod := time.Time{}
	if fi != nil {
		fileSize = fi.Size()
		lastMod = fi.ModTime()
	}
	fr := NewFileResult(dir, fileName, FileTypeArchive, fileSize, lastMod)

	if f.isMatchingArchiveFileResult(fr) {
		return fr
	}
	return nil
}

func (f *Finder) filterRegFileToFileResult(filePath string, fileType FileType, fi os.FileInfo) *FileResult {
	if f.Settings.archivesOnly {
		return nil
	}

	dir, fileName := filepath.Split(filePath)
	if dir == "" {
		dir = Dot
	} else {
		dir = normalizePath(dir)
	}
	if !f.isMatchingDir(dir) {
		return nil
	}
	if !f.Settings.IncludeHidden() && IsHiddenName(fileName) {
		return nil
	}

	var fileSize int64 = 0
	lastMod := time.Time{}
	if fi != nil {
		fileSize = fi.Size()
		lastMod = fi.ModTime()
	}
	fr := NewFileResult(dir, fileName, fileType, fileSize, lastMod)

	if f.isMatchingFileResult(fr) {
		return fr
	}
	return nil
}

func (f *Finder) filterToFileResult(filePath string, fi os.FileInfo) *FileResult {
	fileType := f.fileTypes.GetFileType(filePath)
	if fileType == FileTypeArchive {
		return f.filterArchiveFileToFileResult(filePath, fi)
	}

	return f.filterRegFileToFileResult(filePath, fileType, fi)
}

func (f *Finder) checkAddFileResult(filePath string, fi os.FileInfo) {
	fileResult := f.filterToFileResult(filePath, fi)
	if fileResult != nil {
		f.addResultChan <- fileResult
	}
}

// this method passed to the filepath.WalkDir method, it must have this signature
func (f *Finder) checkAddFindWalkFile(filePath string, entry fs.DirEntry, err error) error {
	if err != nil {
		fmt.Printf("an error occurred accessing path %q: %v\n", filePath, err)
		return err
	}
	if entry.IsDir() && (!f.filterDirByHidden(entry.Name()) || !f.filterDirByOutPatterns(entry.Name())) {
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
		fileInfo, _ := entry.Info()
		if fileInfo.Mode()&os.ModeSymlink == os.ModeSymlink {
			if f.Settings.followSymlinks {
				symlinkFilePath := filepath.Join(path, entry.Name())
				resolvedPath, err := filepath.EvalSymlinks(symlinkFilePath)
				if err != nil {
					return err
				}
				fileInfo, err = os.Stat(resolvedPath)
				if err != nil {
					return err
				}
			} else {
				continue
			}
		}
		if fileInfo.IsDir() && recurse && f.filterDirByHidden(entry.Name()) && f.filterDirByOutPatterns(entry.Name()) {
			dirs = append(dirs, filepath.Join(path, entry.Name()))
		} else if fileInfo.Mode().IsRegular() && (minDepth < 0 || currentDepth >= minDepth) {
			f.checkAddFileResult(filepath.Join(path, entry.Name()), fileInfo)
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
		path = ExpandPath(path)
		fi, err = os.Stat(path)
		if err != nil {
			return err
		}
	}
	if fi.IsDir() {
		// if MaxDepth is zero, we can skip since a directory cannot be a result
		if f.Settings.MaxDepth() == 0 {
			return nil
		}
		if f.filterDirByHidden(path) && f.filterDirByOutPatterns(path) {
			maxDepth := f.Settings.MaxDepth()
			if !f.Settings.Recursive() {
				maxDepth = 1
			}
			return f.recSetFileResultsForPath(path, f.Settings.MinDepth(), maxDepth, 1)
		} else {
			return fmt.Errorf(StartPathNotMatchFindSettings)
		}
	} else if fi.Mode().Type().IsRegular() {
		if f.Settings.minDepth > 0 {
			return nil
		}
		f.checkAddFileResult(path, fi)
	} else {
		// TODO: handle symlinks, etc.?
	}
	return nil
}

func (f *Finder) setFileResults() error {
	//if f.Settings.Verbose() {
	//	Log("\nBuilding file result list")
	//}

	// start the find channels with goroutine
	go f.activateFindChannels()

	for _, p := range f.Settings.Paths() {
		if err := f.setFileResultsForPath(p); err != nil {
			return err
		}
	}

	f.addResultsDoneChan <- true
	return nil
}

func (f *Finder) Find() (*FileResults, error) {
	if err := f.validateSettings(); err != nil {
		return nil, err
	}

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
