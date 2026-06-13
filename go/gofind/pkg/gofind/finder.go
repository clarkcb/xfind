package gofind

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
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

func NewFinder(settings *FindSettings) (*Finder, error) {
	fileTypes, err := FileTypesFromJson()
	if err != nil {
		return nil, err
	}
	finder := &Finder{
		settings,               // Settings
		fileTypes,              // fileTypes
		NewFileResults(),       // fileResults
		[]error{},              // errors
		make(chan *FileResult), // addResultChan
		make(chan bool),        // addResultsDoneChan
		make(chan bool, 1),     // findDoneChan
		make(chan error, 1),    // errChan
	}
	err = finder.validateSettings()
	if err != nil {
		return nil, err
	}
	return finder, nil
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
			if !f.isTraversableDirPath(p) {
				return fmt.Errorf(StartPathNotMatchFindSettings)
			}

		} else if fi.Mode().Type().IsRegular() {
			fileResult := f.filterToFileResult(p, fi)
			if fileResult == nil {
				return fmt.Errorf(StartPathNotMatchFindSettings)
			}

		} else {
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

func emptyOrMatchesAnyPattern(s string, patterns *Patterns) bool {
	return patterns.IsEmpty() || patterns.MatchesAny(s)
}

func emptyOrNotMatchesAnyPattern(s string, patterns *Patterns) bool {
	return patterns.IsEmpty() || !patterns.MatchesAny(s)
}

func emptyOrAnyMatchesAnyPattern(strings []string, patterns *Patterns) bool {
	return patterns.IsEmpty() || patterns.AnyMatchesAny(strings)
}

func emptyOrNotAnyMatchesAnyPattern(strings []string, patterns *Patterns) bool {
	return patterns.IsEmpty() || !patterns.AnyMatchesAny(strings)
}

func emptyOrMatchesAnyString(s string, strings []string) bool {
	return len(strings) == 0 || Contains(strings, s)
}

func emptyOrNotMatchesAnyString(s string, strings []string) bool {
	return len(strings) == 0 || !Contains(strings, s)
}

func emptyOrMatchesAnyFileType(fileType FileType, fileTypes []FileType) bool {
	return len(fileTypes) == 0 || ContainsFileType(fileTypes, fileType)
}

func emptyOrNotMatchesAnyFileType(fileType FileType, fileTypes []FileType) bool {
	return len(fileTypes) == 0 || !ContainsFileType(fileTypes, fileType)
}

func (f *Finder) isMatchingDirPathByHidden(dirPath string) bool {
	return f.Settings.IncludeHidden() || !IsHiddenPath(dirPath)
}

func (f *Finder) isMatchingDirPathByInPatterns(dirPath string) bool {
	return emptyOrAnyMatchesAnyPattern(GetPathElems(dirPath), f.Settings.InDirPatterns())
}

func (f *Finder) isMatchingDirPathByOutPatterns(dirPath string) bool {
	return emptyOrNotAnyMatchesAnyPattern(GetPathElems(dirPath), f.Settings.OutDirPatterns())
}

func (f *Finder) isTraversableDirPath(dirPath string) bool {
	return f.isMatchingDirPathByHidden(dirPath) &&
		f.isMatchingDirPathByOutPatterns(dirPath)
}

func (f *Finder) isMatchingDirPath(dirPath string) bool {
	return f.isMatchingDirPathByHidden(dirPath) &&
		f.isMatchingDirPathByInPatterns(dirPath) &&
		f.isMatchingDirPathByOutPatterns(dirPath)
}

func (f *Finder) isNullOrMatchingDirPath(dirPath string) bool {
	return strings.TrimSpace(dirPath) == "" ||
		(f.isMatchingDirPathByHidden(dirPath) &&
			f.isMatchingDirPathByInPatterns(dirPath) &&
			f.isMatchingDirPathByOutPatterns(dirPath))
}

func (f *Finder) isMatchingFileNameByHidden(fileName string) bool {
	return f.Settings.IncludeHidden() || !IsHiddenName(fileName)
}

func (f *Finder) isMatchingArchiveExtension(ext string) bool {
	return emptyOrMatchesAnyString(ext, f.Settings.InArchiveExtensions()) &&
		emptyOrNotMatchesAnyString(ext, f.Settings.OutArchiveExtensions())
}

func (f *Finder) hasMatchingArchiveExtension(filePath string) bool {
	if len(f.Settings.InArchiveExtensions()) > 0 || len(f.Settings.OutArchiveExtensions()) > 0 {
		ext := GetExtension(filePath)
		return f.isMatchingArchiveExtension(ext)
	}
	return true
}

func (f *Finder) isMatchingArchiveFileName(fileName string) bool {
	return f.isMatchingFileNameByHidden(fileName) &&
		emptyOrMatchesAnyPattern(fileName, f.Settings.InArchiveFilePatterns()) &&
		emptyOrNotMatchesAnyPattern(fileName, f.Settings.OutArchiveFilePatterns())
}

func (f *Finder) hasMatchingArchiveFileName(filePath string) bool {
	if !f.Settings.InArchiveFilePatterns().IsEmpty() || !f.Settings.OutArchiveFilePatterns().IsEmpty() {
		return f.isMatchingArchiveFileName(filepath.Base(filePath))
	}
	return true
}

func (f *Finder) isMatchingArchiveFilePath(filePath string) bool {
	return f.hasMatchingArchiveExtension(filePath) &&
		f.hasMatchingArchiveFileName(filePath)
}

func (f *Finder) isMatchingArchiveFileResult(fr *FileResult) bool {
	return f.hasMatchingArchiveExtension(fr.FilePath) &&
		f.hasMatchingArchiveFileName(fr.FilePath)
}

func (f *Finder) isMatchingExtension(ext string) bool {
	return emptyOrMatchesAnyString(ext, f.Settings.InExtensions()) &&
		emptyOrNotMatchesAnyString(ext, f.Settings.OutExtensions())
}

func (f *Finder) hasMatchingExtension(filePath string) bool {
	if len(f.Settings.InExtensions()) > 0 || len(f.Settings.OutExtensions()) > 0 {
		ext := GetExtension(filePath)
		return f.isMatchingExtension(ext)
	}
	return true
}

func (f *Finder) isMatchingFileName(fileName string) bool {
	return f.isMatchingFileNameByHidden(fileName) &&
		emptyOrMatchesAnyPattern(fileName, f.Settings.InFilePatterns()) &&
		emptyOrNotMatchesAnyPattern(fileName, f.Settings.OutFilePatterns())
}

func (f *Finder) hasMatchingFileName(filePath string) bool {
	if !f.Settings.InFilePatterns().IsEmpty() || !f.Settings.OutFilePatterns().IsEmpty() {
		return f.isMatchingFileName(filepath.Base(filePath))
	}
	return true
}

func (f *Finder) isMatchingFilePath(filePath string) bool {
	return f.isNullOrMatchingDirPath(filepath.Dir(filePath)) &&
		f.hasMatchingExtension(filePath) &&
		f.hasMatchingFileName(filePath)
}

func (f *Finder) isMatchingFileType(fileType FileType) bool {
	return emptyOrMatchesAnyFileType(fileType, f.Settings.InFileTypes()) &&
		emptyOrNotMatchesAnyFileType(fileType, f.Settings.OutFileTypes())
}

func (f *Finder) isMatchingFileSize(fileSize int64) bool {
	return (f.Settings.maxSize == 0 || fileSize <= f.Settings.maxSize) &&
		(f.Settings.minSize == 0 || fileSize >= f.Settings.minSize)
}

func (f *Finder) isMatchingLastMod(lastMod time.Time) bool {
	return (f.Settings.maxLastMod.IsZero() || lastMod.Compare(f.Settings.maxLastMod) <= 0) &&
		(f.Settings.minLastMod.IsZero() || lastMod.Compare(f.Settings.minLastMod) >= 0)
}

func (f *Finder) isMatchingFileResult(fr *FileResult) bool {
	return f.isMatchingFilePath(fr.FilePath) &&
		f.isMatchingFileType(fr.FileType) &&
		f.isMatchingFileSize(fr.FileSize) &&
		f.isMatchingLastMod(fr.LastMod)
}

func (f *Finder) filterArchiveFilePathToFileResult(filePath string, fi os.FileInfo) *FileResult {
	if !f.Settings.includeArchives && !f.Settings.archivesOnly {
		return nil
	}

	if !f.isMatchingArchiveFilePath(filePath) {
		return nil
	}

	var fileSize int64 = 0
	lastMod := time.Time{}
	if fi != nil {
		fileSize = fi.Size()
		lastMod = fi.ModTime()
	}

	return NewFileResult(filePath, FileTypeArchive, fileSize, lastMod)
}

func (f *Finder) filterRegFilePathToFileResult(filePath string, fileType FileType, fi os.FileInfo) *FileResult {
	if f.Settings.archivesOnly {
		return nil
	}

	if !f.isMatchingFilePath(filePath) || !f.isMatchingFileType(fileType) {
		return nil
	}

	var fileSize int64 = 0
	lastMod := time.Time{}
	if fi != nil {
		fileSize = fi.Size()
		lastMod = fi.ModTime()
	}

	return NewFileResult(filePath, fileType, fileSize, lastMod)
}

func (f *Finder) filterToFileResult(filePath string, fi os.FileInfo) *FileResult {
	if !f.isNullOrMatchingDirPath(filepath.Dir(filePath)) || !f.isMatchingFileNameByHidden(filepath.Base(filePath)) {
		return nil
	}

	fileType := f.fileTypes.GetFileType(filePath)
	if fileType == FileTypeArchive {
		return f.filterArchiveFilePathToFileResult(filePath, fi)
	}

	return f.filterRegFilePathToFileResult(filePath, fileType, fi)
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
	if entry.IsDir() && (!f.isTraversableDirPath(entry.Name())) {
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
		if fileInfo.IsDir() && recurse && f.isTraversableDirPath(entry.Name()) {
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

		if f.isTraversableDirPath(path) {
			maxDepth := f.Settings.MaxDepth()
			if !f.Settings.Recursive() {
				maxDepth = 1
			}
			return f.recSetFileResultsForPath(path, f.Settings.MinDepth(), maxDepth, 1)
		}

		return fmt.Errorf(StartPathNotMatchFindSettings)

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
