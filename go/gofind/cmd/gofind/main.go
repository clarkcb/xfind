package main

import (
	"fmt"
	"gofind/pkg/gofind"
	"os"
)

func errorAndExit(err error, findOptions *gofind.FindOptions) {
	fmt.Printf("\nERROR: %s\n", err)
	findOptions.PrintUsage()
}

func main() {
	findOptions := gofind.NewFindOptions()
	settings, err := findOptions.FindSettingsFromArgs(os.Args[1:])
	if err != nil {
		errorAndExit(err, findOptions)
	}

	if settings.PrintUsage() {
		findOptions.PrintUsage()
	}

	if settings.PrintVersion() {
		findOptions.PrintVersion()
	}

	if settings.Debug() {
		fmt.Printf("settings: %s\n", settings.String())
	}

	finder := gofind.NewFinder(settings)
	fileResults, err := finder.Find()
	if err != nil {
		errorAndExit(err, findOptions)
	}

	// print matching dirs
	if settings.ListDirs() {
		fileResults.PrintMatchingDirs()
	}

	// print matching files (should default to true)
	if settings.ListFiles() {
		fileResults.PrintMatchingFiles()
	}
}
