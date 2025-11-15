package main

import (
	"fmt"
	"gofind/pkg/gofind"
	"os"
)

func errorAndExit(err error, findOptions *gofind.FindOptions, settings *gofind.FindSettings) {
	gofind.Log("")
	if settings.Colorize() {
		gofind.LogErrorColor(fmt.Sprintf("%s", err))
	} else {
		gofind.LogError(fmt.Sprintf("%s", err))
	}
	findOptions.PrintUsage()
}

func main() {
	findOptions := gofind.NewFindOptions()
	settings, err := findOptions.FindSettingsFromArgs(os.Args[1:])
	if err != nil {
		errorAndExit(err, findOptions, settings)
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
		errorAndExit(err, findOptions, settings)
	}
	formatter := gofind.NewFileResultFormatter(settings)

	// print matching dirs
	if settings.PrintDirs() {
		fileResults.PrintMatchingDirs(formatter)
	}

	// print matching files (should default to true)
	if settings.PrintFiles() {
		fileResults.PrintMatchingFiles(formatter)
	}
}
