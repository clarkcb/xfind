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

	if settings.PrintUsage {
		findOptions.PrintUsage()
	}

	if settings.PrintVersion {
		findOptions.PrintVersion()
	}

	if settings.Debug {
		fmt.Printf("settings: %s\n", settings.String())
	}

	finder := gofind.NewFinder(settings)
	err = finder.Find()
	if err != nil {
		errorAndExit(err, findOptions)
	}

	// if there are results and PrintResults is true then print them out
	if settings.PrintResults {
		fmt.Println()
		finder.PrintFindResults()
	}

	if settings.ListDirs {
		fmt.Println()
		finder.PrintDirCounts()
	}

	if settings.ListFiles {
		fmt.Println()
		finder.PrintFileCounts()
	}

	if settings.ListLines {
		fmt.Println()
		if settings.UniqueLines {
			finder.PrintUniqueLineCounts()
		} else {
			finder.PrintLineCounts()
		}
	}
}
