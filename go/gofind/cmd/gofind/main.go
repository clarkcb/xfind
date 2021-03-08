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
	findItems, err := finder.Find()
	if err != nil {
		errorAndExit(err, findOptions)
	}

	// if there are results and PrintResults is true then print them out
	if settings.ListDirs {
		findItems.PrintMatchingDirs()
	}

	if settings.ListFiles {
		findItems.PrintMatchingFiles()
	}
}
