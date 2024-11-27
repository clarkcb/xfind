#!/usr/bin/env pwsh
################################################################################
#
# ps1find.ps1
#
# A powershell version of xfind
#
# TODO: install module under $env:PSModulePath
#
################################################################################
using module 'Ps1FindModule'


function PrintMatchingDirs {
    param([FileResult[]]$files)
    $dirs = @()
    if ($files.Count -gt 0) {
        $dirs = $files |
            ForEach-Object { if ($null -eq $_.File.Directory) {"."} else {$_.File.Directory.ToString()} } |
            Select-Object -Unique
    }
    if ($dirs.Count -gt 0) {
        LogMsg("`nMatching directories ($($dirs.Count)):")
        foreach ($d in $dirs) {
            # LogMsg($d.OriginalPath)
            LogMsg($d)
        }
    } else {
        LogMsg("`nMatching directories: 0")
    }
}

function PrintMatchingFiles {
    param([FileResult[]]$files)

    if ($files.Count -gt 0) {
        LogMsg("`nMatching files ($($files.Count)):")
        foreach ($f in $files) {
            LogMsg($f.File)
        }
    } else {
        LogMsg("`nMatching files: 0")
    }
}

function Main {
    param(
        [string[]]$_args
    )

    $options = [FindOptions]::new()

    try {
        $settings = $options.SettingsFromArgs($_args)

        if ($settings.Debug) {
            LogMsg($settings.ToString())
        }

        if ($settings.PrintUsage) {
            LogMsg($options.GetUsageString())
            exit
        }

        $finder = [Finder]::new($settings)
        $files = $finder.Find()

        if ($settings.PrintDirs) {
            PrintMatchingDirs($files)
        }

        if ($settings.PrintFiles) {
            PrintMatchingFiles($files)
        }
    }
    catch {
        LogError($_)
        LogMsg($options.GetUsageString())
    }
}

Main($args)
