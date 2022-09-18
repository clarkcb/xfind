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


function LogMsg {
    param([string]$msg)

    Write-Output $msg
}

function LogError {
    param([string]$msg)

    # Write-Output "ERROR: $msg`n"
    Write-Host "`nERROR: $msg" -ForegroundColor Red
}

function PrintMatchingDirs {
    param([FileResult[]]$files)

    $dirs = $files |
        ForEach-Object { if ($null -eq $_.File.Directory) {"."} else {$_.File.Directory.ToString()} } |
        # Where-Object { $null -ne $_ } |
        # Sort-Object -Property FullName |
        Select-Object -Unique
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
            LogMsg("settings: $($settings.ToString())`n")
        }

        if ($settings.PrintUsage) {
            LogMsg($options.GetUsageString())
            exit
        }

        $finder = [Finder]::new($settings)
        $files = $finder.Find()

        if ($settings.ListDirs) {
            PrintMatchingDirs($files)
        }

        if ($settings.ListFiles) {
            PrintMatchingFiles($files)
        }
    }
    catch {
        LogError($_)
        LogMsg($options.GetUsageString())
    }
}

Main($args)
