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

        $formatter = [FileResultFormatter]::new($settings)

        if ($settings.PrintDirs) {
            $dirs = @()
            if ($files.Count -gt 0) {
                $dirs = $files |
                        ForEach-Object { $_.File.Directory } |
                        Select-Object -Unique
            }
            if ($dirs.Count -gt 0) {
                LogMsg("`nMatching directories ($($dirs.Count)):")
                foreach ($d in $dirs) {
                    LogMsg($formatter.FormatDirectory($d))
                }
            } else {
                LogMsg("`nMatching directories: 0")
            }
        }

        if ($settings.PrintFiles) {
            if ($files.Count -gt 0) {
                LogMsg("`nMatching files ($($files.Count)):")
                foreach ($f in $files) {
                    LogMsg($formatter.FormatFileResult($f))
                }
            } else {
                LogMsg("`nMatching files: 0")
            }
        }
    }
    catch {
        LogError($_)
        LogMsg($options.GetUsageString())
    }
}

Main($args)
