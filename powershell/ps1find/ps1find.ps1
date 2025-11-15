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

    $colorize = $true
    $options = [FindOptions]::new()

    try {
        $settings = $options.SettingsFromArgs($_args)
        $colorize = $settings.Colorize

        if ($settings.Debug) {
            # Set-LogConfiguration -LogLevel Debug
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
            $finder.PrintMatchingDirs($files, $formatter)
        }

        if ($settings.PrintFiles) {
            $finder.PrintMatchingFiles($files, $formatter)
        }
    }
    catch {
        $errMsg = $_.Exception.Message
        if ($colorize) {
            LogErrorColor($errMsg)
        } else {
            LogError($errMsg)
        }
        LogMsg($options.GetUsageString())
    }
}

Main($args)
