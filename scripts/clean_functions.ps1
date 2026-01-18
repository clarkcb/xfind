#!/usr/bin/env pwsh
################################################################################
#
# clean_functions.ps1
#
# Clean functions for xfind or xsearch language versions
#
################################################################################

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

# . (Join-Path $scriptDir 'config.ps1')
. (Join-Path $scriptDir 'common.ps1')

# Global variable to hold last funtion exit code
$global:CLEAN_LASTEXITCODE = 0

# Add failed cleans to this array and report failed cleans at the end
$global:successfulCleans = @()
$global:failedCleans = @()


########################################
# Utility Functions
########################################

function TryRemoveFile
{
    param([string]$filePath)
    try {
        Log("Remove-Item $filePath")
        Remove-Item $filePath
    } catch {
        PrintError("Failed to remove ${filePath}: $($_.Exception.Message)")
        $global:CLEAN_LASTEXITCODE = 1
    }
}

function CleanJsonResources
{
    param([string]$resourcesPath)
    $resourceFiles = Get-ChildItem $resourcesPath -Depth 0 | Where-Object {!$_.PsIsContainer -and $_.Extension -eq '.json'}
    ForEach ($f in $resourceFiles) {
        TryRemoveFile $f
        if ($global:CLEAN_LASTEXITCODE -ne 0) {
            return
        }
    }
}

function CleanTestResources
{
    param([string]$resourcesPath)
    $resourceFiles = Get-ChildItem $resourcesPath -Depth 0 | Where-Object {!$_.PsIsContainer -and $_.Name -like "testFile*" -and $_.Extension -eq '.txt'}
    ForEach ($f in $resourceFiles) {
        TryRemoveFile $f
        if ($global:CLEAN_LASTEXITCODE -ne 0) {
            return
        }
    }
}

function RemoveFromBin
{
    param([string]$binPath, [string]$scriptName)

    if (-not (Test-Path $binPath)) {
        return
    }

    $scriptPath = Join-Path $binPath $scriptName

    if (Test-Path $scriptPath) {
        TryRemoveFile $scriptPath
    }
}

function PrintCleanResults
{
    if ($global:successfulCleans.Count -gt 0) {
        $joinedSuccessfulCleans = $global:successfulCleans -join ', '
        Log("Successful cleans ($($global:successfulCleans.Count)): $joinedSuccessfulCleans")
    } else {
        Log("Successful cleans: 0")
    }
    if ($global:failedCleans.Count -gt 0) {
        $joinedFailedCleans = $global:failedCleans -join ', '
        PrintError("Failed cleans ($($global:failedCleans.Count)): $joinedFailedCleans")
    } else {
        Log("Failed cleans: 0")
    }
}


################################################################################
# Clean functions
################################################################################

function CleanBashVersion
{
    param([string]$basePath, [string]$bashVersionName)

    Log('language: bash')
    Log("bashVersionName: $bashVersionName")

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $bashVersionName
}

function CleanCVersion
{
    param([string]$basePath, [string]$cVersionName)

    Log('language: C')
    Log("cVersionName: $cVersionName")

    $cVersionPath = Join-Path $basePath 'c' $cVersionName
    Log("cVersionPath: $cVersionPath")

    if (-not (Test-Path $cVersionPath)) {
        PrintError("Path not found: $cVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $cVersionPath")
    Set-Location $cVersionPath

    $cmakeBuildDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('cmake-build-')}
    ForEach ($c in $cmakeBuildDirs) {
        if (Test-Path $c) {
            try {
                Log("Remove-Item $c -Recurse -Force")
                Remove-Item $c -Recurse -Force
            }
            catch {
                PrintError("Failed to remove ${c}: $($_.Exception.Message)")
                $global:CLEAN_LASTEXITCODE = 1
                Set-Location $oldPwd
                return
            }
        }
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $cVersionName
}

function CleanClojureVersion
{
    param([string]$basePath, [string]$cljVersionName)

    Log('language: clojure')
    Log("version: $cljVersionName")

    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install leiningen')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $cljVersionPath = Join-Path $basePath 'clojure' $cljVersionName
    Log("cljVersionPath: $cljVersionPath")

    if (-not (Test-Path $cljVersionPath)) {
        PrintError("Path not found: $cljVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $cljVersionPath")
    Set-Location $cljVersionPath

    Log('lein clean')
    lein clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd

    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        return
    }

    $resourcesPath = Join-Path $cljVersionPath 'resources'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $cljVersionName
}

function CleanCppVersion
{
    param([string]$basePath, [string]$cppVersionName)

    Log('language: C++')
    Log("version: $cppVersionName")

    $cppVersionPath = Join-Path $basePath 'cpp' $cppVersionName
    Log("cppVersionPath: $cppVersionPath")

    if (-not (Test-Path $cppVersionPath)) {
        PrintError("Path not found: $cppVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $cppVersionPath")
    Set-Location $cppVersionPath

    $cmakeBuildDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('cmake-build-')}
    ForEach ($c in $cmakeBuildDirs) {
        if (Test-Path $c) {
            try {
                Log("Remove-Item $c -Recurse -Force")
                Remove-Item $c -Recurse -Force
            }
            catch {
                PrintError("Failed to remove ${c}: $($_.Exception.Message)")
                Set-Location $oldPwd
                $global:CLEAN_LASTEXITCODE = 1
                return
            }
        }
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $cppVersionName
}

function CleanCsharpVersion
{
    param([string]$basePath, [string]$csVersionName)

    Log('language: C#')
    Log("version: $csVersionName")

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install dotnet')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $csVersionPath = Join-Path $basePath 'csharp' $csVersionName
    Log("csVersionPath: $csVersionPath")

    if (-not (Test-Path $csVersionPath)) {
        PrintError("Path not found: $csVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $csVersionPath")
    Set-Location $csVersionPath

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    Log("dotnet clean -v minimal")
    dotnet clean -v minimal

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    $projectPrefix = ''
    if ($csVersionName -eq 'csfind') {
        $projectPrefix = 'CsFind'
    } elseif ($csVersionName -eq 'cssearch') {
        $projectPrefix = 'CsSearch'
    } else {
        PrintError("Unknown version name: $csVersionName")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $csProjectDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith($projectPrefix)}
    ForEach ($p in $csProjectDirs) {
        ForEach ($d in @('bin', 'obj')) {
            $dir = Join-Path $p.FullName $d
            if (Test-Path $dir) {
                try {
                    Log("Remove-Item $dir -Recurse -Force")
                    Remove-Item $dir -Recurse -Force
                }
                catch {
                    PrintError("Failed to remove ${dir}: $($_.Exception.Message)")
                    $global:CLEAN_LASTEXITCODE = 1
                    Set-Location $oldPwd
                    return
                }
            }
        }
    }

    Set-Location $oldPwd

    $resourcesPath = Join-Path $csVersionPath "${projectPrefix}Lib" 'Resources'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $csVersionPath "${projectPrefix}Tests" 'Resources'
    if (Test-Path $testResourcesPath) {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $csVersionName
}

function CleanDartVersion
{
    param([string]$basePath, [string]$dartVersionName)

    Log('language: dart')
    Log("version: $dartVersionName")

    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install dart')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $dartVersionPath = Join-Path $basePath 'dart' $dartVersionName
    Log("dartVersionPath: $dartVersionPath")

    if (-not (Test-Path $dartVersionPath)) {
        PrintError("Path not found: $dartVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $dartVersionPath")
    Set-Location $dartVersionPath

    Log('dart pub cache repair')
    dart pub cache repair

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    if ($lock -and (Test-Path 'pubspec.lock')) {
        try {
            Log('Remove-Item pubspec.lock')
            Remove-Item 'pubspec.lock'
        }
        catch {
            PrintError("Failed to remove pubspec.lock: $($_.Exception.Message)")
            Set-Location $oldPwd
            $global:CLEAN_LASTEXITCODE = 1
            return
        }
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $dartVersionName
}

function CleanElixirVersion
{
    param([string]$basePath, [string]$exVersionName)

    Log('language: elixir')
    Log("version: $exVersionName")

    # ensure elixir is installed
    if (-not (Get-Command 'elixir' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install elixir')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install mix')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $exVersionPath = Join-Path $basePath 'elixir' $exVersionName
    Log("exVersionPath: $exVersionPath")

    if (-not (Test-Path $exVersionPath)) {
        PrintError("Path not found: $exVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $exVersionPath")
    Set-Location $exVersionPath

    Log('mix clean')
    mix clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    if ($lock -and (Test-Path 'mix.lock')) {
        try {
            Log('Remove-Item mix.lock')
            Remove-Item 'mix.lock'
        }
        catch {
            PrintError("Failed to remove mix.lock: $($_.Exception.Message)")
            $global:CLEAN_LASTEXITCODE = 1
            Set-Location $oldPwd
            return
        }
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $exVersionName
}

function CleanFsharpVersion
{
    param([string]$basePath, [string]$fsVersionName)

    Log('language: F#')
    Log("version: $fsVersionName")

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install dotnet')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $fsVersionPath = Join-Path $basePath 'fsharp' $fsVersionName
    Log("fsVersionPath: $fsVersionPath")

    if (-not (Test-Path $fsVersionPath)) {
        PrintError("Path not found: $fsVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $fsVersionPath")
    Set-Location $fsVersionPath

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    Log("dotnet clean -v minimal")
    dotnet clean -v minimal

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    $projectPrefix = ''
    if ($fsVersionName -eq 'fsfind') {
        $projectPrefix = 'FsFind'
    } elseif ($fsVersionName -eq 'fssearch') {
        $projectPrefix = 'FsSearch'
    } else {
        PrintError("Unknown version name: $fsVersionName")
        $global:CLEAN_LASTEXITCODE = 1
        Set-Location $oldPwd
        return
    }

    $fsProjectDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith($projectPrefix)}
    ForEach ($p in $fsProjectDirs) {
        ForEach ($d in @('bin', 'obj')) {
            $dir = Join-Path $p.FullName $d
            if (Test-Path $dir) {
                try {
                    Log("Remove-Item $dir -Recurse -Force")
                    Remove-Item $dir -Recurse -Force
                }
                catch {
                    PrintError("Failed to remove ${dir}: $($_.Exception.Message)")
                    $global:CLEAN_LASTEXITCODE = 1
                    Set-Location $oldPwd
                    return
                }
            }
        }
    }

    Set-Location $oldPwd

    $resourcesPath = Join-Path $fsVersionPath "${projectPrefix}Lib" 'Resources'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $fsVersionPath "${projectPrefix}Tests" 'Resources'
    if (Test-Path $testResourcesPath) {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $fsVersionName
}

function CleanGoVersion
{
    param([string]$basePath, [string]$goVersionName)

    Log('language: go')
    Log("version: $goVersionName")

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install go')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $goVersionPath = Join-Path $basePath 'go' $goVersionName
    Log("goVersionPath: $goVersionPath")

    if (-not (Test-Path $goVersionPath)) {
        PrintError("Path not found: $goVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $goVersionPath")
    Set-Location $goVersionPath

    Log('go clean')
    go clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $goVersionName
}

function CleanGroovyVersion
{
    param([string]$basePath, [string]$groovyVersionName)

    Log('language: groovy')
    Log("version: $groovyVersionName")

    $groovyVersionPath = Join-Path $basePath 'groovy' $groovyVersionName
    Log("groovyVersionPath: $groovyVersionPath")

    if (-not (Test-Path $groovyVersionPath)) {
        PrintError("Path not found: $groovyVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $groovyVersionPath")
    Set-Location $groovyVersionPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper) {
        $gradle = $gradleWrapper
    } elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        $global:CLEAN_LASTEXITCODE = 1
        Set-Location $oldPwd
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    $resourcesPath = Join-Path $groovyVersionPath 'lib' 'src' 'main' 'resources'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $groovyVersionPath 'lib' 'src' 'test' 'resources'
    if (Test-Path $testResourcesPath) {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $groovyVersionName
}

function CleanHaskellVersion
{
    param([string]$basePath, [string]$hsVersionName)

    Log('language: haskell')
    Log("version: $hsVersionName")

    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install stack')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $hsVersionPath = Join-Path $basePath 'haskell' $hsVersionName
    Log("hsVersionPath: $hsVersionPath")

    if (-not (Test-Path $hsVersionPath)) {
        PrintError("Path not found: $hsVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $hsVersionPath")
    Set-Location $hsVersionPath

    Log('stack clean')
    stack clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    $resourcesPath = Join-Path $hsVersionPath 'data'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    if ($lock -and (Test-Path 'stack.yaml.lock')) {
        try {
            Log('Remove-Item stack.yaml.lock')
            Remove-Item 'stack.yaml.lock'
        }
        catch {
            PrintError("Failed to remove stack.yaml.lock: $($_.Exception.Message)")
            $global:CLEAN_LASTEXITCODE = 1
            Set-Location $oldPwd
            return
        }
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $hsVersionName
}

function CleanJavaVersion
{
    param([string]$basePath, [string]$javaVersionName)

    Log('language: java')
    Log("version: $javaVersionName")

    $javaVersionPath = Join-Path $basePath 'java' $javaVersionName
    Log("javaVersionPath: $javaVersionPath")

    if (-not (Test-Path $javaVersionPath)) {
        PrintError("Path not found: $javaVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $javaVersionPath")
    Set-Location $javaVersionPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper) {
        $gradle = $gradleWrapper
    } elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    $resourcesPath = Join-Path $javaVersionPath 'lib' 'src' 'main' 'resources'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $javaVersionPath 'lib' 'src' 'test' 'resources'
    if (Test-Path $testResourcesPath) {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $javaVersionName
}

function CleanJavascriptVersion
{
    param([string]$basePath, [string]$jsVersionName)

    Log('language: javascript')
    Log("version: $jsVersionName")

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install node.js/npm')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $jsVersionPath = Join-Path $basePath 'javascript' $jsVersionName
    Log("jsVersionPath: $jsVersionPath")

    if (-not (Test-Path $jsVersionPath)) {
        PrintError("Path not found: $jsVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $jsVersionPath")
    Set-Location $jsVersionPath

    Log('npm run clean')
    npm run clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    $resourcesPath = Join-Path $jsVersionPath 'data'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    if ($lock -and (Test-Path 'package-lock.json')) {
        try {
            Log('Remove-Item package-lock.json')
            Remove-Item 'package-lock.json'
        }
        catch {
            PrintError("Failed to remove package-lock.json: $($_.Exception.Message)")
            $global:CLEAN_LASTEXITCODE = 1
            Set-Location $oldPwd
            return
        }
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $jsVersionName
}

function CleanKotlinVersion
{
    param([string]$basePath, [string]$ktVersionName)

    Log('language: kotlin')
    Log("version: $ktVersionName")

    $ktVersionPath = Join-Path $basePath 'kotlin' $ktVersionName
    Log("ktVersionPath: $ktVersionPath")

    if (-not (Test-Path $ktVersionPath)){
        PrintError("Path not found: $ktVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $ktVersionPath")
    Set-Location $ktVersionPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper) {
        $gradle = $gradleWrapper
    } elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        Set-Location $oldPwd
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    $resourcesPath = Join-Path $ktVersionPath 'lib' 'src' 'main' 'resources'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $ktVersionPath 'lib' 'src' 'test' 'resources'
    if (Test-Path $testResourcesPath) {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $ktVersionName
}

function CleanObjcVersion
{
    param([string]$basePath, [string]$objcVersionName)

    Log('language: objc')
    Log("version: $objcVersionName")

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install swift')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $objcVersionPath = Join-Path $basePath 'objc' $objcVersionName
    Log("objcVersionPath: $objcVersionPath")

    if (-not (Test-Path $objcVersionPath)) {
        PrintError("Path not found: $objcVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $objcVersionPath")
    Set-Location $objcVersionPath

    Log("swift package clean")
    swift package clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $objcVersionName
}

function CleanOcamlVersion
{
    Log('not implemented at this time')
}

function CleanPerlVersion
{
    param([string]$basePath, [string]$plVersionName)

    Log('language: perl')
    Log("version: $plVersionName")

    $plVersionPath = Join-Path $basePath 'perl' $plVersionName
    Log("plVersionPath: $plVersionPath")

    if (-not (Test-Path $plVersionPath)) {
        PrintError("Path not found: $plVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $resourcesPath = Join-Path $plVersionPath 'share'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $plVersionName
}

function CleanPhpVersion
{
    param([string]$basePath, [string]$phpVersionName)

    Log('language: php')
    Log("version: $phpVersionName")

    $phpVersionPath = Join-Path $basePath 'php' $phpVersionName
    Log("phpVersionPath: $phpVersionPath")

    if (-not (Test-Path $phpVersionPath)) {
        PrintError("Path not found: $phpVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $resourcesPath = Join-Path $phpVersionPath 'resources'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    if ($lock -and (Test-Path (Join-Path $phpVersionPath 'composer.lock'))) {
        try {
            Log('Remove-Item composer.lock')
            Remove-Item (Join-Path $phpVersionPath 'composer.lock')
        }
        catch {
            PrintError("Failed to remove composer.lock: $($_.Exception.Message)")
            $global:CLEAN_LASTEXITCODE = 1
            return
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $phpVersionName

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
}

function CleanPowershellVersion
{
    param([string]$basePath, [string]$ps1VersionName)

    Log('language: powershell')
    Log("version: $ps1VersionName")

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $ps1VersionName
}

function CleanPythonVersion
{
    param([string]$basePath, [string]$pyVersionName)

    Log('language: python')
    Log("version: $pyVersionName")

    $pyVersionPath = Join-Path $basePath 'python' $pyVersionName
    Log("pyVersionPath: $pyVersionPath")

    if (-not (Test-Path $pyVersionPath)) {
        PrintError("Path not found: $pyVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $resourcesPath = Join-Path $pyVersionPath $pyVersionName 'data'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $pyVersionName
}

function CleanRubyVersion
{
    param([string]$basePath, [string]$rbVersionName)

    Log('language: ruby')
    Log("version: $rbVersionName")

    $rbVersionPath = Join-Path $basePath 'ruby' $rbVersionName
    Log("rbVersionPath: $rbVersionPath")

    if (-not (Test-Path $rbVersionPath)) {
        PrintError("Path not found: $rbVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $resourcesPath = Join-Path $rbVersionPath 'data'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $rbVersionPath 'test' 'fixtures'
    if (Test-Path $testResourcesPath) {
        CleanTestResources($testResourcesPath)
    }

    if ($lock -and (Test-Path (Join-Path $rbVersionPath 'Gemfile.lock'))) {
        try {
            Log('Remove-Item Gemfile.lock')
            Remove-Item (Join-Path $rbVersionPath 'Gemfile.lock')
        }
        catch {
            PrintError("Failed to remove Gemfile.lock: $($_.Exception.Message)")
            $global:CLEAN_LASTEXITCODE = 1
            return
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $rbVersionName
}

function CleanRustVersion
{
    param([string]$basePath, [string]$rsVersionName)

    Log('language: rust')
    Log("version: $rsVersionName")

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install cargo')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $rsVersionPath = Join-Path $basePath 'rust' $rsVersionName
    Log("rsVersionPath: $rsVersionPath")

    if (-not (Test-Path $rsVersionPath)) {
        PrintError("Path not found: $rsVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $rsVersionPath")
    Set-Location $rsVersionPath

    Log('cargo clean')
    cargo clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    if ($lock -and (Test-Path 'Cargo.lock')) {
        try {
            Log('Remove-Item Cargo.lock')
            Remove-Item 'Cargo.lock'
        }
        catch {
            PrintError("Failed to remove Cargo.lock: $($_.Exception.Message)")
            $global:CLEAN_LASTEXITCODE = 1
            Set-Location $oldPwd
            return
        }
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $rsVersionName
}

function CleanScalaVersion
{
    param([string]$basePath, [string]$scalaVersionName)

    Log('language: scala')
    Log("version: $scalaVersionName")

    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install scala + sbt')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $scalaVersionPath = Join-Path $basePath 'scala' $scalaVersionName
    Log("scalaVersionPath: $scalaVersionPath")

    if (-not (Test-Path $scalaVersionPath)) {
        PrintError("Path not found: $scalaVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $scalaVersionPath")
    Set-Location $scalaVersionPath

    Log('sbt clean')
    sbt clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    $resourcesPath = Join-Path $scalaVersionPath 'src' 'main' 'resources'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $scalaVersionPath 'src' 'test' 'resources'
    if (Test-Path $testResourcesPath) {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $scalaVersionName
}

function CleanSwiftVersion
{
    param([string]$basePath, [string]$swiftVersionName)

    Log('language: swift')
    Log("version: $swiftVersionName")

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install swift')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $swiftVersionPath = Join-Path $basePath 'swift' $swiftVersionName
    Log("swiftVersionPath: $swiftVersionPath")

    if (-not (Test-Path $swiftVersionPath)) {
        PrintError("Path not found: $swiftVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $swiftVersionPath")
    Set-Location $swiftVersionPath

    Log("swift package clean")
    swift package clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $swiftVersionName
}

function CleanTypescriptVersion
{
    param([string]$basePath, [string]$tsVersionName)

    Log('language: typescript')
    Log("version: $tsVersionName")

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install node.js/npm')
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $tsVersionPath = Join-Path $basePath 'typescript' $tsVersionName
    Log("tsVersionPath: $tsVersionPath")

    if (-not (Test-Path $tsVersionPath)) {
        PrintError("Path not found: $tsVersionPath")
        $global:CLEAN_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $tsVersionPath")
    Set-Location $tsVersionPath

    Log('npm run clean')
    npm run clean

    $global:CLEAN_LASTEXITCODE = $LASTEXITCODE
    if ($global:CLEAN_LASTEXITCODE -ne 0) {
        Set-Location $oldPwd
        return
    }

    $resourcesPath = Join-Path $tsVersionPath 'data'
    if (Test-Path $resourcesPath) {
        CleanJsonResources($resourcesPath)
    }

    if ($lock -and (Test-Path 'package-lock.json')) {
        try {
            Log('Remove-Item package-lock.json')
            Remove-Item 'package-lock.json'
        }
        catch {
            PrintError("Failed to remove package-lock.json: $($_.Exception.Message)")
            $global:CLEAN_LASTEXITCODE = 1
            Set-Location $oldPwd
            return
        }
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $tsVersionName
}
