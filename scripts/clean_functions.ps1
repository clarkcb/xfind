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

# Add failed cleans to this array and report failed cleans at the end
$failedCleans = @()


########################################
# Utility Functions
########################################

function CleanJsonResources
{
    param([string]$resourcesPath)
    $resourceFiles = Get-ChildItem $resourcesPath -Depth 0 | Where-Object {!$_.PsIsContainer -and $_.Extension -eq '.json'}
    ForEach ($f in $resourceFiles)
    {
        Log("Remove-Item $f")
        Remove-Item $f
    }
}

function CleanTestResources
{
    param([string]$resourcesPath)
    $resourceFiles = Get-ChildItem $resourcesPath -Depth 0 | Where-Object {!$_.PsIsContainer -and $_.Name -like "testFile*" -and $_.Extension -eq '.txt'}
    ForEach ($f in $resourceFiles)
    {
        Log("Remove-Item $f")
        Remove-Item $f
    }
}

function RemoveFromBin
{
    param([string]$binPath, [string]$scriptName)

    if (-not (Test-Path $binPath))
    {
        return
    }

    $scriptPath = Join-Path $binPath $scriptName

    if (Test-Path $scriptPath)
    {
        Log("Remove-Item $scriptPath")
        Remove-Item $scriptPath
    }
}

function PrintFailedCleans
{
    if ($global:failedCleans.Length -gt 0)
    {
        $joinedCleans = $global:failedCleans -join ', '
        PrintError("Failed cleans: $joinedCleans")
    }
    else
    {
        Log("All cleans succeeded")
    }
}


################################################################################
# Clean functions
################################################################################

function CleanBashVersion
{
    param([string]$basePath, [string]$bashVersionName)
    Log("basePath: $basePath")
    Log("bashVersionName: $bashVersionName")

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $bashVersionName

    return $true
}

function CleanCVersion
{
    param([string]$basePath, [string]$cVersionName)
    Log("basePath: $basePath")
    Log("cVersionName: $cVersionName")

    $cVersionPath = Join-Path $basePath 'c' $cVersionName
    Log("cVersionPath: $cVersionPath")

    if (-not (Test-Path $cVersionPath))
    {
        PrintError("Path not found: $cVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $cVersionPath")
    Set-Location $cVersionPath

    $cmakeBuildDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('cmake-build-')}
    ForEach ($c in $cmakeBuildDirs)
    {
        if (Test-Path $c)
        {
            try {
                Log("Remove-Item $c -Recurse -Force")
                Remove-Item $c -Recurse -Force
            }
            catch {
                PrintError("Failed to remove ${c}: $($_.Exception.Message)")
                return $false
            }
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $cVersionName

    Set-Location $oldPwd

    return $true
}

function CleanCljVersion
{
    param([string]$basePath, [string]$cljVersionName)

    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        return $false
    }

    $cljVersionPath = Join-Path $basePath 'clojure' $cljVersionName
    Log("cljVersionPath: $cljVersionPath")

    if (-not (Test-Path $cljVersionPath))
    {
        PrintError("Path not found: $cljVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $cljVersionPath")
    Set-Location $cljVersionPath

    Log('lein clean')
    lein clean

    $resourcesPath = Join-Path $cljVersionPath 'resources'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $cljVersionName

    Set-Location $oldPwd

    return $true
}

function CleanCppVersion
{
    param([string]$basePath, [string]$cppVersionName)

    $cppVersionPath = Join-Path $basePath 'cpp' $cppVersionName
    Log("cppVersionPath: $cppVersionPath")

    if (-not (Test-Path $cppVersionPath))
    {
        PrintError("Path not found: $cppVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $cppVersionPath")
    Set-Location $cppVersionPath

    $cmakeBuildDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('cmake-build-')}
    ForEach ($c in $cmakeBuildDirs)
    {
        if (Test-Path $c)
        {
            try {
                Log("Remove-Item $c -Recurse -Force")
                Remove-Item $c -Recurse -Force
            }
            catch {
                PrintError("Failed to remove ${c}: $($_.Exception.Message)")
                Set-Location $oldPwd
                return $false
            }
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $cppVersionName

    Set-Location $oldPwd

    return $true
}

function CleanCsVersion
{
    param([string]$basePath, [string]$csVersionName)

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return $false
    }

    $csVersionPath = Join-Path $basePath 'csharp' $csVersionName
    Log("csVersionPath: $csVersionPath")

    if (-not (Test-Path $csVersionPath))
    {
        PrintError("Path not found: $csVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $csVersionPath")
    Set-Location $csVersionPath

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    Log("dotnet clean -v minimal")
    dotnet clean -v minimal

    $projectPrefix = ''
    if ($csVersionName -eq 'csfind')
    {
        $projectPrefix = 'CsFind'
    }
    elseif ($csVersionName -eq 'cssearch')
    {
        $projectPrefix = 'CsSearch'
    }
    else
    {
        PrintError("Unknown version name: $csVersionName")
        return $false
    }

    $csProjectDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith($projectPrefix)}
    ForEach ($p in $csProjectDirs)
    {
        ForEach ($d in @('bin', 'obj'))
        {
            $dir = Join-Path $p.FullName $d
            if (Test-Path $dir)
            {
                try {
                    Log("Remove-Item $dir -Recurse -Force")
                    Remove-Item $dir -Recurse -Force
                }
                catch {
                    PrintError("Failed to remove ${dir}: $($_.Exception.Message)")
                    return $false
                }
            }
        }
    }

    $resourcesPath = Join-Path $csVersionPath "${projectPrefix}Lib" 'Resources'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $csVersionPath "${projectPrefix}Tests" 'Resources'
    if (Test-Path $testResourcesPath)
    {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $csVersionName

    Set-Location $oldPwd

    return $true
}

function CleanDartVersion
{
    param([string]$basePath, [string]$dartVersionName)

    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        return $false
    }

    $dartVersionPath = Join-Path $basePath 'dart' $dartVersionName
    Log("dartVersionPath: $dartVersionPath")

    if (-not (Test-Path $dartVersionPath))
    {
        PrintError("Path not found: $dartVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $dartVersionPath")
    Set-Location $dartVersionPath

    Log('dart pub cache repair')
    dart pub cache repair

    if ($lock -and (Test-Path 'pubspec.lock'))
    {
        try {
            Log('Remove-Item pubspec.lock')
            Remove-Item 'pubspec.lock'
        }
        catch {
            PrintError("Failed to remove pubspec.lock: $($_.Exception.Message)")
            return $false
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $dartVersionName

    Set-Location $oldPwd

    return $true
}

function CleanExVersion
{
    param([string]$basePath, [string]$exVersionName)

    # ensure elixir is installed
    if (-not (Get-Command 'elixir' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install elixir')
        return $false
    }

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mix')
        return $false
    }

    $exVersionPath = Join-Path $basePath 'elixir' $exVersionName
    Log("exVersionPath: $exVersionPath")

    if (-not (Test-Path $exVersionPath))
    {
        PrintError("Path not found: $exVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $exVersionPath")
    Set-Location $exVersionPath

    Log('mix clean')
    mix clean

    if ($lock -and (Test-Path 'mix.lock'))
    {
        try {
            Log('Remove-Item mix.lock')
            Remove-Item 'mix.lock'
        }
        catch {
            PrintError("Failed to remove mix.lock: $($_.Exception.Message)")
            return $false
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $exVersionName

    Set-Location $oldPwd

    return $true
}

function CleanFsVersion
{
    param([string]$basePath, [string]$fsVersionName)

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return $false
    }

    $fsVersionPath = Join-Path $basePath 'fsharp' $fsVersionName
    Log("fsVersionPath: $fsVersionPath")

    if (-not (Test-Path $fsVersionPath))
    {
        PrintError("Path not found: $fsVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $fsVersionPath")
    Set-Location $fsVersionPath

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    Log("dotnet clean -v minimal")
    dotnet clean -v minimal

    $projectPrefix = ''
    if ($fsVersionName -eq 'fsfind')
    {
        $projectPrefix = 'FsFind'
    }
    elseif ($fsVersionName -eq 'fssearch')
    {
        $projectPrefix = 'FsSearch'
    }
    else
    {
        PrintError("Unknown version name: $fsVersionName")
        return $false
    }

    $fsProjectDirs = Get-ChildItem $fsVersionPath -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith($projectPrefix)}
    ForEach ($p in $fsProjectDirs)
    {
        ForEach ($d in @('bin', 'obj'))
        {
            $dir = Join-Path $p.FullName $d
            if (Test-Path $dir)
            {
                try {
                    Log("Remove-Item $dir -Recurse -Force")
                    Remove-Item $dir -Recurse -Force
                }
                catch {
                    PrintError("Failed to remove ${dir}: $($_.Exception.Message)")
                    return $false
                }
            }
        }
    }

    $resourcesPath = Join-Path $fsVersionPath "${projectPrefix}Lib" 'Resources'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $fsVersionPath "${projectPrefix}Tests" 'Resources'
    if (Test-Path $testResourcesPath)
    {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $fsVersionName

    Set-Location $oldPwd

    return $true
}

function CleanGoVersion
{
    param([string]$basePath, [string]$goVersionName)

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        return $false
    }

    $goVersionPath = Join-Path $basePath 'go' $goVersionName
    Log("goVersionPath: $goVersionPath")

    if (-not (Test-Path $goVersionPath))
    {
        PrintError("Path not found: $goVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $goVersionPath")
    Set-Location $goVersionPath

    Log('go clean')
    go clean

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $goVersionName

    Set-Location $oldPwd

    return $true
}

function CleanGroovyVersion
{
    param([string]$basePath, [string]$groovyVersionName)

    $groovyVersionPath = Join-Path $basePath 'groovy' $groovyVersionName
    Log("groovyVersionPath: $groovyVersionPath")

    if (-not (Test-Path $groovyVersionPath))
    {
        PrintError("Path not found: $groovyVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $groovyVersionPath")
    Set-Location $groovyVersionPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        return $false
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $resourcesPath = Join-Path $groovyVersionPath 'lib' 'src' 'main' 'resources'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $groovyVersionPath 'lib' 'src' 'test' 'resources'
    if (Test-Path $testResourcesPath)
    {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $groovyVersionName

    Set-Location $oldPwd

    return $true
}

function CleanHsVersion
{
    param([string]$basePath, [string]$hsVersionName)

    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        return $false
    }

    $hsVersionPath = Join-Path $basePath 'haskell' $hsVersionName
    Log("hsVersionPath: $hsVersionPath")

    if (-not (Test-Path $hsVersionPath))
    {
        PrintError("Path not found: $hsVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $hsVersionPath")
    Set-Location $hsVersionPath

    Log('stack clean')
    stack clean

    $resourcesPath = Join-Path $hsVersionPath 'data'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    if ($lock -and (Test-Path 'stack.yaml.lock'))
    {
        try {
            Log('Remove-Item stack.yaml.lock')
            Remove-Item 'stack.yaml.lock'
        }
        catch {
            PrintError("Failed to remove stack.yaml.lock: $($_.Exception.Message)")
            return $false
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $hsVersionName

    Set-Location $oldPwd

    return $true
}

function CleanJavaVersion
{
    param([string]$basePath, [string]$javaVersionName)

    $javaVersionPath = Join-Path $basePath 'java' $javaVersionName
    Log("javaVersionPath: $javaVersionPath")

    if (-not (Test-Path $javaVersionPath))
    {
        PrintError("Path not found: $javaVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $javaVersionPath")
    Set-Location $javaVersionPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        return $false
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $resourcesPath = Join-Path $javaVersionPath 'lib' 'src' 'main' 'resources'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $javaVersionPath 'lib' 'src' 'test' 'resources'
    if (Test-Path $testResourcesPath)
    {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $javaVersionName

    Set-Location $oldPwd

    return $true
}

function CleanJsVersion
{
    param([string]$basePath, [string]$jsVersionName)

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        return $false
    }

    $jsVersionPath = Join-Path $basePath 'javascript' $jsVersionName
    Log("jsVersionPath: $jsVersionPath")

    if (-not (Test-Path $jsVersionPath))
    {
        PrintError("Path not found: $jsVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $jsVersionPath")
    Set-Location $jsVersionPath

    Log('npm run clean')
    npm run clean

    $resourcesPath = Join-Path $jsVersionPath 'data'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    if ($lock -and (Test-Path 'package-lock.json'))
    {
        try {
            Log('Remove-Item package-lock.json')
            Remove-Item 'package-lock.json'
        }
        catch {
            PrintError("Failed to remove package-lock.json: $($_.Exception.Message)")
            return $false
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $jsVersionName

    Set-Location $oldPwd

    return $true
}

function CleanKtVersion
{
    param([string]$basePath, [string]$ktVersionName)

    $ktVersionPath = Join-Path $basePath 'kotlin' $ktVersionName
    Log("ktVersionPath: $ktVersionPath")

    if (-not (Test-Path $ktVersionPath))
    {
        PrintError("Path not found: $ktVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $ktVersionPath")
    Set-Location $ktVersionPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        Set-Location $oldPwd
        return $false
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $resourcesPath = Join-Path $ktVersionPath 'lib' 'src' 'main' 'resources'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $ktVersionPath 'lib' 'src' 'test' 'resources'
    if (Test-Path $testResourcesPath)
    {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $ktVersionName

    Set-Location $oldPwd

    return $true
}

function CleanObjcVersion
{
    param([string]$basePath, [string]$objcVersionName)

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return $false
    }

    $objcVersionPath = Join-Path $basePath 'objc' $objcVersionName
    Log("objcVersionPath: $objcVersionPath")

    if (-not (Test-Path $objcVersionPath))
    {
        PrintError("Path not found: $objcVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $objcVersionPath")
    Set-Location $objcVersionPath

    Log("swift package clean")
    swift package clean

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $objcVersionName

    Set-Location $oldPwd

    return $true
}

function CleanMlVersion
{
    Log('not implemented at this time')

    return $true
}

function CleanPhpVersion
{
    param([string]$basePath, [string]$phpVersionName)

    $phpVersionPath = Join-Path $basePath 'php' $phpVersionName
    Log("phpVersionPath: $phpVersionPath")

    if (-not (Test-Path $phpVersionPath))
    {
        PrintError("Path not found: $phpVersionPath")
        return $false
    }

    $resourcesPath = Join-Path $phpVersionPath 'resources'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    if ($lock -and (Test-Path (Join-Path $phpVersionPath 'composer.lock')))
    {
        try {
            Log('Remove-Item composer.lock')
            Remove-Item (Join-Path $phpVersionPath 'composer.lock')
        }
        catch {
            PrintError("Failed to remove composer.lock: $($_.Exception.Message)")
            return $false
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $phpVersionName

    return $true
}

function CleanPlVersion
{
    param([string]$basePath, [string]$plVersionName)

    $plVersionPath = Join-Path $basePath 'perl' $plVersionName
    Log("plVersionPath: $plVersionPath")

    if (-not (Test-Path $plVersionPath))
    {
        PrintError("Path not found: $plVersionPath")
        return $false
    }

    $resourcesPath = Join-Path $plVersionPath 'share'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $plVersionName

    return $true
}

function CleanPs1Version
{
    param([string]$basePath, [string]$ps1VersionName)
    Log("basePath: $basePath")
    Log("ps1VersionName: $ps1VersionName")

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $ps1VersionName

    return $true
}

function CleanPyVersion
{
    param([string]$basePath, [string]$pyVersionName)

    $pyVersionPath = Join-Path $basePath 'python' $pyVersionName
    Log("pyVersionPath: $pyVersionPath")

    if (-not (Test-Path $pyVersionPath))
    {
        PrintError("Path not found: $pyVersionPath")
        return $false
    }

    $resourcesPath = Join-Path $pyVersionPath $pyVersionName 'data'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $pyVersionName

    return $true
}

function CleanRbVersion
{
    param([string]$basePath, [string]$rbVersionName)

    $rbVersionPath = Join-Path $basePath 'ruby' $rbVersionName
    Log("rbVersionPath: $rbVersionPath")

    if (-not (Test-Path $rbVersionPath))
    {
        PrintError("Path not found: $rbVersionPath")
        return $false
    }

    $resourcesPath = Join-Path $rbVersionPath 'data'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $rbVersionPath 'test' 'fixtures'
    if (Test-Path $testResourcesPath)
    {
        CleanTestResources($testResourcesPath)
    }

    if ($lock -and (Test-Path (Join-Path $rbVersionPath 'Gemfile.lock')))
    {
        try {
            Log('Remove-Item Gemfile.lock')
            Remove-Item (Join-Path $rbVersionPath 'Gemfile.lock')
        }
        catch {
            PrintError("Failed to remove Gemfile.lock: $($_.Exception.Message)")
            return $false
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $rbVersionName

    return $true
}

function CleanRsVersion
{
    param([string]$basePath, [string]$rsVersionName)

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo')
        return $false
    }

    $rsVersionPath = Join-Path $basePath 'rust' $rsVersionName
    Log("rsVersionPath: $rsVersionPath")

    if (-not (Test-Path $rsVersionPath))
    {
        PrintError("Path not found: $rsVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $rsVersionPath")
    Set-Location $rsVersionPath

    Log('cargo clean')
    cargo clean

    if ($lock -and (Test-Path 'Cargo.lock'))
    {
        try {
            Log('Remove-Item Cargo.lock')
            Remove-Item 'Cargo.lock'
        }
        catch {
            PrintError("Failed to remove Cargo.lock: $($_.Exception.Message)")
            return $false
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $rsVersionName

    Set-Location $oldPwd

    return $true
}

function CleanScalaVersion
{
    param([string]$basePath, [string]$scalaVersionName)

    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install scala + sbt')
        return $false
    }

    $scalaVersionPath = Join-Path $basePath 'scala' $scalaVersionName
    Log("scalaVersionPath: $scalaVersionPath")

    if (-not (Test-Path $scalaVersionPath))
    {
        PrintError("Path not found: $scalaVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $scalaVersionPath")
    Set-Location $scalaVersionPath

    Log('sbt clean')
    sbt clean

    $resourcesPath = Join-Path $scalaVersionPath 'src' 'main' 'resources'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    $testResourcesPath = Join-Path $scalaVersionPath 'src' 'test' 'resources'
    if (Test-Path $testResourcesPath)
    {
        CleanTestResources($testResourcesPath)
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $scalaVersionName

    Set-Location $oldPwd

    return $true
}

function CleanSwiftVersion
{
    param([string]$basePath, [string]$swiftVersionName)

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return $false
    }

    $swiftVersionPath = Join-Path $basePath 'swift' $swiftVersionName
    Log("swiftVersionPath: $swiftVersionPath")

    if (-not (Test-Path $swiftVersionPath))
    {
        PrintError("Path not found: $swiftVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $swiftVersionPath")
    Set-Location $swiftVersionPath

    Log("swift package clean")
    swift package clean

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $swiftVersionName

    Set-Location $oldPwd

    return $true
}

function CleanTsVersion
{
    param([string]$basePath, [string]$tsVersionName)

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        return $false
    }

    $tsVersionPath = Join-Path $basePath 'typescript' $tsVersionName
    Log("tsVersionPath: $tsVersionPath")

    if (-not (Test-Path $tsVersionPath))
    {
        PrintError("Path not found: $tsVersionPath")
        return $false
    }

    $oldPwd = Get-Location
    Log("Set-Location $tsVersionPath")
    Set-Location $tsVersionPath

    Log('npm run clean')
    npm run clean

    $resourcesPath = Join-Path $tsVersionPath 'data'
    if (Test-Path $resourcesPath)
    {
        CleanJsonResources($resourcesPath)
    }

    if ($lock -and (Test-Path 'package-lock.json'))
    {
        try {
            Log('Remove-Item package-lock.json')
            Remove-Item 'package-lock.json'
        }
        catch {
            PrintError("Failed to remove package-lock.json: $($_.Exception.Message)")
            return $false
        }
    }

    $binPath = Join-Path $basePath 'bin'
    RemoveFromBin $binPath $tsVersionName

    Set-Location $oldPwd

    return $true
}
