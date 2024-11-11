#!/usr/bin/env pwsh
################################################################################
#
# clean.ps1
#
# Runs a clean (remove generated files) for each language version
#
################################################################################
param([switch]$help = $false,
      [switch]$all = $false)

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path $scriptDir 'config.ps1')
. (Join-Path $scriptDir 'common.ps1')

# check for help switch
$help = $help.IsPresent

# check for all switch
$all = $all.IsPresent

# args holds the remaining arguments
$langs = $args

Write-Host "help: $help"
Write-Host "all: $all"
Write-Host "langs: $langs"


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: clean.ps1 [-help] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Clean functions
################################################################################

function CleanBashFind
{
    Write-Host
    Hdr('CleanBashFind')
    Log('Nothing to do for bash')
}

function CleanCFind
{
    Write-Host
    Hdr('CleanCFind')

    $oldPwd = Get-Location
    Set-Location $cfindPath

    $cmakeBuildDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.Name.StartsWith('cmake-build-')}
    ForEach ($c in $cmakeBuildDirs)
    {
        if (Test-Path $c)
        {
            Log("Remove-Item $c -Recurse -Force")
            Remove-Item $c -Recurse -Force
        }
    }

    Set-Location $oldPwd
}

function CleanCljFind
{
    Write-Host
    Hdr('CleanCljFind')

    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        return
    }

    $oldPwd = Get-Location
    Set-Location $cljfindPath

    Log('lein clean')
    lein clean

    Set-Location $oldPwd
}

function CleanCppFind
{
    Write-Host
    Hdr('CleanCppFind')

    $oldPwd = Get-Location
    Set-Location $cppfindPath

    $cmakeBuildDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('cmake-build-')}
    ForEach ($c in $cmakeBuildDirs)
    {
        if (Test-Path $c)
        {
            Log("Remove-Item $c -Recurse -Force")
            Remove-Item $c -Recurse -Force
        }
    }

    Set-Location $oldPwd
}

function CleanCsFind
{
    Write-Host
    Hdr('CleanCsFind')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $oldPwd = Get-Location
    Set-Location $csfindPath

    Log('dotnet clean')
    dotnet clean

    $csfindProjectDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('CsFind')}
    ForEach ($p in $csfindProjectDirs)
    {
        $binDir = Join-Path $p.FullName 'bin'
        if (Test-Path $binDir)
        {
            Log("Remove-Item $binDir -Recurse -Force")
            Remove-Item $binDir -Recurse -Force
        }
        $objDir = Join-Path $p.FullName 'obj'
        if (Test-Path $objDir)
        {
            Log("Remove-Item $objDir -Recurse -Force")
            Remove-Item $objDir -Recurse -Force
        }
    }

    Set-Location $oldPwd
}

function CleanDartFind
{
    Write-Host
    Hdr('CleanDartFind')

    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        return
    }

    $oldPwd = Get-Location
    Set-Location $dartfindPath

    Log('dart pub cache repair')
    dart pub cache repair

    Set-Location $oldPwd
}

function CleanExFind
{
    Write-Host
    Hdr('CleanExFind')

    # ensure elixir is installed
    if (-not (Get-Command 'elixir' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install elixir')
        return
    }

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mix')
        return
    }

    $oldPwd = Get-Location
    Set-Location $exfindPath

    Log('mix clean')
    mix clean

    Set-Location $oldPwd
}

function CleanFsFind
{

    Write-Host
    Hdr('CleanFsFind')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $oldPwd = Get-Location
    Set-Location $fsfindPath

    Log('dotnet clean')
    dotnet clean

    $fsfindProjectDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('FsFind')}
    ForEach ($p in $fsfindProjectDirs)
    {
        $binDir = Join-Path $p.FullName 'bin'
        if (Test-Path $binDir)
        {
            Log("Remove-Item $binDir -Recurse -Force")
            Remove-Item $binDir -Recurse -Force
        }
        $objDir = Join-Path $p.FullName 'obj'
        if (Test-Path $objDir)
        {
            Log("Remove-Item $objDir -Recurse -Force")
            Remove-Item $objDir -Recurse -Force
        }
    }

    Set-Location $oldPwd
}

function CleanGoFind
{
    Write-Host
    Hdr('CleanGoFind')

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        return
    }

    $oldPwd = Get-Location
    Set-Location $gofindPath

    Log('go clean')
    go clean

    Set-Location $oldPwd
}

function CleanGroovyFind
{
    Write-Host
    Hdr('CleanGroovyFind')

    $oldPwd = Get-Location
    Set-Location $groovyfindPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    Set-Location $oldPwd
}

function CleanHsFind
{
    Write-Host
    Hdr('CleanHsFind')

    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        return
    }

    $oldPwd = Get-Location
    Set-Location $hsfindPath

    Log('stack clean')
    stack clean

    Set-Location $oldPwd
}

function CleanJavaFind
{
    Write-Host
    Hdr('CleanJavaFind')

    $oldPwd = Get-Location
    Set-Location $javafindPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    Set-Location $oldPwd
}

function CleanJsFind
{
    Write-Host
    Hdr('CleanJsFind')

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        return
    }

    $oldPwd = Get-Location
    Set-Location $jsfindPath

    Log('npm run clean')
    npm run clean

    Set-Location $oldPwd
}

function CleanKtFind
{
    Write-Host
    Hdr('CleanKtFind')

    $oldPwd = Get-Location
    Set-Location $ktfindPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    Set-Location $oldPwd
}

function CleanObjcFind
{
    Write-Host
    Hdr('CleanObjcFind')

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    $oldPwd = Get-Location
    Set-Location $objcfindPath

    Log("swift package clean")
    swift package clean

    Set-Location $oldPwd
}

function CleanMlFind
{
    Write-Host
    Hdr('CleanMlFind')
    Log('not implemented at this time')
}

function CleanPlFind
{
    Write-Host
    Hdr('CleanPlFind')
    Log('Nothing to do for perl')
}

function CleanPhpFind
{
    Write-Host
    Hdr('CleanPhpFind')
    Log('Nothing to do for php')
}

function CleanPs1Find
{
    Write-Host
    Hdr('CleanPs1Find')
    Log('Nothing to do for powershell')
}

function CleanPyFind
{
    Write-Host
    Hdr('CleanPyFind')
    Log('Nothing to do for python')
}

function CleanRbFind
{
    Write-Host
    Hdr('CleanRbFind')
    Log('Nothing to do for ruby')
}

function CleanRsFind
{
    Write-Host
    Hdr('CleanRsFind')

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo')
    }

    $oldPwd = Get-Location
    Set-Location $rsfindPath

    Log('cargo clean')
    cargo clean

    Set-Location $oldPwd
}

function CleanScalaFind
{
    Write-Host
    Hdr('CleanScalaFind')

    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install scala + sbt')
    }

    $oldPwd = Get-Location
    Set-Location $scalafindPath

    Log('sbt clean')
    sbt clean

    Set-Location $oldPwd
}

function CleanSwiftFind
{
    Write-Host
    Hdr('CleanSwiftFind')

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    $oldPwd = Get-Location
    Set-Location $swiftfindPath

    Log("swift package clean")
    swift package clean

    Set-Location $oldPwd
}

function CleanTsFind
{
    Write-Host
    Hdr('CleanTsFind')

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        return
    }

    $oldPwd = Get-Location
    Set-Location $tsfindPath

    Log('npm run clean')
    npm run clean

    Set-Location $oldPwd
}

function CleanLinux
{
    Write-Host
    Hdr('CleanLinux')

    CleanBashFind

    CleanCFind

    # CleanCljFind

    # CleanCppFind

    CleanCsFind

    CleanDartFind

    CleanFsFind

    CleanGoFind

    # CleanGroovyFind

    # CleanHsFind

    CleanJavaFind

    CleanJsFind

    CleanKtFind

    # CleanObjcFind

    # CleanMlFind

    CleanPlFind

    CleanPhpFind

    CleanPyFind

    CleanRbFind

    CleanRsFind

    # CleanScalaFind

    CleanSwiftFind

    CleanTsFind
}

function CleanAll
{
    Write-Host
    Hdr('CleanAll')

    CleanBashFind

    CleanCFind

    CleanCljFind

    CleanCppFind

    CleanCsFind

    CleanDartFind

    CleanFsFind

    CleanGoFind

    CleanGroovyFind

    CleanHsFind

    CleanJavaFind

    CleanJsFind

    CleanKtFind

    CleanObjcFind

    CleanMlFind

    CleanPlFind

    CleanPhpFind

    CleanPs1Find

    CleanPyFind

    CleanRbFind

    CleanRsFind

    CleanScalaFind

    CleanSwiftFind

    CleanTsFind
}

################################################################################
# Main function
################################################################################

function CleanMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        CleanAll
        exit
    }

    ForEach ($lang in $langs)
    {
        switch ($lang)
        {
            'linux'      { CleanLinux }
            'bash'       { CleanBashFind }
            'c'          { CleanCFind }
            'clj'        { CleanCljFind }
            'clojure'    { CleanCljFind }
            'cpp'        { CleanCppFind }
            'cs'         { CleanCsFind }
            'csharp'     { CleanCsFind }
            'dart'       { CleanDartFind }
            'elixir'     { CleanExFind }
            'ex'         { CleanExFind }
            'fs'         { CleanFsFind }
            'fsharp'     { CleanFsFind }
            'go'         { CleanGoFind }
            'groovy'     { CleanGroovyFind }
            'haskell'    { CleanHsFind }
            'hs'         { CleanHsFind }
            'java'       { CleanJavaFind }
            'javascript' { CleanJsFind }
            'js'         { CleanJsFind }
            'kotlin'     { CleanKtFind }
            'kt'         { CleanKtFind }
            'objc'       { CleanObjcFind }
            # 'ocaml'      { CleanMlFind }
            # 'ml'         { CleanMlFind }
            'perl'       { CleanPlFind }
            'pl'         { CleanPlFind }
            'php'        { CleanPhpFind }
            'powershell' { CleanPs1Find }
            'ps1'        { CleanPs1Find }
            'py'         { CleanPyFind }
            'python'     { CleanPyFind }
            'rb'         { CleanRbFind }
            'ruby'       { CleanRbFind }
            'rs'         { CleanRsFind }
            'rust'       { CleanRsFind }
            'scala'      { CleanScalaFind }
            'swift'      { CleanSwiftFind }
            'ts'         { CleanTsFind }
            'typescript' { CleanTsFind }
            default      { ExitWithError("unknown/unsupported language: $lang") }
        }
    }
}

if ($help)
{
    Usage
}

if ($all)
{
    CleanAll
    exit
}

CleanMain $langs
