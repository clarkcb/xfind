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

function CleanC
{
    Write-Host
    Hdr('CleanC')

    $oldPwd = Get-Location
    Set-Location $cfindPath

    $cmakeBuildDirs = Get-ChildItem . | Where-Object {$_.Name.StartsWith('cmake-build-')}
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

function CleanClojure
{
    Write-Host
    Hdr('CleanClojure')

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

function CleanCpp
{
    Write-Host
    Hdr('CleanCpp')

    $oldPwd = Get-Location
    Set-Location $cppfindPath

    $cmakeBuildDirs = Get-ChildItem . | Where-Object {$_.Name.StartsWith('cmake-build-')}
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

function CleanCsharp
{
    Write-Host
    Hdr('CleanCsharp')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $oldPwd = Get-Location
    Set-Location $csfindPath

    Log('dotnet clean')
    dotnet clean

    Set-Location $oldPwd
}

function CleanDart
{
    Write-Host
    Hdr('CleanDart')

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

function CleanElixir
{
    Write-Host
    Hdr('CleanElixir')

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

function CleanFsharp
{

    Write-Host
    Hdr('CleanFsharp')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $oldPwd = Get-Location
    Set-Location $fsfindPath

    Log('dotnet clean')
    dotnet clean

    Set-Location $oldPwd
}

function CleanGo
{
    Write-Host
    Hdr('CleanGo')

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

function CleanGroovy
{
    Write-Host
    Hdr('CleanGroovy')

    if (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        return
    }

    $oldPwd = Get-Location
    Set-Location $groovyfindPath

    Log('gradle -b build.gradle clean')
    gradle -b 'build.gradle' clean

    Set-Location $oldPwd
}

function CleanHaskell
{
    Write-Host
    Hdr('CleanHaskell')

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

function CleanJava
{
    Write-Host
    Hdr('CleanJava')

    if (-not (Get-Command 'mvn' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install maven')
        return
    }

    Log("mvn -f $javafindPath/pom.xml clean")
    mvn -f $javafindPath/pom.xml clean
}

function CleanJavaScript
{
    Write-Host
    Hdr('CleanJavaScript')

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

function CleanKotlin
{
    Write-Host
    Hdr('CleanKotlin')

    if (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        return
    }

    $oldPwd = Get-Location
    Set-Location $ktfindPath

    Log('gradle -b build.gradle clean')
    gradle -b 'build.gradle' clean

    Set-Location $oldPwd
}

function CleanObjc
{
    Write-Host
    Hdr('CleanObjc')
    Log('not implemented at this time')
}

function CleanOcaml
{
    Write-Host
    Hdr('CleanOcaml')
    Log('not implemented at this time')
}

function CleanPerl
{
    Write-Host
    Hdr('CleanPerl')
    Log('Nothing to do for perl')
}

function CleanPhp
{
    Write-Host
    Hdr('CleanPhp')
    Log('Nothing to do for php')
}

function CleanPowerShell
{
    Write-Host
    Hdr('CleanPowerShell')
    Log('Nothing to do for powershell')
}

function CleanPython
{
    Write-Host
    Hdr('CleanPython')
    Log('Nothing to do for python')
}

function CleanRuby
{
    Write-Host
    Hdr('CleanRuby')
    Log('Nothing to do for ruby')
}

function CleanRust
{
    Write-Host
    Hdr('CleanRust')

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install rust')
    }

    $oldPwd = Get-Location
    Set-Location $rsfindPath

    Log('cargo clean')
    cargo clean

    Set-Location $oldPwd
}

function CleanScala
{
    Write-Host
    Hdr('CleanScala')

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

function CleanSwift
{
    Write-Host
    Hdr('CleanSwift')

    # if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    # {
    #     PrintError('You need to install swift')
    #     return
    # }
    Log('not implemented at this time')
}

function CleanTypeScript
{
    Write-Host
    Hdr('CleanTypeScript')

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

    CleanC

    # CleanClojure

    # CleanCpp

    CleanCsharp

    CleanDart

    CleanFsharp

    CleanGo

    # CleanGroovy

    # CleanHaskell

    CleanJava

    CleanJavaScript

    CleanKotlin

    # CleanObjc

    # CleanOcaml

    CleanPerl

    CleanPhp

    CleanPython

    CleanRuby

    CleanRust

    # CleanScala

    CleanSwift

    CleanTypeScript
}

function CleanAll
{
    Write-Host
    Hdr('CleanAll')

    CleanC

    CleanClojure

    CleanCpp

    CleanCsharp

    CleanDart

    CleanFsharp

    CleanGo

    CleanGroovy

    CleanHaskell

    CleanJava

    CleanJavaScript

    CleanKotlin

    CleanObjc

    CleanOcaml

    CleanPerl

    CleanPhp

    CleanPowerShell

    CleanPython

    CleanRuby

    CleanRust

    CleanScala

    CleanSwift

    CleanTypeScript
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
            'c'          { CleanC }
            'clj'        { CleanClojure }
            'clojure'    { CleanClojure }
            'cpp'        { CleanCpp }
            'cs'         { CleanCsharp }
            'csharp'     { CleanCsharp }
            'dart'       { CleanDart }
            'elixir'     { CleanElixir }
            'ex'         { CleanElixir }
            'fs'         { CleanFsharp }
            'fsharp'     { CleanFsharp }
            'go'         { CleanGo }
            'groovy'     { CleanGroovy }
            'haskell'    { CleanHaskell }
            'hs'         { CleanHaskell }
            'java'       { CleanJava }
            'javascript' { CleanJavaScript }
            'js'         { CleanJavaScript }
            'kotlin'     { CleanKotlin }
            'kt'         { CleanKotlin }
            'objc'       { CleanObjc }
            # 'ocaml'      { CleanOcaml }
            'ml'         { CleanOcaml }
            'perl'       { CleanPerl }
            'pl'         { CleanPerl }
            'php'        { CleanPhp }
            'powershell' { CleanPowerShell }
            'ps1'        { CleanPowerShell }
            'py'         { CleanPython }
            'python'     { CleanPython }
            'rb'         { CleanRuby }
            'ruby'       { CleanRuby }
            'rs'         { CleanRust }
            'rust'       { CleanRust }
            'scala'      { CleanScala }
            'swift'      { CleanSwift }
            'ts'         { CleanTypeScript }
            'typescript' { CleanTypeScript }
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
