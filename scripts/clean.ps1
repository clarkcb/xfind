#!/usr/bin/env pwsh
################################################################################
#
# clean.ps1
#
# Runs a clean (remove generated files) for each language version
#
################################################################################
param([string]$lang='all')

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path $scriptDir 'config.ps1')
. (Join-Path $scriptDir 'common.ps1')


################################################################################
# Clean functions
################################################################################

function CleanClojure
{
    Write-Host
    Hdr('CleanClojure')

    if (!(Get-Command 'lein'))
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

    $configurations = @('debug', 'release')
    ForEach ($c in $configurations)
    {
        $cmakeBuildPath = Join-Path $cppfindPath "cmake-build-$c"
        if (Test-Path $cmakeBuildPath)
        {
            Set-Location $cmakeBuildPath
            Get-ChildItem * -Recurse | Remove-Item

            Set-Location $cppfindPath
            Remove-Item $cmakeBuildPath
        }
    }

    Set-Location $oldPwd
}

function CleanCsharp
{
    Write-Host
    Hdr('CleanCsharp')

    $oldPwd = Get-Location
    Set-Location $csfindPath

    $projects = @('CsFind', 'CsFindLib', 'CsFindTests')
    $subdirs = @('bin', 'obj')

    ForEach ($p in $projects)
    {
        $projectPath = Join-Path $csfindPath $p
        ForEach ($d in $subdirs)
        {
            $subdirPath = Join-Path $projectPath $d
            Set-Location $subdirPath
            Get-ChildItem * -Recurse | Remove-Item

            Set-Location $projectPath
            Remove-Item $d
        }
    }

    Set-Location $oldPwd
}

function CleanDart
{
    Write-Host
    Hdr('CleanDart')

    if (!(Get-Command 'dart'))
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

function CleanFsharp
{

    Write-Host
    Hdr('CleanFsharp')

    $oldPwd = Get-Location
    Set-Location $fsfindPath

    $projects = @('FsFind', 'FsFindLib', 'FsFindTests')
    $subdirs = @('bin', 'obj')

    ForEach ($p in $projects)
    {
        $projectPath = Join-Path $fsfindPath $p
        ForEach ($d in $subdirs)
        {
            $subdirPath = Join-Path $projectPath $d
            Set-Location $subdirPath
            Get-ChildItem * -Recurse | Remove-Item

            Set-Location $projectPath
            Remove-Item $d
        }
    }

    Set-Location $oldPwd
}

function CleanGo
{
    Write-Host
    Hdr('CleanGo')

    if (!(Get-Command 'go'))
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

function CleanHaskell
{
    Write-Host
    Hdr('CleanHaskell')

    if (!(Get-Command 'stack'))
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

    if (!(Get-Command 'mvn'))
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

    if (!(Get-Command 'npm'))
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

    if (!(Get-Command 'gradle'))
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
}

function CleanOcaml
{
    Write-Host
    Hdr('CleanOcaml')
}

function CleanPerl
{
    Write-Host
    Hdr('CleanPerl')
}

function CleanPhp
{
    Write-Host
    Hdr('CleanPhp')
}

function CleanPython
{
    Write-Host
    Hdr('CleanPython')
}

function CleanRuby
{
    Write-Host
    Hdr('CleanRuby')
}

function CleanRust
{
    Write-Host
    Hdr('CleanRust')

    if (!(Get-Command 'cargo'))
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

    if (!(Get-Command 'sbt'))
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

    if (!(Get-Command 'swift'))
    {
        PrintError('You need to install swift')
        return
    }
}

function CleanTypeScript
{
    Write-Host
    Hdr('CleanTypeScript')

    if (!(Get-Command 'npm'))
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

    # CleanClojure

    # CleanCpp

    CleanCsharp

    CleanDart

    CleanFsharp

    CleanGo

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

    CleanClojure

    CleanCpp

    CleanCsharp

    CleanDart

    CleanFsharp

    CleanGo

    CleanHaskell

    CleanJava

    CleanJavaScript

    CleanKotlin

    CleanObjc

    CleanOcaml

    CleanPerl

    CleanPhp

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
    param($lang='all')

    switch ($lang)
    {
        'all'        { CleanAll }
        'linux'      { CleanLinux }
        'clj'        { CleanClojure }
        'clojure'    { CleanClojure }
        'cpp'        { CleanCpp }
        'cs'         { CleanCsharp }
        'csharp'     { CleanCsharp }
        'dart'       { CleanDart }
        'fs'         { CleanFsharp }
        'fsharp'     { CleanFsharp }
        'go'         { CleanGo }
        'haskell'    { CleanHaskell }
        'hs'         { CleanHaskell }
        'java'       { CleanJava }
        'javascript' { CleanJavaScript }
        'js'         { CleanJavaScript }
        'kotlin'     { CleanKotlin }
        'kt'         { CleanKotlin }
        'objc'       { CleanObjc }
        'ocaml'      { CleanOcaml }
        'ml'         { CleanOcaml }
        'perl'       { CleanPerl }
        'pl'         { CleanPerl }
        'php'        { CleanPhp }
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
        default      { ExitWithError("Unknown option: $lang") }
    }
}

CleanMain $lang
