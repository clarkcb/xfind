#!/usr/bin/env pwsh
################################################################################
#
# clean.ps1
#
# Clean xfind language versions
#
################################################################################
param([switch]$help = $false,
      [switch]$lock = $false,
      [switch]$all = $false)

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path $scriptDir 'config.ps1')
# . (Join-Path $scriptDir 'common.ps1')
. (Join-Path $scriptDir 'clean_functions.ps1')

# args holds the remaining arguments
$langs = $args

if ($langs -contains 'all')
{
    $all = $true
}

Log("help: $help")
Log("lock: $lock")
Log("all: $all")
if ($langs.Length -gt 0 -and -not $all)
{
    Log("langs ($($langs.Length)): $langs")
}


########################################
# Common Functions
########################################

function Usage
{
    Write-Host "`nUsage: clean.ps1 [-help] [-lock] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Clean functions
################################################################################

function CleanBashFind
{
    Write-Host
    Hdr('CleanBashFind')

    if (CleanBashVersion $xfindPath 'bashfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'bashfind'
    }
}

function CleanCFind
{
    Write-Host
    Hdr('CleanCFind')

    if (CleanCVersion $xfindPath 'cfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'cfind'
    }
}

function CleanCljFind
{
    Write-Host
    Hdr('CleanCljFind')

    if (CleanCljVersion $xfindPath 'cljfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'cljfind'
    }
}

function CleanCppFind
{
    Write-Host
    Hdr('CleanCppFind')

    if (CleanCppVersion $xfindPath 'cppfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'cppfind'
    }
}

function CleanCsFind
{
    Write-Host
    Hdr('CleanCsFind')

    if (CleanCsVersion $xfindPath 'csfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'csfind'
    }
}

function CleanDartFind
{
    Write-Host
    Hdr('CleanDartFind')

    if (CleanDartVersion $xfindPath 'dartfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'dartfind'
    }
}

function CleanExFind
{
    Write-Host
    Hdr('CleanExFind')

    if (CleanExVersion $xfindPath 'exfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'exfind'
    }
}

function CleanFsFind
{

    Write-Host
    Hdr('CleanFsFind')

    if (CleanFsVersion $xfindPath 'fsfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'fsfind'
    }
}

function CleanGoFind
{
    Write-Host
    Hdr('CleanGoFind')

    if (CleanGoVersion $xfindPath 'gofind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'gofind'
    }
}

function CleanGroovyFind
{
    Write-Host
    Hdr('CleanGroovyFind')

    if (CleanGroovyVersion $xfindPath 'groovyfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'groovyfind'
    }
}

function CleanHsFind
{
    Write-Host
    Hdr('CleanHsFind')

    if (CleanHsVersion $xfindPath 'hsfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'hsfind'
    }
}

function CleanJavaFind
{
    Write-Host
    Hdr('CleanJavaFind')

    if (CleanJavaVersion $xfindPath 'javafind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'javafind'
    }
}

function CleanJsFind
{
    Write-Host
    Hdr('CleanJsFind')

    if (CleanJsVersion $xfindPath 'jsfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'jsfind'
    }
}

function CleanKtFind
{
    Write-Host
    Hdr('CleanKtFind')

    if (CleanKtVersion $xfindPath 'ktfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'ktfind'
    }
}

function CleanObjcFind
{
    Write-Host
    Hdr('CleanObjcFind')

    if (CleanObjcVersion $xfindPath 'objcfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'objcfind'
    }
}

function CleanMlFind
{
    Write-Host
    Hdr('CleanMlFind')
    Log('not implemented at this time')
}

function CleanPhpFind
{
    Write-Host
    Hdr('CleanPhpFind')

    if (CleanPhpVersion $xfindPath 'phpfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'phpfind'
    }
}

function CleanPlFind
{
    Write-Host
    Hdr('CleanPlFind')

    if (CleanPlVersion $xfindPath 'plfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'plfind'
    }
}

function CleanPs1Find
{
    Write-Host
    Hdr('CleanPs1Find')

    if (CleanPs1Version $xfindPath 'ps1find')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'ps1find'
    }
}

function CleanPyFind
{
    Write-Host
    Hdr('CleanPyFind')

    if (CleanPyVersion $xfindPath 'pyfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'pyfind'
    }
}

function CleanRbFind
{
    Write-Host
    Hdr('CleanRbFind')

    if (CleanRbVersion $xfindPath 'rbfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'rbfind'
    }
}

function CleanRsFind
{
    Write-Host
    Hdr('CleanRsFind')

    if (CleanRsVersion $xfindPath 'rsfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'rsfind'
    }
}

function CleanScalaFind
{
    Write-Host
    Hdr('CleanScalaFind')

    if (CleanScalaVersion $xfindPath 'scalafind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'scalafind'
    }
}

function CleanSwiftFind
{
    Write-Host
    Hdr('CleanSwiftFind')

    if (CleanSwiftVersion $xfindPath 'swiftfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'swiftfind'
    }
}

function CleanTsFind
{
    Write-Host
    Hdr('CleanTsFind')

    if (CleanTsVersion $xfindPath 'tsfind')
    {
        Log("Clean succeeded")
    }
    else
    {
        PrintError('Clean failed')
        $global:failedBuilds += 'tsfind'
    }
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

    CleanExFind

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

    PrintFailedCleans

    exit
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

    CleanExFind

    CleanFsFind

    CleanGoFind

    CleanGroovyFind

    CleanHsFind

    CleanJavaFind

    CleanJsFind

    CleanKtFind

    CleanObjcFind

    # CleanMlFind

    CleanPlFind

    CleanPhpFind

    CleanPs1Find

    CleanPyFind

    CleanRbFind

    CleanRsFind

    CleanScalaFind

    CleanSwiftFind

    CleanTsFind

    PrintFailedCleans

    exit
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
            'pwsh'       { CleanPs1Find }
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

    PrintFailedCleans
}

if ($help)
{
    Usage
}

$oldPwd = Get-Location

try {
    if ($all)
    {
        CleanAll
    }
    
    CleanMain $langs    
}
catch {
    PrintError($_.Exception.Message)
}
finally {
    Set-Location $oldPwd
}
