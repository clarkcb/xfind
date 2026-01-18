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

if ($langs -contains 'all') {
    $all = $true
}

Log("help: $help")
Log("lock: $lock")
Log("all: $all")
if ($langs.Length -gt 0 -and -not $all) {
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

function CleanLangVersion
{
    param([string]$langName, [string]$versionName)

    $langName = (Get-Culture).TextInfo.ToTitleCase($langName.ToLower())

    $functionName = "Clean${langName}Version"

    if (Get-Command $functionName -ErrorAction 'SilentlyContinue') {
        & $functionName $xfindPath $versionName

        if ($global:CLEAN_LASTEXITCODE -eq 0) {
            Log("$versionName clean succeeded")
            $global:successfulCleans += $versionName
        } else {
            PrintError("$versionName clean failed")
            $global:failedCleans += $versionName
        }
    }
}

function CleanBashFind
{
    Write-Host
    Hdr('CleanBashFind')

    CleanLangVersion 'bash' 'bashfind'
}

function CleanCFind
{
    Write-Host
    Hdr('CleanCFind')

    CleanLangVersion 'c' 'cfind'
}

function CleanCljFind
{
    Write-Host
    Hdr('CleanCljFind')

    CleanLangVersion 'clojure' 'cljfind'
}

function CleanCppFind
{
    Write-Host
    Hdr('CleanCppFind')

    CleanLangVersion 'cpp' 'cppfind'
}

function CleanCsFind
{
    Write-Host
    Hdr('CleanCsFind')

    CleanLangVersion 'csharp' 'csfind'
}

function CleanDartFind
{
    Write-Host
    Hdr('CleanDartFind')

    CleanLangVersion 'dart' 'dartfind'
}

function CleanExFind
{
    Write-Host
    Hdr('CleanExFind')

    CleanLangVersion 'elixir' 'exfind'
}

function CleanFsFind
{

    Write-Host
    Hdr('CleanFsFind')

    CleanLangVersion 'fsharp' 'fsfind'
}

function CleanGoFind
{
    Write-Host
    Hdr('CleanGoFind')

    CleanLangVersion 'go' 'gofind'
}

function CleanGroovyFind
{
    Write-Host
    Hdr('CleanGroovyFind')

    CleanLangVersion 'groovy' 'groovyfind'
}

function CleanHsFind
{
    Write-Host
    Hdr('CleanHsFind')

    CleanLangVersion 'haskell' 'hsfind'
}

function CleanJavaFind
{
    Write-Host
    Hdr('CleanJavaFind')

    CleanLangVersion 'java' 'javafind'
}

function CleanJsFind
{
    Write-Host
    Hdr('CleanJsFind')

    CleanLangVersion 'javascript' 'jsfind'
}

function CleanKtFind
{
    Write-Host
    Hdr('CleanKtFind')

    CleanLangVersion 'kotlin' 'ktfind'
}

function CleanObjcFind
{
    Write-Host
    Hdr('CleanObjcFind')

    CleanLangVersion 'objc' 'objcfind'
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

    CleanLangVersion 'php' 'phpfind'
}

function CleanPlFind
{
    Write-Host
    Hdr('CleanPlFind')

    CleanLangVersion 'perl' 'plfind'
}

function CleanPs1Find
{
    Write-Host
    Hdr('CleanPs1Find')

    CleanLangVersion 'powershell' 'ps1find'
}

function CleanPyFind
{
    Write-Host
    Hdr('CleanPyFind')

    CleanLangVersion 'python' 'pyfind'
}

function CleanRbFind
{
    Write-Host
    Hdr('CleanRbFind')

    CleanLangVersion 'ruby' 'rbfind'
}

function CleanRsFind
{
    Write-Host
    Hdr('CleanRsFind')

    CleanLangVersion 'rust' 'rsfind'
}

function CleanScalaFind
{
    Write-Host
    Hdr('CleanScalaFind')

    CleanLangVersion 'scala' 'scalafind'
}

function CleanSwiftFind
{
    Write-Host
    Hdr('CleanSwiftFind')

    CleanLangVersion 'swift' 'swiftfind'
}

function CleanTsFind
{
    Write-Host
    Hdr('CleanTsFind')

    CleanLangVersion 'typescript' 'tsfind'
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

    PrintCleanResults

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

    PrintCleanResults

    exit
}

################################################################################
# Main function
################################################################################

function CleanMain
{
    param($langs=@())

    if ($langs.Count -eq 0) {
        Usage
    }

    if ($langs -contains 'all') {
        CleanAll
        exit
    }

    ForEach ($lang in $langs) {
        switch ($lang) {
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

    PrintCleanResults
}

if ($help) {
    Usage
}

$oldPwd = Get-Location

try {
    if ($all) {
        CleanAll
    }
    
    CleanMain $langs    
} catch {
    PrintError($_.Exception.Message)
} finally {
    Set-Location $oldPwd
}
