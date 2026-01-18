#!/usr/bin/env pwsh
################################################################################
#
# unittest.ps1
#
# Runs unit tests for specified language version of xfind, or all versions
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
# . (Join-Path $scriptDir 'common.ps1')
. (Join-Path $scriptDir 'unittest_functions.ps1')

# args holds the remaining arguments
$langs = $args
$hostname = [System.Net.Dns]::GetHostName()

Hdr('xfind unittest script')
Log("user: $env:USER")
Log("host: $hostname")
if ($IsWindows) {
    Log("os: $env:OS")
} elseif ($IsLinux) {
    Log("os: Linux")
} elseif ($IsMacOS) {
    Log("os: Darwin")
} else {
    Log("os: unknown")
}

$gitBranch = git branch --show-current
$gitCommit = git rev-parse --short HEAD
Log("git branch: $gitBranch ($gitCommit)")

if ($langs -contains 'all') {
    $all = $true
}

Log("help: $help")
Log("all: $all")
if ($langs.Length -gt 0 -and -not $all) {
    Log("langs ($($langs.Length)): $langs")
}


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: unittest.ps1 [-help] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Unit Test functions
################################################################################

function UnitTestLangVersion
{
    param([string]$langName, [string]$versionName)

    $langName = (Get-Culture).TextInfo.ToTitleCase($langName.ToLower())

    $functionName = "UnitTest${langName}Version"

    if (Get-Command $functionName -ErrorAction 'SilentlyContinue') {
        & $functionName $xfindPath $versionName

        if ($global:UNITTEST_LASTEXITCODE -eq 0) {
            Log("$versionName tests succeeded")
            $global:successfulTests += $versionName
        } else {
            PrintError("$versionName tests failed")
            $global:failedTests += $versionName
        }
    }
}

function UnitTestBashFind
{
    Write-Host
    Hdr('UnitTestBashFind')

    UnitTestLangVersion 'bash' 'bashfind'
}

function UnitTestCFind
{
    Write-Host
    Hdr('UnitTestCFind')

    UnitTestLangVersion 'c' 'cfind'
}

function UnitTestCljFind
{
    Write-Host
    Hdr('UnitTestCljFind')

    UnitTestLangVersion 'clojure' 'cljfind'
}

function UnitTestCppFind
{
    Write-Host
    Hdr('UnitTestCppFind')

    UnitTestLangVersion 'cpp' 'cppfind'
}

function UnitTestCsFind
{
    Write-Host
    Hdr('UnitTestCsFind')

    UnitTestLangVersion 'csharp' 'csfind'
}

function UnitTestDartFind
{
    Write-Host
    Hdr('UnitTestDartFind')

    UnitTestLangVersion 'dart' 'dartfind'
}

function UnitTestExFind
{
    Write-Host
    Hdr('UnitTestExFind')

    UnitTestLangVersion 'elixir' 'exfind'
}

function UnitTestFsFind
{
    Write-Host
    Hdr('UnitTestFsFind')

    UnitTestLangVersion 'fsharp' 'fsfind'
}

function UnitTestGoFind
{
    Write-Host
    Hdr('UnitTestGoFind')

    UnitTestLangVersion 'go' 'gofind'
}

function UnitTestGroovyFind
{
    Write-Host
    Hdr('UnitTestGroovyFind')

    UnitTestLangVersion 'groovy' 'groovyfind'
}

function UnitTestHsFind
{
    Write-Host
    Hdr('UnitTestHsFind')

    UnitTestLangVersion 'haskell' 'hsfind'
}

function UnitTestJavaFind
{
    Write-Host
    Hdr('UnitTestJavaFind')

    UnitTestLangVersion 'java' 'javafind'
}

function UnitTestJsFind
{
    Write-Host
    Hdr('UnitTestJsFind')

    UnitTestLangVersion 'javascript' 'jsfind'
}

function UnitTestKtFind
{
    Write-Host
    Hdr('UnitTestKtFind')

    UnitTestLangVersion 'kotlin' 'ktfind'
}

function UnitTestObjcFind
{
    Write-Host
    Hdr('UnitTestObjcFind')

    UnitTestLangVersion 'objc' 'objcfind'
}

function UnitTestMlFind
{
    Write-Host
    Hdr('UnitTestMlFind')
    Log('not implemented at this time')
}

function UnitTestPlFind
{
    Write-Host
    Hdr('UnitTestPlFind')

    UnitTestLangVersion 'perl' 'plfind'
}

function UnitTestPhpFind
{
    Write-Host
    Hdr('UnitTestPhpFind')

    UnitTestLangVersion 'php' 'phpfind'
}

function UnitTestPs1Find
{
    Write-Host
    Hdr('UnitTestPs1Find')

    UnitTestLangVersion 'powershell' 'ps1find'
}

function UnitTestPyFind
{
    Write-Host
    Hdr('UnitTestPyFind')

    UnitTestLangVersion 'python' 'pyfind'
}

function UnitTestRbFind
{
    Write-Host
    Hdr('UnitTestRbFind')

    UnitTestLangVersion 'ruby' 'rbfind'
}

function UnitTestRsFind
{
    Write-Host
    Hdr('UnitTestRsFind')

    UnitTestLangVersion 'rust' 'rsfind'
}

function UnitTestScalaFind
{
    Write-Host
    Hdr('UnitTestScalaFind')

    UnitTestLangVersion 'scala' 'scalafind'
}

function UnitTestSwiftFind
{
    Write-Host
    Hdr('UnitTestSwiftFind')

    UnitTestLangVersion 'swift' 'swiftfind'
}

function UnitTestTsFind
{
    Write-Host
    Hdr('UnitTestTsFind')

    UnitTestLangVersion 'typescript' 'tsfind'
}

function UnitTestAll
{
    Write-Host
    Hdr('UnitTestAll')

    Measure-Command { UnitTestBashFind }

    Measure-Command { UnitTestCFind }

    Measure-Command { UnitTestCljFind }

    Measure-Command { UnitTestCppFind }

    Measure-Command { UnitTestCsFind }

    Measure-Command { UnitTestDartFind }

    Measure-Command { UnitTestExFind }

    Measure-Command { UnitTestFsFind }

    Measure-Command { UnitTestGoFind }

    Measure-Command { UnitTestGroovyFind }

    Measure-Command { UnitTestHsFind }

    Measure-Command { UnitTestJavaFind }

    Measure-Command { UnitTestJsFind }

    Measure-Command { UnitTestKtFind }

    Measure-Command { UnitTestObjcFind }

    Measure-Command { UnitTestMlFind }

    Measure-Command { UnitTestPlFind }

    Measure-Command { UnitTestPhpFind }

    Measure-Command { UnitTestPs1Find }

    Measure-Command { UnitTestPyFind }

    Measure-Command { UnitTestRbFind }

    Measure-Command { UnitTestRsFind }

    Measure-Command { UnitTestScalaFind }

    Measure-Command { UnitTestSwiftFind }

    Measure-Command { UnitTestTsFind }

    PrintTestResults

    exit
}

################################################################################
# Main function
################################################################################

function UnitTestMain
{
    param($langs=@())

    if ($langs.Count -eq 0) {
        Usage
    }

    if ($langs -contains 'all') {
        UnitTestAll
    }

    ForEach ($lang in $langs) {
        switch ($lang) {
            # 'bash'       { UnitTestBashFind }
            'bash'       { Measure-Command { UnitTestBashFind } }
            'c'          { Measure-Command { UnitTestCFind } }
            'clj'        { Measure-Command { UnitTestCljFind } }
            'clojure'    { Measure-Command { UnitTestCljFind } }
            'cpp'        { Measure-Command { UnitTestCppFind } }
            'cs'         { Measure-Command { UnitTestCsFind } }
            'csharp'     { Measure-Command { UnitTestCsFind } }
            'dart'       { Measure-Command { UnitTestDartFind } }
            'elixir'     { Measure-Command { UnitTestExFind } }
            'ex'         { Measure-Command { UnitTestExFind } }
            'fs'         { Measure-Command { UnitTestFsFind } }
            'fsharp'     { Measure-Command { UnitTestFsFind } }
            'go'         { Measure-Command { UnitTestGoFind } }
            'groovy'     { Measure-Command { UnitTestGroovyFind } }
            'haskell'    { Measure-Command { UnitTestHsFind } }
            'hs'         { Measure-Command { UnitTestHsFind } }
            'java'       { Measure-Command { UnitTestJavaFind } }
            'javascript' { Measure-Command { UnitTestJsFind } }
            'js'         { Measure-Command { UnitTestJsFind } }
            'kotlin'     { Measure-Command { UnitTestKtFind } }
            'kt'         { Measure-Command { UnitTestKtFind } }
            'objc'       { Measure-Command { UnitTestObjcFind } }
            'ocaml'      { Measure-Command { UnitTestMlFind } }
            'ml'         { Measure-Command { UnitTestMlFind } }
            'perl'       { Measure-Command { UnitTestPlFind } }
            'pl'         { Measure-Command { UnitTestPlFind } }
            'php'        { Measure-Command { UnitTestPhpFind } }
            'powershell' { Measure-Command { UnitTestPs1Find } }
            'ps1'        { Measure-Command { UnitTestPs1Find } }
            'pwsh'       { Measure-Command { UnitTestPs1Find } }
            'py'         { Measure-Command { UnitTestPyFind } }
            'python'     { Measure-Command { UnitTestPyFind } }
            'rb'         { Measure-Command { UnitTestRbFind } }
            'ruby'       { Measure-Command { UnitTestRbFind } }
            'rs'         { Measure-Command { UnitTestRsFind } }
            'rust'       { Measure-Command { UnitTestRsFind } }
            'scala'      { Measure-Command { UnitTestScalaFind } }
            'swift'      { Measure-Command { UnitTestSwiftFind } }
            'ts'         { Measure-Command { UnitTestTsFind } }
            'typescript' { Measure-Command { UnitTestTsFind } }
            default      { ExitWithError("unknown/unsupported language: $lang") }
        }
    }

    PrintTestResults
}

if ($help) {
    Usage
}

$oldPwd = Get-Location

try {
    if ($all) {
        UnitTestAll
    }

    UnitTestMain $langs
} catch {
    PrintError($_.Exception.Message)
} finally {
    Set-Location $oldPwd
}
