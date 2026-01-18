#!/usr/bin/env pwsh
################################################################################
#
# build.ps1
#
# Builds specified language versions of xfind, or all versions
#
################################################################################
param([switch]$help = $false,
      [switch]$debug = $false,
      [switch]$release = $false,
      [switch]$venv = $false,
      [switch]$all = $false)

########################################
# Configuration
########################################

$xfindScriptPath = $MyInvocation.MyCommand.Path
$xfindScriptDir = Split-Path $xfindScriptPath -Parent

. (Join-Path -Path $xfindScriptDir -ChildPath 'config.ps1')
. (Join-Path -Path $xfindScriptDir -ChildPath 'build_functions.ps1')

if (-not $release) {
    $debug = $true
}

# args holds the remaining arguments
$langs = $args
$hostname = [System.Net.Dns]::GetHostName()

Hdr('xfind build script')
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
Log("debug: $debug")
Log("release: $release")
Log("venv: $venv")
Log("all: $all")
Log("args: $args")
if ($langs.Length -gt 0 -and -not $all) {
    Log("langs ($($langs.Length)): $langs")
}


########################################
# Common Functions
########################################

function Usage
{
    Write-Host "`nUsage: build.ps1 [-help] [-debug] [-release] [-venv] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Build functions
################################################################################

function BuildLangVersion
{
    param([string]$langName, [string]$versionName)

    $langName = (Get-Culture).TextInfo.ToTitleCase($langName.ToLower())

    $functionName = "Build${langName}Version"

    if (Get-Command $functionName -ErrorAction 'SilentlyContinue') {
        & $functionName $xfindPath $versionName

        if ($global:BUILD_LASTEXITCODE -eq 0) {
            Log("$versionName build succeeded")
            $global:successfulBuilds += $versionName
        } else {
            PrintError("$versionName build failed")
            $global:failedBuilds += $versionName
        }
    }
}

function BuildBashFind
{
    Write-Host
    Hdr('BuildBashFind')

    BuildLangVersion 'bash' 'bashfind'
}

function BuildCFind
{
    Write-Host
    Hdr('BuildCFind')

    BuildLangVersion 'c' 'cfind'
}

function BuildCljFind
{
    Write-Host
    Hdr('BuildCljFind')

    BuildLangVersion 'clojure' 'cljfind'
}

function BuildCppFind
{
    Write-Host
    Hdr('BuildCppFind')

    BuildLangVersion 'cpp' 'cppfind'
}

function BuildCsFind
{
    Write-Host
    Hdr('BuildCsFind')

    BuildLangVersion 'csharp' 'csfind'
}

function BuildDartFind
{
    Write-Host
    Hdr('BuildDartFind')

    BuildLangVersion 'dart' 'dartfind'
}

function BuildExFind
{
    Write-Host
    Hdr('BuildExFind')

    BuildLangVersion 'elixir' 'exfind'
}

function BuildFsFind
{
    Write-Host
    Hdr('BuildFsFind')

    BuildLangVersion 'fsharp' 'fsfind'
}

function BuildGoFind
{
    Write-Host
    Hdr('BuildGoFind')

    BuildLangVersion 'go' 'gofind'
}

function BuildGroovyFind
{
    Write-Host
    Hdr('BuildGroovyFind')

    BuildLangVersion 'groovy' 'groovyfind'
}

function BuildHsFind
{
    Write-Host
    Hdr('BuildHsFind')

    BuildLangVersion 'haskell' 'hsfind'
}

function BuildJavaFind
{
    Write-Host
    Hdr('BuildJavaFind')

    BuildLangVersion 'java' 'javafind'
}

function BuildJsFind
{
    Write-Host
    Hdr('BuildJsFind')

    BuildLangVersion 'javascript' 'jsfind'
}

function BuildKtFind
{
    Write-Host
    Hdr('BuildKtFind')

    BuildLangVersion 'kotlin' 'ktfind'
}

function BuildMlFind
{
    Write-Host
    Hdr('BuildMlFind')
    Log("language: ocaml")

    Log("Not currently implemented")
}

function BuildObjcFind
{
    Write-Host
    Hdr('BuildObjcFind')

    BuildLangVersion 'objc' 'objcfind'
}

function BuildPlFind
{
    Write-Host
    Hdr('BuildPlFind')

    BuildLangVersion 'perl' 'plfind'
}

function BuildPhpFind
{
    Write-Host
    Hdr('BuildPhpFind')

    BuildLangVersion 'php' 'phpfind'
}

function BuildPs1Find
{
    Write-Host
    Hdr('BuildPs1Find')

    BuildLangVersion 'powershell' 'ps1find'
}

function BuildPyFind
{
    Write-Host
    Hdr('BuildPyFind')

    BuildLangVersion 'python' 'pyfind'
}

function BuildRbFind
{
    Write-Host
    Hdr('BuildRbFind')

    BuildLangVersion 'ruby' 'rbfind'
}

function BuildRsFind
{
    Write-Host
    Hdr('BuildRsFind')

    BuildLangVersion 'rust' 'rsfind'
}

function BuildScalaFind
{
    Write-Host
    Hdr('BuildScalaFind')

    BuildLangVersion 'scala' 'scalafind'
}

function BuildSwiftFind
{
    Write-Host
    Hdr('BuildSwiftFind')

    BuildLangVersion 'swift' 'swiftfind'
}

function BuildTsFind
{
    Write-Host
    Hdr('BuildTsFind')

    BuildLangVersion 'typescript' 'tsfind'
}

function BuildLinux
{
    Write-Host
    Hdr('BuildLinux')

    Measure-Command { BuildBashFind }

    Measure-Command { BuildCFind }

    # Measure-Command { BuildCljFind }

    # Measure-Command { BuildCppFind }

    Measure-Command { BuildCsFind }

    Measure-Command { BuildDartFind }

    Measure-Command { BuildFsFind }

    Measure-Command { BuildGoFind }

    Measure-Command { BuildJavaFind }

    Measure-Command { BuildJsFind }

    # Measure-Command { BuildKtFind }

    Measure-Command { BuildPlFind }

    Measure-Command { BuildPhpFind }

    # Measure-Command { BuildPs1Find }

    Measure-Command { BuildPyFind }

    Measure-Command { BuildRbFind }

    Measure-Command { BuildRsFind }

    # Measure-Command { BuildScalaFind }

    Measure-Command { BuildSwiftFind }

    Measure-Command { BuildTsFind }

    PrintBuildResults

    exit
}

function BuildAll
{
    Write-Host
    Hdr('BuildAll')

    Measure-Command { BuildBashFind }

    Measure-Command { BuildCFind }

    Measure-Command { BuildCljFind }

    Measure-Command { BuildCppFind }

    Measure-Command { BuildCsFind }

    Measure-Command { BuildDartFind }

    Measure-Command { BuildExFind }

    Measure-Command { BuildFsFind }

    Measure-Command { BuildGoFind }

    Measure-Command { BuildGroovyFind }

    Measure-Command { BuildHsFind }

    Measure-Command { BuildJavaFind }

    Measure-Command { BuildJsFind }

    Measure-Command { BuildKtFind }

    Measure-Command { BuildObjcFind }

    # Measure-Command { BuildMlFind }

    Measure-Command { BuildPlFind }

    Measure-Command { BuildPhpFind }

    Measure-Command { BuildPs1Find }

    Measure-Command { BuildPyFind }

    Measure-Command { BuildRbFind }

    Measure-Command { BuildRsFind }

    Measure-Command { BuildScalaFind }

    Measure-Command { BuildSwiftFind }

    Measure-Command { BuildTsFind }

    PrintBuildResults

    exit
}

################################################################################
# Main function
################################################################################

function BuildMain
{
    param($langs=@())

    if ($langs.Count -eq 0) {
        Usage
    }

    if ($langs -contains 'all') {
        BuildAll
        exit
    }
    if ($langs -contains 'linux') {
        BuildLinux
        exit
    }

    ForEach ($lang in $langs) {
        switch ($lang.ToLower()) {
            'bash'       { Measure-Command { BuildBashFind } }
            'c'          { Measure-Command { BuildCFind } }
            'clj'        { Measure-Command { BuildCljFind } }
            'clojure'    { Measure-Command { BuildCljFind } }
            'cpp'        { Measure-Command { BuildCppFind } }
            'cs'         { Measure-Command { BuildCsFind } }
            'csharp'     { Measure-Command { BuildCsFind } }
            'dart'       { Measure-Command { BuildDartFind } }
            'elixir'     { Measure-Command { BuildExFind } }
            'ex'         { Measure-Command { BuildExFind } }
            'fs'         { Measure-Command { BuildFsFind } }
            'fsharp'     { Measure-Command { BuildFsFind } }
            'go'         { Measure-Command { BuildGoFind } }
            'groovy'     { Measure-Command { BuildGroovyFind } }
            'haskell'    { Measure-Command { BuildHsFind } }
            'hs'         { Measure-Command { BuildHsFind } }
            'java'       { Measure-Command { BuildJavaFind } }
            'javascript' { Measure-Command { BuildJsFind } }
            'js'         { Measure-Command { BuildJsFind } }
            'kotlin'     { Measure-Command { BuildKtFind } }
            'kt'         { Measure-Command { BuildKtFind } }
            'objc'       { Measure-Command { BuildObjcFind } }
            'ocaml'      { Measure-Command { BuildMlFind } }
            'ml'         { Measure-Command { BuildMlFind } }
            'perl'       { Measure-Command { BuildPlFind } }
            'pl'         { Measure-Command { BuildPlFind } }
            'php'        { Measure-Command { BuildPhpFind } }
            'powershell' { Measure-Command { BuildPs1Find } }
            'ps1'        { Measure-Command { BuildPs1Find } }
            'pwsh'       { Measure-Command { BuildPs1Find } }
            'py'         { Measure-Command { BuildPyFind } }
            'python'     { Measure-Command { BuildPyFind } }
            'rb'         { Measure-Command { BuildRbFind } }
            'ruby'       { Measure-Command { BuildRbFind } }
            'rs'         { Measure-Command { BuildRsFind } }
            'rust'       { Measure-Command { BuildRsFind } }
            'scala'      { Measure-Command { BuildScalaFind } }
            'swift'      { Measure-Command { BuildSwiftFind } }
            'ts'         { Measure-Command { BuildTsFind } }
            'typescript' { Measure-Command { BuildTsFind } }
            default      { ExitWithError("unknown/unsupported language: $lang") }
        }
    }

    PrintBuildResults
}

if ($help) {
    Usage
}

$oldPwd = Get-Location

try {
    if ($all) {
        BuildAll
    }

    BuildMain $langs
} catch {
    PrintError($_.Exception.Message)
} finally {
    Set-Location $oldPwd
}
