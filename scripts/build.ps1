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

if (-not $release)
{
    $debug = $true
}

# args holds the remaining arguments
$langs = $args
$hostname = [System.Net.Dns]::GetHostName()

Hdr('xfind build script')
Log("user: $env:USER")
Log("host: $hostname")
if ($IsWindows)
{
    Log("os: $env:OS")
}
elseif ($IsLinux)
{
    Log("os: Linux")
}
elseif ($IsMacOS)
{
    Log("os: Darwin")
}
else
{
    Log("os: unknown")
}

$gitBranch = git branch --show-current
$gitCommit = git rev-parse --short HEAD
Log("git branch: $gitBranch ($gitCommit)")

if ($langs -contains 'all')
{
    $all = $true
}

Log("help: $help")
Log("debug: $debug")
Log("release: $release")
Log("venv: $venv")
Log("all: $all")
Log("args: $args")
if ($langs.Length -gt 0 -and -not $all)
{
    Log("langs ($($langs.Length)): $langs")
}


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: build.ps1 [-help] [-debug] [-release] [-venv] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Build functions
################################################################################

function BuildBashFind
{
    Write-Host
    Hdr('BuildBashFind')

    if (BuildBashVersion $xfindPath 'bashfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'bashfind'
    }
}

function BuildCFind
{
    Write-Host
    Hdr('BuildCFind')

    if (BuildCVersion $xfindPath 'cfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'cfind'
    }
}

function BuildCljFind
{
    Write-Host
    Hdr('BuildCljFind')

    if (BuildCljVersion $xfindPath 'cljfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'cljfind'
    }
}

function BuildCppFind
{
    Write-Host
    Hdr('BuildCppFind')

    if (BuildCppVersion $xfindPath 'cppfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'cppfind'
    }
}

function BuildCsFind
{
    Write-Host
    Hdr('BuildCsFind')

    if (BuildCsVersion $xfindPath 'csfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'csfind'
    }
}

function BuildDartFind
{
    Write-Host
    Hdr('BuildDartFind')

    if (BuildDartVersion $xfindPath 'dartfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'dartfind'
    }
}

function BuildExFind
{
    Write-Host
    Hdr('BuildExFind')

    if (BuildExVersion $xfindPath 'exfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'exfind'
    }
}

function BuildFsFind
{
    Write-Host
    Hdr('BuildFsFind')

    if (BuildFsVersion $xfindPath 'fsfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'fsfind'
    }
}

function BuildGoFind
{
    Write-Host
    Hdr('BuildGoFind')

    if (BuildGoVersion $xfindPath 'gofind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'gofind'
    }
}

function BuildGroovyFind
{
    Write-Host
    Hdr('BuildGroovyFind')

    if (BuildGroovyVersion $xfindPath 'groovyfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'groovyfind'
    }
}

function BuildHsFind
{
    Write-Host
    Hdr('BuildHsFind')

    if (BuildHsVersion $xfindPath 'hsfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'hsfind'
    }
}

function BuildJavaFind
{
    Write-Host
    Hdr('BuildJavaFind')

    if (BuildJavaVersion $xfindPath 'javafind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'javafind'
    }
}

function BuildJsFind
{
    Write-Host
    Hdr('BuildJsFind')

    if (BuildJsVersion $xfindPath 'jsfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'jsfind'
    }
}

function BuildKtFind
{
    Write-Host
    Hdr('BuildKtFind')

    if (BuildKtVersion $xfindPath 'ktfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'ktfind'
    }
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

    if (BuildObjcVersion $xfindPath 'objcfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'objcfind'
    }
}

function BuildPlFind
{
    Write-Host
    Hdr('BuildPlFind')

    if (BuildPlVersion $xfindPath 'plfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'plfind'
    }
}

function BuildPhpFind
{
    Write-Host
    Hdr('BuildPhpFind')

    if (BuildPhpVersion $xfindPath 'phpfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'phpfind'
    }
}

function BuildPs1Find
{
    Write-Host
    Hdr('BuildPs1Find')

    if (BuildPs1Version $xfindPath 'ps1find')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'ps1find'
    }
}

function BuildPyFind
{
    Write-Host
    Hdr('BuildPyFind')

    if (BuildPyVersion $xfindPath 'pyfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'pyfind'
    }
}

function BuildRbFind
{
    Write-Host
    Hdr('BuildRbFind')

    if (BuildRbVersion $xfindPath 'rbfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'rbfind'
    }
}

function BuildRsFind
{
    Write-Host
    Hdr('BuildRsFind')

    if (BuildRsVersion $xfindPath 'rsfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'rsfind'
    }
}

function BuildScalaFind
{
    Write-Host
    Hdr('BuildScalaFind')

    if (BuildScalaVersion $xfindPath 'scalafind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'scalafind'
    }
}

function BuildSwiftFind
{
    Write-Host
    Hdr('BuildSwiftFind')

    if (BuildSwiftVersion $xfindPath 'swiftfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'swiftfind'
    }
}

function BuildTsFind
{
    Write-Host
    Hdr('BuildTsFind')

    if (BuildTsVersion $xfindPath 'tsfind')
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'tsfind'
    }
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

    PrintFailedBuilds

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

    PrintFailedBuilds

    exit
}

################################################################################
# Main function
################################################################################

function BuildMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        BuildAll
        exit
    }
    if ($langs -contains 'linux')
    {
        BuildLinux
        exit
    }

    ForEach ($lang in $langs)
    {
        switch ($lang.ToLower())
        {
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

    PrintFailedBuilds
}

if ($help)
{
    Usage
}

$oldPwd = Get-Location

try {
    if ($all)
    {
        BuildAll
    }

    BuildMain $langs
}
catch {
    PrintError($_.Exception.Message)
}
finally {
    Set-Location $oldPwd
}
