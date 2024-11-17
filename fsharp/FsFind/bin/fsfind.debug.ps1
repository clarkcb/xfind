#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$fsFindPath = Join-Path $env:XFIND_PATH 'fsharp' 'FsFind'

$configuration = 'Debug'
# $configuration = 'Release'
$dotNetVersion = 'net9.0'
$fsFindExe = Join-Path $fsFindPath 'FsFind' 'bin' $configuration $dotNetVersion 'FsFind'

if (Test-Path $fsFindExe -PathType Leaf)
{
    & $fsFindExe $Args
}
else
{
    Write-Host 'fsfind executable not found, need to run build first'
}
