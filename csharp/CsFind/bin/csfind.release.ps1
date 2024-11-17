#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$csFindPath = Join-Path $env:XFIND_PATH 'csharp' 'CsFind'

# $configuration = 'Debug'
$configuration = 'Release'
$dotNetVersion = 'net9.0'
$csFindExe = Join-Path $csFindPath 'CsFind' 'bin' $configuration $dotNetVersion 'CsFind'

if (Test-Path $csFindExe -PathType Leaf)
{
    & $csFindExe $Args
}
else
{
    Write-Host 'csfind executable not found, need to run build first'
}
