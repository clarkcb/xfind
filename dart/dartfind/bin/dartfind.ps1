#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$dartFindPath = Join-Path $env:XFIND_PATH 'dart' 'dartfind'

$packagesPath = Join-Path $dartFindPath '.packages'
$dartFindExe = Join-Path $dartFindPath 'bin' 'dartfind.exe'

if (Test-Path $packagesPath -PathType Leaf)
{
    & $dartFindExe $Args
}
else
{
    Write-Host 'dartfind executable not found, need to run build first'
}
