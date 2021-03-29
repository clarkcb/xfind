#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$swiftFindExe = Join-Path $env:XFIND_PATH 'swift' 'swiftfind' '.build' 'release' 'swiftfindApp'

if (Test-Path $swiftFindExe -PathType Leaf)
{
    & $swiftFindExe $Args
}
else
{
    Write-Host 'swiftfind executable not found, need to run build first'
}
