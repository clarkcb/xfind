#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$jsFindExe = Join-Path $env:XFIND_PATH 'javascript' 'jsfind' 'dist' 'jsfind.js'

if (Test-Path $jsFindExe -PathType Leaf)
{
    & node $jsFindExe $Args
}
else
{
    Write-Host 'jsfind executable not found, need to run build first'
}
