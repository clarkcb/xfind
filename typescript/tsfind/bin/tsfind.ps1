#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$tsFindExe = Join-Path $env:XFIND_PATH 'typescript' 'tsfind' 'dist' 'tsfind.js'

if (Test-Path $tsFindExe -PathType Leaf)
{
    & node $tsFindExe $Args
}
else
{
    Write-Host 'tsfind executable not found, need to run build first'
}
