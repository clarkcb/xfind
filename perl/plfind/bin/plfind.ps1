#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$plFindExe = Join-Path $env:XFIND_PATH 'perl' 'plfind' 'bin' 'plfind.pl'

if (Test-Path $plFindExe -PathType Leaf)
{
    & perl $plFindExe $Args
}
else
{
    Write-Host 'plfind executable not found, need to run build first'
}
