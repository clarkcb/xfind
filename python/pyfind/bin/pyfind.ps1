#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$pyFindExe = Join-Path $env:XFIND_PATH 'python' 'pyfind' 'bin' 'pyfind.py'

if (Test-Path $pyFindExe -PathType Leaf)
{
    & python3 $pyFindExe $Args
}
else
{
    Write-Host 'pyfind executable not found, need to run build first'
}
