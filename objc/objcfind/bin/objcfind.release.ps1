#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$objcFindExe = Join-Path $env:XFIND_PATH 'objc' 'objcfind' '.build' 'release' 'objcfindApp'

if (Test-Path $objcFindExe -PathType Leaf)
{
    & $objcFindExe $Args
}
else
{
    Write-Host 'objcfind executable not found, need to run build first'
}
