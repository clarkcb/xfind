#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$rbFindExe = Join-Path $env:XFIND_PATH 'ruby' 'rbfind' 'bin' 'rbfind.rb'

if (Test-Path $rbFindExe -PathType Leaf)
{
    & ruby $rbFindExe $Args
}
else
{
    Write-Host 'rbfind executable not found, need to run build first'
}
