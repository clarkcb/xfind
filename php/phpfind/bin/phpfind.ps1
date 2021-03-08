#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$phpFindExe = Join-Path $env:XFIND_PATH 'php' 'phpfind' 'bin' 'phpfind.php'

if (Test-Path $phpFindExe -PathType Leaf)
{
    & php $phpFindExe $Args
}
else
{
    Write-Host 'phpfind executable not found, need to run build first'
}
