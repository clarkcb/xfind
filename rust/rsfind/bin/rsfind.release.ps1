#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

# $buildProfile = 'debug'
$buildProfile = 'release'

$rsFindExe = Join-Path $env:XFIND_PATH 'rust' 'rsfind' 'target' $buildProfile 'rsfind'

if (Test-Path $rsFindExe -PathType Leaf)
{
    & $rsFindExe $Args
}
else
{
    Write-Host 'rsfind executable not found, need to run build first'
}
