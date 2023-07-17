#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$cppFindPath = Join-Path $env:XFIND_PATH 'cpp' 'cppfind'

# $configuration = 'debug'
$configuration = 'release'
$cmakeBuildPath = Join-Path $cppFindPath "cmake-build-$configuration"
$cppFindExe = Join-Path $cmakeBuildPath 'cppfindapp'

if (Test-Path $cppFindExe -PathType Leaf)
{
    & $cppFindExe $Args
}
else
{
    Write-Host 'cppfind executable not found, need to run build first'
}
