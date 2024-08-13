#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$gemfile = Join-Path $env:XFIND_PATH 'ruby' 'rbfind' 'Gemfile'
$rbFindExe = Join-Path $env:XFIND_PATH 'ruby' 'rbfind' 'bin' 'rbfind.rb'

if (Test-Path $rbFindExe -PathType Leaf)
{
    & bundle exec --gemfile $gemfile ruby $rbFindExe $Args
}
else
{
    Write-Host 'rbfind executable not found, need to run build first'
}
