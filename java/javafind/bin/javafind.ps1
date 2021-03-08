#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$javaFindJarPath = Join-Path $env:XFIND_PATH 'java' 'javafind' 'target'

$javaFindJars = @(Get-ChildItem $javaFindJarPath) |
    Where-Object{ !$_.PSIsContainer -and $_.Extension -eq '.jar' -and $_ -match 'javafind' }

if ($javaFindJars.count -gt 0)
{
    & java -cp $javaFindJars[0] 'javafind.FindMain' $Args
}
else
{
    Write-Host 'javafind executable not found, need to run build first'
}
