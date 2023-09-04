#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$scalaVersion = '3.2.2'
$scalaFindJarPath = Join-Path $env:XFIND_PATH 'scala' 'scalafind' 'target' "scala-$scalaVersion"
$scalaFindJars = @(Get-ChildItem $scalaFindJarPath) |
    Where-Object{ !$_.PSIsContainer -and $_.Extension -eq '.jar' -and $_ -match 'scalafind-assembly' }

if ($scalaFindJars.count -gt 0)
{
    & java -cp $scalaFindJars[0] 'scalafind.FindMain' $Args
}
else
{
    Write-Host 'scalafind executable not found, need to run build first'
}
