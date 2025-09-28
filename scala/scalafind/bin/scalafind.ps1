#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$scalaVersion = '3.7.3'
$scalaFindJarPath = Join-Path $env:XFIND_PATH 'scala' 'scalafind' 'target' "scala-$scalaVersion"
$scalaFindVersion = '0.1.0'
$scalaFindJarName = "scalafind-assembly-$scalaFindVersion.jar"
$scalaFindJar = Join-Path $scalaFindJarPath $scalaFindJarName

if (Test-Path $scalaFindJar)
{
    & java -cp $scalaFindJar 'scalafind.FindMain' $Args
}
else
{
    Write-Host 'scalafind executable not found, need to run build first'
}
