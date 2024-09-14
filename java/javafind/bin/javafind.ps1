#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$javaFindJarPath = Join-Path $env:XFIND_PATH 'java' 'javafind' 'build' 'libs'
$javaFindVersion = '0.1.0-SNAPSHOT'
$javaFindJarName = "javafind-$javaFindVersion.jar"
$javaFindJar = Join-Path $javaFindJarPath $javaFindJarName

if (Test-Path $javaFindJar)
{
    & java -cp $javaFindJar 'javafind.JavaFind' $Args
}
else
{
    Write-Host 'javafind executable not found, need to run build first'
}
