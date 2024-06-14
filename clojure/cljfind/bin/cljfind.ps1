#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$cljFindJarPath = Join-Path $env:XFIND_PATH 'clojure' 'cljfind' 'target' 'uberjar'
$cljFindVersion = '0.1.0-SNAPSHOT'
$cljFindJarName = "cljfind-$cljFindVersion-standalone.jar"
$cljFindJar = Join-Path $cljFindJarPath $cljFindJarName

if (Test-Path $cljFindJar)
{
    & java -jar $cljFindJar $Args
}
else
{
    Write-Host 'cljfind executable not found, need to run build first'
}
