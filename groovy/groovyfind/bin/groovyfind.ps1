#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$groovyFindVersion = '0.1.0'
$groovyFindJarName = "groovyfind-$groovyFindVersion-SNAPSHOT.jar"
$groovyFindJarPath = Join-Path $env:XFIND_PATH 'groovy' 'groovyfind' 'build' 'libs' $groovyFindJarName

if (Test-Path $groovyAppJar)
{
    & java -jar $groovyFindJarPath $Args
}
else
{
    Write-Host 'groovyfind executable not found, need to run build first'
}
