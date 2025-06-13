#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$javaFindPath = Join-Path $env:XFIND_PATH 'java' 'javafind'
$javaFindAppJar = Join-Path $javaFindPath 'app' 'build' 'libs' 'app.jar'

$env:JAVA_HOME = /usr/libexec/java_home -v17

if (Test-Path $javaFindAppJar)
{
    & java -jar $javaFindAppJar $Args
}
else
{
    Write-Host 'javafind executable not found, need to run build first'
}
