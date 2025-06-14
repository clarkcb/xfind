#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$groovyFindPath = Join-Path $env:XFIND_PATH 'groovy' 'groovyfind'
$groovyFindAppJar = Join-Path $groovyFindPath 'app' 'build' 'libs' 'app.jar'

$env:JAVA_HOME = /usr/libexec/java_home -v17

if (Test-Path $groovyFindAppJar)
{
    & java -jar $groovyFindAppJar $Args
}
else
{
    Write-Host 'groovyfind executable not found, need to run build first'
}
