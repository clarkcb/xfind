#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$groovyAppJar = Join-Path $env:XFIND_PATH 'groovy' 'groovyfind' 'app' 'build' 'libs' 'app.jar'

if (Test-Path $groovyAppJar)
{
    & java -jar $groovyAppJar $Args
}
else
{
    Write-Host 'groovyfind executable not found, need to run build first'
}
