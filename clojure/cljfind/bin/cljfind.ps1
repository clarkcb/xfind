#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$cljFindJarPath = Join-Path $env:XFIND_PATH 'clojure' 'cljfind' 'target' 'uberjar'

$cljFindJars = @(Get-ChildItem $cljFindJarPath) |
    Where-Object{ !$_.PSIsContainer -and $_.Extension -eq '.jar' -and $_ -match 'cljfind.+standalone' }

if ($cljFindJars.count -gt 0)
{
    & java -jar $cljFindJars[0] $Args
}
else
{
    Write-Host 'cljfind executable not found, need to run build first'
}
