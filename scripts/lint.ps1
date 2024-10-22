#!/usr/bin/env pwsh
################################################################################
#
# lint.ps1
#
# Run static code analysis tools
#
################################################################################
param([switch]$help = $false,
      [switch]$all = $false)

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path $scriptDir 'config.ps1')
. (Join-Path $scriptDir 'common.ps1')

# check for help switch
$help = $help.IsPresent

# check for all switch
$all = $all.IsPresent

# args holds the remaining arguments
$langs = $args

Write-Host "help: $help"
Write-Host "all: $all"
Write-Host "langs: $langs"


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: lint.ps1 [-help] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Lint functions
################################################################################

function LintC
{
    Write-Host
    Hdr('LintC')

    Log('Not implemented at this time')
}

function LintClojure
{
    Write-Host
    Hdr('LintClojure')

    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        return
    }

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    $leinVersion = lein version
    Log("lein version: $leinVersion")

    $oldPwd = Get-Location
    Set-Location $cljfindPath

    Log('Linting cljfind')
    Log('lein eastwood')
    lein eastwood

    Set-Location $oldPwd
}

function LintCpp
{
    Write-Host
    Hdr('LintCpp')

    Log('Not implemented at this time')
}

function LintCsharp
{
    Write-Host
    Hdr('LintCsharp')

    Log('Not implemented at this time')
}

function LintDart
{
    Write-Host
    Hdr('LintDart')

    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        return
    }

    $dartVersion = dart --version
    Log("$dartVersion")

    Log('Linting dartfind')
    Log("dart analyze $dartfindPath")
    dart analyze $dartfindPath
}

function LintElixir
{
    Write-Host
    Hdr('LintElixir')

    # ensure elixir is installed
    if (-not (Get-Command 'elixir' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install elixir')
        return
    }

    $elixirVersion = elixir --version | Select-String -Pattern 'Elixir'
    Log("elixir version: $elixirVersion")

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mix')
        return
    }

    $mixVersion = mix --version | Select-String -Pattern 'Mix'
    Log("mix version: $mixVersion")

    Log('Linting exfind')
    Log("mix credo $exfindPath")
    mix credo $exfindPath
}

function LintFsharp
{

    Write-Host
    Hdr('LintFsharp')

    Log('Not implemented at this time')
}

function LintGo
{
    Write-Host
    Hdr('LintGo')

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        return
    }

    $goVersion = (go version) -replace 'go version ',''
    Log("go version: $goVersion")

    $oldPwd = Get-Location
    Set-Location $gofindPath

    Log('Linting gofind')
    Log('go vet ./...')
    go vet ./...

    Set-Location $oldPwd
}

function LintGroovy
{
    Write-Host
    Hdr('LintGroovy')

    Log('Not implemented at this time')
}

function LintHaskell
{
    Write-Host
    Hdr('LintHaskell')

    $hlint = Join-Path $env:HOME '.local' 'bin' 'hlint'

    if (-not (Test-Path $hlint))
    {
        PrintError('You need to install hlint')
        return
    }

    Log('Linting hsfind')
    Log("hlint $hsfindPath")
    & $hlint $hsfindPath
}

function LintJava
{
    Write-Host
    Hdr('LintJava')

    $toolsPath = Join-Path $xfindJavaPath 'tools'

    if (-not (Test-Path $toolsPath))
    {
        Log("mkdir $toolsPath")
        mkdir $toolsPath
    }

    $checkStyleVersion = '8.41'
    # $checkStyleVersion = '10.17.0'

    $checkStyleJar = Get-ChildItem -Path $toolsPath |
        Where-Object { $_.Name -like 'checkstyle-*.jar' } |
        Where-Object { $_.Name -like "*$checkStyleVersion*" }

    if (-not (Test-Path $checkStyleJar))
    {
        Log("Checkstyle jar not found, downloading")
        $Url = "https://github.com/checkstyle/checkstyle/releases/download/checkstyle-$checkStyleVersion/checkstyle-$checkStyleVersion-all.jar"
        $oldPwd = Get-Location
        Set-Location $toolsPath
        Log("curl -J -L -O $Url")
        curl -J -L -O "$Url"
        Set-Location $oldPwd
        $checkStyleJar = Get-ChildItem -Path $toolsPath |
            Where-Object { $_.Name -like 'checkstyle-*.jar' } |
            Where-Object { $_.Name -like "*$checkStyleVersion*" }
    }

    Log("Checkstyle jar: $checkStyleJar")

    $checkStyleConfig = Join-Path $javafindPath 'google_checks.xml'

    Log('Linting javafind')
    $javaSrcPath = Join-Path $javafindPath 'src'
    $javaFiles = Get-ChildItem -Path "$javaSrcPath" -Recurse -Include *.java
    ForEach ($f in $javaFiles)
    {
        Log("java -jar $checkStyleJar -c $checkStyleConfig $f")
        java -jar $checkStyleJar -c $checkStyleConfig $f
    }
}

function LintJavaScript
{
    Write-Host
    Hdr('LintJavaScript')

    $jshint = Join-Path $jsfindPath 'node_modules' 'bin' 'jshint'

    if (-not (Test-Path $jshint))
    {
        PrintError('You need to install jshint')
        return
    }

    Log('Linting jsfind')
    $jsSrcPath = Join-Path $jsfindPath 'src'
    $jsFiles = Get-ChildItem -Path $jsSrcPath -Recurse -Include *.js
    ForEach ($f in $jsFiles)
    {
        Log("jshint $f")
        & $jshint $f
    }
}

function LintKotlin
{
    Write-Host
    Hdr('LintKotlin')

    if (-not (Get-Command 'ktlint' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install ktlint')
        return
    }

    $oldPwd = Get-Location
    Set-Location $ktfindPath

    Log('ktlint')
    ktlint

    Set-Location $oldPwd
}

function LintObjc
{
    Write-Host
    Hdr('LintObjc')

    Log('not implemented at this time')
}

function LintOcaml
{
    Write-Host
    Hdr('LintOcaml')

    Log('not implemented at this time')
}

function LintPerl
{
    Write-Host
    Hdr('LintPerl')

    Log('not implemented at this time')
}

function LintPhp
{
    Write-Host
    Hdr('LintPhp')

    $phpStan = Join-Path $phpfindPath 'vendor' 'bin' 'phpstan'
    if (-not (Test-Path $phpStan))
    {
        PrintError('You need to install phpstan')
        return
    }

    $oldPwd = Get-Location
    Set-Location $phpfindPath

    Log('Linting phpfind')
    Log("$phpStan analyse src tests")
    & $phpStan analyse src tests

    Set-Location $oldPwd
}

function LintPowerShell
{
    Write-Host
    Hdr('LintPowerShell')

    Log('Linting ps1find')

    $oldPwd = Get-Location
    Set-Location $ps1findPath

    Log('Invoke-ScriptAnalyzer -Path .')
    Invoke-ScriptAnalyzer -Path .

    Set-Location $oldPwd
}

function LintPython
{
    Write-Host
    Hdr('LintPython')

    $linter = 'ruff'
    $lintCmd = 'ruff check'

    if (-not (Get-Command $linter -ErrorAction 'SilentlyContinue'))
    {
        Log('Linter ruff not found, trying pylint')
        $linter = 'pylint'
        $lintCmd = 'pylint'


        if (-not (Get-Command $linter -ErrorAction 'SilentlyContinue'))
        {
            PrintError('Linter pylint not found, need to install either ruff or pylint')
        }
    }

    Log('Linting pyfind')

    $oldPwd = Get-Location
    Set-Location $pyfindPath

    Log("$lintCmd pyfind")
    & $lintCmd pyfind

    Set-Location $oldPwd
}

function LintRuby
{
    Write-Host
    Hdr('LintRuby')

    if (-not (Get-Command 'ruby-lint' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install ruby-lint')
    }

    Log('Linting rbfind')
    $rbFiles = Get-ChildItem -Path "$rbfindPath" -Recurse -Include *.rb
    ForEach ($f in $rbFiles)
    {
        Log("ruby-lint $f")
        ruby-lint $f
    }
}

function LintRust
{
    Write-Host
    Hdr('LintRust')

    Log('not implemented at this time')
}

function LintScala
{
    Write-Host
    Hdr('LintScala')

    Log('not implemented at this time (scalastyle is not available for scala 3.x)')
}

function LintSwift
{
    Write-Host
    Hdr('LintSwift')

    if (-not (Get-Command 'swiftlint' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swiftlint')
        return
    }

    $oldPwd = Get-Location

    Log('Linting swiftfind Sources')
    $sourcesPath = Join-Path $swiftfindPath 'Sources'
    Set-Location $sourcesPath
    swiftlint

    Log('Linting swiftfind Tests')
    $testsPath = Join-Path $swiftfindPath 'Tests'
    Set-Location $testsPath
    swiftlint

    Set-Location $oldPwd
}

function LintTypeScript
{
    Write-Host
    Hdr('LintTypeScript')

    Log('not implemented at this time')
}

function LintLinux
{
    Write-Host
    Hdr('LintLinux')

    LintC

    # LintClojure

    # LintCpp

    LintCsharp

    LintDart

    LintFsharp

    LintGo

    # LintGroovy

    # LintHaskell

    LintJava

    LintJavaScript

    LintKotlin

    # LintObjc

    # LintOcaml

    LintPerl

    LintPhp

    LintPython

    LintRuby

    LintRust

    # LintScala

    LintSwift

    LintTypeScript
}

function LintAll
{
    Write-Host
    Hdr('LintAll')

    LintC

    LintClojure

    LintCpp

    LintCsharp

    LintDart

    LintFsharp

    LintGo

    LintGroovy

    LintHaskell

    LintJava

    LintJavaScript

    LintKotlin

    LintObjc

    LintOcaml

    LintPerl

    LintPhp

    LintPowerShell

    LintPython

    LintRuby

    LintRust

    LintScala

    LintSwift

    LintTypeScript
}

################################################################################
# Main function
################################################################################

function LintMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        LintAll
        exit
    }

    ForEach ($lang in $langs)
    {
        switch ($lang)
        {
            'linux'      { LintLinux }
            'c'          { LintC }
            'clj'        { LintClojure }
            'clojure'    { LintClojure }
            'cpp'        { LintCpp }
            'cs'         { LintCsharp }
            'csharp'     { LintCsharp }
            'dart'       { LintDart }
            'elixir'     { LintElixir }
            'ex'         { LintElixir }
            'fs'         { LintFsharp }
            'fsharp'     { LintFsharp }
            'go'         { LintGo }
            'groovy'     { LintGroovy }
            'haskell'    { LintHaskell }
            'hs'         { LintHaskell }
            'java'       { LintJava }
            'javascript' { LintJavaScript }
            'js'         { LintJavaScript }
            'kotlin'     { LintKotlin }
            'kt'         { LintKotlin }
            'objc'       { LintObjc }
            # 'ocaml'      { LintOcaml }
            'ml'         { LintOcaml }
            'perl'       { LintPerl }
            'pl'         { LintPerl }
            'php'        { LintPhp }
            'powershell' { LintPowerShell }
            'ps1'        { LintPowerShell }
            'py'         { LintPython }
            'python'     { LintPython }
            'rb'         { LintRuby }
            'ruby'       { LintRuby }
            'rs'         { LintRust }
            'rust'       { LintRust }
            'scala'      { LintScala }
            'swift'      { LintSwift }
            'ts'         { LintTypeScript }
            'typescript' { LintTypeScript }
            default      { ExitWithError("unknown/unsupported language: $lang") }
        }
    }
}

if ($help)
{
    Usage
}

if ($all)
{
    LintAll
    exit
}

LintMain $langs
