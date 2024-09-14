#!/usr/bin/env pwsh
################################################################################
#
# unittest.ps1
#
# Runs unit tests for specified language version of xfind, or all versions
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
    Write-Host "`nUsage: unittest.ps1 [-help] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Unit Test functions
################################################################################

function UnitTestC
{
    Write-Host
    Hdr('UnitTestC')

    $oldPwd = Get-Location
    Set-Location $cfindPath

    # if cmake is installed, display version
    if (Get-Command 'cmake' -ErrorAction 'SilentlyContinue')
    {
        # cmake --version output looks like this: cmake version 3.30.2
        $cmakeVersion = cmake --version | Select-String -Pattern '^cmake version'
        $cmakeVersion = @($cmakeVersion -split '\s+')[2]
        Log("cmake version: $cmakeVersion")
    }
    
    Log('Unit-testing cfind')

    $configurations = @('debug', 'release')
    ForEach ($c in $configurations)
    {
        $cmakeBuildDir = Join-Path $cfindPath "cmake-build-$c"

        if (Test-Path $cmakeBuildDir)
        {
            $cfindTestExe = Join-Path $cmakeBuildDir 'cfind-tests'
            if (Test-Path $cfindTestExe)
            {
                # run tests
                Log($cfindTestExe)
                & $cfindTestExe
            }
            else
            {
                LogError("cfind-tests not found: $cfindTestExe")
            }
        }
        else
        {
            LogError("cmake build directory not found: $cmmakeBuildDir")
        }
    }

    Set-Location $oldPwd
}

function UnitTestClojure
{
    Write-Host
    Hdr('UnitTestClojure')

    if (Get-Command 'lein' -ErrorAction 'SilentlyContinue')
    {
        # clj -version output looks like this: Clojure CLI version 1.11.4.1474
        $clojureVersion = clj -version 2>&1
        Log("clojure version: $clojureVersion")
    }

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

    # Test with lein
    Log('Unit-testing cljfind')
    Log('lein test')
    lein test

    Set-Location $oldPwd
}

function UnitTestCpp
{
    Write-Host
    Hdr('UnitTestCpp')

    # if cmake is installed, display version
    if (Get-Command 'cmake' -ErrorAction 'SilentlyContinue')
    {
        # cmake --version output looks like this: cmake version 3.30.2
        $cmakeVersion = cmake --version | Select-String -Pattern '^cmake version'
        $cmakeVersion = @($cmakeVersion -split '\s+')[2]
        Log("cmake version: $cmakeVersion")
    }

    $configurations = @('debug', 'release')
    ForEach ($c in $configurations)
    {
        $cmakeBuildDir = Join-Path $cppfindPath "cmake-build-$c"

        if (Test-Path $cmakeBuildDir)
        {
            $cppfindTestExe = Join-Path $cmakeBuildDir 'cppfind-tests'
            if (Test-Path $cppfindTestExe)
            {
                # run tests
                Log($cppfindTestExe)
                & $cppfindTestExe
            }
            else
            {
                LogError("cppfind-tests not found: $cppfindTestExe")
            }
        }
        else
        {
            LogError("cmake build directory not found: $cmmakeBuildDir")
        }
    }
}

function UnitTestCsharp
{
    Write-Host
    Hdr('UnitTestCsharp')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $csfindSolutionPath = Join-Path $csfindPath 'CsFind.sln'
    # $verbosity = 'quiet'
    # $verbosity = 'minimal'
    $verbosity = 'normal'
    # $verbosity = 'detailed'

    # run tests
    Log('Unit-testing csfind')
    Write-Host "dotnet test $csfindSolutionPath --verbosity $verbosity"
    dotnet test $csfindSolutionPath --verbosity $verbosity
}

function UnitTestDart
{
    Write-Host
    Hdr('UnitTestDart')

    # ensure dart is installed
    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        return
    }

    $dartVersion = dart --version
    Log("dart version: $dartVersion")

    $oldPwd = Get-Location
    Set-Location $dartfindPath

    # run tests
    Log('Unit-testing dartfind')
    Log('dart run test')
    dart run test

    Set-Location $oldPwd
}

function UnitTestElixir
{
    Write-Host
    Hdr('UnitTestElixir')

    if (Get-Command 'elixir' -ErrorAction 'SilentlyContinue')
    {
        $elixirVersion = elixir --version | Select-String -Pattern 'Elixir'
        Log("elixir version: $elixirVersion")
    }

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mix')
        return
    }

    $oldPwd = Get-Location
    Set-Location $exfindPath

    # run tests
    Log('Unit-testing exfind')
    Log('mix test')
    mix test

    Set-Location $oldPwd
}

function UnitTestFsharp
{
    Write-Host
    Hdr('UnitTestFsharp')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $fsfindSolutionPath = Join-Path $fsfindPath 'FsFind.sln'
    # $verbosity = 'quiet'
    # $verbosity = 'minimal'
    $verbosity = 'normal'
    # $verbosity = 'detailed'

    # run tests
    Log('Unit-testing fsfind')
    Write-Host "dotnet test $fsfindSolutionPath --verbosity $verbosity"
    dotnet test $fsfindSolutionPath --verbosity $verbosity
}

function UnitTestGo
{
    Write-Host
    Hdr('UnitTestGo')

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        return
    }

    $goVersion = (go version) -replace 'go version ', ''
    Log("go version: $goVersion")

    $oldPwd = Get-Location
    Set-Location $gofindPath

    # run tests
    Log('Unit-testing gofind')
    Log('go test --cover ./...')
    go test --cover ./...

    Set-Location $oldPwd
}

function UnitTestGroovy
{
    Write-Host
    Hdr('UnitTestGroovy')

    # if groovy is installed, display version
    if (Get-Command 'groovy' -ErrorAction 'SilentlyContinue')
    {
        $groovyVersion = groovy --version
        Log("groovy version: $groovyVersion")
    }

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        return
    }

    $gradleVersion = & $gradle --version | Select-String -Pattern 'Gradle'
    Log("$gradle version: $gradleVersion")

    $oldPwd = Get-Location
    Set-Location $groovyfindPath

    # run tests via gradle
    Log('Unit-testing ktfind')
    Log("$gradle --warning-mode all test")
    & $gradle --warning-mode all test

    Set-Location $oldPwd
}

function UnitTestHaskell
{
    Write-Host
    Hdr('UnitTestHaskell')

    # if ghc is installed, display version
    if (Get-Command 'ghc' -ErrorAction 'SilentlyContinue')
    {
        $ghcVersion = ghc --version
        Log("ghc version: $ghcVersion")
    }
    
    # ensure stack is installed
    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        return
    }

    $oldPwd = Get-Location
    Set-Location $hsfindPath

    # test with stack
    Log('Unit-testing hsfind')
    Log('stack test')
    stack test

    Set-Location $oldPwd
}

function UnitTestJava
{
    Write-Host
    Hdr('UnitTestJava')

    # if java is installed, display version
    if (Get-Command 'java' -ErrorAction 'SilentlyContinue')
    {
        $javaVersion = java -version 2>&1 | Select-String -Pattern 'java version'
        Log("java version: $javaVersion")
    }

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        return
    }

    $gradleVersion = & $gradle --version | Select-String -Pattern 'Gradle'
    Log("$gradle version: $gradleVersion")

    $oldPwd = Get-Location
    Set-Location $ktfindPath

    # run tests via gradle
    Log('Unit-testing javafind')

    Log('gradle --warning-mode all test')
    gradle --warning-mode all test

    Set-Location $oldPwd
}

function UnitTestJavaScript
{
    Write-Host
    Hdr('UnitTestJavaScript')

    # if node is installed, display version
    if (Get-Command 'node' -ErrorAction 'SilentlyContinue')
    {
        $nodeVersion = node --version
        Log("node version: $nodeVersion")    
    }

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        return
    }

    $oldPwd = Get-Location
    Set-Location $jsfindPath

    # run tests via npm
    Log('Unit-testing jsfind')
    Log('npm test')
    npm test

    Set-Location $oldPwd
}

function UnitTestKotlin
{
    Write-Host
    Hdr('UnitTestKotlin')

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        return
    }

    $gradleVersion = & $gradle --version | Select-String -Pattern 'Gradle'
    Log("$gradle version: $gradleVersion")

    $oldPwd = Get-Location
    Set-Location $ktfindPath

    # run tests via gradle
    Log('Unit-testing ktfind')
    # $buildGradlePath = Join-Path $ktfindPath 'build.gradle'
    Log('gradle --warning-mode all test')
    gradle --warning-mode all test

    Set-Location $oldPwd
}

function UnitTestObjc
{
    Write-Host
    Hdr('UnitTestObjc')

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $objcfindPath

    # run tests
    Log('Unit-testing objcfind')
    Log('swift test')
    swift test

    Set-Location $oldPwd
}

function UnitTestOcaml
{
    Write-Host
    Hdr('UnitTestOcaml - currently unimplemented')
}

function UnitTestPerl
{
    Write-Host
    Hdr('UnitTestPerl')

    if (-not (Get-Command 'perl' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install perl')
        return
    }

    $perlVersion = perl -e 'print $^V' | Select-String -Pattern 'v5'
    if (-not $perlVersion)
    {
        PrintError('A 5.x version of perl is required')
        return
    }

    Log("perl version: $perlVersion")

    $plTestsPath = Join-Path $plfindPath 't'

    # run tests
    Log('Unit-testing plfind')
    $pltests = @(Get-ChildItem $plTestsPath |
        Where-Object{ !$_.PSIsContainer -and $_.Extension -eq '.pl' })
    ForEach ($pltest in $pltests)
    {
        Log("perl $pltest")
        perl $pltest
    }
}

function UnitTestPhp
{
    Write-Host
    Hdr('UnitTestPhp')

    # if php is installed, display version
    if (-not (Get-Command 'php' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install php')
        return
    }

    $phpVersion = & php -v | Select-String -Pattern '^PHP [78]' 2>&1
    if (-not $phpVersion)
    {
        PrintError('A version of PHP >= 7.x is required')
        return
    }
    Log("php version: $phpVersion")

    # if composer is installed, display version
    if (Get-Command 'composer' -ErrorAction 'SilentlyContinue')
    {
        $composerVersion = composer --version 2>&1 | Select-String -Pattern '^Composer'
        Log("composer version: $composerVersion")    
    }

    if (-not (Get-Command 'phpunit' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install phpunit')
        return
    }

    $phpTestsPath = Join-Path $phpfindPath 'tests'

    # run tests
    Log('Unit-testing plfind')
    Log("phpunit $phpTestsPath")
    phpunit $phpTestsPath
}

function UnitTestPowershell
{
    Write-Host
    Hdr('UnitTestPowershell')

    # We don't need to check for powershell, as we're running in it

    $powershellVersion = pwsh -v
    Log("powershell version: $powershellVersion")
    
    $testsScriptPath = Join-Path $ps1findPath 'ps1find.tests.ps1'
    if (-not (Test-Path $testsScriptPath))
    {  
        Log("Test script not found: $testsScriptPath")
        return
    }

    # run tests
    Log('Unit-testing ps1find')
    Log("& $testsScriptPath")
    & $testsScriptPath
}

function UnitTestPython
{
    Write-Host
    Hdr('UnitTestPython')

    $venvPath = Join-Path $pyfindPath 'venv'
    if (-not (Test-Path $venvPath))
    {  
        Log('venv path not found, you probably need to run the python build (./build.ps1 python)')
        return
    }

    $oldPwd = Get-Location
    Set-Location $pyfindPath

    # activate the virtual env
    $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
    Log("$activatePath")
    & $activatePath

    Log('Unit-testing pyfind')
    # Run the individual tests
    Log('pytest')
    pytest

    # deactivate at end of setup process
    Log('deactivate')
    deactivate

    Set-Location $oldPwd
}

function UnitTestRuby
{
    Write-Host
    Hdr('UnitTestRuby')

    # ensure ruby3.x is installed
    if (-not (Get-Command 'ruby' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install ruby')
        return
    }

    $rubyVersion = & ruby -v 2>&1 | Select-String -Pattern '^ruby 3' 2>&1
    if (-not $rubyVersion)
    {
        PrintError('A version of ruby >= 3.x is required')
        return
    }

    Log("ruby version: $rubyVersion")

    # ensure bundler is installed
    # if (-not (Get-Command 'bundle' -ErrorAction 'SilentlyContinue'))
    # {
    #     PrintError('You need to install bundler: https://bundler.io/')
    #     return
    # }

    # ensure rake is installed
    if (-not (Get-Command 'rake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install rake')
        return
    }

    $oldPwd = Get-Location
    Set-Location $rbfindPath

    # run tests
    Log('Unit-testing rbfind')
    Log('bundle exec rake test')
    bundle exec rake test

    Set-Location $oldPwd
}

function UnitTestRust
{
    Write-Host
    Hdr('UnitTestRust')

    # if rust is installed, display version
    if (-not (Get-Command 'rustc' -ErrorAction 'SilentlyContinue'))
    {
        $rustVersion = rustc --version | Select-String -Pattern 'rustc'
        Log("rustc version: $rustVersion")
    }

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo/rust')
        return
    }

    $cargoVersion = cargo --version
    Log("cargo version: $cargoVersion")

    $oldPwd = Get-Location
    Set-Location $rsfindPath

    # run tests
    Log('Unit-testing rsfind')
    Log('cargo test --package rsfind --bin rsfind')
    cargo test --package rsfind --bin rsfind

    Set-Location $oldPwd
}

function UnitTestScala
{
    Write-Host
    Hdr('UnitTestScala')

    # if scala is installed, display version
    if (Get-Command 'scala' -ErrorAction 'SilentlyContinue')
    {
        $scalaVersion = scala --version 2>&1 | Select-Object -Last 1
        Log($scalaVersion)
    }

    # ensure sbt is installed
    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install sbt')
        return
    }

    $oldPwd = Get-Location
    Set-Location $scalafindPath

    # run tests
    Log('Unit-testing scalafind')
    Log('sbt test')
    sbt test

    Set-Location $oldPwd
}

function UnitTestSwift
{
    Write-Host
    Hdr('UnitTestSwift')

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $swiftfindPath

    # run tests
    Log('Unit-testing swiftfind')
    Log('swift test')
    swift test

    Set-Location $oldPwd
}

function UnitTestTypeScript
{
    Write-Host
    Hdr('UnitTestTypeScript')

    # if node is installed, display version
    if (Get-Command 'node' -ErrorAction 'SilentlyContinue')
    {
        $nodeVersion = node --version
        Log("node version: $nodeVersion")
    }

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    $oldPwd = Get-Location
    Set-Location $tsfindPath

    # run tests
    Log('Unit-testing tsfind')
    Log('npm test')
    npm test

    Set-Location $oldPwd
}

function UnitTestAll
{
    Write-Host
    Hdr('UnitTestAll')

    UnitTestC

    UnitTestClojure

    UnitTestCpp

    UnitTestCsharp

    UnitTestDart

    UnitTestElixir

    UnitTestFsharp

    UnitTestGo

    UnitTestGroovy

    UnitTestHaskell

    UnitTestJava

    UnitTestJavaScript

    UnitTestKotlin

    UnitTestObjc

    # UnitTestOcaml

    UnitTestPerl

    UnitTestPhp

    UnitTestPython

    UnitTestRuby

    UnitTestRust

    UnitTestScala

    UnitTestSwift

    UnitTestTypeScript
}

################################################################################
# Main function
################################################################################

function UnitTestMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        UnitTestAll
        exit
    }

    ForEach ($lang in $langs)
    {
        switch ($lang)
        {
            'c'          { UnitTestC }
            'clj'        { UnitTestClojure }
            'clojure'    { UnitTestClojure }
            'cpp'        { UnitTestCpp }
            'cs'         { UnitTestCsharp }
            'csharp'     { UnitTestCsharp }
            'dart'       { UnitTestDart }
            'elixir'     { UnitTestElixir }
            'ex'         { UnitTestElixir }
            'fs'         { UnitTestFsharp }
            'fsharp'     { UnitTestFsharp }
            'go'         { UnitTestGo }
            'groovy'     { UnitTestGroovy }
            'haskell'    { UnitTestHaskell }
            'hs'         { UnitTestHaskell }
            'java'       { UnitTestJava }
            'javascript' { UnitTestJavaScript }
            'js'         { UnitTestJavaScript }
            'kotlin'     { UnitTestKotlin }
            'kt'         { UnitTestKotlin }
            'objc'       { UnitTestObjc }
            # 'ocaml'      { UnitTestOcaml }
            'perl'       { UnitTestPerl }
            'php'        { UnitTestPhp }
            'powershell' { UnitTestPowershell }
            'ps1'        { UnitTestPowershell }
            'py'         { UnitTestPython }
            'python'     { UnitTestPython }
            'rb'         { UnitTestRuby }
            'ruby'       { UnitTestRuby }
            'rs'         { UnitTestRust }
            'rust'       { UnitTestRust }
            'scala'      { UnitTestScala }
            'swift'      { UnitTestSwift }
            'ts'         { UnitTestTypeScript }
            'typescript' { UnitTestTypeScript }
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
    UnitTestAll
    exit
}

UnitTestMain $langs
