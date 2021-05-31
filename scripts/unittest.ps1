#!/usr/bin/env pwsh
################################################################################
#
# unittest.ps1
#
# Runs unit tests for specified language version of xfind, or all versions
#
################################################################################
param([string]$lang='all')

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path $scriptDir 'config.ps1')
. (Join-Path $scriptDir 'common.ps1')


################################################################################
# Unit Test functions
################################################################################

function UnitTestC
{
    Write-Host
    Hdr('UnitTestC')

    $oldPwd = Get-Location
    Set-Location $cfindPath

    Log('Unit-testing cfind')
    Log('not implemented at this time')

    Set-Location $oldPwd
}

function UnitTestClojure
{
    Write-Host
    Hdr('UnitTestClojure')

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

    $configurations = @('debug', 'release')
    ForEach ($c in $configurations)
    {
        $cmakeBuildDir = "$cppfindPath/cmake-build-$c"

        if (Test-Path $cmakeBuildDir)
        {
            $cppfindTestExe = Join-Path $cmakeBuildDir 'cppfind-tests'
            Log($cppfindTestExe)
            & $cppfindTestExe
        }
    }
}

function UnitTestCsharp
{
    Write-Host
    Hdr('UnitTestCsharp')

    $csfindSolutionPath = Join-Path $csfindPath 'CsFind.sln'
    # $verbosity = 'quiet'
    # $verbosity = 'minimal'
    $verbosity = 'normal'
    # $verbosity = 'detailed'

    Log('Unit-testing csfind')
    Write-Host "dotnet test $csfindSolutionPath --verbosity $verbosity"
    dotnet test $csfindSolutionPath --verbosity $verbosity
}

function UnitTestDart
{
    Write-Host
    Hdr('UnitTestDart')

    $oldPwd = Get-Location
    Set-Location $dartfindPath

    Log('Unit-testing dartfind')
    Log('pub run test')
    pub run test

    Set-Location $oldPwd
}

function UnitTestFsharp
{
    Write-Host
    Hdr('UnitTestFsharp')

    $fsfindSolutionPath = Join-Path $fsfindPath 'FsFind.sln'
    # $verbosity = 'quiet'
    # $verbosity = 'minimal'
    $verbosity = 'normal'
    # $verbosity = 'detailed'

    Log('Unit-testing fsfind')
    Write-Host "dotnet test $fsfindSolutionPath --verbosity $verbosity"
    dotnet test $fsfindSolutionPath --verbosity $verbosity
}

function UnitTestGo
{
    Write-Host
    Hdr('UnitTestGo')

    $oldPwd = Get-Location
    Set-Location $gofindPath

    Log('Unit-testing gofind')
    Log('go test --cover ./...')
    go test --cover ./...

    Set-Location $oldPwd
}

function UnitTestHaskell
{
    Write-Host
    Hdr('UnitTestHaskell')

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

    # run tests via maven
    Log('Unit-testing javafind')
    $pomPath = Join-Path $javafindPath 'pom.xml'
    Log("mvn -f $pomPath test")
    mvn -f $pomPath test
}

function UnitTestJavaScript
{
    Write-Host
    Hdr('UnitTestJavaScript')

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

    # run tests via gradle
    Log('Unit-testing ktfind')
    $buildGradlePath = Join-Path $ktfindPath 'build.gradle'
    Log("gradle -b $buildGradlePath test")
    gradle -b $buildGradlePath test
}

function UnitTestObjc
{
    Write-Host
    Hdr('UnitTestObjc - currently unimplemented')
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

    $plTestsPath = Join-Path $plfindPath 't'

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

    if (!(Get-Command 'phpunit'))
    {
        PrintError('You need to install phpunit')
        return
    }

    $phpTestsPath = Join-Path $phpfindPath 'tests'

    Log('Unit-testing plfind')
    Log("phpunit $phpTestsPath")
    phpunit $phpTestsPath
}

function UnitTestPython
{
    Write-Host
    Hdr('UnitTestPython')

    $venvPath = Join-Path $pyfindPath 'venv'
    if (!(Test-Path $venvPath))
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
    Log('nosetests')
    nosetests

    # deactivate at end of setup process
    Log('deactivate')
    deactivate

    Set-Location $oldPwd
}

function UnitTestRuby
{
    Write-Host
    Hdr('UnitTestRuby')

    $oldPwd = Get-Location
    Set-Location $rbfindPath

    Log('Unit-testing rbfind')
    Log('rake test')
    rake test

    Set-Location $oldPwd
}

function UnitTestRust
{
    Write-Host
    Hdr('UnitTestRust')

    $oldPwd = Get-Location
    Set-Location $rsfindPath

    Log('Unit-testing rsfind')
    Log('cargo test')
    cargo test

    Set-Location $oldPwd
}

function UnitTestScala
{
    Write-Host
    Hdr('UnitTestScala')

    $oldPwd = Get-Location
    Set-Location $scalafindPath

    Log('Unit-testing scalafind')
    Log('sbt test')
    sbt test

    Set-Location $oldPwd
}

function UnitTestSwift
{
    Write-Host
    Hdr('UnitTestSwift')

    $oldPwd = Get-Location
    Set-Location $swiftfindPath

    Log('Unit-testing swiftfind')
    Log('swift test')
    swift test

    Set-Location $oldPwd
}

function UnitTestTypeScript
{
    Write-Host
    Hdr('UnitTestTypeScript')

    $oldPwd = Get-Location
    Set-Location $tsfindPath

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

    UnitTestFsharp

    UnitTestGo

    UnitTestHaskell

    UnitTestJava

    UnitTestJavaScript

    UnitTestKotlin

    UnitTestObjc

    UnitTestOcaml

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
    param($lang='all')

    switch ($lang)
    {
        'all'        { UnitTestAll }
        'c'          { UnitTestC }
        'clojure'    { UnitTestClojure }
        'cpp'        { UnitTestCpp }
        'csharp'     { UnitTestCsharp }
        'dart'       { UnitTestDart }
        'fsharp'     { UnitTestFsharp }
        'go'         { UnitTestGo }
        'haskell'    { UnitTestHaskell }
        'java'       { UnitTestJava }
        'javascript' { UnitTestJavaScript }
        'kotlin'     { UnitTestKotlin }
        'objc'       { UnitTestObjc }
        'ocaml'      { UnitTestOcaml }
        'perl'       { UnitTestPerl }
        'php'        { UnitTestPhp }
        'python'     { UnitTestPython }
        'ruby'       { UnitTestRuby }
        'rust'       { UnitTestRust }
        'scala'      { UnitTestScala }
        'swift'      { UnitTestSwift }
        'typescript' { UnitTestTypeScript }
        default      { ExitWithError("Unknown option: $lang") }
    }
}

UnitTestMain $lang
