#!/usr/bin/env pwsh
################################################################################
#
# unittest.ps1
#
# Runs unit tests for specified language version of xfind, or all versions
#
################################################################################
param([switch]$help = $false,
      [string]$lang='')

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path $scriptDir 'config.ps1')
. (Join-Path $scriptDir 'common.ps1')

# check for help switch
$help = $help.IsPresent


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: unittest.ps1 [-help] {""all"" | langcode}`n"
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

    Log('Unit-testing cfind')
    Log('make run_tests')
    make run_tests

    Set-Location $oldPwd
}

function UnitTestClojure
{
    Write-Host
    Hdr('UnitTestClojure')

    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        return
    }

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

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

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
    Log('dart run test')
    dart run test

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

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        return
    }

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

    if (-not (Get-Command 'mvn' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mvn')
        return
    }

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

    if (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        return
    }

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

    if (-not (Get-Command 'xcodebuild' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install xcode')
        return
    }

    $oldPwd = Get-Location
    Set-Location $objcfindPath

    Log('Unit-testing objcfind')
    Log('xcodebuild test -project objcfind.xcodeproj -scheme objcfind_tests')
    xcodebuild test -project objcfind.xcodeproj -scheme objcfind_tests

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

    if (-not (Get-Command 'phpunit' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install phpunit')
        return
    }

    $phpTestsPath = Join-Path $phpfindPath 'tests'

    Log('Unit-testing plfind')
    Log("phpunit $phpTestsPath")
    phpunit $phpTestsPath
}

function UnitTestPowershell
{
    Write-Host
    Hdr('UnitTestPowershell')

    if (-not (Get-Command 'invoke-pester' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install Pester')
        return
    }

    # $phpTestsPath = Join-Path $ps1findPath 'tests'

    Log('Unit-testing ps1find')
    Log("invoke-pester $ps1findPath")
    invoke-pester $ps1findPath
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

    if (-not (Get-Command 'rake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install rake')
        return
    }

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

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo/rust')
        return
    }

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

    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install sbt')
        return
    }

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

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

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

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        return
    }

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
    param($lang='all')

    switch ($lang)
    {
        'all'        { UnitTestAll }
        'c'          { UnitTestC }
        'clj'        { UnitTestClojure }
        'clojure'    { UnitTestClojure }
        'cpp'        { UnitTestCpp }
        'cs'         { UnitTestCsharp }
        'csharp'     { UnitTestCsharp }
        'dart'       { UnitTestDart }
        'fs'         { UnitTestFsharp }
        'fsharp'     { UnitTestFsharp }
        'go'         { UnitTestGo }
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
        default      { ExitWithError("Unknown option: $lang") }
    }
}

if ($help -or $lang -eq '')
{
    Usage
}

UnitTestMain $lang
