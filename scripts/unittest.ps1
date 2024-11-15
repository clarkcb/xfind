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

# args holds the remaining arguments
$langs = $args

if ($langs -contains 'all')
{
    $all = $true
}

Write-Host "help: $help"
Write-Host "all: $all"
if ($langs.Length -gt 0 -and -not $all)
{
    Log("langs ($($langs.Length)): $langs")
}


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

function UnitTestBashFind
{
    Write-Host
    Hdr('UnitTestBashFind')

    # ensure bash is installed
    if (-not (Get-Command 'bash' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install bash')
        return
    }

    $bashVersion = bash --version | Select-String -Pattern 'version'
    Log("bash version: $bashVersion")

    $bashFindTestPath = Join-Path $bashFindPath 'test'
    $bashFindTestScript = Join-Path $bashFindTestPath 'bashfindtests.bash'

    if (-not (Test-Path $bashFindTestScript))
    {
        LogError("Test script not found: $bashFindTestScript")
        return
    }

    # run tests
    Log('Unit-testing bashfind')
    Log("bash $bashFindTestScript")
    bash $bashFindTestScript
}

function UnitTestCFind
{
    Write-Host
    Hdr('UnitTestCFind')

    $oldPwd = Get-Location
    Set-Location $cFindPath

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
        $cmakeBuildDir = Join-Path $cFindPath "cmake-build-$c"

        if (Test-Path $cmakeBuildDir)
        {
            $cFindTestExe = Join-Path $cmakeBuildDir 'cfind-tests'
            if (Test-Path $cFindTestExe)
            {
                # run tests
                Log($cFindTestExe)
                & $cFindTestExe
            }
            else
            {
                LogError("cfind-tests not found: $cFindTestExe")
            }
        }
        else
        {
            LogError("cmake build directory not found: $cmmakeBuildDir")
        }
    }

    Set-Location $oldPwd
}

function UnitTestCljFind
{
    Write-Host
    Hdr('UnitTestCljFind')

    if (Get-Command 'clj' -ErrorAction 'SilentlyContinue')
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
    Set-Location $cljFindPath

    # Test with lein
    Log('Unit-testing cljfind')
    Log('lein test')
    lein test

    Set-Location $oldPwd
}

function UnitTestCppFind
{
    Write-Host
    Hdr('UnitTestCppFind')

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
        $cmakeBuildDir = Join-Path $cppFindPath "cmake-build-$c"

        if (Test-Path $cmakeBuildDir)
        {
            $cppFindTestExe = Join-Path $cmakeBuildDir 'cppfind-tests'
            if (Test-Path $cppFindTestExe)
            {
                # run tests
                Log($cppFindTestExe)
                & $cppFindTestExe
            }
            else
            {
                LogError("cppfind-tests not found: $cppFindTestExe")
            }
        }
        else
        {
            LogError("cmake build directory not found: $cmmakeBuildDir")
        }
    }
}

function UnitTestCsFind
{
    Write-Host
    Hdr('UnitTestCsFind')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $csFindSolutionPath = Join-Path $csFindPath 'CsFind.sln'
    # $verbosity = 'quiet'
    $verbosity = 'minimal'
    # $verbosity = 'normal'
    # $verbosity = 'detailed'

    # run tests
    Log('Unit-testing csfind')
    Write-Host "dotnet test $csFindSolutionPath --verbosity $verbosity"
    dotnet test $csFindSolutionPath --verbosity $verbosity
}

function UnitTestDartFind
{
    Write-Host
    Hdr('UnitTestDartFind')

    # ensure dart is installed
    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        return
    }

    $dartVersion = dart --version
    Log("$dartVersion")

    $oldPwd = Get-Location
    Set-Location $dartFindPath

    # run tests
    Log('Unit-testing dartfind')
    Log('dart run test')
    dart run test

    Set-Location $oldPwd
}

function UnitTestExFind
{
    Write-Host
    Hdr('UnitTestExFind')

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

    $mixVersion = mix --version | Select-String -Pattern 'Mix'
    Log("mix version: $mixVersion")

    $oldPwd = Get-Location
    Set-Location $exFindPath

    # run tests
    Log('Unit-testing exfind')
    Log('mix test')
    mix test

    Set-Location $oldPwd
}

function UnitTestFsFind
{
    Write-Host
    Hdr('UnitTestFsFind')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $fsFindSolutionPath = Join-Path $fsFindPath 'FsFind.sln'
    # $verbosity = 'quiet'
    $verbosity = 'minimal'
    # $verbosity = 'normal'
    # $verbosity = 'detailed'

    # run tests
    Log('Unit-testing fsfind')
    Write-Host "dotnet test $fsFindSolutionPath --verbosity $verbosity"
    dotnet test $fsFindSolutionPath --verbosity $verbosity
}

function UnitTestGoFind
{
    Write-Host
    Hdr('UnitTestGoFind')

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        return
    }

    $goVersion = (go version) -replace 'go version ',''
    Log("go version: $goVersion")

    $oldPwd = Get-Location
    Set-Location $goFindPath

    # run tests
    Log('Unit-testing gofind')
    Log('go test --cover ./...')
    go test --cover ./...

    Set-Location $oldPwd
}

function UnitTestGroovyFind
{
    Write-Host
    Hdr('UnitTestGroovyFind')

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

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle ',''}
    Log("$gradle version: $gradleVersion")

    $gradleGroovyVersion = $gradleOutput | Where-Object {$_.Contains('Groovy')} | ForEach-Object {$_ -replace 'Groovy:\s+',''}
    Log("Gradle Groovy version: $gradleGroovyVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    $oldPwd = Get-Location
    Set-Location $groovyFindPath

    # run tests via gradle
    Log('Unit-testing ktfind')
    Log("$gradle --warning-mode all test")
    & $gradle --warning-mode all test

    Set-Location $oldPwd
}

function UnitTestHsFind
{
    Write-Host
    Hdr('UnitTestHsFind')

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

    $stackVersion = stack --version
    Log("stack version: $stackVersion")

    $oldPwd = Get-Location
    Set-Location $hsFindPath

    # test with stack
    Log('Unit-testing hsfind')
    Log('stack test')
    stack test

    Set-Location $oldPwd
}

function UnitTestJavaFind
{
    Write-Host
    Hdr('UnitTestJavaFind')

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

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle ',''}
    Log("$gradle version: $gradleVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    $oldPwd = Get-Location
    Set-Location $ktFindPath

    # run tests via gradle
    Log('Unit-testing javafind')

    Log('gradle --warning-mode all test')
    gradle --warning-mode all test

    Set-Location $oldPwd
}

function UnitTestJsFind
{
    Write-Host
    Hdr('UnitTestJsFind')

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
    Set-Location $jsFindPath

    # run tests via npm
    Log('Unit-testing jsfind')
    Log('npm test')
    npm test

    Set-Location $oldPwd
}

function UnitTestKtFind
{
    Write-Host
    Hdr('UnitTestKtFind')

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

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle\s+',''}
    Log("$gradle version: $gradleVersion")

    $kotlinVersion = $gradleOutput | Where-Object {$_.Contains('Kotlin')} | ForEach-Object {$_ -replace 'Kotlin:\s+',''}
    Log("Kotlin version: $kotlinVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    $oldPwd = Get-Location
    Set-Location $ktFindPath

    # run tests via gradle
    Log('Unit-testing ktfind')
    Log("$gradle --warning-mode all test")
    & $gradle --warning-mode all test

    Set-Location $oldPwd
}

function UnitTestObjcFind
{
    Write-Host
    Hdr('UnitTestObjcFind')

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $objcFindPath

    # run tests
    Log('Unit-testing objcfind')
    Log('swift test')
    swift test

    Set-Location $oldPwd
}

function UnitTestMlFind
{
    Write-Host
    Hdr('UnitTestMlFind')
    Log('not implemented at this time')
}

function UnitTestPlFind
{
    Write-Host
    Hdr('UnitTestPlFind')

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

    $plTestsPath = Join-Path $plFindPath 't'

    # run tests
    Log('Unit-testing plfind')
    $plTests = @(Get-ChildItem $plTestsPath |
        Where-Object{ !$_.PSIsContainer -and $_.Extension -eq '.pl' })
    ForEach ($plTest in $plTests)
    {
        Log("perl $plTest")
        perl $plTest
    }
}

function UnitTestPhpFind
{
    Write-Host
    Hdr('UnitTestPhpFind')

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

    $phpTestsPath = Join-Path $phpFindPath 'tests'

    # run tests
    Log('Unit-testing plfind')
    Log("phpunit $phpTestsPath")
    phpunit $phpTestsPath
}

function UnitTestPs1Find
{
    Write-Host
    Hdr('UnitTestPs1Find')

    # We don't need to check for powershell, as we're running in it

    $powershellVersion = pwsh -v
    Log("powershell version: $powershellVersion")

    $testsScriptPath = Join-Path $ps1FindPath 'ps1find.tests.ps1'
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

function UnitTestPyFind
{
    Write-Host
    Hdr('UnitTestPyFind')

    $venvPath = Join-Path $pyFindPath 'venv'
    if (-not (Test-Path $venvPath))
    {
        Log('venv path not found, you probably need to run the python build (./build.ps1 python)')
        return
    }

    $oldPwd = Get-Location
    Set-Location $pyFindPath

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

function UnitTestRbFind
{
    Write-Host
    Hdr('UnitTestRbFind')

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
    Set-Location $rbFindPath

    # run tests
    Log('Unit-testing rbfind')
    Log('bundle exec rake test')
    bundle exec rake test

    Set-Location $oldPwd
}

function UnitTestRsFind
{
    Write-Host
    Hdr('UnitTestRsFind')

    # if rust is installed, display version
    if (-not (Get-Command 'rustc' -ErrorAction 'SilentlyContinue'))
    {
        $rustVersion = rustc --version | Select-String -Pattern 'rustc'
        Log("rustc version: $rustVersion")
    }

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo')
        return
    }

    $cargoVersion = cargo --version
    Log("cargo version: $cargoVersion")

    $oldPwd = Get-Location
    Set-Location $rsFindPath

    # run tests
    Log('Unit-testing rsfind')
    Log('cargo test --package rsfind --bin rsfind')
    cargo test --package rsfind --bin rsfind

    Set-Location $oldPwd
}

function UnitTestScalaFind
{
    Write-Host
    Hdr('UnitTestScalaFind')

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

    $sbtOutput = sbt --version

    $sbtProjectVersion = $sbtOutput | Select-String -Pattern 'project'
    Log("$sbtProjectVersion")

    $sbtScriptVersion = $sbtOutput | Select-String -Pattern 'script'
    Log("$sbtScriptVersion")

    $oldPwd = Get-Location
    Set-Location $scalaFindPath

    # run tests
    Log('Unit-testing scalafind')
    Log('sbt test')
    sbt test

    Set-Location $oldPwd
}

function UnitTestSwiftFind
{
    Write-Host
    Hdr('UnitTestSwiftFind')

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $swiftFindPath

    # run tests
    Log('Unit-testing swiftfind')
    Log('swift test')
    swift test

    Set-Location $oldPwd
}

function UnitTestTsFind
{
    Write-Host
    Hdr('UnitTestTsFind')

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
    Set-Location $tsFindPath

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

    UnitTestBashFind

    UnitTestCFind

    UnitTestCljFind

    UnitTestCppFind

    UnitTestCsFind

    UnitTestDartFind

    UnitTestExFind

    UnitTestFsFind

    UnitTestGoFind

    UnitTestGroovyFind

    UnitTestHsFind

    UnitTestJavaFind

    UnitTestJsFind

    UnitTestKtFind

    UnitTestObjcFind

    UnitTestMlFind

    UnitTestPlFind

    UnitTestPhpFind

    UnitTestPs1Find

    UnitTestPyFind

    UnitTestRbFind

    UnitTestRsFind

    UnitTestScalaFind

    UnitTestSwiftFind

    UnitTestTsFind

    exit
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
    }

    ForEach ($lang in $langs)
    {
        switch ($lang)
        {
            'bash'       { UnitTestBashFind }
            'c'          { UnitTestCFind }
            'clj'        { UnitTestCljFind }
            'clojure'    { UnitTestCljFind }
            'cpp'        { UnitTestCppFind }
            'cs'         { UnitTestCsFind }
            'csharp'     { UnitTestCsFind }
            'dart'       { UnitTestDartFind }
            'elixir'     { UnitTestExFind }
            'ex'         { UnitTestExFind }
            'fs'         { UnitTestFsFind }
            'fsharp'     { UnitTestFsFind }
            'go'         { UnitTestGoFind }
            'groovy'     { UnitTestGroovyFind }
            'haskell'    { UnitTestHsFind }
            'hs'         { UnitTestHsFind }
            'java'       { UnitTestJavaFind }
            'javascript' { UnitTestJsFind }
            'js'         { UnitTestJsFind }
            'kotlin'     { UnitTestKtFind }
            'kt'         { UnitTestKtFind }
            'objc'       { UnitTestObjcFind }
            'ocaml'      { UnitTestMlFind }
            'ml'         { UnitTestMlFind }
            'perl'       { UnitTestPlFind }
            'php'        { UnitTestPhpFind }
            'powershell' { UnitTestPs1Find }
            'ps1'        { UnitTestPs1Find }
            'pwsh'       { UnitTestPs1Find }
            'py'         { UnitTestPyFind }
            'python'     { UnitTestPyFind }
            'rb'         { UnitTestRbFind }
            'ruby'       { UnitTestRbFind }
            'rs'         { UnitTestRsFind }
            'rust'       { UnitTestRsFind }
            'scala'      { UnitTestScalaFind }
            'swift'      { UnitTestSwiftFind }
            'ts'         { UnitTestTsFind }
            'typescript' { UnitTestTsFind }
            default      { ExitWithError("unknown/unsupported language: $lang") }
        }
    }
}

if ($help)
{
    Usage
}

$oldPwd = Get-Location

try {
    if ($all)
    {
        UnitTestAll
    }

    UnitTestMain $langs
}
catch {
    PrintError($_.Exception.Message)
}
finally {
    Set-Location $oldPwd
}
