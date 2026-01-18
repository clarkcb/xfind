#!/usr/bin/env pwsh
################################################################################
#
# unittest.ps1
#
# Runs unit tests for specified language version of xfind, or all versions
#
################################################################################

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

# . (Join-Path $scriptDir 'config.ps1')
. (Join-Path $scriptDir 'common.ps1')

# Global variable to hold last funtion exit code
$global:UNITTEST_LASTEXITCODE = 0

# Keep track of successful and failed tests and report at the end
$global:successfulTests = @()
$global:failedTests = @()


########################################
# Utility Functions
########################################

function PrintTestResults
{
    if ($global:successfulTests.Count -gt 0) {
        $joinedSuccessfulTests = $global:successfulTests -join ', '
        Log("Successful tests ($($global:successfulTests.Count)): $joinedSuccessfulTests")
    } else {
        Log("Successful tests: 0")
    }
    if ($global:failedTests.Count -gt 0) {
        $joinedFailedTests = $global:failedTests -join ', '
        PrintError("Failed tests ($($global:failedTests.Count)): $joinedFailedTests")
    } else {
        Log("Failed tests: 0")
    }
}


################################################################################
# Unit Test functions
################################################################################

function UnitTestBashVersion
{
    param([string]$basePath, [string]$bashVersionName)

    Log('language: bash')
    Log("version: $bashVersionName")

    # ensure bash is installed
    if (-not (Get-Command 'bash' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install bash')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $bashVersion = bash --version | Select-String -Pattern 'version'
    Log("bash version: $bashVersion")

    $bashVersionPath = Join-Path $basePath 'bash' $bashVersionName
    Log("bashVersionPath: $bashVersionPath")

    if (-not (Test-Path $bashVersionPath)) {
        PrintError("Path not found: $bashVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $bashVersionTestScript = Join-Path $bashVersionPath 'test' "${bashVersionName}tests.bash"

    if (-not (Test-Path $bashVersionTestScript)) {
        LogError("Test script not found: $bashVersionTestScript")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    # run tests
    Log("bash $bashVersionTestScript")
    bash $bashVersionTestScript | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE
}

function UnitTestCVersion
{
    param([string]$basePath, [string]$cVersionName)

    Log('language: C')
    Log("version: $cVersionName")

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install cmake')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    # cmake --version output looks like this: cmake version 3.30.2
    $cmakeVersion = cmake --version | Select-String -Pattern '^cmake version'
    $cmakeVersion = @($cmakeVersion -split '\s+')[2]
    Log("cmake version: $cmakeVersion")

    $cVersionPath = Join-Path $basePath 'c' $cVersionName
    Log("cVersionPath: $cVersionPath")

    if (-not (Test-Path $cVersionPath)) {
        PrintError("Path not found: $cVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $configurations = @('debug', 'release')
    ForEach ($c in $configurations) {
        $cmakeBuildDir = Join-Path $cVersionPath "cmake-build-$c"

        if (Test-Path $cmakeBuildDir) {
            $cVersionTestExe = Join-Path $cmakeBuildDir 'cfind-tests'
            if (Test-Path $cVersionTestExe) {
                # run tests
                Log($cVersionTestExe)
                & $cVersionTestExe | Write-Host

                $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

                # check for success/failure
                if ($global:UNITTEST_LASTEXITCODE -ne 0) {
                    # PrintError('Tests failed')
                    # $global:UNITTEST_LASTEXITCODE = 1
                    return
                }
            } else {
                LogError("${cVersionName}-tests not found: $cVersionTestExe")
                $global:UNITTEST_LASTEXITCODE = 1
                return
            }
        }
        # else
        # {
        #     LogError("cmake build directory not found: $cmmakeBuildDir")
        #     $global:UNITTEST_LASTEXITCODE = 1
        #     return
        # }
    }

    $global:UNITTEST_LASTEXITCODE = 0
}

function UnitTestClojureVersion
{
    param([string]$basePath, [string]$cljVersionName)

    Log('language: clojure')
    Log("version: $cljVersionName")

    # if clojure is installed, display version
    if (Get-Command 'clj' -ErrorAction 'SilentlyContinue') {
        # clj -version output looks like this: Clojure CLI version 1.11.4.1474
        $clojureVersion = clj -version 2>&1
        Log("clojure version: $clojureVersion")
    }

    # ensure lein is installed
    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install leiningen')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    $leinVersion = lein version
    Log("lein version: $leinVersion")

    $cljVersionPath = Join-Path $basePath 'clojure' $cljVersionName
    Log("cljVersionPath: $cljVersionPath")

    if (-not (Test-Path $cljVersionPath)) {
        PrintError("Path not found: $cljVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $cljVersionPath")
    Set-Location $cljVersionPath

    # Test with lein
    Log('lein test')
    lein test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE
    
    Set-Location $oldPwd
}

function UnitTestCppVersion
{
    param([string]$basePath, [string]$cppVersionName)

    Log('language: C++')
    Log("version: $cppVersionName")

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install cmake')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    # cmake --version output looks like this: cmake version 3.30.2
    $cmakeVersion = cmake --version | Select-String -Pattern '^cmake version'
    $cmakeVersion = @($cmakeVersion -split '\s+')[2]
    Log("cmake version: $cmakeVersion")

    $cppVersionPath = Join-Path $basePath 'cpp' $cppVersionName
    Log("cppVersionPath: $cppVersionPath")

    if (-not (Test-Path $cppVersionPath)) {
        PrintError("Path not found: $cppVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $configurations = @('debug', 'release')
    ForEach ($c in $configurations) {
        $cmakeBuildDir = Join-Path $cppVersionPath "cmake-build-$c"

        if (Test-Path $cmakeBuildDir) {
            $cppVersionTestExe = Join-Path $cmakeBuildDir "${cppVersionName}-tests"
            if (Test-Path $cppVersionTestExe) {
                # run tests
                Log($cppVersionTestExe)
                & $cppVersionTestExe | Write-Host

                # check for success/failure
                if ($LASTEXITCODE -ne 0) {
                    # PrintError('Tests failed')
                    $global:UNITTEST_LASTEXITCODE = 1
                    return
                }
            } else {
                LogError("${cppVersionName}-tests not found: $cppVersionTestExe")
                $global:UNITTEST_LASTEXITCODE = 1
                return
            }
        }
        # else
        # {
        #     LogError("cmake build directory not found: $cmmakeBuildDir")
        #     $global:UNITTEST_LASTEXITCODE = 1
        #     return
        # }
    }

    $global:UNITTEST_LASTEXITCODE = 0
}

function UnitTestCsharpVersion
{
    param([string]$basePath, [string]$csVersionName)

    Log('language: C#')
    Log("version: $csVersionName")

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install dotnet')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $csVersionPath = Join-Path $basePath 'csharp' $csVersionName
    Log("csVersionPath: $csVersionPath")

    if (-not (Test-Path $csVersionPath)) {
        PrintError("Path not found: $csVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $projectPrefix = ''
    if ($csVersionName -ieq 'csfind') {
        $projectPrefix = 'CsFind'
    } elseif ($csVersionName -ieq 'cssearch') {
        $projectPrefix = 'CsSearch'
    } else {
        PrintError("Unknown C# version name: $csVersionName")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $csVersionSolutionPath = Join-Path $csVersionPath "${projectPrefix}.sln"

    # $verbosity = 'quiet'
    $verbosity = 'minimal'
    # $verbosity = 'normal'
    # $verbosity = 'detailed'

    # run tests
    Write-Host "dotnet test $csVersionSolutionPath --verbosity $verbosity"
    dotnet test $csVersionSolutionPath --verbosity $verbosity | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE
}

function UnitTestDartVersion
{
    param([string]$basePath, [string]$dartVersionName)

    Log('language: dart')
    Log("version: $dartVersionName")

    # ensure dart is installed
    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install dart')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $dartVersion = dart --version
    Log("$dartVersion")

    $dartVersionPath = Join-Path $basePath 'dart' $dartVersionName
    Log("dartVersionPath: $dartVersionPath")

    if (-not (Test-Path $dartVersionPath)) {
        PrintError("Path not found: $dartVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $dartVersionPath")
    Set-Location $dartVersionPath

    # run tests
    Log('dart run test')
    dart run test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestElixirVersion
{
    param([string]$basePath, [string]$exVersionName)

    Log('language: elixir')
    Log("version: $exVersionName")

    if (Get-Command 'elixir' -ErrorAction 'SilentlyContinue') {
        $elixirVersion = elixir --version | Select-String -Pattern 'Elixir'
        Log("elixir version: $elixirVersion")
    }

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install mix')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $mixVersion = mix --version | Select-String -Pattern 'Mix'
    Log("mix version: $mixVersion")

    $exVersionPath = Join-Path $basePath 'elixir' $exVersionName
    Log("exVersionPath: $exVersionPath")

    if (-not (Test-Path $exVersionPath)) {
        PrintError("Path not found: $exVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $exVersionPath")
    Set-Location $exVersionPath

    # run tests
    Log('mix test')
    mix test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestFsharpVersion
{
    param([string]$basePath, [string]$fsVersionName)

    Log('language: F#')
    Log("version: $fsVersionName")

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install dotnet')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $fsVersionPath = Join-Path $basePath 'fsharp' $fsVersionName
    Log("fsVersionPath: $fsVersionPath")

    if (-not (Test-Path $fsVersionPath)) {
        PrintError("Path not found: $fsVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $projectPrefix = ''
    if ($fsVersionName -ieq 'fsfind') {
        $projectPrefix = 'FsFind'
    } elseif ($fsVersionName -ieq 'fssearch') {
        $projectPrefix = 'FsSearch'
    } else {
        PrintError("Unknown F# version name: $fsVersionName")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    # $verbosity = 'quiet'
    $verbosity = 'minimal'
    # $verbosity = 'normal'
    # $verbosity = 'detailed'

    $fsVersionSolutionPath = Join-Path $fsVersionPath "$projectPrefix.sln"

    # run tests
    Write-Host "dotnet test $fsVersionSolutionPath --verbosity $verbosity"
    dotnet test $fsVersionSolutionPath --verbosity $verbosity | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE
}

function UnitTestGoVersion
{
    param([string]$basePath, [string]$goVersionName)

    Log('language: go')
    Log("version: $goVersionName")

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install go')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $goVersion = (go version) -replace 'go version ',''
    Log("go version: $goVersion")

    $goVersionPath = Join-Path $basePath 'go' $goVersionName
    Log("goVersionPath: $goVersionPath")

    if (-not (Test-Path $goVersionPath)) {
        PrintError("Path not found: $goVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $goVersionPath")
    Set-Location $goVersionPath

    # run tests
    Log('go test --cover ./...')
    go test --cover ./... | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestGroovyVersion
{
    param([string]$basePath, [string]$groovyVersionName)

    Log('language: groovy')
    Log("version: $groovyVersionName")

    # ensure groovy is installed
    if (-not (Get-Command 'groovy' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install groovy')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $groovyVersion = groovy --version
    Log("groovy version: $groovyVersion")

    $groovyVersionPath = Join-Path $basePath 'groovy' $groovyVersionName
    Log("groovyVersionPath: $groovyVersionPath")

    if (-not (Test-Path $groovyVersionPath)) {
        PrintError("Path not found: $groovyVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $groovyVersionPath")
    Set-Location $groovyVersionPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper) {
        $gradle = $gradleWrapper
    } elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        Set-Location $oldPwd
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle ',''}
    Log("$gradle version: $gradleVersion")

    $gradleGroovyVersion = $gradleOutput | Where-Object {$_.Contains('Groovy')} | ForEach-Object {$_ -replace 'Groovy:\s+',''}
    Log("Gradle Groovy version: $gradleGroovyVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    # run tests via gradle
    Log("$gradle --warning-mode all test")
    & $gradle --warning-mode all test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestHaskellVersion
{
    param([string]$basePath, [string]$hsVersionName)

    Log('language: haskell')
    Log("version: $hsVersionName")

    # ensure ghc is installed
    if (-not (Get-Command 'ghc' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install ghc')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $ghcVersion = ghc --version
    Log("ghc version: $ghcVersion")

    # ensure stack is installed
    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install stack')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $stackVersion = stack --version
    Log("stack version: $stackVersion")

    $hsVersionPath = Join-Path $basePath 'haskell' $hsVersionName
    Log("hsVersionPath: $hsVersionPath")

    if (-not (Test-Path $hsVersionPath)) {
        PrintError("Path not found: $hsVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $hsVersionPath")
    Set-Location $hsVersionPath

    # test with stack
    Log('stack test')
    stack test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestJavaVersion
{
    param([string]$basePath, [string]$javaVersionName)

    Log('language: java')
    Log("version: $javaVersionName")

    # ensure java is installed
    if (-not (Get-Command 'java' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install java')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $javaVersion = java -version 2>&1 | Select-String -Pattern 'version'
    Log("java version: $javaVersion")

    $javaVersionPath = Join-Path $basePath 'java' $javaVersionName
    Log("javaVersionPath: $javaVersionPath")

    if (-not (Test-Path $javaVersionPath)) {
        PrintError("Path not found: $javaVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $javaVersionPath")
    Set-Location $javaVersionPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper) {
        $gradle = $gradleWrapper
    } elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle\s+',''}
    Log("$gradle version: $gradleVersion")

    $kotlinVersion = $gradleOutput | Where-Object {$_.Contains('Kotlin')} | ForEach-Object {$_ -replace 'Kotlin:\s+',''}
    Log("Kotlin version: $kotlinVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    # run tests via gradle
    Log("$gradle --warning-mode all test")
    & $gradle --warning-mode all test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestJavascriptVersion
{
    param([string]$basePath, [string]$jsVersionName)

    Log('language: javascript')
    Log("version: $jsVersionName")

    # ensure node is installed
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install node.js')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install npm')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    $jsVersionPath = Join-Path $basePath 'javascript' $jsVersionName
    Log("jsVersionPath: $jsVersionPath")

    if (-not (Test-Path $jsVersionPath)) {
        PrintError("Path not found: $jsVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $jsVersionPath")
    Set-Location $jsVersionPath

    # run tests via npm
    Log('npm test')
    npm test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestKotlinVersion
{
    param([string]$basePath, [string]$ktVersionName)

    Log('language: kotlin')
    Log("version: $ktVersionName")

    $ktVersionPath = Join-Path $basePath 'kotlin' $ktVersionName
    Log("ktVersionPath: $ktVersionPath")

    if (-not (Test-Path $ktVersionPath)) {
        PrintError("Path not found: $ktVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $ktVersionPath")
    Set-Location $ktVersionPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper) {
        $gradle = $gradleWrapper
    } elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle\s+',''}
    Log("$gradle version: $gradleVersion")

    $kotlinVersion = $gradleOutput | Where-Object {$_.Contains('Kotlin')} | ForEach-Object {$_ -replace 'Kotlin:\s+',''}
    Log("Kotlin version: $kotlinVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    # run tests via gradle
    Log("$gradle --warning-mode all test")
    & $gradle --warning-mode all test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestObjcVersion
{
    param([string]$basePath, [string]$objcVersionName)

    Log('language: objc')
    Log("version: $objcVersionName")

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install swift')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $objcVersionPath = Join-Path $basePath 'objc' $objcVersionName
    Log("objcVersionPath: $objcVersionPath")

    if (-not (Test-Path $objcVersionPath)) {
        PrintError("Path not found: $objcVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $objcVersionPath")
    Set-Location $objcVersionPath

    # run tests
    Log('swift test')
    swift test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestMlVersion
{
    Write-Host
    Hdr('UnitTestMlVersion')
    Log('not implemented at this time')
}

function UnitTestPerlVersion
{
    param([string]$basePath, [string]$plVersionName)

    Log('language: perl')
    Log("version: $plVersionName")

    if (-not (Get-Command 'perl' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install perl')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $perlVersion = perl -e 'print $^V' | Select-String -Pattern 'v5'
    if (-not $perlVersion) {
        PrintError('A 5.x version of perl is required')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    Log("perl version: $perlVersion")

    $plVersionPath = Join-Path $basePath 'perl' $plVersionName
    Log("plVersionPath: $plVersionPath")

    if (-not (Test-Path $plVersionPath)) {
        PrintError("Path not found: $plVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $plTestsPath = Join-Path $plVersionPath 't'

    # run tests
    $plTests = @(Get-ChildItem $plTestsPath |
        Where-Object{ !$_.PSIsContainer -and $_.Name -like '*_test.pl' })
    ForEach ($plTest in $plTests) {
        Log("perl $plTest")
        perl $plTest | Write-Host

        $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE
        if ($global:UNITTEST_LASTEXITCODE -ne 0) {
            return
        }
    }

    $global:UNITTEST_LASTEXITCODE = 0
}

function UnitTestPhpVersion
{
    param([string]$basePath, [string]$phpVersionName)

    Log('language: php')
    Log("version: $phpVersionName")

    # ensure php is installed
    if (-not (Get-Command 'php' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install php')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $phpVersion = & php -v | Select-String -Pattern '^PHP [78]' 2>&1
    if (-not $phpVersion) {
        PrintError('A version of PHP >= 7.x is required')
        $global:failedBuilds += 'phpfind'
        return
    }
    Log("php version: $phpVersion")

    # ensure composer is installed
    if (-not (Get-Command 'composer' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install composer')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $composerVersion = composer --version 2>&1 | Select-String -Pattern '^Composer'
    Log("composer version: $composerVersion")

    $phpVersionPath = Join-Path $basePath 'php' $phpVersionName
    Log("phpVersionPath: $phpVersionPath")

    if (-not (Test-Path $phpVersionPath)) {
        PrintError("Path not found: $phpVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $phpUnit = Join-Path $phpVersionPath 'vendor' 'bin' 'phpunit'
    
    if (-not (Test-Path $phpUnit)) {
        PrintError('You need to install phpunit')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $phpUnitVersion = & $phpUnit --version 2>&1 | Select-String -Pattern '^PHPUnit'
    Log("phpunit version: $phpUnitVersion")

    $phpTestsPath = Join-Path $phpVersionPath 'tests'

    # run tests
    Log("& $phpUnit $phpTestsPath")
    & $phpUnit $phpTestsPath | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE
}

function UnitTestPowershellVersion
{
    param([string]$basePath, [string]$ps1VersionName)

    Log('language: powershell')
    Log("version: $ps1VersionName")

    # We don't need to check for powershell, as we're running in it

    $powershellVersion = pwsh -v
    Log("powershell version: $powershellVersion")

    $ps1VersionPath = Join-Path $basePath 'powershell' $ps1VersionName
    Log("ps1VersionPath: $ps1VersionPath")

    if (-not (Test-Path $ps1VersionPath)) {
        PrintError("Path not found: $ps1VersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $testsScriptPath = Join-Path $ps1VersionPath "${ps1VersionName}.tests.ps1"
    if (-not (Test-Path $testsScriptPath)) {
        Log("Test script not found: $testsScriptPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    # run tests
    Log("& $testsScriptPath")
    & $testsScriptPath | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE
}

function UnitTestPythonVersion
{
    param([string]$basePath, [string]$pyVersionName)

    Log('language: python')
    Log("version: $pyVersionName")

    $pyVersionPath = Join-Path $basePath 'python' $pyVersionName
    Log("pyVersionPath: $pyVersionPath")

    if (-not (Test-Path $pyVersionPath)) {
        PrintError("Path not found: $pyVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $pyVersionPath")
    Set-Location $pyVersionPath

    # Set to $true venv dir exists
    # $useVenv=$false
    $python = ''
    $activeVenv = $false
    $activatedVenv = $false

    $venvPath = Join-Path $pyVersionPath 'venv'
    if (Test-Path $venvPath) {
        # $useVenv = $true
        if ($env:VIRTUAL_ENV) {
            $activeVenv = $true
        } else {
            # activate the virtual env
            $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
            Log("$activatePath")
            . $activatePath
            $activatedVenv = $true
        }
    }

    $err = $false
    $pythonCmd = Get-Command 'python3' -ErrorAction 'SilentlyContinue'
    if ($null -ne $pythonCmd) {
        $python = 'python3'
    } else {        
        PrintError('You need to install python3')
        $err = $true
    }

    if ($err -eq $false) {
        # Run the tests
        Log('pytest')
        pytest | Write-Host

        # check for success/failure
        if ($LASTEXITCODE -ne 0) {
            $err = $true
        }
    }

    # deactivate at end of setup process
    if ($activatedVenv -eq $true) {
        Log('deactivate venv')
        deactivate
    }

    Set-Location $oldPwd

    if ($err -eq $false) {
        $global:UNITTEST_LASTEXITCODE = 0
    } else {
        $global:UNITTEST_LASTEXITCODE = 1
    }
}

function UnitTestRubyVersion
{
    param([string]$basePath, [string]$rbVersionName)

    Log('language: ruby')
    Log("version: $rbVersionName")

    # ensure ruby3.x is installed
    if (-not (Get-Command 'ruby' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install ruby')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $rubyVersion = & ruby -v 2>&1 | Select-String -Pattern '^ruby 3' 2>&1
    if (-not $rubyVersion) {
        PrintError('A version of ruby >= 3.x is required')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    Log("ruby version: $rubyVersion")

    # ensure bundler is installed
    if (-not (Get-Command 'bundle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install bundler: https://bundler.io/')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $bundleVersion = bundle version
    Log("bundle version: $bundleVersion")

    # ensure rake is installed
    if (-not (Get-Command 'rake' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install rake')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $rakeVersion = rake --version | Select-String -Pattern 'rake'
    Log("rake version: $rakeVersion")

    $rbVersionPath = Join-Path $basePath 'ruby' $rbVersionName
    Log("rbVersionPath: $rbVersionPath")

    if (-not (Test-Path $rbVersionPath)) {
        PrintError("Path not found: $rbVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $rbVersionPath")
    Set-Location $rbVersionPath

    # run tests
    Log('bundle exec rake test')
    bundle exec rake test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestRustVersion
{
    param([string]$basePath, [string]$rsVersionName)

    Log('language: rust')
    Log("version: $rsVersionName")

    # ensure rust is installed
    if (-not (Get-Command 'rustc' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install rust')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $rustVersion = rustc --version | Select-String -Pattern 'rustc'
    Log("rustc version: $rustVersion")

    # ensure cargo is installed
    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install cargo')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $cargoVersion = cargo --version | Select-String -Pattern 'cargo'
    Log("cargo version: $cargoVersion")

    $rsVersionPath = Join-Path $basePath 'rust' $rsVersionName
    Log("rsVersionPath: $rsVersionPath")

    if (-not (Test-Path $rsVersionPath)) {
        PrintError("Path not found: $rsVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $rsVersionPath")
    Set-Location $rsVersionPath

    # run tests
    Log("cargo test --package $rsVersionNamed --bin $rsVersionName")
    cargo test --package $rsVersionName --bin $rsVersionName | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestScalaVersion
{
    param([string]$basePath, [string]$scalaVersionName)

    Log('language: scala')
    Log("version: $scalaVersionName")

    # ensure scalac is installed
    if (-not (Get-Command 'scala' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install scala')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    # scala --version output looks like this:
    # Scala code runner version: 1.4.3
    # Scala version (default): 3.5.2
    $scalaVersion = scala --version 2>&1 | Select-Object -Last 1
    $scalaVersion = @($scalaVersion -split '\s+')[3]
    Log("scala version: $scalaVersion")

    # ensure sbt is installed
    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install sbt')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $sbtOutput = sbt --version

    $sbtProjectVersion = $sbtOutput | Select-String -Pattern 'project'
    Log("$sbtProjectVersion")

    $sbtScriptVersion = $sbtOutput | Select-String -Pattern 'script'
    Log("$sbtScriptVersion")

    $jdkVersion = java -version 2>&1 | Select-Object -First 1
    Log("JDK version: $jdkVersion")

    $scalaVersionPath = Join-Path $basePath 'scala' $scalaVersionName
    Log("scalaVersionPath: $scalaVersionPath")

    if (-not (Test-Path $scalaVersionPath)) {
        PrintError("Path not found: $scalaVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $scalaVersionPath")
    Set-Location $scalaVersionPath

    # run tests
    Log('sbt test')
    sbt test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestSwiftVersion
{
    param([string]$basePath, [string]$swiftVersionName)

    Log('language: swift')
    Log("version: $swiftVersionName")

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install swift')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Apple Swift'
    $swiftVersion = @($swiftVersion -split '\s+')[3]
    Log("swift version: Apple Swift version $swiftVersion")

    $swiftVersionPath = Join-Path $basePath 'swift' $swiftVersionName
    Log("swiftVersionPath: $swiftVersionPath")

    if (-not (Test-Path $swiftVersionPath)) {
        PrintError("Path not found: $swiftVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $swiftVersionPath")
    Set-Location $swiftVersionPath

    # run tests
    Log('swift test')
    swift test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function UnitTestTypescriptVersion
{
    param([string]$basePath, [string]$tsVersionName)

    Log('language: typescript')
    Log("version: $tsVersionName")

    # ensure node is installed
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install node.js')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install npm')
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    $tsVersionPath = Join-Path $basePath 'typescript' $tsVersionName
    Log("tsVersionPath: $tsVersionPath")

    if (-not (Test-Path $tsVersionPath)) {
        PrintError("Path not found: $tsVersionPath")
        $global:UNITTEST_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $tsVersionPath")
    Set-Location $tsVersionPath

    # run tests
    Log('npm test')
    npm test | Write-Host

    $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}
