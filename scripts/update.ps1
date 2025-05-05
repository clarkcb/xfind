#!/usr/bin/env pwsh
################################################################################
#
# update.ps1
#
# Runs checks projects for updates to language and dependencies.
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

# Add failed builds to this array and report failed builds at the end
$failedBuilds = @()


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: update.ps1 [-help] {""all"" | lang [lang...]}`n"
    exit
}

function PrintFailedBuilds
{
    if ($global:failedBuilds.Length -gt 0)
    {
        $joinedBuilds = $global:failedBuilds -join ', '
        PrintError("Failed builds ($($global:failedBuilds.Length)): $joinedBuilds")
    }
    else
    {
        Log("All updates succeeded")
    }
}


################################################################################
# Update functions
################################################################################

function UpdateBashFind
{
    Write-Host
    Hdr('UpdateBashFind')

    # ensure bash is installed
    if (-not (Get-Command 'bash' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install bash')
        $global:failedBuilds += 'bashfind'
        return
    }

    $bashVersion = bash --version | Select-String -Pattern 'version'
    Log("bash version: $bashVersion")

    $deps = @('awk', 'find', 'grep', 'jq', 'sed', 'sort')
    ForEach ($d in $deps)
    {
        if (-not (Get-Command $d -ErrorAction 'SilentlyContinue'))
        {
            PrintError("You need to install $d")
            $global:failedBuilds += 'bashfind'
            return
        }
    }

    $jqVersion = jq --version
    Log("jq version: $jqVersion")
}

function UpdateCFind
{
    Write-Host
    Hdr('UpdateCFind')

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cmake')
        $global:failedBuilds += 'cfind'
        return
    }

    # cmake --version output looks like this: cmake version 3.30.2
    $cmakeVersion = cmake --version | Select-String -Pattern '^cmake version'
    $cmakeVersion = @($cmakeVersion -split '\s+')[2]
    Log("cmake version: $cmakeVersion")

    # TODO: figure out how to check for new dependency versions
}

function UpdateCljFind
{
    Write-Host
    Hdr('UpdateCljFind')

    # ensure clojure is installed
    if (-not (Get-Command 'clj' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install clojure')
        $global:failedBuilds += 'cljfind'
        return
    }

    $clojureVersion = clj -version 2>&1
    Log("clojure version: $clojureVersion")
    
    # ensure lein is installed
    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        $global:failedBuilds += 'cljfind'
        return
    }

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    $leinVersion = lein version
    Log("lein version: $leinVersion")

    # TODO: figure out how to check for new dependency versions
}

function UpdateCppFind
{
    Write-Host
    Hdr('UpdateCppFind')

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cmake')
        $global:failedBuilds += 'cppfind'
        return
    }

    $cmakeVersion = cmake --version | Select-String -Pattern '^cmake version'
    $cmakeVersion = @($cmakeVersion -split '\s+')[2]
    Log("cmake version: $cmakeVersion")
    
    # TODO: figure out how to check for new dependency versions
}

function UpdateCsFind
{
    Write-Host
    Hdr('UpdateCsFind')

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'csfind'
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    # ensure nuget is installed
    if (-not (Get-Command 'nuget' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install nuget')
        $global:failedBuilds += 'csfind'
        return
    }

    $nugetVersion = nuget 2>&1 | Select-String -Pattern 'NuGet Version:'
    $nugetVersion = $nugetVersion -replace 'NuGet Version: ',''
    Log("nuget version: $nugetVersion")

    $oldPwd = Get-Location
    Set-Location $csFindPath

    $projectsJson = dotnet list package --format json | jq '.projects'
    $projects = $projectsJson | ConvertFrom-Json
    $packageHash = @{}
    foreach ($project in $projects)
    {
        foreach ($f in $project.frameworks)
        {
            foreach ($package in $f.topLevelPackages)
            {
                $packageHash[$package.id] = $package
            }
        }
    }
    $packageNames = $packageHash.Keys | Sort-Object
    foreach ($packageName in $packageNames)
    {
        $package = $packageHash[$packageName]
        if ($package.requestedVersion -ne $package.resolvedVersion)
        {
            LogError("Package `"$packageName`" requested ($($package.requestedVersion)) != resolved ($($package.resolvedVersion))")
            $global:failedBuilds += 'csfind'
        } else {
            Log("Package `"$packageName`" requested == resolved: $($package.resolvedVersion)")
        }
    }

    Set-Location $oldPwd
}

function UpdateDartFind
{
    Write-Host
    Hdr('UpdateDartFind')

    # ensure dart is installed
    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        $global:failedBuilds += 'dartfind'
        return
    }

    $dartVersion = dart --version
    Log("$dartVersion")

    $oldPwd = Get-Location
    Set-Location $dartFindPath

    $packagesJson = dart pub outdated --json | jq '[.packages[]]'
    $packages = $packagesJson | ConvertFrom-Json
    foreach ($p in $packages)
    {
        $packageName = $p.package
        if ($p.current.version -eq $p.upgradable.version -and $p.upgradable.version -eq $p.resolvable.version)
        {
            Log("Package `"$packageName`" at latest resolvable version: $($p.resolvable.version)")
        } else {
            LogError("Package `"$packageName`" current version ($($p.current.version)) != resolvable version ($($p.resolvable.version))")
            $global:failedBuilds += 'dartfind'
        }
    }

    Set-Location $oldPwd
}

function UpdateExFind
{
    Write-Host
    Hdr('UpdateExFind')

    # ensure elixir is installed
    if (-not (Get-Command 'elixir' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install elixir')
        $global:failedBuilds += 'exfind'
        return
    }

    $elixirVersion = elixir --version | Select-String -Pattern 'Elixir'
    Log("elixir version: $elixirVersion")

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mix')
        $global:failedBuilds += 'exfind'
        return
    }

    $mixVersion = mix --version | Select-String -Pattern 'Mix'
    Log("mix version: $mixVersion")

    $oldPwd = Get-Location
    Set-Location $exFindPath

    # update dependencies
    Log('Updating exfind')
    Log('mix deps.update --all')
    mix deps.update --all

    Set-Location $oldPwd
}

function UpdateFsFind
{
    Write-Host
    Hdr('UpdateFsFind')

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'fsfind'
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    # ensure nuget is installed
    if (-not (Get-Command 'nuget' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install nuget')
        $global:failedBuilds += 'fsfind'
        return
    }

    $nugetVersion = nuget 2>&1 | Select-String -Pattern 'NuGet Version:'
    $nugetVersion = $nugetVersion -replace 'NuGet Version: ',''
    Log("nuget version: $nugetVersion")

    $oldPwd = Get-Location
    Set-Location $fsFindPath

    $projectsJson = dotnet list package --format json | jq '.projects'
    $projects = $projectsJson | ConvertFrom-Json
    $packageHash = @{}
    foreach ($project in $projects)
    {
        foreach ($f in $project.frameworks)
        {
            foreach ($package in $f.topLevelPackages)
            {
                $packageHash[$package.id] = $package
            }
        }
    }
    $packageNames = $packageHash.Keys | Sort-Object
    foreach ($packageName in $packageNames)
    {
        $package = $packageHash[$packageName]
        if ($package.requestedVersion -ne $package.resolvedVersion)
        {
            LogError("Package `"$packageName`" requested ($($package.requestedVersion)) != resolved ($($package.resolvedVersion))")
            $global:failedBuilds += 'fsfind'
        } else {
            Log("Package `"$packageName`" requested == resolved: $($package.resolvedVersion)")
        }
    }

    Set-Location $oldPwd
}

function UpdateGoFind
{
    Write-Host
    Hdr('UpdateGoFind')

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        $global:failedBuilds += 'gofind'
        return
    }

    $goVersion = (go version) -replace 'go version ',''
    Log("go version: $goVersion")

    $oldPwd = Get-Location
    Set-Location $goFindPath

    # update dependencies
    Log('Updating gofind')
    Log('go get -t -u ./...')
    go get -t -u ./...

    Set-Location $oldPwd
}

function UpdateGroovyFind
{
    Write-Host
    Hdr('UpdateGroovyFind')

    # if groovy is installed, display version
    if (Get-Command 'groovy' -ErrorAction 'SilentlyContinue')
    {
        $groovyVersion = groovy --version
        Log("groovy version: $groovyVersion")
    }

    $oldPwd = Get-Location
    Set-Location $groovyFindPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        $global:failedBuilds += 'groovyfind'
        return
    }

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle ',''}
    Log("$gradle version: $gradleVersion")

    $gradleGroovyVersion = $gradleOutput | Where-Object {$_.Contains('Groovy')} | ForEach-Object {$_ -replace 'Groovy:\s+',''}
    Log("Gradle Groovy version: $gradleGroovyVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    Log("Checking for dependency updates")
    Log("$gradle checkForDependencyUpdates")
    & $gradle checkForDependencyUpdates

    Set-Location $oldPwd
}

function UpdateHsFind
{
    Write-Host
    Hdr('UpdateHsFind')

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
        $global:failedBuilds += 'hsfind'
        return
    }

    $stackVersion = stack --version
    Log("stack version: $stackVersion")

    $oldPwd = Get-Location
    Set-Location $hsFindPath

    # update stack
    Log('Updating stack')
    Log('stack update')
    stack update

    Log('stack upgrade')
    stack upgrade

    Set-Location $oldPwd
}

function UpdateJavaFind
{
    Write-Host
    Hdr('UpdateJavaFind')

    # ensure java is installed
    if (-not (Get-Command 'java' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install java')
        $global:failedBuilds += 'javafind'
        return
    }

    $javaVersion = java -version 2>&1 | Select-String -Pattern 'java version'
    Log("java version: $javaVersion")

    $oldPwd = Get-Location
    Set-Location $javaFindPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        $global:failedBuilds += 'javafind'
        Set-Location $oldPwd
        return
    }

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle ',''}
    Log("$gradle version: $gradleVersion")

    $kotlinVersion = $gradleOutput | Where-Object {$_.Contains('Kotlin')} | ForEach-Object {$_ -replace 'Kotlin:\s+',''}
    Log("Kotlin version: $kotlinVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    Log("Checking for dependency updates")
    Log("$gradle checkForDependencyUpdates")
    & $gradle checkForDependencyUpdates

    Set-Location $oldPwd
}

function UpdateJsFind
{
    Write-Host
    Hdr('UpdateJsFind')

    # ensure node is installed
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node')
        $global:failedBuilds += 'jsfind'
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        $global:failedBuilds += 'jsfind'
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    $oldPwd = Get-Location
    Set-Location $jsFindPath

    # run tests via npm
    Log('npm outdated --json')
    $outdatedJson = npm outdated --json
    $outdated = $outdatedJson | ConvertFrom-Json -AsHashtable
    foreach ($p in $outdated.Keys)
    {
        $package = $outdated[$p]
        if ($package.current -ne $package.wanted)
        {
            LogError("Package `"$p`" current version ($($package.current)) != wanted version ($($package.wanted)) (latest: $($package.latest))")
            $global:failedBuilds += 'jsfind'
        } else {
            Log("Package `"$p`" current version == wanted ($($package.wanted)) (latest: $($package.latest))")
        }
    }

    Set-Location $oldPwd
}

function UpdateKtFind
{
    Write-Host
    Hdr('UpdateKtFind')

    $oldPwd = Get-Location
    Set-Location $ktFindPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        $global:failedBuilds += 'ktfind'
        return
    }

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle\s+',''}
    Log("$gradle version: $gradleVersion")

    $kotlinVersion = $gradleOutput | Where-Object {$_.Contains('Kotlin')} | ForEach-Object {$_ -replace 'Kotlin:\s+',''}
    Log("Kotlin version: $kotlinVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    Log("Checking for dependency updates")
    Log("$gradle checkForDependencyUpdates")
    & $gradle checkForDependencyUpdates

    Set-Location $oldPwd
}

function UpdateObjcFind
{
    Write-Host
    Hdr('UpdateObjcFind')

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'objcfind'
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $objcFindPath

    Log('swift package show-dependencies')
    swift package show-dependencies

    Log('swift package update')
    swift package update

    Set-Location $oldPwd
}

function UpdateMlFind
{
    Write-Host
    Hdr('UpdateMlFind')
    Log('not implemented at this time')
}

function UpdatePlFind
{
    Write-Host
    Hdr('UpdatePlFind')

    # ensure perl is installed
    if (-not (Get-Command 'perl' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install perl')
        $global:failedBuilds += 'plfind'
        return
    }

    $perlVersion = perl -e 'print $^V' | Select-String -Pattern 'v5'
    if (-not $perlVersion)
    {
        PrintError('A 5.x version of perl is required')
        $global:failedBuilds += 'plfind'
        return
    }

    Log("perl version: $perlVersion")

    # TODO: figure out how to check for new dependency versions
}

function UpdatePhpFind
{
    Write-Host
    Hdr('UpdatePhpFind')

    # ensure php is installed
    if (-not (Get-Command 'php' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install php')
        $global:failedBuilds += 'phpfind'
        return
    }

    $phpVersion = & php -v | Select-String -Pattern '^PHP [78]' 2>&1
    if (-not $phpVersion)
    {
        PrintError('A version of PHP >= 7.x is required')
        $global:failedBuilds += 'phpfind'
        return
    }
    Log("php version: $phpVersion")

    # ensure composer is installed
    if (-not (Get-Command 'composer' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install composer')
        $global:failedBuilds += 'phpfind'
        return
    }

    $composerVersion = composer --version 2>&1 | Select-String -Pattern '^Composer'
    Log("composer version: $composerVersion")

    $oldPwd = Get-Location
    Set-Location $phpFindPath

    $packagesJson = composer outdated --direct --format=json | jq '[.installed[]]'
    $packages = $packagesJson | ConvertFrom-Json
    foreach ($p in $packages)
    {
        if ($p.version -ne $p.latest)
        {
            if ($p.'latest-status' -eq 'update-possible')
            {
                Log("Package `"$($p.name)`" version upgrade possible: $($p.version) --> $($p.latest)")
            } else {
                LogError("Package `"$($p.name)`" installed ($($p.version)) != latest ($($p.latest))")
                $global:failedBuilds += 'phpfind'
                Set-Location $oldPwd
                return
            }
        } else {
            Log("Package `"$($p.name)`" installed version == latest: $($p.latest)")
        }
    }

    Set-Location $oldPwd
}

function UpdatePs1Find
{
    Write-Host
    Hdr('UpdatePs1Find')

    # We don't need to check for powershell, as we're running in it

    $powershellVersion = pwsh -v
    Log("powershell version: $powershellVersion")

    $testsScriptPath = Join-Path $ps1FindPath 'ps1find.tests.ps1'
    if (-not (Test-Path $testsScriptPath))
    {
        Log("Test script not found: $testsScriptPath")
        $global:failedBuilds += 'ps1find'
        return
    }

    # TODO: figure out how to check for new dependency versions
}

function UpdatePyFind
{
    Write-Host
    Hdr('UpdatePyFind')

    $venvPath = Join-Path $pyFindPath 'venv'
    if (-not (Test-Path $venvPath))
    {
        PrintError('venv path not found, you probably need to run the python build (./build.ps1 python)')
        $global:failedBuilds += 'pyfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $pyFindPath

    # activate the virtual env
    $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
    Log("$activatePath")
    & $activatePath

    Log('pip list --outdated')
    pip list --outdated

    # deactivate at end of setup process
    Log('deactivate')
    deactivate

    Set-Location $oldPwd
}

function UpdateRbFind
{
    Write-Host
    Hdr('UpdateRbFind')

    # ensure ruby3.x is installed
    if (-not (Get-Command 'ruby' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install ruby')
        $global:failedBuilds += 'rbfind'
        return
    }

    $rubyVersion = & ruby -v 2>&1 | Select-String -Pattern '^ruby 3' 2>&1
    if (-not $rubyVersion)
    {
        PrintError('A version of ruby >= 3.x is required')
        $global:failedBuilds += 'rbfind'
        return
    }

    Log("ruby version: $rubyVersion")

    # ensure bundler is installed
    if (-not (Get-Command 'bundle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install bundler: https://bundler.io/')
        $global:failedBuilds += 'rbfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $rbFindPath

    Log('bundle update')
    bundle update

    Set-Location $oldPwd
}

function UpdateRsFind
{
    Write-Host
    Hdr('UpdateRsFind')

    # if rust is installed, display version
    if (-not (Get-Command 'rustc' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install rust')
        $global:failedBuilds += 'rsfind'
        return
    }

    $rustVersion = rustc --version | Select-String -Pattern 'rustc'
    Log("rustc version: $rustVersion")

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo')
        $global:failedBuilds += 'rsfind'
        return
    }

    $cargoVersion = cargo --version
    Log("cargo version: $cargoVersion")

    $oldPwd = Get-Location
    Set-Location $rsFindPath

    # TODO: figure out how to check for new dependency versions

    Set-Location $oldPwd
}

function UpdateScalaFind
{
    Write-Host
    Hdr('UpdateScalaFind')

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
        $global:failedBuilds += 'scalafind'
        return
    }

    $sbtOutput = sbt --version

    $sbtProjectVersion = $sbtOutput | Select-String -Pattern 'project'
    Log("$sbtProjectVersion")

    $sbtScriptVersion = $sbtOutput | Select-String -Pattern 'script'
    Log("$sbtScriptVersion")

    $oldPwd = Get-Location
    Set-Location $scalaFindPath

    Log("sbt dependencyUpdates")
    sbt dependencyUpdates

    Set-Location $oldPwd
}

function UpdateSwiftFind
{
    Write-Host
    Hdr('UpdateSwiftFind')

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'swiftfind'
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $swiftFindPath

    Log('swift package show-dependencies')
    swift package show-dependencies

    Log('swift package update')
    swift package update

    Set-Location $oldPwd
}

function UpdateTsFind
{
    Write-Host
    Hdr('UpdateTsFind')

    # if node is installed, display version
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node')
        $global:failedBuilds += 'tsfind'
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        $global:failedBuilds += 'tsfind'
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    $oldPwd = Get-Location
    Set-Location $tsFindPath

    # run tests via npm
    Log('npm outdated --json')
    $outdatedJson = npm outdated --json
    $outdated = $outdatedJson | ConvertFrom-Json -AsHashtable
    foreach ($p in $outdated.Keys)
    {
        $package = $outdated[$p]
        if ($package.current -ne $package.wanted)
        {
            LogError("Package `"$p`" current version ($($package.current)) != wanted version ($($package.wanted)) (latest: $($package.latest))")
            $global:failedBuilds += 'tsfind'
        } else {
            Log("Package `"$p`" current version == wanted ($($package.wanted)) (latest: $($package.latest))")
        }
    }

    Set-Location $oldPwd
}

function UpdateAll
{
    Write-Host
    Hdr('UpdateAll')

    UpdateBashFind

    UpdateCFind

    UpdateCljFind

    UpdateCppFind

    UpdateCsFind

    UpdateDartFind

    UpdateExFind

    UpdateFsFind

    UpdateGoFind

    UpdateGroovyFind

    UpdateHsFind

    UpdateJavaFind

    UpdateJsFind

    UpdateKtFind

    UpdateObjcFind

    UpdateMlFind

    UpdatePlFind

    UpdatePhpFind

    UpdatePs1Find

    UpdatePyFind

    UpdateRbFind

    UpdateRsFind

    UpdateScalaFind

    UpdateSwiftFind

    UpdateTsFind

    PrintFailedBuilds

    exit
}

################################################################################
# Main function
################################################################################

function UpdateMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        UpdateAll
    }

    ForEach ($lang in $langs)
    {
        switch ($lang)
        {
            'bash'       { UpdateBashFind }
            'c'          { UpdateCFind }
            'clj'        { UpdateCljFind }
            'clojure'    { UpdateCljFind }
            'cpp'        { UpdateCppFind }
            'cs'         { UpdateCsFind }
            'csharp'     { UpdateCsFind }
            'dart'       { UpdateDartFind }
            'elixir'     { UpdateExFind }
            'ex'         { UpdateExFind }
            'fs'         { UpdateFsFind }
            'fsharp'     { UpdateFsFind }
            'go'         { UpdateGoFind }
            'groovy'     { UpdateGroovyFind }
            'haskell'    { UpdateHsFind }
            'hs'         { UpdateHsFind }
            'java'       { UpdateJavaFind }
            'javascript' { UpdateJsFind }
            'js'         { UpdateJsFind }
            'kotlin'     { UpdateKtFind }
            'kt'         { UpdateKtFind }
            'objc'       { UpdateObjcFind }
            'ocaml'      { UpdateMlFind }
            'ml'         { UpdateMlFind }
            'perl'       { UpdatePlFind }
            'pl'         { UpdatePlFind }
            'php'        { UpdatePhpFind }
            'powershell' { UpdatePs1Find }
            'ps1'        { UpdatePs1Find }
            'pwsh'       { UpdatePs1Find }
            'py'         { UpdatePyFind }
            'python'     { UpdatePyFind }
            'rb'         { UpdateRbFind }
            'ruby'       { UpdateRbFind }
            'rs'         { UpdateRsFind }
            'rust'       { UpdateRsFind }
            'scala'      { UpdateScalaFind }
            'swift'      { UpdateSwiftFind }
            'ts'         { UpdateTsFind }
            'typescript' { UpdateTsFind }
            default      { ExitWithError("unknown/unsupported language: $lang") }
        }
    }

    PrintFailedBuilds
}

if ($help)
{
    Usage
}

$oldPwd = Get-Location

try {
    if ($all)
    {
        UpdateAll
    }

    UpdateMain $langs
}
catch {
    PrintError($_.Exception.Message)
}
finally {
    Set-Location $oldPwd
}
