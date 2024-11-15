#!/usr/bin/env pwsh
################################################################################
#
# build.ps1
#
# Builds specified language versions of xfind, or all versions
#
################################################################################
param([switch]$help = $false,
      [switch]$debug = $false,
      [switch]$release = $false,
      [switch]$venv = $false,
      [switch]$all = $false)

########################################
# Configuration
########################################

$xfindScriptPath = $MyInvocation.MyCommand.Path
$xfindScriptDir = Split-Path $xfindScriptPath -Parent

. (Join-Path -Path $xfindScriptDir -ChildPath 'config.ps1')
. (Join-Path -Path $xfindScriptDir -ChildPath 'common.ps1')

if (-not $release)
{
    $debug = $true
}

# args holds the remaining arguments
$langs = $args
$hostname = [System.Net.Dns]::GetHostName()

Hdr('xfind build script')
Log("user: $env:USER")
Log("host: $hostname")
if ($IsWindows)
{
    Log("os: $env:OS")
}
elseif ($IsLinux)
{
    Log("os: Linux")
}
elseif ($IsMacOS)
{
    Log("os: Darwin")
}
else
{
    Log("os: unknown")
}

$gitBranch = git branch --show-current
$gitCommit = git rev-parse --short HEAD
Log("git branch: $gitBranch ($gitCommit)")

if ($langs -contains 'all')
{
    $all = $true
}

Log("help: $help")
Log("debug: $debug")
Log("release: $release")
Log("venv: $venv")
Log("all: $all")
if ($langs.Length -gt 0 -and -not $all)
{
    Log("langs ($($langs.Length)): $langs")
}


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: build.ps1 [-help] [-debug] [-release] [-venv] {""all"" | lang [lang...]}`n"
    exit
}

function CopyFileTypesJsonResources
{
    param([string]$resourcesPath)
    $fileTypesPath = Join-Path $xfindSharedPath 'filetypes.json'
    Log("Copy-Item $fileTypesPath -Destination $resourcesPath")
    Copy-Item $fileTypesPath -Destination $resourcesPath
}

function CopyFindOptionsJsonResources
{
    param([string]$resourcesPath)
    $findOptionsPath = Join-Path $xfindSharedPath 'findoptions.json'
    Log("Copy-Item $findOptionsPath -Destination $resourcesPath")
    Copy-Item $findOptionsPath -Destination $resourcesPath
}

function CopyJsonResources
{
    param([string]$resourcesPath)
    CopyFileTypesJsonResources($resourcesPath)
    CopyFindOptionsJsonResources($resourcesPath)
}

function CopyTestResources
{
    param([string]$testResourcesPath)
    Log("Copy-Item $xfindTestFilePath -Include testFile*.txt -Destination $testResourcesPath")
    Copy-Item $xfindTestFilePath -Include testFile*.txt -Destination $testResourcesPath
}

function AddSoftLink
{
    param([string]$linkPath, [string]$targetPath, [bool]$replaceLink=$true)
    # Write-Host "linkPath: $linkPath"
    # Write-Host "targetPath: $targetPath"

    if ((Test-Path $linkPath) -and $replaceLink)
    {
        if ((Get-Item $linkPath).LinkType -eq 'SymbolicLink')
        {
            Log("Remove-Item $linkPath")
            Remove-Item $linkPath
        }
    }

    if (-not (Test-Path $linkPath))
    {
        # from https://winaero.com/create-symbolic-link-windows-10-powershell/
        # New-Item -ItemType SymbolicLink -Path "Link" -Target "Target"
        Log("New-Item -ItemType SymbolicLink -Path $linkPath -Target $targetPath")
        New-Item -ItemType SymbolicLink -Path $linkPath -Target $targetPath
    }
}

function AddToBin
{
    param([string]$xfindScriptPath)

    if (-not (Test-Path $xfindBinPath))
    {
        New-Item -ItemType directory -Path $xfindBinPath
    }

    # get the base filename, minus path and any extension
    $baseName = [io.path]::GetFileNameWithoutExtension($xfindScriptPath)
    if ($baseName.EndsWith('.debug') -or $baseName.EndsWith('.release'))
    {
        $baseName = $baseName.Split('.')[0]
    }

    $linkPath = Join-Path $xfindBinPath $baseName

    AddSoftLink $linkPath $xfindScriptPath
}


################################################################################
# Build functions
################################################################################

function BuildBashFind
{
    Write-Host
    Hdr('BuildBashFind')
    Log("language: bash")

    # ensure bash is installed
    if (-not (Get-Command 'bash' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install bash')
        return
    }

    $bashVersion = bash --version | Select-String -Pattern 'version'
    Log("bash version: $bashVersion")

    # add to bin
    $bashFindExe = Join-Path $bashFindPath 'bin' 'bashfind.bash'
    AddToBin($bashFindExe)
}

function BuildCFind
{
    Write-Host
    Hdr('BuildCFind')
    Log("language: C")

    if ($IsWindows)
    {
        Log('BuildCFind - currently unimplemented for Windows')
        return
    }
    if (!$IsMacOS -and !$IsLinux)
    {
        Log('Skipping for unknown/unsupported OS')
        return
    }

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cmake')
        return
    }

    # cmake --version output looks like this: cmake version 3.30.2
    $cmakeVersion = cmake --version | Select-String -Pattern '^cmake version'
    $cmakeVersion = @($cmakeVersion -split '\s+')[2]
    Log("cmake version: $cmakeVersion")

    $oldPwd = Get-Location
    Set-Location $cFindPath

    $configurations = @()
    if ($debug)
    {
        $configurations += 'debug'
    }
    if ($release)
    {
        $configurations += 'release'
    }
    ForEach ($c in $configurations)
    {
        $cmakeBuildDir = "cmake-build-$c"
        $cmakeBuildPath = Join-Path $cFindPath $cmakeBuildDir

        if (-not (Test-Path $cmakeBuildPath))
        {
            New-Item -ItemType directory -Path $cmakeBuildPath

            Set-Location $cmakeBuildPath

            Log("cmake -G ""Unix Makefiles"" -DCMAKE_BUILD_TYPE=$c ..")
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            # Log("make -f Makefile")
            # make -f Makefile

            Set-Location $cFindPath
        }

        $targets = @('clean', 'cfind', 'cfindapp', 'cfind-tests')
        ForEach ($t in $targets)
        {
            Log("cmake --build $cmakeBuildDir --config $c --target $t")
            cmake --build $cmakeBuildDir --config $c --target $t
            if ($LASTEXITCODE -eq 0)
            {
                Log("Build target $t succeeded")
            }
            else
            {
                PrintError("Build target $t failed")
                Set-Location $oldPwd
                return
            }
        }
    }

    if ($release)
    {
        # add release to bin
        $cFindExe = Join-Path $cFindPath 'bin' 'cfind.release.ps1'
        AddToBin($cFindExe)
    }
    else
    {
        # add debug to bin
        $cFindExe = Join-Path $cFindPath 'bin' 'cfind.debug.ps1'
        AddToBin($cFindExe)
    }

    Set-Location $oldPwd
}

function BuildCljFind
{
    Write-Host
    Hdr('BuildCljFind')
    Log("language: clojure")

    # ensure clojure is installed
    if (-not (Get-Command 'clj' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install clojure')
        return
    }

    # clj -version output looks like this: Clojure CLI version 1.11.4.1474
    $clojureVersion = clj -version 2>&1
    Log("clojure version: $clojureVersion")

    # ensure leiningen is installed
    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        return
    }

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    $leinVersion = lein version
    Log("lein version: $leinVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $cljFindPath 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $cljFindPath

    # Create uberjar with lein
    Log('Building cljfind')
    Log('lein clean')
    lein clean
    Log('lein install')
    lein install
    Log('lein uberjar')
    lein uberjar

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $cljFindExe = Join-Path $cljFindPath 'bin' 'cljfind.ps1'
    AddToBin($cljFindExe)

    Set-Location $oldPwd
}

function BuildCppFind
{
    Write-Host
    Hdr('BuildCppFind')
    Log("language: C++")

    if ($IsWindows)
    {
        Log('BuildCppFind - currently unimplemented for Windows')
        return
    }
    if (!$IsMacOS -and !$IsLinux)
    {
        Log('Skipping for unknown/unsupported OS')
        return
    }

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cmake')
        return
    }

    # cmake --version output looks like this: cmake version 3.30.2
    $cmakeVersion = cmake --version
    $cmakeVersion = @($cmakeVersion -split '\s+')[2]
    Log("cmake version: $cmakeVersion")

    $oldPwd = Get-Location
    Set-Location $cppFindPath

    # Set CMAKE_CXX_FLAGS
    $cmakeCxxFlags = "-W -Wall -Werror -Wextra -Wshadow -Wnon-virtual-dtor -pedantic"

    # Add AddressSanitizer
    # $cmakeCxxFlags = "$cmakeCxxFlags -fsanitize=address -fno-omit-frame-pointer"

    $configurations = @()
    if ($debug)
    {
        $configurations += 'debug'
    }
    if ($release)
    {
        $configurations += 'release'
    }
    ForEach ($c in $configurations)
    {
        $cmakeBuildDir = "cmake-build-$c"
        $cmakeBuildPath = Join-Path $cppFindPath $cmakeBuildDir

        if (-not (Test-Path $cmakeBuildPath))
        {
            New-Item -ItemType directory -Path $cmakeBuildPath

            Set-Location $cmakeBuildPath

            Log("cmake -G ""Unix Makefiles"" -DCMAKE_BUILD_TYPE=$c ..")
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            # Log("make -f Makefile")
            # make -f Makefile

            Set-Location $cppFindPath
        }

        $targets = @('clean', 'cppfind', 'cppfindapp', 'cppfind-tests')
        ForEach ($t in $targets)
        {
            Log("cmake --build $cmakeBuildDir --config $c --target $t -- $cmakeCxxFlags")
            cmake --build $cmakeBuildDir --config $c --target $t -- $cmakeCxxFlags
            if ($LASTEXITCODE -eq 0)
            {
                Log("Build target $t succeeded")
            }
            else
            {
                PrintError("Build target $t failed")
                Set-Location $oldPwd
                return
            }
        }
    }

    if ($release)
    {
        # add release to bin
        $cppFindExe = Join-Path $cppFindPath 'bin' 'cppfind.release.ps1'
        AddToBin($cppFindExe)
    }
    else
    {
        # add debug to bin
        $cppFindExe = Join-Path $cppFindPath 'bin' 'cppfind.debug.ps1'
        AddToBin($cppFindExe)
    }

    Set-Location $oldPwd
}

function BuildCsFind
{
    Write-Host
    Hdr('BuildCsFind')
    Log("language: C#")

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $resourcesPath = Join-Path $csFindPath 'CsFindLib' 'Resources'
    $testResourcesPath = Join-Path $csFindPath 'CsFindTests' 'Resources'

    # copy the shared json files to the local resource location
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the shared test files to the local test resource location
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $csFindPath

    $csFindSolutionPath = Join-Path $csFindPath 'CsFind.sln'

    $configurations = @()
    if ($debug)
    {
        $configurations += 'Debug'
    }
    if ($release)
    {
        $configurations += 'Release'
    }

    # run dotnet build selected configurations
    ForEach ($c in $configurations)
    {
        Log("Building CsFind solution for $c configuration")
        Log("dotnet build $csFindSolutionPath --configuration $c")
        dotnet build $csFindSolutionPath --configuration $c

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            Set-Location $oldPwd
            return
        }
    }

    if ($release)
    {
        # add release to bin
        $csFindExe = Join-Path $csFindPath 'bin' 'csfind.release.ps1'
        AddToBin($csFindExe)
    }
    else
    {
        # add debug to bin
        $csFindExe = Join-Path $csFindPath 'bin' 'csfind.debug.ps1'
        AddToBin($csFindExe)
    }

    Set-Location $oldPwd
}

function BuildDartFind
{
    Write-Host
    Hdr('BuildDartFind')
    Log("language: dart")

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

    Log('Building dartfind')
    if ((-not (Test-Path (Join-Path $dartFindPath '.dart_tool' 'package_config.json'))) -and
        (-not (Test-Path (Join-Path $dartFindPath '.packages'))))
    {
        Log('dart pub get')
        dart pub get
    }
    else
    {
        Log('dart pub upgrade')
        dart pub upgrade
    }

    Log('Compiling dartfind')
    $dartScript = Join-Path $dartFindPath 'bin' 'dartfind.dart'
    Log("dart compile exe $dartScript")
    dart compile exe $dartScript

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $dartFindExe = Join-Path $dartFindPath 'bin' 'dartfind.ps1'
    AddToBin($dartFindExe)

    Set-Location $oldPwd
}

function BuildExFind
{
    Write-Host
    Hdr('BuildExFind')
    Log("language: elixir")

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

    $oldPwd = Get-Location
    Set-Location $exFindPath

    Log('Getting exfind dependencies')
    Log('mix deps.get')
    mix deps.get

    Log('Compiling exfind')
    Log('mix compile')
    mix compile

    Log('Creating exfind executable')
    Log('mix escript.build')
    mix escript.build

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $exFindExe = Join-Path $exFindPath 'bin' 'exfind'
    AddToBin($exFindExe)

    Set-Location $oldPwd
}

function BuildFsFind
{
    Write-Host
    Hdr('BuildFsFind')
    Log("language: F#")

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $resourcesPath = Join-Path $fsFindPath 'FsFindLib' 'Resources'
    $testResourcesPath = Join-Path $fsFindPath 'FsFindTests' 'Resources'

    # copy the shared json files to the local resource location
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the shared test files to the local test resource location
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $fsFindPath

    $fsFindSolutonPath = Join-Path $fsFindPath 'FsFind.sln'

    $configurations = @()
    if ($debug)
    {
        $configurations += 'Debug'
    }
    if ($release)
    {
        $configurations += 'Release'
    }

    # run dotnet build for selected configurations
    ForEach ($c in $configurations)
    {
        Log("Building FsFind solution for $c configuration")
        Log("dotnet build $fsFindSolutonPath --configuration $c")
        dotnet build $fsFindSolutonPath --configuration $c

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            Set-Location $oldPwd
            return
        }
    }

    if ($release)
    {
        # add release to bin
        $fsFindExe = Join-Path $fsFindPath 'bin' 'fsfind.release.ps1'
        AddToBin($fsFindExe)
    }
    else
    {
        # add debug to bin
        $fsFindExe = Join-Path $fsFindPath 'bin' 'fsfind.debug.ps1'
        AddToBin($fsFindExe)
    }

    Set-Location $oldPwd
}

function BuildGoFind
{
    Write-Host
    Hdr('BuildGoFind')
    Log("language: go")

    # ensure go is installed
    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        return
    }

    $goVersion = (go version) -replace 'go version ', ''
    Log("go version: $goVersion")

    $oldPwd = Get-Location
    Set-Location $goFindPath

    # go fmt the gofind source (for auto-generated code)
    Log('Auto-formatting gofind')
    Log('go fmt ./...')
    go fmt ./...

    # create the bin dir if it doesn't already exist
    if (-not (Test-Path $xfindBinPath))
    {
        New-Item -ItemType directory -Path $xfindBinPath
    }

    # if GOBIN not defined, set to BIN_PATH
    if (-not (Test-Path Env:GOBIN))
    {
        $env:GOBIN = $xfindBinPath
    }

    # now build gofind
    Log('Building gofind')
    Log('go install ./...')
    go install ./...

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    if ($env:GOBIN -ne $xfindBinPath)
    {
        # add to bin
        $goFindExe = Join-Path $env:GOBIN 'gofind'
        AddToBin($goFindExe)
    }

    Set-Location $oldPwd
}

function BuildGroovyFind
{
    Write-Host
    Hdr('BuildGroovyFind')
    Log("language: groovy")

    # ensure groovy is installed
    if (-not (Get-Command 'groovy' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install groovy')
        return
    }

    $groovyVersion = groovy --version
    Log("groovy version: $groovyVersion")

    $oldPwd = Get-Location
    Set-Location $groovyFindPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        return
    }

    $gradleOutput = & $gradle --version
    # ------------------------------------------------------------
    # Gradle 8.10.2
    # ------------------------------------------------------------

    # Build time:    2024-09-23 21:28:39 UTC
    # Revision:      415adb9e06a516c44b391edff552fd42139443f7

    # Kotlin:        1.9.24
    # Groovy:        3.0.22
    # Ant:           Apache Ant(TM) version 1.10.14 compiled on August 16 2023
    # Launcher JVM:  11.0.24 (Homebrew 11.0.24+0)
    # Daemon JVM:    /usr/local/Cellar/openjdk@11/11.0.24/libexec/openjdk.jdk/Contents/Home (no JDK specified, using current Java home)
    # OS:            Mac OS X 14.6.1 x86_64

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle ',''}
    Log("$gradle version: $gradleVersion")

    $gradleGroovyVersion = $gradleOutput | Where-Object {$_.Contains('Groovy')} | ForEach-Object {$_ -replace 'Groovy:\s+',''}
    Log("Gradle Groovy version: $gradleGroovyVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $groovyFindPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $groovyFindPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    # run the gradle command to build
    Log('Building groovyfind')

    $gradleArgs = '--warning-mode all'
    $gradleTasks = 'clean jar'
    Log("$gradle $gradleArgs $gradleTasks")
    & $gradle --warning-mode all clean jar

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        return
    }

    # Command to install to local maven repository
    # What worked for me is gradle install -Dmaven.repo.local=~/.m2/repository.

    # add to bin
    $groovyFindExe = Join-Path $groovyFindPath 'bin' 'groovyfind.ps1'
    AddToBin($groovyFindExe)

    Set-Location $oldPwd
}

function BuildHsFind
{
    Write-Host
    Hdr('BuildHsFind')
    Log("language: haskell")

    # ensure ghc is installed
    if (-not (Get-Command 'ghc' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install ghc')
        return
    }

    $ghcVersion = ghc --version
    Log("ghc version: $ghcVersion")

    # ensure stack is installed
    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        return
    }

    $stackVersion = stack --version
    Log("stack version: $stackVersion")

    # set the default stack settings, e.g. use system ghc
    $stackDir = Join-Path $HOME '.stack'
    if (-not (Test-Path $stackDir))
    {
        New-Item -ItemType directory -Path $stackDir
    }
    $configYaml = Join-Path $stackDir 'config.yaml'
    if (-not (Test-Path $configYaml))
    {
        New-Item -ItemType file -Path $stackDir -Name "config.yaml" -Value "install-ghc: false`nsystem-ghc: true"
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $hsFindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $hsFindPath

    # build with stack (via make)
    Log('Building hsfind')
    Log('stack setup')
    make setup

    Log('stack build')
    make build

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Log("stack install --local-bin-path $xfindBinPath")
    stack install --local-bin-path $xfindBinPath

    Set-Location $oldPwd
}

function BuildJavaFind
{
    Write-Host
    Hdr('BuildJavaFind')
    Log("language: java")

    # ensure java is installed
    if (-not (Get-Command 'java' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install java')
        return
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
    # ------------------------------------------------------------
    # Gradle 8.10.2
    # ------------------------------------------------------------

    # Build time:    2024-09-23 21:28:39 UTC
    # Revision:      415adb9e06a516c44b391edff552fd42139443f7

    # Kotlin:        1.9.24
    # Groovy:        3.0.22
    # Ant:           Apache Ant(TM) version 1.10.14 compiled on August 16 2023
    # Launcher JVM:  11.0.24 (Homebrew 11.0.24+0)
    # Daemon JVM:    /usr/local/Cellar/openjdk@11/11.0.24/libexec/openjdk.jdk/Contents/Home (no JDK specified, using current Java home)
    # OS:            Mac OS X 14.6.1 x86_64

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle ',''}
    Log("$gradle version: $gradleVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    $oldPwd = Get-Location
    Set-Location $javaFindPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $javaFindPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $javaFindPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    # run a gradle build
    Log('Building javafind')

    # Log('gradle --warning-mode all clean jar publishToMavenLocal')
    # gradle --warning-mode all clean jar publishToMavenLocal
    $gradleArgs = '--warning-mode all'
    $gradleTasks = 'clean jar'
    Log("$gradle $gradleArgs $gradleTasks")
    & $gradle --warning-mode all clean jar

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        return
    }

    # add to bin
    $javaFindExe = Join-Path $javaFindPath 'bin' 'javafind.ps1'
    AddToBin($javaFindExe)

    Set-Location $oldPwd
}

function BuildJsFind
{
    Write-Host
    Hdr('BuildJsFind')
    Log("language: javascript")

    # ensure node is installed
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js')
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $jsFindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $jsFindPath

    # run npm install and build
    Log('Building jsfind')
    Log('npm install')
    npm install

    Log('npm run build')
    npm run build

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $jsFindExe = Join-Path $jsFindPath 'bin' 'jsfind.ps1'
    AddToBin($jsFindExe)

    Set-Location $oldPwd
}

function BuildKtFind
{
    Write-Host
    Hdr('BuildKtFind')
    Log("language: kotlin")

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
        return
    }

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle\s+',''}
    Log("$gradle version: $gradleVersion")

    $kotlinVersion = $gradleOutput | Where-Object {$_.Contains('Kotlin')} | ForEach-Object {$_ -replace 'Kotlin:\s+',''}
    Log("Kotlin version: $kotlinVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $ktFindPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $ktFindPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    # run a gradle build
    Log('Building ktfind')
    # Log('gradle --warning-mode all clean jar publishToMavenLocal')
    # gradle --warning-mode all clean jar publishToMavenLocal
    $gradleArgs = '--warning-mode all'
    $gradleTasks = 'clean jar'
    Log("$gradle $gradleArgs $gradleTasks")
    & $gradle --warning-mode all clean jar

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $ktFindExe = Join-Path $ktFindPath 'bin' 'ktfind.ps1'
    AddToBin($ktFindExe)

    Set-Location $oldPwd
}

function BuildObjcFind
{
    Write-Host
    Hdr('BuildObjcFind')
    Log("language: objc")

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Apple Swift'
    $swiftVersion = @($swiftVersion -split '\s+')[3]
    Log("swift version: Apple Swift version $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $objcFindPath

    if ($debug)
    {
        Log("swift build")
        swift build

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            Set-Location $oldPwd
            return
        }
    }
    if ($release)
    {
        Log("swift build --configuration release")
        swift build --configuration release

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            Set-Location $oldPwd
            return
        }

        # add release to bin
        $objcFindExe = Join-Path $objcFindPath 'bin' 'objcfind.release.ps1'
        AddToBin($objcFindExe)
    }
    else
    {
        # add debug to bin
        $objcFindExe = Join-Path $objcFindPath 'bin' 'objcfind.debug.ps1'
        AddToBin($objcFindExe)
    }

    Set-Location $oldPwd
}

function BuildMlFind
{
    Write-Host
    Hdr('BuildMlFind')
    Log("language: ocaml")

    Log("Not currently implemented")
}

function BuildPlFind
{
    Write-Host
    Hdr('BuildPlFind')
    Log("language: perl")

    # ensure perl is installed
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

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $plFindPath 'share'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        return
    }

    # add to bin
    $plFindExe = Join-Path $plFindPath 'bin' 'plfind.ps1'
    AddToBin($plFindExe)
}

function BuildPhpFind
{
    Write-Host
    Hdr('BuildPhpFind')
    Log("language: php")

    # ensure php is installed
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

    # ensure composer is installed
    if (-not (Get-Command 'composer' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install composer')
        return
    }

    $composerVersion = composer --version 2>&1 | Select-String -Pattern '^Composer'
    Log("composer version: $composerVersion")

    # copy the shared config json file to the local config location
    $configFilePath = Join-Path $xfindSharedPath 'config.json'
    $configPath = Join-Path $phpFindPath 'config'
    if (-not (Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $phpFindPath 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $phpFindPath

    # run a composer build
    Log('Building phpfind')

    if (Test-Path (Join-Path $phpFindPath 'vendor'))
    {
        Log('composer update')
        composer update
    }
    else
    {
        Log('composer install')
        composer install
    }

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $phpFindExe = Join-Path $phpFindPath 'bin' 'phpfind.ps1'
    AddToBin($phpFindExe)

    Set-Location $oldPwd
}

function BuildPs1Find
{
    Write-Host
    Hdr('BuildPs1Find')
    Log("language: powershell")

    # We don't need to check for powershell, as we're running in it

    $powershellVersion = pwsh -v
    Log("powershell version: $powershellVersion")

    $oldPwd = Get-Location
    Set-Location $ps1FindPath

    Log('Building ps1find')

    # copy the file to the first of the module paths, if defined
    $modulePaths = @($env:PSModulePath -split ':')
    if ($modulePaths.Count -gt 0) {
        $ps1findTargetModulePath = Join-Path $modulePaths[0] 'Ps1FindModule'
        if (-not (Test-Path $ps1findTargetModulePath)) {
            Log("New-Item -Path $ps1findTargetModulePath -ItemType Directory")
            New-Item -Path $ps1findTargetModulePath -ItemType Directory
        }
        $ps1findModulePath = Join-Path $ps1FindPath 'Ps1FindModule.psm1'
        Log("Copy-Item $ps1findModulePath -Destination $ps1findTargetModulePath")
        Copy-Item $ps1findModulePath -Destination $ps1findTargetModulePath
    }

    # add to bin
    $ps1FindExe = Join-Path $ps1FindPath 'ps1find.ps1'
    AddToBin($ps1FindExe)

    Set-Location $oldPwd
}

function BuildPyFind
{
    Write-Host
    Hdr('BuildPyFind')
    Log("language: python")

    $oldPwd = Get-Location
    Set-Location $pyFindPath

    # Set to $true to use venv
    $useVenv=$venv
    # $pythonVersions = @('python3.12', 'python3.11', 'python3.10', 'python3.9')
    # We don't want to use python3.12 yet
    $pythonVersions = @('python3.11', 'python3.10', 'python3.9')
    $python = ''
    $venvPath = Join-Path $pyFindPath 'venv'

    if ($useVenv)
    {
        Log('Using venv')

        if (Test-Path $venvPath)
        {
            Log('Using existing venv')

            # activate the virtual env
            $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
            Log("$activatePath")
            & $activatePath

            ForEach ($p in $pythonVersions)
            {
                $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
                if ($null -ne $pythonCmd)
                {
                    $python = $p
                    break
                }
            }
        }
        else
        {
            # ensure python3.9+ is installed
            ForEach ($p in $pythonVersions)
            {
                $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
                if ($null -ne $pythonCmd)
                {
                    $python = $p
                    break
                }
            }

            if (-not $python)
            {
                PrintError('You need to install python(>= 3.9)')
                return
            }

            Log('Creating new venv')

            # create a virtual env to run from and install to
            Log("$python -m venv venv")
            & $python -m venv venv

            # activate the virtual env
            $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
            Log("$activatePath")
            & $activatePath
        }
    }
    else
    {
        Log('Not using venv')

        # ensure python3.9+ is installed
        ForEach ($p in $pythonVersions)
        {
            $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
            if ($null -ne $pythonCmd)
            {
                $python = $p
                break
            }
        }

        if (-not $python)
        {
            PrintError('You need to install python(>= 3.9)')
            return
        }
    }

    $pythonExePath = Get-Command $python | Select-Object -ExpandProperty Source
    Log("Using $python ($pythonExePath)")
    $pythonVersion = & $python -V | Select-String -Pattern '^Python'
    Log("Version: $pythonVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $pyFindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # install dependencies in requirements.txt
    Log('pip3 install -r requirements.txt')
    pip3 install -r requirements.txt

    # check for success/failure
    $buildError = $false
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $buildError = $true
    }

    if ($useVenv)
    {
        # deactivate at end of setup process
        Log('deactivate')
        deactivate
    }

    if ($buildError)
    {
        Set-Location $oldPwd
        return
    }

    # add to bin
    $pyFindExe = Join-Path $pyFindPath 'bin' 'pyfind.ps1'
    AddToBin($pyFindExe)

    Set-Location $oldPwd
}

function BuildRbFind
{
    Write-Host
    Hdr('BuildRbFind')
    Log("language: ruby")

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

    # copy the shared config json file to the local config location
    $configFilePath = Join-Path $xfindSharedPath 'config.json'
    $configPath = Join-Path $rbFindPath 'data'
    if (-not (Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $rbFindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $rbFindPath

    Log('Building rbfind')
    Log('bundle install')
    bundle install

    # add to bin
    $rbFindExe = Join-Path $rbFindPath 'bin' 'rbfind.ps1'
    AddToBin($rbFindExe)

    Set-Location $oldPwd
}

function BuildRsFind
{
    Write-Host
    Hdr('BuildRsFind')
    Log("language: rust")

    # ensure rust is installed
    if (-not (Get-Command 'rustc' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install rust')
        return
    }

    $rustVersion = rustc --version | Select-String -Pattern 'rustc'
    Log("rustc version: $rustVersion")

    # ensure cargo is installed
    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo')
        return
    }

    $cargoVersion = cargo --version | Select-String -Pattern 'cargo'
    Log("cargo version: $cargoVersion")

    $oldPwd = Get-Location
    Set-Location $rsFindPath

    Log('Building rsfind')

    if ($debug)
    {
        Log('cargo build')
        cargo build

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            Set-Location $oldPwd
            return
        }
    }

    if ($release)
    {
        Log('cargo build --release')
        cargo build --release

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            Set-Location $oldPwd
            return
        }

        # add release to bin
        $rsFindExe = Join-Path $rsFindPath 'bin' 'rsfind.release.ps1'
        AddToBin($rsFindExe)
    }
    else
    {
        # add debug to bin
        $rsFindExe = Join-Path $rsFindPath 'bin' 'rsfind.debug.ps1'
        AddToBin($rsFindExe)
    }

    Set-Location $oldPwd
}

function BuildScalaFind
{
    Write-Host
    Hdr('BuildScalaFind')
    Log("language: scala")

    # ensure scalac is installed
    if (-not (Get-Command 'scala' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install scala')
        return
    }

    # scala --version output looks like this:
    # Scala code runner version: 1.4.3
    # Scala version (default): 3.5.2
    $scalaVersion = scala --version 2>&1 | Select-Object -Last 1
    $scalaVersion = @($scalaVersion -split '\s+')[3]
    Log("scala version: $scalaVersion")

    $oldPwd = Get-Location
    Set-Location $scalaFindPath

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

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $scalaFindPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $scalaFindPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    # run sbt assembly
    Log('Building scalafind')
    Log("sbt 'set test in assembly := {}' clean assembly")
    sbt 'set test in assembly := {}' clean assembly

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $scalaFindExe = Join-Path $scalaFindPath 'bin' 'scalafind.ps1'
    AddToBin($scalaFindExe)

    Set-Location $oldPwd
}

function BuildSwiftFind
{
    Write-Host
    Hdr('BuildSwiftFind')
    Log("language: swift")

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Apple Swift'
    $swiftVersion = @($swiftVersion -split '\s+')[3]
    Log("swift version: Apple Swift version $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $swiftFindPath

    Log('Building swiftfind')

    if ($debug)
    {
        Log('swift build')
        swift build

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            Set-Location $oldPwd
            return
        }
    }

    if ($release)
    {
        Log('swift build --configuration release')
        swift build --configuration release

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            Set-Location $oldPwd
            return
        }

        # add release to bin
        $swiftFindExe = Join-Path $swiftFindPath 'bin' 'swiftfind.release.ps1'
        AddToBin($swiftFindExe)
        }
    else
    {
        # add debug to bin
        $swiftFindExe = Join-Path $swiftFindPath 'bin' 'swiftfind.debug.ps1'
        AddToBin($swiftFindExe)
        }

    Set-Location $oldPwd
}

function BuildTsFind
{
    Write-Host
    Hdr('BuildTsFind')
    Log("language: typescript")

    # ensure node is installed
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js')
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $tsFindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $tsFindPath

    # run npm install and build
    Log('Building tsfind')
    Log('npm install')
    npm install

    Log('npm run build')
    npm run build

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $tsFindExe = Join-Path $tsFindPath 'bin' 'tsfind.ps1'
    AddToBin($tsFindExe)

    Set-Location $oldPwd
}

function BuildLinux
{
    Write-Host
    Hdr('BuildLinux')

    Measure-Command { BuildBashFind }

    Measure-Command { BuildCFind }

    # Measure-Command { BuildCljFind }

    # Measure-Command { BuildCppFind }

    Measure-Command { BuildCsFind }

    Measure-Command { BuildDartFind }

    Measure-Command { BuildFsFind }

    Measure-Command { BuildGoFind }

    Measure-Command { BuildJavaFind }

    Measure-Command { BuildJsFind }

    # Measure-Command { BuildKtFind }

    Measure-Command { BuildPlFind }

    Measure-Command { BuildPhpFind }

    # Measure-Command { BuildPs1Find }

    Measure-Command { BuildPyFind }

    Measure-Command { BuildRbFind }

    Measure-Command { BuildRsFind }

    # Measure-Command { BuildScalaFind }

    Measure-Command { BuildSwiftFind }

    Measure-Command { BuildTsFind }

    exit
}

function BuildAll
{
    Write-Host
    Hdr('BuildAll')

    Measure-Command { BuildBashFind }

    Measure-Command { BuildCFind }

    Measure-Command { BuildCljFind }

    Measure-Command { BuildCppFind }

    Measure-Command { BuildCsFind }

    Measure-Command { BuildDartFind }

    Measure-Command { BuildExFind }

    Measure-Command { BuildFsFind }

    Measure-Command { BuildGoFind }

    Measure-Command { BuildGroovyFind }

    Measure-Command { BuildHsFind }

    Measure-Command { BuildJavaFind }

    Measure-Command { BuildJsFind }

    Measure-Command { BuildKtFind }

    Measure-Command { BuildObjcFind }

    # Measure-Command { BuildMlFind }

    Measure-Command { BuildPlFind }

    Measure-Command { BuildPhpFind }

    Measure-Command { BuildPs1Find }

    Measure-Command { BuildPyFind }

    Measure-Command { BuildRbFind }

    Measure-Command { BuildRsFind }

    Measure-Command { BuildScalaFind }

    Measure-Command { BuildSwiftFind }

    Measure-Command { BuildTsFind }

    exit
}

################################################################################
# Main function
################################################################################

function BuildMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        BuildAll
        exit
    }

    ForEach ($lang in $langs)
    {
        switch ($lang.ToLower())
        {
            'linux'      { BuildLinux }
            'bash'       { Measure-Command { BuildBashFind } }
            'c'          { Measure-Command { BuildCFind } }
            'clj'        { Measure-Command { BuildCljFind } }
            'clojure'    { Measure-Command { BuildCljFind } }
            'cpp'        { Measure-Command { BuildCppFind } }
            'cs'         { Measure-Command { BuildCsFind } }
            'csharp'     { Measure-Command { BuildCsFind } }
            'dart'       { Measure-Command { BuildDartFind } }
            'elixir'     { Measure-Command { BuildExFind } }
            'ex'         { Measure-Command { BuildExFind } }
            'fs'         { Measure-Command { BuildFsFind } }
            'fsharp'     { Measure-Command { BuildFsFind } }
            'go'         { Measure-Command { BuildGoFind } }
            'groovy'     { Measure-Command { BuildGroovyFind } }
            'haskell'    { Measure-Command { BuildHsFind } }
            'hs'         { Measure-Command { BuildHsFind } }
            'java'       { Measure-Command { BuildJavaFind } }
            'javascript' { Measure-Command { BuildJsFind } }
            'js'         { Measure-Command { BuildJsFind } }
            'kotlin'     { Measure-Command { BuildKtFind } }
            'kt'         { Measure-Command { BuildKtFind } }
            'objc'       { Measure-Command { BuildObjcFind } }
            'ocaml'      { Measure-Command { BuildMlFind } }
            'ml'         { Measure-Command { BuildMlFind } }
            'perl'       { Measure-Command { BuildPlFind } }
            'pl'         { Measure-Command { BuildPlFind } }
            'php'        { Measure-Command { BuildPhpFind } }
            'powershell' { Measure-Command { BuildPs1Find } }
            'ps1'        { Measure-Command { BuildPs1Find } }
            'pwsh'       { Measure-Command { BuildPs1Find } }
            'py'         { Measure-Command { BuildPyFind } }
            'python'     { Measure-Command { BuildPyFind } }
            'rb'         { Measure-Command { BuildRbFind } }
            'ruby'       { Measure-Command { BuildRbFind } }
            'rs'         { Measure-Command { BuildRsFind } }
            'rust'       { Measure-Command { BuildRsFind } }
            'scala'      { Measure-Command { BuildScalaFind } }
            'swift'      { Measure-Command { BuildSwiftFind } }
            'ts'         { Measure-Command { BuildTsFind } }
            'typescript' { Measure-Command { BuildTsFind } }
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
        BuildAll
    }

    BuildMain $langs
}
catch {
    PrintError($_.Exception.Message)
}
finally {
    Set-Location $oldPwd
}
