#!/usr/bin/env pwsh
################################################################################
#
# build.ps1
#
# Builds specified language version of xfind, or all versions
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

Log("help: $help")
Log("debug: $debug")
Log("release: $release")
Log("venv: $venv")
Log("all: $all")
Log("langs: $langs")


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
    CopyFileTypesJsonResources($resourcesPath)
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

function CopyXmlResources
{
    param([string]$resourcesPath)
    $fileTypesPath = Join-Path $xfindSharedPath 'filetypes.xml'
    Log("Copy-Item $fileTypesPath -Destination $resourcesPath")
    Copy-Item $fileTypesPath -Destination $resourcesPath
    $findOptionsPath = Join-Path $xfindSharedPath 'findoptions.xml'
    Log("Copy-Item $findOptionsPath -Destination $resourcesPath")
    Copy-Item $findOptionsPath -Destination $resourcesPath
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

function BuildC
{
    Write-Host
    Hdr('BuildC')

    if ($IsWindows)
    {
        Log('BuildC - currently unimplemented for Windows')
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
    Set-Location $cfindPath

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
        $cmakeBuildPath = Join-Path $cfindPath $cmakeBuildDir

        if (-not (Test-Path $cmakeBuildPath))
        {
            New-Item -ItemType directory -Path $cmakeBuildPath

            Set-Location $cmakeBuildPath

            Log("cmake -G ""Unix Makefiles"" -DCMAKE_BUILD_TYPE=$c ..")
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            # Log("make -f Makefile")
            # make -f Makefile

            Set-Location $cfindPath
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
        $cfindExe = Join-Path $cfindPath 'bin' 'cfind.release.ps1'
        AddToBin($cfindExe)
    }
    else
    {
        # add debug to bin
        $cfindExe = Join-Path $cfindPath 'bin' 'cfind.debug.ps1'
        AddToBin($cfindExe)
    }

    Set-Location $oldPwd
}

function BuildClojure
{
    Write-Host
    Hdr('BuildClojure')

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
    $resourcesPath = Join-Path $cljfindPath 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $cljfindPath

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
    $cljfindExe = Join-Path $cljfindPath 'bin' 'cljfind.ps1'
    AddToBin($cljfindExe)

    Set-Location $oldPwd
}

function BuildCpp
{
    Write-Host
    Hdr('BuildCpp')

    if ($IsWindows)
    {
        Log('BuildCpp - currently unimplemented for Windows')
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
    Set-Location $cppfindPath

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
        $cmakeBuildPath = Join-Path $cppfindPath $cmakeBuildDir

        if (-not (Test-Path $cmakeBuildPath))
        {
            New-Item -ItemType directory -Path $cmakeBuildPath

            Set-Location $cmakeBuildPath

            Log("cmake -G ""Unix Makefiles"" -DCMAKE_BUILD_TYPE=$c ..")
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            # Log("make -f Makefile")
            # make -f Makefile

            Set-Location $cppfindPath
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
        $cppfindExe = Join-Path $cppfindPath 'bin' 'cppfind.release.ps1'
        AddToBin($cppfindExe)
    }
    else
    {
        # add debug to bin
        $cppfindExe = Join-Path $cppfindPath 'bin' 'cppfind.debug.ps1'
        AddToBin($cppfindExe)
    }

    Set-Location $oldPwd
}

function BuildCsharp
{
    Write-Host
    Hdr('BuildCsharp')

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $resourcesPath = Join-Path $csfindPath 'CsFindLib' 'Resources'
    $testResourcesPath = Join-Path $csfindPath 'CsFindTests' 'Resources'

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
    Set-Location $csfindPath

    $csFindSolutionPath = Join-Path $csfindPath 'CsFind.sln'

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
        $csfindExe = Join-Path $csfindPath 'bin' 'csfind.release.ps1'
        AddToBin($csfindExe)
    }
    else
    {
        # add debug to bin
        $csfindExe = Join-Path $csfindPath 'bin' 'csfind.debug.ps1'
        AddToBin($csfindExe)
    }

    Set-Location $oldPwd
}

function BuildDart
{
    Write-Host
    Hdr('BuildDart')

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

    Log('Building dartfind')
    if ((-not (Test-Path (Join-Path $dartfindPath '.dart_tool' 'package_config.json'))) -and
        (-not (Test-Path (Join-Path $dartfindPath '.packages'))))
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
    $dartScript = Join-Path $dartfindPath 'bin' 'dartfind.dart'
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
    $dartfindExe = Join-Path $dartfindPath 'bin' 'dartfind.ps1'
    AddToBin($dartfindExe)

    Set-Location $oldPwd
}

function BuildElixir
{
    Write-Host
    Hdr('BuildElixir')

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
    Set-Location $exfindPath

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
    $exfindExe = Join-Path $exfindPath 'bin' 'exfind'
    AddToBin($exfindExe)

    Set-Location $oldPwd
}

function BuildFsharp
{
    Write-Host
    Hdr('BuildFsharp')

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $resourcesPath = Join-Path $fsfindPath 'FsFindLib' 'Resources'
    $testResourcesPath = Join-Path $fsfindPath 'FsFindTests' 'Resources'

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
    Set-Location $fsfindPath

    $fsFindSolutonPath = Join-Path $fsfindPath 'FsFind.sln'

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
        $fsfindExe = Join-Path $fsfindPath 'bin' 'fsfind.release.ps1'
        AddToBin($fsfindExe)
    }
    else
    {
        # add debug to bin
        $fsfindExe = Join-Path $fsfindPath 'bin' 'fsfind.debug.ps1'
        AddToBin($fsfindExe)
    }

    Set-Location $oldPwd
}

function BuildGo
{
    Write-Host
    Hdr('BuildGo')

    # ensure go is installed
    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        return
    }

    $goVersion = (go version) -replace 'go version ', ''
    Log("go version: $goVersion")

    $oldPwd = Get-Location
    Set-Location $gofindPath

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
        $gofindExe = Join-Path $env:GOBIN 'gofind'
        AddToBin($gofindExe)
    }

    Set-Location $oldPwd
}

function BuildGroovy
{
    Write-Host
    Hdr('BuildGroovy')

    # ensure groovy is installed
    if (-not (Get-Command 'groovy' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install groovy')
        return
    }

    $groovyVersion = groovy --version
    Log("groovy version: $groovyVersion")

    $oldPwd = Get-Location
    Set-Location $groovyfindPath

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

    $gradleVersion = & $gradle --version | Select-String -Pattern 'Gradle'
    Log("$gradle version: $gradleVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $groovyfindPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $groovyfindPath 'src' 'test' 'resources'
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

    # add to bin
    $groovyfindExe = Join-Path $groovyfindPath 'bin' 'groovyfind.ps1'
    AddToBin($groovyfindExe)

    Set-Location $oldPwd
}

function BuildHaskell
{
    Write-Host
    Hdr('BuildHaskell')

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
    $resourcesPath = Join-Path $hsfindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $hsfindPath

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

function BuildJava
{
    Write-Host
    Hdr('BuildJava')

    # ensure java is installed
    if (-not (Get-Command 'java' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install java')
        return
    }

    $javaVersion = java -version 2>&1 | Select-String -Pattern 'java version'
    Log("java version: $javaVersion")

    # ensure mvn is installed
    if (-not (Get-Command 'mvn' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install maven')
        return
    }

    $mvnVersion = mvn --version | Select-String -Pattern 'Apache Maven'
    Log("mvn version: $mvnVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $javafindPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $javafindPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    # run maven clean package (skip testing as this is run via unittest.sh)
    Log('Building javafind')
    Log("mvn -f $javafindPath/pom.xml clean package -Dmaven.test.skip=true -Dmaven.plugin.validation=DEFAULT")
    mvn -f $javafindPath/pom.xml clean package '-Dmaven.test.skip=true' '-Dmaven.plugin.validation=DEFAULT'

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
    $javafindExe = Join-Path $javafindPath 'bin' 'javafind.ps1'
    AddToBin($javafindExe)
}

function BuildJavaScript
{
    Write-Host
    Hdr('BuildJavaScript')

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
    $resourcesPath = Join-Path $jsfindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $jsfindPath

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
    $jsfindExe = Join-Path $jsfindPath 'bin' 'jsfind.ps1'
    AddToBin($jsfindExe)

    Set-Location $oldPwd
}

function BuildKotlin
{
    Write-Host
    Hdr('BuildKotlin')

    $oldPwd = Get-Location
    Set-Location $ktfindPath

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

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $ktfindPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $ktfindPath 'src' 'test' 'resources'
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
    $ktfindExe = Join-Path $ktfindPath 'bin' 'ktfind.ps1'
    AddToBin($ktfindExe)

    Set-Location $oldPwd
}

function BuildObjc
{
    Write-Host
    Hdr('BuildObjc')

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
        $objcfindExe = Join-Path $objcfindPath 'bin' 'objcfind.release.ps1'
        AddToBin($objcfindExe)
    }
    else
    {
        # add debug to bin
        $objcfindExe = Join-Path $objcfindPath 'bin' 'objcfind.debug.ps1'
        AddToBin($objcfindExe)
    }

    Set-Location $oldPwd
}

function BuildOcaml
{
    Write-Host
    Hdr('BuildOcaml - currently unimplemented')
}

function BuildPerl
{
    Write-Host
    Hdr('BuildPerl')

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
    $resourcesPath = Join-Path $plfindPath 'share'
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
    $plfindExe = Join-Path $plfindPath 'bin' 'plfind.ps1'
    AddToBin($plfindExe)
}

function BuildPhp
{
    Write-Host
    Hdr('BuildPhp')

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
    $configPath = Join-Path $phpfindPath 'config'
    if (-not (Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $phpfindPath 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $phpfindPath

    # run a composer build
    Log('Building phpfind')

    if (Test-Path (Join-Path $phpfindPath 'vendor'))
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
    $phpfindExe = Join-Path $phpfindPath 'bin' 'phpfind.ps1'
    AddToBin($phpfindExe)

    Set-Location $oldPwd
}

function BuildPowerShell
{
    Write-Host
    Hdr('BuildPowerShell')

    # We don't need to check for powershell, as we're running in it

    $powershellVersion = pwsh -v
    Log("powershell version: $powershellVersion")

    $oldPwd = Get-Location
    Set-Location $ps1findPath

    Log('Building ps1find')

    # copy the file to the first of the module paths, if defined
    $modulePaths = @($env:PSModulePath -split ':')
    if ($modulePaths.Count -gt 0) {
        $ps1findTargetModulePath = Join-Path $modulePaths[0] 'Ps1FindModule'
        if (-not (Test-Path $ps1findTargetModulePath)) {
            Log("New-Item -Path $ps1findTargetModulePath -ItemType Directory")
            New-Item -Path $ps1findTargetModulePath -ItemType Directory
        }
        $ps1findModulePath = Join-Path $ps1findPath 'Ps1FindModule.psm1'
        Log("Copy-Item $ps1findModulePath -Destination $ps1findTargetModulePath")
        Copy-Item $ps1findModulePath -Destination $ps1findTargetModulePath
    }

    # add to bin
    $ps1findExe = Join-Path $ps1findPath 'ps1find.ps1'
    AddToBin($ps1findExe)

    Set-Location $oldPwd
}

function BuildPython
{
    Write-Host
    Hdr('BuildPython')

    $oldPwd = Get-Location
    Set-Location $pyfindPath

    # Set to $true to use venv
    $useVenv=$venv
    # $pythonVersions = @('python3.12', 'python3.11', 'python3.10', 'python3.9')
    # We don't want to use python3.12 yet
    $pythonVersions = @('python3.11', 'python3.10', 'python3.9')
    $python = ''
    $venvPath = Join-Path $pyfindPath 'venv'

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
    $resourcesPath = Join-Path $pyfindPath 'data'
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
    $pyfindExe = Join-Path $pyfindPath 'bin' 'pyfind.ps1'
    AddToBin($pyfindExe)

    Set-Location $oldPwd
}

function BuildRuby
{
    Write-Host
    Hdr('BuildRuby')

    # ensure ruby2.x is installed
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
    $configPath = Join-Path $rbfindPath 'data'
    if (-not (Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $rbfindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $rsfindPath

    Log('Building rbfind')
    Log('bundle install')
    bundle install

    # add to bin
    $rbfindExe = Join-Path $rbfindPath 'bin' 'rbfind.ps1'
    AddToBin($rbfindExe)

    Set-Location $oldPwd
}

function BuildRust
{
    Write-Host
    Hdr('BuildRust')

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
    Set-Location $rsfindPath

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
        $rsfindExe = Join-Path $rsfindPath 'bin' 'rsfind.release.ps1'
        AddToBin($rsfindExe)
    }
    else
    {
        # add debug to bin
        $rsfindExe = Join-Path $rsfindPath 'bin' 'rsfind.debug.ps1'
        AddToBin($rsfindExe)
    }

    Set-Location $oldPwd
}

function BuildScala
{
    Write-Host
    Hdr('BuildScala')

    # ensure scalac is installed
    if (-not (Get-Command 'scalac' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install scala')
        return
    }

    $scalaVersion = scala --version 2>&1 | Select-String -Pattern 'Scala'
    Log("scala version: $scalaVersion")

    $oldPwd = Get-Location
    Set-Location $scalafindPath

    # ensure sbt is installed
    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install sbt')
        return
    }

    $sbtVersion = sbt --version | Select-String -Pattern 'project'
    Log("sbt version: $sbtVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $scalafindPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $scalafindPath 'src' 'test' 'resources'
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
    $scalafindExe = Join-Path $scalafindPath 'bin' 'scalafind.ps1'
    AddToBin($scalafindExe)

    Set-Location $oldPwd
}

function BuildSwift
{
    Write-Host
    Hdr('BuildSwift')

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $swiftfindPath

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
        $swiftfindExe = Join-Path $swiftfindPath 'bin' 'swiftfind.release.ps1'
        AddToBin($swiftfindExe)
        }
    else
    {
        # add debug to bin
        $swiftfindExe = Join-Path $swiftfindPath 'bin' 'swiftfind.debug.ps1'
        AddToBin($swiftfindExe)
        }

    Set-Location $oldPwd
}

function BuildTypeScript
{
    Write-Host
    Hdr('BuildTypeScript')

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
    $resourcesPath = Join-Path $tsfindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $tsfindPath

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
    $tsfindExe = Join-Path $tsfindPath 'bin' 'tsfind.ps1'
    AddToBin($tsfindExe)

    Set-Location $oldPwd
}

function BuildLinux
{
    Write-Host
    Hdr('BuildLinux')

    Measure-Command { BuildC }

    # Measure-Command { BuildClojure }

    # Measure-Command { BuildCpp }

    Measure-Command { BuildCsharp }

    Measure-Command { BuildDart }

    Measure-Command { BuildFsharp }

    Measure-Command { BuildGo }

    Measure-Command { BuildJava }

    Measure-Command { BuildJavaScript }

    # Measure-Command { BuildKotlin }

    Measure-Command { BuildPerl }

    Measure-Command { BuildPhp }

    # Measure-Command { BuildPowerShell }

    Measure-Command { BuildPython }

    Measure-Command { BuildRuby }

    Measure-Command { BuildRust }

    # Measure-Command { BuildScala }

    Measure-Command { BuildSwift }

    Measure-Command { BuildTypeScript }
}

function BuildAll
{
    Write-Host
    Hdr('BuildAll')

    Measure-Command { BuildC }

    Measure-Command { BuildClojure }

    Measure-Command { BuildCpp }

    Measure-Command { BuildCsharp }

    Measure-Command { BuildDart }

    Measure-Command { BuildElixir }

    Measure-Command { BuildFsharp }

    Measure-Command { BuildGo }

    Measure-Command { BuildGroovy }

    Measure-Command { BuildHaskell }

    Measure-Command { BuildJava }

    Measure-Command { BuildJavaScript }

    Measure-Command { BuildKotlin }

    Measure-Command { BuildObjc }

    # Measure-Command { BuildOcaml }

    Measure-Command { BuildPerl }

    Measure-Command { BuildPhp }

    Measure-Command { BuildPowerShell }

    Measure-Command { BuildPython }

    Measure-Command { BuildRuby }

    Measure-Command { BuildRust }

    Measure-Command { BuildScala }

    Measure-Command { BuildSwift }

    Measure-Command { BuildTypeScript }
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
        switch ($lang)
        {
            'linux'      { BuildLinux }
            'c'          { Measure-Command { BuildC } }
            'clj'        { Measure-Command { BuildClojure } }
            'clojure'    { Measure-Command { BuildClojure } }
            'cpp'        { Measure-Command { BuildCpp } }
            'cs'         { Measure-Command { BuildCsharp } }
            'csharp'     { Measure-Command { BuildCsharp } }
            'dart'       { Measure-Command { BuildDart } }
            'elixir'     { Measure-Command { BuildElixir } }
            'ex'         { Measure-Command { BuildElixir } }
            'fs'         { Measure-Command { BuildFsharp } }
            'fsharp'     { Measure-Command { BuildFsharp } }
            'go'         { Measure-Command { BuildGo } }
            'groovy'     { Measure-Command { BuildGroovy } }
            'haskell'    { Measure-Command { BuildHaskell } }
            'hs'         { Measure-Command { BuildHaskell } }
            'java'       { Measure-Command { BuildJava } }
            'javascript' { Measure-Command { BuildJavaScript } }
            'js'         { Measure-Command { BuildJavaScript } }
            'kotlin'     { Measure-Command { BuildKotlin } }
            'kt'         { Measure-Command { BuildKotlin } }
            'objc'       { Measure-Command { BuildObjc } }
            'ocaml'      { Measure-Command { BuildOcaml } }
            'ml'         { Measure-Command { BuildOcaml } }
            'perl'       { Measure-Command { BuildPerl } }
            'pl'         { Measure-Command { BuildPerl } }
            'php'        { Measure-Command { BuildPhp } }
            'powershell' { Measure-Command { BuildPowerShell } }
            'ps1'        { Measure-Command { BuildPowerShell } }
            'py'         { Measure-Command { BuildPython } }
            'python'     { Measure-Command { BuildPython } }
            'rb'         { Measure-Command { BuildRuby } }
            'ruby'       { Measure-Command { BuildRuby } }
            'rs'         { Measure-Command { BuildRust } }
            'rust'       { Measure-Command { BuildRust } }
            'scala'      { Measure-Command { BuildScala } }
            'swift'      { Measure-Command { BuildSwift } }
            'ts'         { Measure-Command { BuildTypeScript } }
            'typescript' { Measure-Command { BuildTypeScript } }
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
    BuildAll
    exit
}

BuildMain $langs
