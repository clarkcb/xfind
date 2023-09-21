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
      [string]$lang = '')

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path -Path $scriptDir -ChildPath 'config.ps1')
. (Join-Path -Path $scriptDir -ChildPath 'common.ps1')

# check for help switch
$help = $help.IsPresent

# for languages that have debug and release builds
$debug = $debug.IsPresent
$release = $release.IsPresent
if (-not $release)
{
    $debug = $true
}

# for python
$venv = $venv.IsPresent


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: build.ps1 [-help] [-debug] [-release] [-venv] {""all"" | langcode}`n"
    exit
}

function CopyJsonResources
{
    param([string]$resourcesPath)
    $fileTypesPath = Join-Path $sharedPath 'filetypes.json'
    Log("Copy-Item $fileTypesPath -Destination $resourcesPath")
    Copy-Item $fileTypesPath -Destination $resourcesPath
    $findOptionsPath = Join-Path $sharedPath 'findoptions.json'
    Log("Copy-Item $findOptionsPath -Destination $resourcesPath")
    Copy-Item $findOptionsPath -Destination $resourcesPath
}

function CopyXmlResources
{
    param([string]$resourcesPath)
    $fileTypesPath = Join-Path $sharedPath 'filetypes.xml'
    Log("Copy-Item $fileTypesPath -Destination $resourcesPath")
    Copy-Item $fileTypesPath -Destination $resourcesPath
    $findOptionsPath = Join-Path $sharedPath 'findoptions.xml'
    Log("Copy-Item $findOptionsPath -Destination $resourcesPath")
    Copy-Item $findOptionsPath -Destination $resourcesPath
}

function CopyTestResources
{
    param([string]$testResourcesPath)
    Log("Copy-Item $testFilePath -Include testFile*.txt -Destination $testResourcesPath")
    Copy-Item $testFilePath -Include testFile*.txt -Destination $testResourcesPath
}

function AddSoftLink
{
    param([string]$linkPath, [string]$targetPath, [bool]$replaceLink=$true)
    Write-Host "linkPath: $linkPath"
    Write-Host "targetPath: $targetPath"

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
    param([string]$scriptPath)

    if (-not (Test-Path $binPath))
    {
        New-Item -ItemType directory -Path $binPath
    }

    # get the base filename, minus path and any extension
    $baseName = [io.path]::GetFileNameWithoutExtension($scriptPath)
    if ($baseName.EndsWith(".debug") -or $baseName.EndsWith(".release"))
    {
        $baseName = $baseName.Split(".")[0]
    }

    $linkPath = Join-Path $binPath $baseName

    AddSoftLink $linkPath $scriptPath
}


################################################################################
# Build functions
################################################################################

function BuildC
{
    Write-Host
    Hdr('BuildC')

    # ensure make is installed
    if (-not (Get-Command 'make' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install make')
        return
    }

    $oldPwd = Get-Location
    Set-Location $cfindPath

    Log('Building cfind')
    Log('make')
    make

    # add to bin
    $cfindExe = Join-Path $cfindPath 'cfind'
    AddToBin($cfindExe)

    Set-Location $oldPwd
}

function BuildClojure
{
    Write-Host
    Hdr('BuildClojure')

    # ensure leiningen is installed
    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        return
    }

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
        Log('Skipping for unknown OS')
        return
    }

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cmake')
        return
    }

    $oldPwd = Get-Location
    Set-Location $cppfindPath

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

            Log('cmake -G "Unix Makefiles" ..')
            cmake -G "Unix Makefiles" ..

            Log("make -f Makefile")
            make -f Makefile

            Set-Location $cppfindPath
        }

        $targets = @('clean', 'cppfind', 'cppfind-tests')
        ForEach ($t in $targets)
        {
            Log("cmake --build $cmakeBuildDir --target $t -- -W -Wall -Werror")
            cmake --build $cmakeBuildDir --target $t -- -W -Wall -Werror
            if ($LASTEXITCODE -ne 0)
            {
                Log("An error occurred while trying to run build target $t")
                exit
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

    # add to bin
    $dartfindExe = Join-Path $dartfindPath 'bin' 'dartfind.ps1'
    AddToBin($dartfindExe)

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

    $oldPwd = Get-Location
    Set-Location $gofindPath

    # go fmt the gofind source (for auto-generated code)
    Log('Auto-formatting gofind')
    Log('go fmt ./...')
    go fmt ./...

    # create the bin dir if it doesn't already exist
    if (-not (Test-Path $binPath))
    {
        New-Item -ItemType directory -Path $binPath
    }

    # if GOBIN not defined, set to BIN_PATH
    if (-not (Test-Path Env:GOBIN))
    {
        $env:GOBIN = $binPath
    }

    # now build gofind
    Log('Building gofind')
    Log('go install ./...')
    go install ./...

    if ($env:GOBIN -ne $binPath)
    {
        # add to bin
        $gofindExe = Join-Path $env:GOBIN 'gofind'
        AddToBin($gofindExe)
    }

    Set-Location $oldPwd
}

function BuildHaskell
{
    Write-Host
    Hdr('BuildHaskell')

    # ensure stack is installed
    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        return
    }

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

    Log("stack install --local-bin-path $binPath")
    stack install --local-bin-path $binPath

    Set-Location $oldPwd
}

function BuildJava
{
    Write-Host
    Hdr('BuildJava')

    # ensure mvn is installed
    if (-not (Get-Command 'mvn' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install maven')
        return
    }

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
    Log("mvn -f $javafindPath/pom.xml clean package -Dmaven.test.skip=true")
    mvn -f $javafindPath/pom.xml clean package '-Dmaven.test.skip=true'

    # add to bin
    $javafindExe = Join-Path $javafindPath 'bin' 'javafind.ps1'
    AddToBin($javafindExe)
}

function BuildJavaScript
{
    Write-Host
    Hdr('BuildJavaScript')

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        return
    }

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

    # add to bin
    $jsfindExe = Join-Path $jsfindPath 'bin' 'jsfind.ps1'
    AddToBin($jsfindExe)

    Set-Location $oldPwd
}

function BuildKotlin
{
    Write-Host
    Hdr('BuildKotlin')

    # ensure gradle is installed
    if (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        return
    }

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

    $oldPwd = Get-Location
    Set-Location $ktfindPath

    # run a gradle build
    Log('Building ktfind')
    Log('gradle --warning-mode all clean jar publishToMavenLocal')
    gradle --warning-mode all clean jar publishToMavenLocal

    # add to bin
    $ktfindExe = Join-Path $ktfindPath 'bin' 'ktfind.ps1'
    AddToBin($ktfindExe)

    Set-Location $oldPwd
}

function BuildObjc
{
    Write-Host
    Hdr('BuildObjc')

    $target = 'alltargets'

    # ensure xcode is installed
    if (-not (Get-Command 'xcodebuild' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install Xcode')
        return
    }

    $oldPwd = Get-Location
    Set-Location $objcfindPath

    $configurations = @()
    if ($debug)
    {
        $configurations += 'Debug'
    }
    if ($release)
    {
        $configurations += 'Release'
    }

    # run build for selected configurations
    ForEach ($c in $configurations)
    {
        if ($target -eq 'alltargets')
        {
            Log("xcodebuild -alltargets -configuration $c")
            xcodebuild -alltargets -configuration $c
        }
        else
        {
            Log("xcodebuild -project $target -configuration $c")
            xcodebuild -project $target -configuration $c
        }
    }

    if ($release)
    {
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

    $versionOutput = & perl -v | Select-String -Pattern 'This is perl 5' 2>&1
    if (-not $versionOutput)
    {
        PrintError('A 5.x version of perl is required')
        return
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $plfindPath 'share'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

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

    $versionOutput = & php -v | Select-String -Pattern 'PHP [78]' 2>&1
    if (-not $versionOutput)
    {
        PrintError('A version of PHP >= 7.x is required')
        return
    }

    # ensure composer is installed
    if (-not (Get-Command 'composer' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install composer')
        return
    }

    # copy the shared config json file to the local config location
    $configFilePath = Join-Path $sharedPath 'config.json'
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

    # add to bin
    $phpfindExe = Join-Path $phpfindPath 'bin' 'phpfind.ps1'
    AddToBin($phpfindExe)

    Set-Location $oldPwd
}

function BuildPowerShell
{
    Write-Host
    Hdr('BuildPowerShell')

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

    # ensure python3.7+ is installed
    $pythonVersions = @('python3.11', 'python3.10', 'python3.9', 'python3.8', 'python3.7')
    $python = ''
    ForEach ($p in $pythonVersions)
    {
        if (Get-Command $p -ErrorAction 'SilentlyContinue')
        {
            $python = $p
            Log("Using $python")
            break
        }
    }

    if (-not $python)
    {
        PrintError('You need to install python(>= 3.7)')
        return
    }

    # Set to $true to use venv
    $useVenv=$venv

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $pyfindPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $pyfindPath

    if ($useVenv)
    {
        # create a virtual env to run from and install to
        $venvPath = Join-Path $pyfindPath 'venv'
        if (-not (Test-Path $venvPath))
        {
            Log("$python -m venv venv")
            & $python -m venv venv
        }
    
        # activate the virtual env
        $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
        Log("$activatePath")
        & $activatePath
    }

    # install dependencies in requirements.txt
    Log('pip3 install -r requirements.txt')
    pip3 install -r requirements.txt

    if ($useVenv)
    {
        # deactivate at end of setup process
        Log('deactivate')
        deactivate
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

    $versionOutput = & ruby -v | Select-String -Pattern 'ruby 2' 2>&1
    if (-not $versionOutput)
    {
        PrintError('A version of ruby >= 2.x is required')
        return
    }

    # copy the shared config json file to the local config location
    $configFilePath = Join-Path $sharedPath 'config.json'
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

    # add to bin
    $rbfindExe = Join-Path $rbfindPath 'bin' 'rbfind.ps1'
    AddToBin($rbfindExe)
}

function BuildRust
{
    Write-Host
    Hdr('BuildRust')

    # ensure cargo/rust is installed
    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install rust')
    }

    $oldPwd = Get-Location
    Set-Location $rsfindPath

    Log('Building rsfind')

    if ($debug)
    {
        Log('cargo build')
        cargo build
    }

    if ($release)
    {
        Log('cargo build --release')
        cargo build --release

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

    # ensure sbt is installed
    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install scala + sbt')
    }

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

    $oldPwd = Get-Location
    Set-Location $scalafindPath

    # run sbt assembly
    Log('Building scalafind')
    Log("sbt 'set test in assembly := {}' clean assembly")
    sbt 'set test in assembly := {}' clean assembly

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

    $oldPwd = Get-Location
    Set-Location $swiftfindPath

    Log('Building swiftfind')

    if ($debug)
    {
        Log('swift build')
        swift build
    }

    if ($release)
    {
        Log('swift build --configuration release')
        swift build --configuration release

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

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        return
    }

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

    Measure-Command { BuildFsharp }

    Measure-Command { BuildGo }

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
    param($lang='')

    switch ($lang)
    {
        'all'        { BuildAll }
        'linux'      { BuildLinux }
        'c'          { Measure-Command { BuildC } }
        'clj'        { Measure-Command { BuildClojure } }
        'clojure'    { Measure-Command { BuildClojure } }
        'cpp'        { Measure-Command { BuildCpp } }
        'cs'         { Measure-Command { BuildCsharp } }
        'csharp'     { Measure-Command { BuildCsharp } }
        'dart'       { Measure-Command { BuildDart } }
        'fs'         { Measure-Command { BuildFsharp } }
        'fsharp'     { Measure-Command { BuildFsharp } }
        'go'         { Measure-Command { BuildGo } }
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
        default      { ExitWithError("Unknown option: $lang") }
    }
}

if ($help -or $lang -eq '')
{
    Usage
}

BuildMain $lang
