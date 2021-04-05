#!/usr/bin/env pwsh
################################################################################
#
# build.ps1
#
# Builds specified language version of xfind, or all versions
#
################################################################################
param([switch]$debug = $false,
      [switch]$release = $false,
      [string]$lang='all')

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path $scriptDir 'config.ps1')
. (Join-Path $scriptDir 'common.ps1')


$debug = $debug.IsPresent
$release = $release.IsPresent
if (!$release)
{
    $debug = $true
}

########################################
# Utility Functions
########################################

function CopyJsonResources
{
    param($resourcesPath)
    $fileTypesPath = Join-Path $sharedPath 'filetypes.json'
    Log("Copy-Item $fileTypesPath -Destination $resourcesPath")
    Copy-Item $fileTypesPath -Destination $resourcesPath
    $findOptionsPath = Join-Path $sharedPath 'findoptions.json'
    Log("Copy-Item $findOptionsPath -Destination $resourcesPath")
    Copy-Item $findOptionsPath -Destination $resourcesPath
}

function CopyXmlResources
{
    param($resourcesPath)
    $fileTypesPath = Join-Path $sharedPath 'filetypes.xml'
    Log("Copy-Item $fileTypesPath -Destination $resourcesPath")
    Copy-Item $fileTypesPath -Destination $resourcesPath
    $findOptionsPath = Join-Path $sharedPath 'findoptions.xml'
    Log("Copy-Item $findOptionsPath -Destination $resourcesPath")
    Copy-Item $findOptionsPath -Destination $resourcesPath
}

function CopyTestResources
{
    param($testResourcesPath)
    Log("Copy-Item $testFilePath -Include testFile*.txt -Destination $testResourcesPath")
    Copy-Item $testFilePath -Include testFile*.txt -Destination $testResourcesPath
}

function AddToBin
{
    param($scriptPath)

    if (!(Test-Path $binPath))
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

    if (Test-Path $linkPath)
    {
        if ((Get-Item $linkPath).LinkType -eq 'SymbolicLink')
        {
            Log("Remove-Item $linkPath")
            Remove-Item $linkPath
        }
    }

    if (!(Test-Path $linkPath))
    {
        # from https://winaero.com/create-symbolic-link-windows-10-powershell/
        # New-Item -ItemType SymbolicLink -Path "Link" -Target "Target"
        Log("New-Item -ItemType SymbolicLink -Path $linkPath -Target $scriptPath")
        New-Item -ItemType SymbolicLink -Path $linkPath -Target $scriptPath
    }
}


################################################################################
# Build functions
################################################################################

function BuildClojure
{
    Write-Host
    Hdr('BuildClojure')

    if (!(Get-Command 'lein'))
    {
        PrintError('You need to install leiningen')
        return
    }

    $resourcesPath = Join-Path $cljfindPath 'resources'
    if (!(Test-Path $resourcesPath))
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

        if (!(Test-Path $cmakeBuildPath))
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

    if (!(Get-Command 'dotnet'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $resourcesPath = Join-Path $csfindPath 'CsFindLib' 'Resources'
    $testResourcesPath = Join-Path $csfindPath 'CsFindTests' 'Resources'

    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    if (!(Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $csfindPath

    $csFindSolutonPath = Join-Path $csfindPath 'CsFind.sln'

    $configurations = @()
    if ($debug)
    {
        $configurations += 'Debug'
    }
    if ($release)
    {
        $configurations += 'Release'
    }

    ForEach ($c in $configurations)
    {
        Log("Building CsFind solution for $c configuration")
        Log("dotnet build $csFindSolutonPath --configuration $c")
        dotnet build $csFindSolutonPath --configuration $c
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

    if (!(Get-Command 'dart'))
    {
        PrintError('You need to install dart')
        return
    }

    $oldPwd = Get-Location
    Set-Location $dartfindPath

    Log('Building dartfind')
    if (!(Test-Path (Join-Path $dartfindPath '.packages')))
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

    if (!(Get-Command 'dotnet'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $resourcesPath = Join-Path $fsfindPath 'FsFindLib' 'Resources'
    $testResourcesPath = Join-Path $fsfindPath 'FsFindTests' 'Resources'

    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    if (!(Test-Path $testResourcesPath))
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

    if (!(Get-Command 'go'))
    {
        PrintError('You need to install go')
        return
    }

    $oldPwd = Get-Location
    Set-Location $gofindPath

    Log('Auto-formatting gofind')
    Log('go fmt ./...')
    go fmt ./...

    if (!(Test-Path $binPath))
    {
        New-Item -ItemType directory -Path $binPath
    }

    Log('Building gofind')

    if (-not (Test-Path Env:GOBIN))
    {
        $env:GOBIN = $binPath
    }

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

    if (!(Get-Command 'stack'))
    {
        PrintError('You need to install stack')
        return
    }

    $resourcesPath = Join-Path $hsfindPath 'data'
    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $hsfindPath

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

    if (!(Get-Command 'mvn'))
    {
        PrintError('You need to install maven')
        return
    }

    $resourcesPath = Join-Path $javafindPath 'src' 'main' 'resources'
    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)
    $testResourcesPath = Join-Path $javafindPath 'src' 'test' 'resources'
    if (!(Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

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

    if (!(Get-Command 'npm'))
    {
        PrintError('You need to install node.js/npm')
        return
    }

    $resourcesPath = Join-Path $jsfindPath 'data'
    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $jsfindPath

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

    if (!(Get-Command 'gradle'))
    {
        PrintError('You need to install gradle')
        return
    }

    $resourcesPath = Join-Path $ktfindPath 'src' 'main' 'resources'
    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)
    $testResourcesPath = Join-Path $ktfindPath 'src' 'test' 'resources'
    if (!(Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $ktfindPath

    Log('Building ktfind')
    Log('gradle -b build.gradle clean jar')
    gradle -b 'build.gradle' clean jar

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

    if (!(Get-Command 'xcodebuild'))
    {
        PrintError('You need to install Xcode')
        return
    }

    $oldPwd = Get-Location
    Set-Location $objcfindPath

    Log('Building objcfind')
    if ($target -eq 'alltargets')
    {
        Log('xcodebuild -alltargets')
        xcodebuild -alltargets
    }
    else
    {
        Log("xcodebuild -project $target")
        xcodebuild -project $target
    }

    # add to bin
    $objcfindExe = Join-Path $objcfindPath 'bin' 'objcfind.ps1'
    AddToBin($objcfindExe)

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

    if (!(Get-Command 'perl'))
    {
        PrintError('You need to install perl')
        return
    }

    $versionOutput = & perl -v | Select-String -Pattern 'This is perl 5' 2>&1
    if (!$versionOutput)
    {
        PrintError('A 5.x version of perl is required')
        return
    }

    $resourcesPath = Join-Path $plfindPath 'share'
    if (!(Test-Path $resourcesPath))
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

    if (!(Get-Command 'php'))
    {
        PrintError('You need to install php')
        return
    }

    $versionOutput = & php -v | Select-String -Pattern 'PHP [78]' 2>&1
    if (!$versionOutput)
    {
        PrintError('A version of PHP >= 7.x is required')
        return
    }

    $configFilePath = Join-Path $sharedPath 'config.json'
    $configPath = Join-Path $phpfindPath 'config'
    if (!(Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    $resourcesPath = Join-Path $phpfindPath 'resources'
    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    if (!(Get-Command 'composer'))
    {
        PrintError('You need to install composer')
        return
    }

    $oldPwd = Get-Location
    Set-Location $phpfindPath

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

function BuildPython
{
    Write-Host
    Hdr('BuildPython')

    if (!(Get-Command 'python3'))
    {
        PrintError('You need to install python(>= 3.7)')
        return
    }

    # Set to $true to use venv
    $useVenv=$false

    $pythonVersions = @('python3.9', 'python3.8', 'python3.7')
    $python = ''
    ForEach ($p in $pythonVersions)
    {
        if (Get-Command $p -ErrorAction "SilentlyContinue")
        {
            $python = $p
            Log("Using $python")
            break
        }
    }

    $resourcesPath = Join-Path $pyfindPath 'data'
    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $pyfindPath

    if ($useVenv)
    {
        $venvPath = Join-Path $pyfindPath 'venv'
        if (!(Test-Path $venvPath))
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

    if (!(Get-Command 'ruby'))
    {
        PrintError('You need to install ruby')
        return
    }

    $versionOutput = & ruby -v | Select-String -Pattern 'ruby 2' 2>&1
    if (!$versionOutput)
    {
        PrintError('A version of ruby >= 2.x is required')
        return
    }

    $configFilePath = Join-Path $sharedPath 'config.json'
    $configPath = Join-Path $rbfindPath 'data'
    if (!(Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    $resourcesPath = Join-Path $rbfindPath 'data'
    if (!(Test-Path $resourcesPath))
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

    if (!(Get-Command 'cargo'))
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

    if (!(Get-Command 'sbt'))
    {
        PrintError('You need to install scala + sbt')
    }

    $resourcesPath = Join-Path $scalafindPath 'src' 'main' 'resources'
    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)
    $testResourcesPath = Join-Path $scalafindPath 'src' 'test' 'resources'
    if (!(Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $scalafindPath

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

    if (!(Get-Command 'swift'))
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

    if (!(Get-Command 'npm'))
    {
        PrintError('You need to install node.js/npm')
        return
    }

    $resourcesPath = Join-Path $tsfindPath 'data'
    if (!(Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $tsfindPath

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

    Measure-Command { BuildOcaml }

    Measure-Command { BuildPerl }

    Measure-Command { BuildPhp }

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
    param($lang='all')

    switch ($lang)
    {
        'all'        { BuildAll }
        'linux'      { BuildLinux }
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

BuildMain $lang
