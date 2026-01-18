#!/usr/bin/env pwsh
################################################################################
#
# build.ps1
#
# Builds specified language versions of xfind, or all versions
#
################################################################################

########################################
# Configuration
########################################

$xfindScriptPath = $MyInvocation.MyCommand.Path
$xfindScriptDir = Split-Path $xfindScriptPath -Parent

# . (Join-Path -Path $xfindScriptDir -ChildPath 'config.ps1')
. (Join-Path -Path $xfindScriptDir -ChildPath 'common.ps1')

# Global variable to hold last funtion exit code
$global:BUILD_LASTEXITCODE = 0

# Keep track of successful and failed builds and report at the end
$global:successfulBuilds = @()
$global:failedBuilds = @()


########################################
# Utility Functions
########################################

function CopyJsonResources
{
    param([string]$sharedPath, [string]$resourcesPath)

    $jsonFiles = @(Get-ChildItem -Path $sharedPath -File) |
                   Where-Object { $_.Extension -eq '.json' }
    foreach ($jsonFile in $jsonFiles) {
        Log("Copy-Item $jsonFile -Destination $resourcesPath")
        Copy-Item $jsonFile -Destination $resourcesPath
    }
}

function CopyTestResources
{
    param([string]$testSharedPath, [string]$testResourcesPath)

    $testFiles = @(Get-ChildItem -Path $testSharedPath -File) |
                   Where-Object { $_.Name.StartsWith('testFile') -and $_.Extension -eq '.txt' }
    foreach ($testFile in $testFiles) {
        Log("Copy-Item $testFile -Destination $testResourcesPath")
        Copy-Item $testFile -Destination $testResourcesPath
    }
}

function AddSoftLink
{
    param([string]$linkPath, [string]$targetPath, [bool]$replaceLink=$true)
    # Write-Host "linkPath: $linkPath"
    # Write-Host "targetPath: $targetPath"

    if ((Test-Path $linkPath) -and $replaceLink) {
        if ((Get-Item $linkPath).LinkType -eq 'SymbolicLink') {
            # Log("Remove-Item $linkPath")
            Remove-Item $linkPath
        }
    }

    if (-not (Test-Path $linkPath)) {
        # from https://winaero.com/create-symbolic-link-windows-10-powershell/
        # New-Item -ItemType SymbolicLink -Path "Link" -Target "Target"
        # Log("New-Item -ItemType SymbolicLink -Path $linkPath -Target $targetPath")
        New-Item -ItemType SymbolicLink -Path $linkPath -Target $targetPath
    }
}

function AddToBin
{
    param([string]$binPath, [string]$scriptPath)

    # Log("binPath: $binPath")
    # Log("scriptPath: $scriptPath")

    if (-not (Test-Path $binPath)) {
        New-Item -ItemType directory -Path $binPath
    }

    # get the base filename, minus path and any extension
    $baseName = [io.path]::GetFileNameWithoutExtension($scriptPath)
    if ($baseName.EndsWith('.debug') -or $baseName.EndsWith('.release')) {
        $baseName = $baseName.Split('.')[0]
    }

    $linkPath = Join-Path $binPath $baseName

    AddSoftLink $linkPath $scriptPath
}

function PrintBuildResults
{
    if ($global:successfulBuilds.Count -gt 0) {
        $joinedSuccessfulBuilds = $global:successfulBuilds -join ', '
        Log("Successful builds ($($global:successfulBuilds.Count)): $joinedSuccessfulBuilds")
    } else {
        Log("Successful builds: 0")
    }
    if ($global:failedBuilds.Count -gt 0) {
        $joinedFailedBuilds = $global:failedBuilds -join ', '
        PrintError("Failed builds ($($global:failedBuilds.Count)): $joinedFailedBuilds")
    } else {
        Log("Failed builds: 0")
    }
}


################################################################################
# Build functions
################################################################################

function BuildBashVersion
{
    param([string]$basePath, [string]$bashVersionName)

    Log('language: bash')
    Log("bashVersionName: $bashVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure bash is installed
    if (-not (Get-Command 'bash' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install bash')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $bashVersion = bash --version | Select-String -Pattern 'version'
    Log("bash version: $bashVersion")

    $bashVersionPath = Join-Path $basePath 'bash' $bashVersionName
    Log("bashVersionPath: $bashVersionPath")

    if (-not (Test-Path $bashVersionPath)) {
        PrintError("Path not found: $bashVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $scriptPath = Join-Path $bashVersionPath 'bin' "$bashVersionName.bash"

    if (-not (Test-Path $scriptPath)) {
        PrintError("File not found: $scriptPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildCVersion
{
    param([string]$basePath, [string]$cVersionName)

    Log('language: C')
    Log("cVersionName: $cVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # if ($IsWindows)
    # {
    #     Log('BuildCVersion - currently unimplemented for Windows')
    #     return
    # }
    # if (!$IsMacOS -and !$IsLinux)
    # {
    #     Log('Skipping for unknown/unsupported OS')
    #     return
    # }

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install cmake')
        $global:BUILD_LASTEXITCODE = 1
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
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $cVersionPath")
    Set-Location $cVersionPath

    $configurations = @()
    if ($debug) {
        $configurations += 'debug'
    }
    if ($release) {
        $configurations += 'release'
    }

    ForEach ($c in $configurations) {
        $cmakeBuildDir = "cmake-build-$c"
        $cmakeBuildPath = Join-Path $cVersionPath $cmakeBuildDir

        if (-not (Test-Path $cmakeBuildPath)) {
            Log("New-Item -ItemType directory -Path $cmakeBuildPath")
            New-Item -ItemType directory -Path $cmakeBuildPath

            Log("Set-Location $cmakeBuildPath")
            Set-Location $cmakeBuildPath

            Log("cmake -G ""Unix Makefiles"" -DCMAKE_BUILD_TYPE=$c ..")
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            Set-Location $cVersionPath
        }

        if (Test-Path $cmakeBuildPath) {
            $targets = @('clean', $cVersionName, "${cVersionName}app", "${cVersionName}-tests")
            ForEach ($t in $targets) {
                Log("cmake --build $cmakeBuildDir --config $c --target $t")
                cmake --build $cmakeBuildDir --config $c --target $t

                $global:BUILD_LASTEXITCODE = $LASTEXITCODE
                if ($global:BUILD_LASTEXITCODE -ne 0) {
                    # PrintError("Build target $t failed")
                    Set-Location $oldPwd
                    return
                }
            }
        }
    }

    $binPath = Join-Path $basePath 'bin'
    $scriptPath = ''

    if ($release) {
        # add release to bin
        $scriptPath = Join-Path $cVersionPath 'bin' "${cVersionName}.release.ps1"
    } else {
        # add debug to bin
        $scriptPath = Join-Path $cVersionPath 'bin' "${cVersionName}.debug.ps1"
    }

    Set-Location $oldPwd

    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildClojureVersion
{
    param([string]$basePath, [string]$cljVersionName)

    Log('language: clojure')
    Log("version: $cljVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure clojure is installed
    if (-not (Get-Command 'clj' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install clojure')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # clj -version output looks like this: Clojure CLI version 1.11.4.1474
    $clojureVersion = clj -version 2>&1
    Log("clojure version: $clojureVersion")

    # ensure leiningen is installed
    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install leiningen')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    $leinVersion = lein version
    Log("lein version: $leinVersion")

    $cljVersionPath = Join-Path $basePath 'clojure' $cljVersionName
    Log("cljVersionPath: $cljVersionPath")

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    if (-not (Test-Path $sharedPath)) {
        PrintError("Path not found: $sharedPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }
    $resourcesPath = Join-Path $cljVersionPath 'resources'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    $oldPwd = Get-Location
    Log("Set-Location $cljVersionPath")
    Set-Location $cljVersionPath

    # run clean
    Log('lein clean')
    lein clean

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # create uberjar
    Log('lein uberjar')
    lein uberjar

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # install to local maven repository
    Log('lein install')
    lein install

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $cljVersionPath 'bin' "${cljVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildCppVersion
{
    param([string]$basePath, [string]$cppVersionName)

    Log('language: C++')
    Log("version: $cppVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # if ($IsWindows)
    # {
    #     Log('BuildCppVersion - currently unimplemented for Windows')
    #     return
    # }
    # if (!$IsMacOS -and !$IsLinux)
    # {
    #     Log('Skipping for unknown/unsupported OS')
    #     return
    # }

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install cmake')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # cmake --version output looks like this: cmake version 3.30.2
    $cmakeVersion = cmake --version
    $cmakeVersion = @($cmakeVersion -split '\s+')[2]
    Log("cmake version: $cmakeVersion")

    $cppVersionPath = Join-Path $basePath 'cpp' $cppVersionName
    Log("cppVersionPath: $cppVersionPath")

    if (-not (Test-Path $cppVersionPath)) {
        PrintError("Path not found: $cppVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $cppVersionPath")
    Set-Location $cppVersionPath

    # Set CMAKE_CXX_FLAGS
    $cmakeCxxFlags = "-W -Wall -Werror -Wextra -Wshadow -Wnon-virtual-dtor -pedantic"

    # Add AddressSanitizer
    # $cmakeCxxFlags = "$cmakeCxxFlags -fsanitize=address -fno-omit-frame-pointer"

    $configurations = @()
    if ($debug) {
        $configurations += 'debug'
    }
    if ($release) {
        $configurations += 'release'
    }
    ForEach ($c in $configurations) {
        $cmakeBuildDir = "cmake-build-$c"
        $cmakeBuildPath = Join-Path $cppVersionPath $cmakeBuildDir

        if (-not (Test-Path $cmakeBuildPath)) {
            Log("New-Item -ItemType directory -Path $cmakeBuildPath")
            New-Item -ItemType directory -Path $cmakeBuildPath

            Log("Set-Location $cmakeBuildPath")
            Set-Location $cmakeBuildPath

            Log("cmake -G ""Unix Makefiles"" -DCMAKE_BUILD_TYPE=$c ..")
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            Set-Location $cppVersionPath
        }

        if (Test-Path $cmakeBuildPath) {
            $targets = @('clean', $cppVersionName, "${cppVersionName}app", "${cppVersionName}-tests")
            ForEach ($t in $targets) {
                Log("cmake --build $cmakeBuildDir --config $c --target $t -- $cmakeCxxFlags")
                cmake --build $cmakeBuildDir --config $c --target $t -- $cmakeCxxFlags

                $global:BUILD_LASTEXITCODE = $LASTEXITCODE
                if ($global:BUILD_LASTEXITCODE -eq 0) {
                    Log "Build target $t succeeded"
                } else {
                    PrintError("Build target $t failed")
                    Set-Location $oldPwd
                    return
                }
            }
        }
    }

    Set-Location $oldPwd

    $binPath = Join-Path $basePath 'bin'
    $scriptPath = ''
    if ($release) {
        # add release to bin
        $scriptPath = Join-Path $cppVersionPath 'bin' "${cppVersionName}.release.ps1"
    } else {
        # add debug to bin
        $scriptPath = Join-Path $cppVersionPath 'bin' "${cppVersionName}.debug.ps1"
    }

    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildCsharpVersion
{
    param([string]$basePath, [string]$csVersionName)

    Log('language: C#')
    Log("version: $csVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install dotnet')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $csVersionPath = Join-Path $basePath 'csharp' $csVersionName
    Log("csVersionPath: $csVersionPath")

    if (-not (Test-Path $csVersionPath)) {
        PrintError("Path not found: $csVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $projectPrefix = ''
    if ($csVersionName -ieq 'csfind') {
        $projectPrefix = 'CsFind'
    } elseif ($csVersionName -ieq 'cssearch') {
        $projectPrefix = 'CsSearch'
    } else {
        PrintError("Unknown C# version name: $csVersionName")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $resourcesPath = Join-Path $csVersionPath "${projectPrefix}Lib" 'Resources'
    $testResourcesPath = Join-Path $csVersionPath "${projectPrefix}Tests" 'Resources'

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    # copy the shared test files to the local test resource location
    $testFilesPath = Join-Path $sharedPath 'testFiles'
    if (-not (Test-Path $testResourcesPath)) {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources $testFilesPath $testResourcesPath

    # $oldPwd = Get-Location
    # Log("Set-Location $csVersionPath")
    # Set-Location $csVersionPath

    $csVersionSolutionPath = Join-Path $csVersionPath "${projectPrefix}.sln"

    $configurations = @()
    if ($debug) {
        $configurations += 'Debug'
    }
    if ($release) {
        $configurations += 'Release'
    }

    # run dotnet build selected configurations
    ForEach ($c in $configurations) {
        Log("Building $projectPrefix solution for $c configuration")
        Log("dotnet build $csVersionSolutionPath --configuration $c")
        dotnet build $csVersionSolutionPath --configuration $c

        $global:BUILD_LASTEXITCODE = $LASTEXITCODE
        if ($global:BUILD_LASTEXITCODE -ne 0) {
            # PrintError('Build failed')
            # Set-Location $oldPwd
            return
        }
    }

    $binPath = Join-Path $basePath 'bin'
    $scriptPath = ''

    if ($release) {
        # add release to bin
        $scriptPath = Join-Path $csVersionPath 'bin' "${csVersionName}.release.ps1"
    } else {
        # add debug to bin
        $scriptPath = Join-Path $csVersionPath 'bin' "${csVersionName}.debug.ps1"
    }

    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath

    # Set-Location $oldPwd
}

function BuildDartVersion
{
    param([string]$basePath, [string]$dartVersionName)

    Log('language: dart')
    Log("version: $dartVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure dart is installed
    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install dart')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $dartVersion = dart --version
    Log("$dartVersion")

    $dartVersionPath = Join-Path $basePath 'dart' $dartVersionName
    Log("dartVersionPath: $dartVersionPath")

    if (-not (Test-Path $dartVersionPath)) {
        PrintError("Path not found: $dartVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $dartVersionPath")
    Set-Location $dartVersionPath

    Log("Building $dartVersionName")
    if ((-not (Test-Path (Join-Path $dartVersionPath '.dart_tool' 'package_config.json'))) -and
        (-not (Test-Path (Join-Path $dartVersionPath '.packages')))) {
        Log('dart pub get')
        dart pub get
    } else {
        Log('dart pub upgrade')
        dart pub upgrade
    }

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Log("Compiling $dartVersionName")
    $dartScript = Join-Path $dartVersionPath 'bin' "$dartVersionName.dart"
    Log("dart compile exe $dartScript")
    dart compile exe $dartScript

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    if (-not (Test-Path $binPath)) {
        New-Item -ItemType directory -Path $binPath
    }
    $scriptPath = Join-Path $dartVersionPath 'bin' "$dartVersionName.ps1"
    if (-not (Test-Path $scriptPath)) {
        PrintError("File not found: $scriptPath")
        $global:BUILD_LASTEXITCODE = 1
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildElixirVersion
{
    param([string]$basePath, [string]$exVersionName)

    Log('language: elixir')
    Log("version: $exVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure elixir is installed
    if (-not (Get-Command 'elixir' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install elixir')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $elixirVersion = elixir --version | Select-String -Pattern 'Elixir'
    Log("elixir version: $elixirVersion")

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install mix')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $mixVersion = mix --version | Select-String -Pattern 'Mix'
    Log("mix version: $mixVersion")

    $exVersionPath = Join-Path $basePath 'elixir' $exVersionName
    Log("exVersionPath: $exVersionPath")

    if (-not (Test-Path $exVersionPath)) {
        PrintError("Path not found: $exVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $exVersionPath")
    Set-Location $exVersionPath

    Log("Getting $exVersionName dependencies")
    Log('mix deps.get')
    mix deps.get

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Log("Compiling $exVersionName")
    Log('mix compile')
    mix compile

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Log("Creating $exVersionName executable")
    Log('mix escript.build')
    mix escript.build

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $exVersionPath 'bin' $exVersionName
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildFsharpVersion
{
    param([string]$basePath, [string]$fsVersionName)

    Log('language: F#')
    Log("version: $fsVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install dotnet')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $fsVersionPath = Join-Path $basePath 'fsharp' $fsVersionName
    Log("fsVersionPath: $fsVersionPath")

    if (-not (Test-Path $fsVersionPath)) {
        PrintError("Path not found: $fsVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $projectPrefix = ''
    if ($fsVersionName -ieq 'fsfind') {
        $projectPrefix = 'FsFind'
    } elseif ($fsVersionName -ieq 'fssearch') {
        $projectPrefix = 'FsSearch'
    } else {
        PrintError("Unknown F# version name: $fsVersionName")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $resourcesPath = Join-Path $fsVersionPath "${projectPrefix}Lib" 'Resources'
    $testResourcesPath = Join-Path $fsVersionPath "${projectPrefix}Tests" 'Resources'

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    # copy the shared test files to the local test resource location
    $testFilesPath = Join-Path $sharedPath 'testFiles'
    if (-not (Test-Path $testResourcesPath)) {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources $testFilesPath $testResourcesPath

    # $oldPwd = Get-Location
    # Set-Location $fsVersionPath

    $fsVersionSolutionPath = Join-Path $fsVersionPath "${projectPrefix}.sln"

    $configurations = @()
    if ($debug) {
        $configurations += 'Debug'
    }
    if ($release) {
        $configurations += 'Release'
    }

    # run dotnet build for selected configurations
    ForEach ($c in $configurations) {
        Log("Building $projectPrefix solution for $c configuration")
        Log("dotnet build $fsVersionSolutionPath --configuration $c")
        dotnet build $fsVersionSolutionPath --configuration $c

        $global:BUILD_LASTEXITCODE = $LASTEXITCODE
        if ($global:BUILD_LASTEXITCODE -ne 0) {
            # PrintError('Build failed')
            # Set-Location $oldPwd
            return
        }
    }

    $binPath = Join-Path $basePath 'bin'
    $scriptPath = ''

    if ($release) {
        # add release to bin
        $scriptPath = Join-Path $fsVersionPath 'bin' "${fsVersionName}.release.ps1"
    } else {
        # add debug to bin
        $scriptPath = Join-Path $fsVersionPath 'bin' "${fsVersionName}.debug.ps1"
    }

    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath

    # Set-Location $oldPwd
}

function BuildGoVersion
{
    param([string]$basePath, [string]$goVersionName)

    Log('language: go')
    Log("version: $goVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure go is installed
    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install go')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $goVersion = (go version) -replace 'go version ', ''
    Log("go version: $goVersion")

    $goVersionPath = Join-Path $basePath 'go' $goVersionName
    Log("goVersionPath: $goVersionPath")

    if (-not (Test-Path $goVersionPath)) {
        PrintError("Path not found: $goVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $goVersionPath")
    Set-Location $goVersionPath

    # go fmt the go version source (for auto-generated code)
    Log("Auto-formatting $goVersionName")
    Log('go fmt ./...')
    go fmt ./...

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # create the bin dir if it doesn't already exist
    $binPath = Join-Path $basePath 'bin'
    if (-not (Test-Path $binPath)) {
        New-Item -ItemType directory -Path $binPath
    }

    # if GOBIN not defined, set to BIN_PATH
    if (-not (Test-Path Env:GOBIN)) {
        $env:GOBIN = $binPath
    }

    # now build the go version
    Log("Building $goVersionName")
    Log('go install ./...')
    go install ./...

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function BuildGroovyVersion
{
    param([string]$basePath, [string]$groovyVersionName)

    Log('language: groovy')
    Log("version: $groovyVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure groovy is installed
    if (-not (Get-Command 'groovy' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install groovy')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $groovyVersion = groovy --version
    Log("groovy version: $groovyVersion")

    $groovyVersionPath = Join-Path $basePath 'groovy' $groovyVersionName
    Log("groovyVersionPath: $groovyVersionPath")

    if (-not (Test-Path $groovyVersionPath)) {
        PrintError("Path not found: $groovyVersionPath")
        $global:BUILD_LASTEXITCODE = 1
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
        $global:BUILD_LASTEXITCODE = 1
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
    $sharedPath = Join-Path $basePath 'shared'
    $resourcesPath = Join-Path $groovyVersionPath 'lib' 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    # copy the test files to the local test resource location
    $testFilesPath = Join-Path $sharedPath 'testFiles'
    $testResourcesPath = Join-Path $groovyVersionPath 'lib' 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath)) {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources $testFilesPath $testResourcesPath

    # run the gradle command to build
    Log("Building $groovyVersionName")

    $gradleArgs = '--warning-mode all'
    $gradleTasks = 'clean jar'
    Log("$gradle $gradleArgs $gradleTasks")
    & $gradle --warning-mode all clean jar

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # Command to install to local maven repository
    # What worked for me is gradle install -Dmaven.repo.local=~/.m2/repository.

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $groovyVersionPath 'bin' "${groovyVersionName}.ps1"
    AddToBin $binPath $scriptPath
}

function BuildHaskellVersion
{
    param([string]$basePath, [string]$hsVersionName)

    Log('language: haskell')
    Log("version: $hsVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure ghc is installed
    if (-not (Get-Command 'ghc' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install ghc')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $ghcVersion = ghc --version
    Log("ghc version: $ghcVersion")

    # ensure stack is installed
    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install stack')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $stackVersion = stack --version
    Log("stack version: $stackVersion")

    # set the default stack settings, e.g. use system ghc
    $stackDir = Join-Path $HOME '.stack'
    if (-not (Test-Path $stackDir)) {
        New-Item -ItemType directory -Path $stackDir
    }
    $configYaml = Join-Path $stackDir 'config.yaml'
    if (-not (Test-Path $configYaml)) {
        New-Item -ItemType file -Path $stackDir -Name "config.yaml" -Value "install-ghc: false`nsystem-ghc: true"
    }

    $hsVersionPath = Join-Path $basePath 'haskell' $hsVersionName
    Log("hsVersionPath: $hsVersionPath")

    if (-not (Test-Path $hsVersionPath)) {
        PrintError("Path not found: $hsVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    $resourcesPath = Join-Path $hsVersionPath 'data'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    $oldPwd = Get-Location
    Log("Set-Location $hsVersionPath")
    Set-Location $hsVersionPath

    # build with stack (via make)
    Log("Building $hsVersionName")
    Log('stack setup')
    make setup

    Log('stack build')
    make build

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    $binPath = Join-Path $basePath 'bin'
    if (-not (Test-Path $binPath)) {
        New-Item -ItemType directory -Path $binPath
    }

    Log("stack install --local-bin-path $binPath")
    stack install --local-bin-path $binPath

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE

    Set-Location $oldPwd
}

function BuildJavaVersion
{
    param([string]$basePath, [string]$javaVersionName)

    Log('language: java')
    Log("version: $javaVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure java is installed
    if (-not (Get-Command 'java' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install java')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $javaVersion = java -version 2>&1 | Select-String -Pattern 'version'
    Log("java version: $javaVersion")

    $javaVersionPath = Join-Path $basePath 'java' $javaVersionName
    Log("javaVersionPath: $javaVersionPath")

    if (-not (Test-Path $javaVersionPath)) {
        PrintError("Path not found: $javaVersionPath")
        $global:BUILD_LASTEXITCODE = 1
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
        $global:BUILD_LASTEXITCODE = 1
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

    $kotlinVersion = $gradleOutput | Where-Object {$_.Contains('Kotlin')} | ForEach-Object {$_ -replace 'Kotlin:\s+',''}
    Log("Kotlin version: $kotlinVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    $resourcesPath = Join-Path $javaVersionPath 'lib' 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    # copy the test files to the local test resource location
    $testFilesPath = Join-Path $sharedPath 'testFiles'
    $testResourcesPath = Join-Path $javaVersionPath 'lib' 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath)) {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources $testFilesPath $testResourcesPath

    # run a gradle build
    Log("Building $javaVersionName")
    # Log('gradle --warning-mode all clean jar publishToMavenLocal')
    # gradle --warning-mode all clean jar publishToMavenLocal
    $gradleArgs = '--warning-mode all'
    $gradleTasks = @('clean', ':lib:jar', ':lib:publishToMavenLocal', ':app:jar')

    ForEach ($t in $gradleTasks) {
        Log("$gradle $gradleArgs $t")
        & $gradle --warning-mode all $t
    }

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $javaVersionPath 'bin' "${javaVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildJavascriptVersion
{
    param([string]$basePath, [string]$jsVersionName)

    Log('language: javascript')
    Log("version: $jsVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure node is installed
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install node.js')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install npm')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    $jsVersionPath = Join-Path $basePath 'javascript' $jsVersionName
    Log("jsVersionPath: $jsVersionPath")

    if (-not (Test-Path $jsVersionPath)) {
        PrintError("Path not found: $jsVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    $resourcesPath = Join-Path $jsVersionPath 'data'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    $oldPwd = Get-Location
    Log("Set-Location $jsVersionPath")
    Set-Location $jsVersionPath

    # run npm install and build
    Log("Building $jsVersionName")
    Log('npm install')
    npm install

    Log('npm run build')
    npm run build

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $jsVersionPath 'bin' "${jsVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildKotlinVersion
{
    param([string]$basePath, [string]$ktVersionName)

    Log('language: kotlin')
    Log("version: $ktVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    $ktVersionPath = Join-Path $basePath 'kotlin' $ktVersionName
    Log("ktVersionPath: $ktVersionPath")

    if (-not (Test-Path $ktVersionPath)) {
        PrintError("Path not found: $ktVersionPath")
        $global:BUILD_LASTEXITCODE = 1
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
        $global:BUILD_LASTEXITCODE = 1
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
    $sharedPath = Join-Path $basePath 'shared'
    $resourcesPath = Join-Path $ktVersionPath 'lib' 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    # copy the test files to the local test resource location
    $testFilesPath = Join-Path $sharedPath 'testFiles'
    $testResourcesPath = Join-Path $ktVersionPath 'lib' 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath)) {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources $testFilesPath $testResourcesPath

    # run a gradle build
    Log("Building $ktVersionName")
    # Log('gradle --warning-mode all clean jar publishToMavenLocal')
    # gradle --warning-mode all clean jar publishToMavenLocal
    $gradleArgs = '--warning-mode all'
    $gradleTasks = 'clean jar'
    $gradleTasks = @('clean', ':lib:jar', ':lib:publishToMavenLocal', ':app:jar')
    ForEach ($t in $gradleTasks) {
        Log("$gradle $gradleArgs $t")
        & $gradle --warning-mode all $t

        $global:UNITTEST_LASTEXITCODE = $LASTEXITCODE

        if ($global:UNITTEST_LASTEXITCODE -ne 0) {
            break
        }
    }

    # check for success/failure
    if ($global:UNITTEST_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $ktVersionPath 'bin' "${ktVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildObjcVersion
{
    param([string]$basePath, [string]$objcVersionName)

    Log('language: objc')
    Log("version: $objcVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install swift')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Apple Swift'
    $swiftVersion = @($swiftVersion -split '\s+')[3]
    Log("swift version: Apple Swift version $swiftVersion")

    $objcVersionPath = Join-Path $basePath 'objc' $objcVersionName
    Log("objcVersionPath: $objcVersionPath")

    if (-not (Test-Path $objcVersionPath)) {
        PrintError("Path not found: $objcVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $objcVersionPath")
    Set-Location $objcVersionPath

    Log("Building $objcVersionName")

    if ($debug) {
        Log("swift build")
        swift build

        $global:BUILD_LASTEXITCODE = $LASTEXITCODE
        if ($global:BUILD_LASTEXITCODE -ne 0) {
            # PrintError('Build failed')
            Set-Location $oldPwd
            return
        }
    }

    $binPath = Join-Path $basePath 'bin'
    $scriptPath = ''

    if ($release) {
        Log("swift build --configuration release")
        swift build --configuration release

        $global:BUILD_LASTEXITCODE = $LASTEXITCODE
        if ($global:BUILD_LASTEXITCODE -ne 0) {
            # PrintError('Build failed')
            Set-Location $oldPwd
            return
        }

        # add release to bin
        $scriptPath = Join-Path $objcVersionPath 'bin' "${objcVersionName}.release.ps1"
    } else {
        # add debug to bin
        $scriptPath = Join-Path $objcVersionPath 'bin' "${objcVersionName}.debug.ps1"
    }

    Set-Location $oldPwd

    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildOcamlVersion
{
    Log("Not currently implemented")
}

function BuildPerlVersion
{
    param([string]$basePath, [string]$plVersionName)

    Log('language: perl')
    Log("version: $plVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure perl is installed
    if (-not (Get-Command 'perl' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install perl')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $perlVersion = perl -e 'print $^V' | Select-String -Pattern 'v5'
    if (-not $perlVersion) {
        PrintError('A 5.x version of perl is required')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    Log("perl version: $perlVersion")

    $plVersionPath = Join-Path $basePath 'perl' $plVersionName
    Log("plVersionPath: $plVersionPath")

    if (-not (Test-Path $plVersionPath)) {
        PrintError("Path not found: $plVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    $resourcesPath = Join-Path $plVersionPath 'share'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        return
    }

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $plVersionPath 'bin' "${plVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildPhpVersion
{
    param([string]$basePath, [string]$phpVersionName)

    Log('language: php')
    Log("version: $phpVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure php is installed
    if (-not (Get-Command 'php' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install php')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $phpVersion = & php -v | Select-String -Pattern '^PHP [78]' 2>&1
    if (-not $phpVersion) {
        PrintError('A version of PHP >= 7.x is required')
        $global:BUILD_LASTEXITCODE = 1
        return
    }
    Log("php version: $phpVersion")

    # ensure composer is installed
    if (-not (Get-Command 'composer' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install composer')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $composerVersion = composer --version 2>&1 | Select-String -Pattern '^Composer'
    Log("composer version: $composerVersion")

    $phpVersionPath = Join-Path $basePath 'php' $phpVersionName
    Log("phpVersionPath: $phpVersionPath")

    if (-not (Test-Path $phpVersionPath)) {
        PrintError("Path not found: $phpVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # copy the shared config json file to the local config location
    $sharedPath = Join-Path $basePath 'shared'
    $configFilePath = Join-Path $sharedPath 'config.json'
    $configPath = Join-Path $phpVersionPath 'config'
    if (-not (Test-Path $configPath)) {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $phpVersionPath 'resources'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    $oldPwd = Get-Location
    Log("Set-Location $phpVersionPath")
    Set-Location $phpVersionPath

    # run a composer build
    Log("Building $phpVersionName")

    if (Test-Path (Join-Path $phpVersionPath 'vendor')) {
        Log('composer update')
        composer update
    } else {
        Log('composer install')
        composer install
    }

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $phpVersionPath 'bin' "${phpVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildPowershellVersion
{
    param([string]$basePath, [string]$ps1VersionName)

    Log('language: powershell')
    Log("version: $ps1VersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # We don't need to check for powershell, as we're running in it

    $powershellVersion = pwsh -v
    Log("powershell version: $powershellVersion")

    $ps1VersionPath = Join-Path $basePath 'powershell' $ps1VersionName
    Log("ps1VersionPath: $ps1VersionPath")

    if (-not (Test-Path $ps1VersionPath)) {
        PrintError("Path not found: $ps1VersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $projectPrefix = ''
    if ($ps1VersionName -ieq 'ps1find') {
        $projectPrefix = 'Ps1Find'
    } elseif ($ps1VersionName -ieq 'ps1search') {
        $projectPrefix = 'Ps1Search'
    } else {
        PrintError("Unknown ps1 version name: $ps1VersionName")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # copy the file to the first of the module paths, if defined
    if (-not $env:PSModulePath) {
        PrintError("PSModulePath is not defined, cannot copy ${projectPrefix}Module")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    Log("Building $ps1VersionName")

    $modulePaths = @($env:PSModulePath -split ':')
    if ($modulePaths.Count -gt 0) {
        $ps1VersionTargetModulePath = Join-Path $modulePaths[0] "${projectPrefix}Module"
        if (-not (Test-Path $ps1VersionTargetModulePath)) {
            Log("New-Item -Path $ps1VersionTargetModulePath -ItemType Directory")
            New-Item -Path $ps1VersionTargetModulePath -ItemType Directory
        }
        $ps1VersionModulePath = Join-Path $ps1VersionPath "${projectPrefix}Module.psm1"
        Log("Copy-Item $ps1VersionModulePath -Destination $ps1VersionTargetModulePath")
        Copy-Item $ps1VersionModulePath -Destination $ps1VersionTargetModulePath
    }

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $ps1VersionPath "${ps1VersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildPythonVersion
{
    param([string]$basePath, [string]$pyVersionName)

    Log('language: python')
    Log("version: $pyVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    $pyVersionPath = Join-Path $basePath 'python' $pyVersionName
    Log("pyVersionPath: $pyVersionPath")

    if (-not (Test-Path $pyVersionPath)) {
        PrintError("Path not found: $pyVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $pyVersionPath")
    Set-Location $pyVersionPath

    # Set to $true to use venv
    $useVenv=$venv
    # $pythonVersions = @('python3.12', 'python3.11', 'python3.10', 'python3.9')
    # We don't want to use python3.12 yet
    $pythonVersions = @('python3.11', 'python3.10', 'python3.9')
    $python = ''
    $venvPath = Join-Path $pyVersionPath 'venv'

    $activeVenv = $false

    if ($useVenv) {
        Log('Using venv')

        # 3 possibilities:
        # 1. venv exists and is active
        # 2. venv exists and is not active
        # 3. venv does not exist

        if ($env:VIRTUAL_ENV) {
            # 1. venv exists and is active
            Log("Already active venv: $env:VIRTUAL_ENV")
            # $activeVenv = $env:VIRTUAL_ENV
            $activeVenv = $true

            # ForEach ($p in $pythonVersions)
            # {
            #     $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
            #     if ($null -ne $pythonCmd)
            #     {
            #         $python = $p
            #         break
            #     }
            # }

            $pythonCmd = Get-Command 'python3' -ErrorAction 'SilentlyContinue'
            if ($null -ne $pythonCmd) {
                $python = 'python3'
            }
        } elseif (Test-Path $venvPath) {
            # 2. venv exists and is not active
            Log('Using existing venv')

            # activate the venv
            $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
            Log(". $activatePath")
            . $activatePath

            # ForEach ($p in $pythonVersions)
            # {
            #     $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
            #     if ($null -ne $pythonCmd)
            #     {
            #         $python = $p
            #         break
            #     }
            # }

            $pythonCmd = Get-Command 'python3' -ErrorAction 'SilentlyContinue'
            if ($null -ne $pythonCmd) {
                $python = 'python3'
            }
        } else {
            # 3. venv does not exist
            # ensure python3.9+ is installed
            ForEach ($p in $pythonVersions) {
                $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
                if ($null -ne $pythonCmd) {
                    $python = $p
                    break
                }
            }

            if (-not $python) {
                PrintError('You need to install python(>= 3.9)')
                return $false
            }

            Log('Creating new venv')

            # create a virtual env to run from and install to
            Log("$python -m venv venv")
            & $python -m venv venv

            # activate the virtual env
            $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
            Log(". $activatePath")
            . $activatePath
        }
    } else {
        Log('Not using venv')

        # ensure python3.9+ is installed
        ForEach ($p in $pythonVersions) {
            $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
            if ($null -ne $pythonCmd) {
                $python = $p
                break
            }
        }

        if (-not $python) {
            PrintError('You need to install python(>= 3.9)')
            return $false
        }
    }

    $pythonExePath = Get-Command $python | Select-Object -ExpandProperty Source
    Log("Using $python ($pythonExePath)")
    $pythonVersion = & $python -V | Select-String -Pattern '^Python'
    Log("Version: $pythonVersion")

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    $resourcesPath = Join-Path $pyVersionPath $pyVersionName 'data'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    # install dependencies in requirements.txt
    Log('pip3 install -r requirements.txt')
    pip3 install -r requirements.txt

    # check for success/failure
    $buildError = $false

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -eq 0) {
        Log('Build succeeded')
    } else {
        PrintError('Build failed')
        $buildError = $true
    }

    # if there was not an active venv before the build, deactivate the venv
    if ($useVenv -and -not $activeVenv) {
        # deactivate at end of setup process
        Log('deactivate')
        deactivate
    }

    if ($buildError) {
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $pyVersionPath 'bin' "${pyVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildRubyVersion
{
    param([string]$basePath, [string]$rbVersionName)

    Log('language: ruby')
    Log("version: $rbVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure ruby3.x is installed
    if (-not (Get-Command 'ruby' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install ruby')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $rubyVersion = & ruby -v 2>&1 | Select-String -Pattern '^ruby 3' 2>&1
    if (-not $rubyVersion) {
        PrintError('A version of ruby >= 3.x is required')
        $global:BUILD_LASTEXITCODE = 1
        return
    }
    Log("ruby version: $rubyVersion")

    # ensure bundler is installed
    if (-not (Get-Command 'bundle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install bundler: https://bundler.io/')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $bundleVersion = bundle version
    Log("bundle version: $bundleVersion")

    $rbVersionPath = Join-Path $basePath 'ruby' $rbVersionName
    Log("rbVersionPath: $rbVersionPath")

    if (-not (Test-Path $rbVersionPath)) {
        PrintError("Path not found: $rbVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    $resourcesPath = Join-Path $rbVersionPath 'data'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    # copy the shared test files to the local test resource location
    $testFilesPath = Join-Path $sharedPath 'testFiles'
    $testResourcesPath = Join-Path $rbVersionPath 'test' 'fixtures'
    if (-not (Test-Path $testResourcesPath)) {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources $testFilesPath $testResourcesPath

    $oldPwd = Get-Location
    Log("Set-Location $rbVersionPath")
    Set-Location $rbVersionPath

    Log("Building $rbVersionName")
    Log('bundle install')
    bundle install

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('bundle install failed')
        Set-Location $oldPwd
        return
    }

    # Build the gem
    Log("gem build ${rbVersionName}.gemspec")
    gem build "${rbVersionName}.gemspec"

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # TODO: install the gem?
    Log("gem install ${rbVersionName}.1.0.gem")
    gem install "${rbVersionName}.1.0.gem"

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $rbVersionPath 'bin' "${rbVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildRustVersion
{
    param([string]$basePath, [string]$rsVersionName)

    Log('language: rust')
    Log("version: $rsVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure rust is installed
    if (-not (Get-Command 'rustc' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install rust')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $rustVersion = rustc --version | Select-String -Pattern 'rustc'
    Log("rustc version: $rustVersion")

    # ensure cargo is installed
    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install cargo')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $cargoVersion = cargo --version | Select-String -Pattern 'cargo'
    Log("cargo version: $cargoVersion")

    $rsVersionPath = Join-Path $basePath 'rust' $rsVersionName
    Log("rsVersionPath: $rsVersionPath")

    if (-not (Test-Path $rsVersionPath)) {
        PrintError("Path not found: $rsVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $rsVersionPath")
    Set-Location $rsVersionPath

    Log("Building $rsVersionName")

    if ($debug) {
        Log('cargo build')
        cargo build

        $global:BUILD_LASTEXITCODE = $LASTEXITCODE
        if ($global:BUILD_LASTEXITCODE -ne 0) {
            # PrintError('Build failed')
            Set-Location $oldPwd
            return
        }
    }

    $binPath = Join-Path $basePath 'bin'
    $scriptPath = ''

    if ($release) {
        Log('cargo build --release')
        cargo build --release

        $global:BUILD_LASTEXITCODE = $LASTEXITCODE
        if ($global:BUILD_LASTEXITCODE -ne 0) {
            # PrintError('Build failed')
            Set-Location $oldPwd
            return
        }

        # add release to bin
        $scriptPath = Join-Path $rsVersionPath 'bin' "${rsVersionName}.release.ps1"
    } else {
        # add debug to bin
        $scriptPath = Join-Path $rsVersionPath 'bin' "${rsVersionName}.debug.ps1"
    }

    Set-Location $oldPwd

    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildScalaVersion
{
    param([string]$basePath, [string]$scalaVersionName)

    Log('language: scala')
    Log("version: $scalaVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure scalac is installed
    if (-not (Get-Command 'scala' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install scala')
        $global:BUILD_LASTEXITCODE = 1
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
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # $sbtOutput = sbt --version

    # $sbtProjectVersion = $sbtOutput | Select-String -Pattern 'project'
    # Log("$sbtProjectVersion")

    # $sbtScriptVersion = $sbtOutput | Select-String -Pattern 'script'
    # Log("$sbtScriptVersion")

    $jdkVersion = java -version 2>&1 | Select-Object -First 1
    Log("JDK version: $jdkVersion")

    $scalaVersionPath = Join-Path $basePath 'scala' $scalaVersionName
    Log("scalaVersionPath: $scalaVersionPath")

    if (-not (Test-Path $scalaVersionPath)) {
        PrintError("Path not found: $scalaVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # copy the shared json files to the local resource location
    $sharedPath = Join-Path $basePath 'shared'
    $resourcesPath = Join-Path $scalaVersionPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources $sharedPath $resourcesPath

    # copy the test files to the local test resource location
    $testFilesPath = Join-Path $sharedPath 'testFiles'
    $testResourcesPath = Join-Path $scalaVersionPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath)) {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources $testFilesPath $testResourcesPath

    $oldPwd = Get-Location
    Log("Set-Location $scalaVersionPath")
    Set-Location $scalaVersionPath

    # run sbt assembly
    Log("Building $scalaVersionName")
    Log("sbt 'set test in assembly := {}' clean package assembly")
    sbt 'set test in assembly := {}' clean package assembly

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $scalaVersionPath 'bin' "${scalaVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath

    Set-Location $oldPwd

    $global:BUILD_LASTEXITCODE = 0
}

function BuildSwiftVersion
{
    param([string]$basePath, [string]$swiftVersionName)

    Log('language: swift')
    Log("version: $swiftVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install swift')
        $global:BUILD_LASTEXITCODE = 1
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
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $oldPwd = Get-Location
    Log("Set-Location $swiftVersionPath")
    Set-Location $swiftVersionPath

    Log("Building $swiftVersionName")

    if ($debug) {
        Log('swift build')
        swift build

        $global:BUILD_LASTEXITCODE = $LASTEXITCODE
        if ($global:BUILD_LASTEXITCODE -ne 0) {
            # PrintError('Build failed')
            Set-Location $oldPwd
            return
        }
    }

    $binPath = Join-Path $basePath 'bin'
    $scriptPath = ''

    if ($release) {
        Log('swift build --configuration release')
        swift build --configuration release

        $global:BUILD_LASTEXITCODE = $LASTEXITCODE
        if ($global:BUILD_LASTEXITCODE -ne 0) {
            # PrintError('Build failed')
            Set-Location $oldPwd
            return
        }

        # add release to bin
        $scriptPath = Join-Path $swiftVersionPath 'bin' "${swiftVersionName}.release.ps1"
    } else {
        # add debug to bin
        $scriptPath = Join-Path $swiftVersionPath 'bin' "${swiftVersionName}.debug.ps1"
    }

    Set-Location $oldPwd

    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}

function BuildTypescriptVersion
{
    param([string]$basePath, [string]$tsVersionName)

    Log('language: typescript')
    Log("version: $tsVersionName")

    # start with non-error code
    $global:BUILD_LASTEXITCODE = 0

    # ensure node is installed
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install node.js')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install npm')
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    $tsVersionPath = Join-Path $basePath 'typescript' $tsVersionName
    Log("tsVersionPath: $tsVersionPath")

    if (-not (Test-Path $tsVersionPath)) {
        PrintError("Path not found: $tsVersionPath")
        $global:BUILD_LASTEXITCODE = 1
        return
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $tsVersionPath 'data'
    if (-not (Test-Path $resourcesPath)) {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Log("Set-Location $tsVersionPath")
    Set-Location $tsVersionPath

    # run npm install and build
    Log("Building $tsVersionName")
    Log('npm install')
    npm install

    Log('npm run build')
    npm run build

    $global:BUILD_LASTEXITCODE = $LASTEXITCODE
    if ($global:BUILD_LASTEXITCODE -ne 0) {
        # PrintError('Build failed')
        Set-Location $oldPwd
        return
    }

    Set-Location $oldPwd

    # add to bin
    $binPath = Join-Path $basePath 'bin'
    $scriptPath = Join-Path $tsVersionPath 'bin' "${tsVersionName}.ps1"
    Log("AddToBin $binPath $scriptPath")
    AddToBin $binPath $scriptPath
}
