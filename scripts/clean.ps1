#!/usr/bin/env pwsh
################################################################################
#
# clean.ps1
#
# Runs a clean (remove generated files) for each language version
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

Log("help: $help")
Log("all: $all")
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
    Write-Host "`nUsage: clean.ps1 [-help] {""all"" | lang [lang...]}`n"
    exit
}

function CleanJsonResources
{
    param([string]$resourcesPath)
    $resourceFiles = Get-ChildItem $resourcesPath -Depth 0 | Where-Object {!$_.PsIsContainer -and $_.Extension -eq '.json'}
    ForEach ($f in $resourceFiles)
    {
        Log("Remove-Item $f")
        Remove-Item $f
    }
}

function CleanTestResources
{
    param([string]$resourcesPath)
    $resourceFiles = Get-ChildItem $resourcesPath -Depth 0 | Where-Object {!$_.PsIsContainer -and $_.Name -like "testFile*" -and $_.Extension -eq '.txt'}
    ForEach ($f in $resourceFiles)
    {
        Log("Remove-Item $f")
        Remove-Item $f
    }

function PrintFailedBuilds
{
    if ($global:failedBuilds.Length -gt 0)
    {
        Write-Host "`nFailed builds:"
        ForEach ($fb in $global:failedBuilds)
        {
            Write-Host $fb
        }
    }
    else
    {
        Write-Host "`nAll builds succeeded"
    }
}


################################################################################
# Clean functions
################################################################################

function CleanBashFind
{
    Write-Host
    Hdr('CleanBashFind')
    Log('Nothing to do for bash')
}

function CleanCFind
{
    Write-Host
    Hdr('CleanCFind')

    $oldPwd = Get-Location
    Set-Location $cFindPath

    $cmakeBuildDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('cmake-build-')}
    ForEach ($c in $cmakeBuildDirs)
    {
        if (Test-Path $c)
        {
            Log("Remove-Item $c -Recurse -Force")
            $global:failedBuilds += 'cfind'
            Remove-Item $c -Recurse -Force
        }
    }

    Set-Location $oldPwd
}

function CleanCljFind
{
    Write-Host
    Hdr('CleanCljFind')

    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        $global:failedBuilds += 'cljfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $cljFindPath

    Log('lein clean')
    lein clean

    $resourcesPath = Join-Path $cljFindPath 'resources'
    CleanJsonResources($resourcesPath)

    Set-Location $oldPwd
}

function CleanCppFind
{
    Write-Host
    Hdr('CleanCppFind')

    $oldPwd = Get-Location
    Set-Location $cppFindPath

    $cmakeBuildDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('cmake-build-')}
    ForEach ($c in $cmakeBuildDirs)
    {
        if (Test-Path $c)
        {
            Log("Remove-Item $c -Recurse -Force")
            Remove-Item $c -Recurse -Force
        }
    }

    Set-Location $oldPwd
}

function CleanCsFind
{
    Write-Host
    Hdr('CleanCsFind')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'csfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $csFindPath

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    Log("dotnet clean -v minimal")
    dotnet clean -v minimal

    $csFindProjectDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('CsFind')}
    ForEach ($p in $csFindProjectDirs)
    {
        $binDir = Join-Path $p.FullName 'bin'
        if (Test-Path $binDir)
        {
            Log("Remove-Item $binDir -Recurse -Force")
            Remove-Item $binDir -Recurse -Force
        }
        $objDir = Join-Path $p.FullName 'obj'
        if (Test-Path $objDir)
        {
            Log("Remove-Item $objDir -Recurse -Force")
            Remove-Item $objDir -Recurse -Force
        }
    }

    $resourcesPath = Join-Path $csFindPath 'CsFindLib' 'Resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $csFindPath 'CsFindTests' 'Resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanDartFind
{
    Write-Host
    Hdr('CleanDartFind')

    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        $global:failedBuilds += 'dartfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $dartFindPath

    Log('dart pub cache repair')
    dart pub cache repair

    Set-Location $oldPwd
}

function CleanExFind
{
    Write-Host
    Hdr('CleanExFind')

    # ensure elixir is installed
    if (-not (Get-Command 'elixir' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install elixir')
        $global:failedBuilds += 'exfind'
        return
    }

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mix')
        $global:failedBuilds += 'exfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $exFindPath

    Log('mix clean')
    mix clean

    Set-Location $oldPwd
}

function CleanFsFind
{

    Write-Host
    Hdr('CleanFsFind')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'fsfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $fsFindPath

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    Log("dotnet clean -v minimal")
    dotnet clean -v minimal

    $fsFindProjectDirs = Get-ChildItem $fsFindPath -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('FsFind')}
    ForEach ($p in $fsFindProjectDirs)
    {
        $binDir = Join-Path $p.FullName 'bin'
        if (Test-Path $binDir)
        {
            Log("Remove-Item $binDir -Recurse -Force")
            Remove-Item $binDir -Recurse -Force
        }
        $objDir = Join-Path $p.FullName 'obj'
        if (Test-Path $objDir)
        {
            Log("Remove-Item $objDir -Recurse -Force")
            Remove-Item $objDir -Recurse -Force
        }
    }

    $resourcesPath = Join-Path $fsFindPath 'FsFindLib' 'Resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $fsFindPath 'FsFindTests' 'Resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanGoFind
{
    Write-Host
    Hdr('CleanGoFind')

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        $global:failedBuilds += 'gofind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $goFindPath

    Log('go clean')
    go clean

    Set-Location $oldPwd
}

function CleanGroovyFind
{
    Write-Host
    Hdr('CleanGroovyFind')

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
        $global:failedBuilds += 'groovyfind'
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $resourcesPath = Join-Path $groovyFindPath 'src' 'main' 'resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $groovyFindPath 'src' 'test' 'resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanHsFind
{
    Write-Host
    Hdr('CleanHsFind')

    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        $global:failedBuilds += 'hsfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $hsFindPath

    Log('stack clean')
    stack clean

    $resourcesPath = Join-Path $hsFindPath 'data'
    CleanJsonResources($resourcesPath)

    Set-Location $oldPwd
}

function CleanJavaFind
{
    Write-Host
    Hdr('CleanJavaFind')

    $oldPwd = Get-Location
    Set-Location $javaFindPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        $global:failedBuilds += 'javafind'
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $resourcesPath = Join-Path $javaFindPath 'src' 'main' 'resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $javaFindPath 'src' 'test' 'resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanJsFind
{
    Write-Host
    Hdr('CleanJsFind')

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        $global:failedBuilds += 'jsfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $jsFindPath

    Log('npm run clean')
    npm run clean

    $resourcesPath = Join-Path $jsFindPath 'data'
    CleanJsonResources($resourcesPath)

    Set-Location $oldPwd
}

function CleanKtFind
{
    Write-Host
    Hdr('CleanKtFind')

    $oldPwd = Get-Location
    Set-Location $ktFindPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue')) {
        PrintError('You need to install gradle')
        $global:failedBuilds += 'ktfind'
        Set-Location $oldPwd
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $resourcesPath = Join-Path $ktFindPath 'src' 'main' 'resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $ktFindPath 'src' 'test' 'resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanObjcFind
{
    Write-Host
    Hdr('CleanObjcFind')

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'objcfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $objcFindPath

    Log("swift package clean")
    swift package clean

    Set-Location $oldPwd
}

function CleanMlFind
{
    Write-Host
    Hdr('CleanMlFind')
    Log('not implemented at this time')
}

function CleanPlFind
{
    Write-Host
    Hdr('CleanPlFind')

    $resourcesPath = Join-Path $plFindPath 'share'
    CleanJsonResources($resourcesPath)
}

function CleanPhpFind
{
    Write-Host
    Hdr('CleanPhpFind')

    $resourcesPath = Join-Path $phpFindPath 'resources'
    CleanJsonResources($resourcesPath)
}

function CleanPs1Find
{
    Write-Host
    Hdr('CleanPs1Find')
    Log('Nothing to do for powershell')
}

function CleanPyFind
{
    Write-Host
    Hdr('CleanPyFind')

    $resourcesPath = Join-Path $pyFindPath 'pyfind' 'data'
    CleanJsonResources($resourcesPath)
}

function CleanRbFind
{
    Write-Host
    Hdr('CleanRbFind')

    $resourcesPath = Join-Path $rbFindPath 'data'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $rbFindPath 'test' 'fixtures'
    CleanTestResources($testResourcesPath)
}

function CleanRsFind
{
    Write-Host
    Hdr('CleanRsFind')

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo')
        $global:failedBuilds += 'rsfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $rsFindPath

    Log('cargo clean')
    cargo clean

    Set-Location $oldPwd
}

function CleanScalaFind
{
    Write-Host
    Hdr('CleanScalaFind')

    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install scala + sbt')
        $global:failedBuilds += 'scalafind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $scalaFindPath

    Log('sbt clean')
    sbt clean

    $resourcesPath = Join-Path $scalaFindPath 'src' 'main' 'resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $scalaFindPath 'src' 'test' 'resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanSwiftFind
{
    Write-Host
    Hdr('CleanSwiftFind')

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'swiftfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $swiftFindPath

    Log("swift package clean")
    swift package clean

    Set-Location $oldPwd
}

function CleanTsFind
{
    Write-Host
    Hdr('CleanTsFind')

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        $global:failedBuilds += 'tsfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $tsFindPath

    Log('npm run clean')
    npm run clean

    $resourcesPath = Join-Path $tsFindPath 'data'
    CleanJsonResources($resourcesPath)

    Set-Location $oldPwd
}

function CleanLinux
{
    Write-Host
    Hdr('CleanLinux')

    CleanBashFind

    CleanCFind

    # CleanCljFind

    # CleanCppFind

    CleanCsFind

    CleanDartFind

    CleanFsFind

    CleanGoFind

    # CleanGroovyFind

    # CleanHsFind

    CleanJavaFind

    CleanJsFind

    CleanKtFind

    # CleanObjcFind

    # CleanMlFind

    CleanPlFind

    CleanPhpFind

    CleanPyFind

    CleanRbFind

    CleanRsFind

    # CleanScalaFind

    CleanSwiftFind

    CleanTsFind

    PrintFailedBuilds

    exit
}

function CleanAll
{
    Write-Host
    Hdr('CleanAll')

    CleanBashFind

    CleanCFind

    CleanCljFind

    CleanCppFind

    CleanCsFind

    CleanDartFind

    CleanFsFind

    CleanGoFind

    CleanGroovyFind

    CleanHsFind

    CleanJavaFind

    CleanJsFind

    CleanKtFind

    CleanObjcFind

    CleanMlFind

    CleanPlFind

    CleanPhpFind

    CleanPs1Find

    CleanPyFind

    CleanRbFind

    CleanRsFind

    CleanScalaFind

    CleanSwiftFind

    CleanTsFind

    PrintFailedBuilds

    exit
}

################################################################################
# Main function
################################################################################

function CleanMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        CleanAll
        exit
    }

    ForEach ($lang in $langs)
    {
        switch ($lang)
        {
            'linux'      { CleanLinux }
            'bash'       { CleanBashFind }
            'c'          { CleanCFind }
            'clj'        { CleanCljFind }
            'clojure'    { CleanCljFind }
            'cpp'        { CleanCppFind }
            'cs'         { CleanCsFind }
            'csharp'     { CleanCsFind }
            'dart'       { CleanDartFind }
            'elixir'     { CleanExFind }
            'ex'         { CleanExFind }
            'fs'         { CleanFsFind }
            'fsharp'     { CleanFsFind }
            'go'         { CleanGoFind }
            'groovy'     { CleanGroovyFind }
            'haskell'    { CleanHsFind }
            'hs'         { CleanHsFind }
            'java'       { CleanJavaFind }
            'javascript' { CleanJsFind }
            'js'         { CleanJsFind }
            'kotlin'     { CleanKtFind }
            'kt'         { CleanKtFind }
            'objc'       { CleanObjcFind }
            # 'ocaml'      { CleanMlFind }
            # 'ml'         { CleanMlFind }
            'perl'       { CleanPlFind }
            'pl'         { CleanPlFind }
            'php'        { CleanPhpFind }
            'powershell' { CleanPs1Find }
            'ps1'        { CleanPs1Find }
            'pwsh'       { CleanPs1Find }
            'py'         { CleanPyFind }
            'python'     { CleanPyFind }
            'rb'         { CleanRbFind }
            'ruby'       { CleanRbFind }
            'rs'         { CleanRsFind }
            'rust'       { CleanRsFind }
            'scala'      { CleanScalaFind }
            'swift'      { CleanSwiftFind }
            'ts'         { CleanTsFind }
            'typescript' { CleanTsFind }
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
        CleanAll
    }
    
    CleanMain $langs    
}
catch {
    PrintError($_.Exception.Message)
}
finally {
    Set-Location $oldPwd
}
