################################################################################
#
# Ps1FindModule.psm1
#
# A single module file for ps1find
#
# install module under $env:PSModulePath
#
################################################################################

#region Config
########################################
# Config
########################################
if (-not (Test-Path env:XFIND_PATH)) { $env:XFIND_PATH = Join-Path -Path $HOME -ChildPath 'src' -AdditionalChildPath 'xfind' }
$xfindPath = $env:XFIND_PATH
$sharedPath = Join-Path -Path $xfindPath -ChildPath 'shared'
$fileTypesPath = Join-Path -Path $sharedPath -ChildPath 'filetypes.json'
$findOptionsPath = Join-Path -Path $sharedPath -ChildPath 'findoptions.json'
#endregion


#region FileUtil
########################################
# FileUtil
########################################
$dotPaths = @('.', '..')

function IsDotDir {
    param([System.IO.FileSystemInfo]$f)

    return $dotPaths.Contains($f.Name)
}

function IsHiddenFile {
    [OutputType([bool])]
    param([System.IO.FileSystemInfo]$f)

    return ($f.Attributes.HasFlag([System.IO.FileAttributes]::Hidden)) -or
        ($f.Name.StartsWith('.') -and (-not (IsDotDir $f.Name)))
}

function IsReadableFile {
    [OutputType([bool])]
    param([System.IO.FileSystemInfo]$f)

    $readable = $false
    try {
        [System.IO.File]::OpenRead($f).Close()
        $readable = $true
    } catch {
        $readable = $false
    }
    return $readable
}
#endregion


#region FileTypes
########################################
# FileTypes
########################################
enum FileType {
    Unknown
    Archive
    Binary
    Code
    Text
    Xml
}

function GetFileTypeFromName {
    [OutputType([FileType])]
    param([string]$name)

    switch ($name.ToUpper())
    {
        'ARCHIVE' {return [FileType]::Archive}
        'BINARY'  {return [FileType]::Binary}
        'CODE'    {return [FileType]::Code}
        'TEXT'    {return [FileType]::Text}
        'XML'     {return [FileType]::Xml}
    }
    return [FileType]::Unknown
}

class FileTypes {
    $FileTypeExtMap = @{}
    $FileTypeNameMap = @{}

    FileTypes() {
        $this.LoadFileTypesFromJson()
    }

    [void]LoadFileTypesFromJson() {
        $fileTypesHash = Get-Content $script:fileTypesPath | ConvertFrom-Json -AsHashtable
        if ($fileTypesHash.ContainsKey('filetypes')) {
            foreach ($fileTypeObj in $fileTypesHash['filetypes']) {
                $fileType = $fileTypeObj['type']
                $exts = $fileTypeObj['extensions'] | ForEach-Object { ".$_" }
                $this.FileTypeExtMap[$fileType] = $exts
                if ($fileTypeObj.ContainsKey('names')) {
                    $this.FileTypeNameMap[$fileType] = $fileTypeObj['names']
                } else {
                    $this.FileTypeNameMap[$fileType] = @()
                }
            }
        } else {
            throw "Missing filetypes in JSON"
        }
    }

    [FileType]GetFileType([System.IO.FileInfo]$fileInfo) {
        if ($this.IsCodeFile($fileInfo)) {
            return [FileType]::Code
        }
        if ($this.IsXmlFile($fileInfo)) {
            return [FileType]::Xml
        }
        if ($this.IsTextFile($fileInfo)) {
            return [FileType]::Text
        }
        if ($this.IsBinaryFile($fileInfo)) {
            return [FileType]::Binary
        }
        if ($this.IsArchiveFile($fileInfo)) {
            return [FileType]::Archive
        }
        return [FileType]::Unknown
    }

    [bool]IsArchiveFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['archive'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['archive']
    }

    [bool]IsBinaryFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['binary'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['binary']
    }

    [bool]IsCodeFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['code'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['code']
    }

    [bool]IsSearchableFile([System.IO.FileInfo]$fileInfo) {
        return $this.GetFileType($fileInfo) -ne [FileType]::Unknown
    }

    [bool]IsTextFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['text'] -or
            $fileInfo.Name -in $this.FileTypeNameMap['text']
    }

    [bool]IsUnknownFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['unknown'] -or
            $this.GetFileType($fileInfo) -eq [FileType]::Unknown
    }

    [bool]IsXmlFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['xml'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['xml']
    }
}
#endregion


#region FindSettings
########################################
# FindSettings
########################################
enum SortBy {
    FilePath
    FileName
    FileType
}

function GetSortByFromName {
    [OutputType([SortBy])]
    param([string]$name)

    switch ($name.ToUpper())
    {
        'PATH' {return [SortBy]::FilePath}
        'NAME' {return [SortBy]::FileName}
        'TYPE' {return [SortBy]::FileType}
    }
    return [SortBy]::FilePath
}

class FindSettings {
    [bool]$ArchivesOnly
    [bool]$Debug
    [bool]$ExcludeHidden
    [string[]]$InArchiveExtensions
    [Regex[]]$InArchiveFilePatterns
    [Regex[]]$InDirPatterns
    [string[]]$InExtensions
    [Regex[]]$InFilePatterns
    [FileType[]]$InFileTypes
    [bool]$IncludeArchives
    [bool]$ListDirs
    [bool]$ListFiles
    [string[]]$OutArchiveExtensions
    [Regex[]]$OutArchiveFilePatterns
    [Regex[]]$OutDirPatterns
    [string[]]$OutExtensions
    [Regex[]]$OutFilePatterns
    [FileType[]]$OutFileTypes
    [string[]]$Paths
    [bool]$PrintUsage
    [bool]$PrintVersion
    [bool]$Recursive
    [SortBy]$SortBy
    [bool]$SortDescending
    [bool]$Verbose

    FindSettings() {
		$this.ArchivesOnly = $false
		$this.Debug = $false
		$this.ExcludeHidden = $true
		$this.InArchiveExtensions = @()
		$this.InArchiveFilePatterns = @()
		$this.InDirPatterns = @()
		$this.InExtensions = @()
		$this.InFilePatterns = @()
		$this.InFileTypes = @()
		$this.IncludeArchives = $false
		$this.ListDirs = $false
		$this.ListFiles = $false
		$this.OutArchiveExtensions = @()
		$this.OutArchiveFilePatterns = @()
		$this.OutDirPatterns = @()
		$this.OutExtensions = @()
		$this.OutFilePatterns = @()
		$this.OutFileTypes = @()
		$this.Paths = @()
		$this.PrintUsage = $false
		$this.PrintVersion = $false
		$this.Recursive = $true
		$this.SortBy = [SortBy]::FilePath
		$this.SortDescending = $false
		$this.Verbose = $false
    }

    [string[]]GetExtensions([string]$ext) {
        return [regex]::split($ext, "\W+") |
            Where-Object { $_ -ne '' } |
            ForEach-Object { if ($_.StartsWith('.')) {$_} else {".$_"} }
    }

    [void]SetArchivesOnly([bool]$b) {
        $this.ArchivesOnly = $b
        if ($b) {
            $this.IncludeArchives = $b
        }
    }

    [void]SetDebug([bool]$b) {
        $this.Debug = $b
        if ($b) {
            $this.Verbose = $b
        }
    }

    [string]StringArrayToString([string[]]$arr) {
        if ($arr.Length -eq 0) {
            return '[]'
        }
        return '["' + ($arr -join '", "' ) + '"]'
    }

    [string]FileTypeArrayToString([FileType[]]$arr) {
        if ($arr.Length -eq 0) {
            return '[]'
        }
        return '[' + ($arr -join ', ' ) + ']'
    }

    [string]ToString() {
        return "FindSettings(" +
            "ArchivesOnly: $($this.ArchivesOnly)" +
            ", Debug: $($this.Debug)" +
            ", ExcludeHidden: $($this.ExcludeHidden)" +
            ", InArchiveExtensions: $($this.StringArrayToString($this.InArchiveExtensions))" +
            ", InArchiveFilePatterns: $($this.StringArrayToString($this.InArchiveFilePatterns))" +
            ", InDirPatterns: $($this.StringArrayToString($this.InDirPatterns))" +
            ", InExtensions: $($this.StringArrayToString($this.InExtensions))" +
            ", InFilePatterns: $($this.StringArrayToString($this.InFilePatterns))" +
            ", InFileTypes: $($this.FileTypeArrayToString($this.InFileTypes))" +
            ", IncludeArchives: $($this.IncludeArchives)" +
            ", ListDirs: $($this.ListDirs)" +
            ", ListFiles: $($this.ListFiles)" +
            ", OutArchiveExtensions: $($this.StringArrayToString($this.OutArchiveExtensions))" +
            ", OutArchiveFilePatterns: $($this.StringArrayToString($this.OutArchiveFilePatterns))" +
            ", OutDirPatterns: $($this.StringArrayToString($this.OutDirPatterns))" +
            ", OutExtensions: $($this.StringArrayToString($this.OutExtensions))" +
            ", OutFilePatterns: $($this.StringArrayToString($this.OutFilePatterns))" +
            ", OutFileTypes: $($this.FileTypeArrayToString($this.OutFileTypes))" +
            ", Paths: $($this.StringArrayToString($this.Paths))" +
            ", PrintUsage: $($this.PrintUsage)" +
            ", PrintVersion: $($this.PrintVersion)" +
            ", Recursive: $($this.Recursive)" +
            ", SortBy: $($this.SortBy)" +
            ", SortDescending: $($this.SortDescending)" +
            ", Verbose: $($this.Verbose)" +
            ")"
    }
}
#endregion


#region FindOptions
########################################
# FindOptions
########################################
class FindOption {
    [string]$ShortArg
    [string]$LongArg
    [string]$Desc
    [string]$SortArg

    FindOption([string]$ShortArg, [string]$LongArg, [string]$Desc) {
        $this.ShortArg = $ShortArg
        $this.LongArg = $LongArg
        $this.Desc = $Desc
        $this.SortArg = $this.GetSortArg()
    }

    [string]GetSortArg() {
        if ($this.ShortArg) {
            return "$($this.ShortArg.ToLower())a$($this.LongArg)"
        }
        return $this.LongArg
    }
}

class FindOptions {
    [FindOption[]]$Options = @()
    # $LongArgMap = @{}
    # instantiate this way to get case sensitivity of keys
    $LongArgMap = [system.collections.hashtable]::new()
    $ArgActionMap = @{
        "in-archiveext" = {
            param([string]$s, [FindSettings]$settings)
            $settings.InArchiveExtensions += $settings.GetExtensions($s)
        }
        "in-archivefilepattern" = {
            param([string]$s, [FindSettings]$settings)
            $settings.InArchiveFilePatterns += [regex]$s
        }
        "in-dirpattern" = {
            param([string]$s, [FindSettings]$settings)
            $settings.InDirPatterns += [regex]$s
        }
        "in-ext" = {
            param([string]$s, [FindSettings]$settings)
            $settings.InExtensions += $settings.GetExtensions($s)
        }
        "in-filepattern" = {
            param([string]$s, [FindSettings]$settings)
            $settings.InFilePatterns += [regex]$s
        }
        "in-filetype" = {
            param([string]$s, [FindSettings]$settings)
            $settings.InFileTypes += GetFileTypeFromName($s)
        }
        "out-archiveext" = {
            param([string]$s, [FindSettings]$settings)
            $settings.OutArchiveExtensions += $settings.GetExtensions($s)
        }
        "out-archivefilepattern" = {
            param([string]$s, [FindSettings]$settings)
            $settings.OutArchiveFilePatterns += [regex]$s
        }
        "out-dirpattern" = {
            param([string]$s, [FindSettings]$settings)
            $settings.OutDirPatterns += [regex]$s
        }
        "out-ext" = {
            param([string]$s, [FindSettings]$settings)
            $settings.OutExtensions += $settings.GetExtensions($s)
        }
        "out-filepattern" = {
            param([string]$s, [FindSettings]$settings)
            $settings.OutFilePatterns += [regex]$s
        }
        "out-filetype" = {
            param([string]$s, [FindSettings]$settings)
            $settings.OutFileTypes += GetFileTypeFromName($s)
        }
        "path" = {
            param([string]$s, [FindSettings]$settings)
            $settings.Paths += $s
        }
        "sort-by" = {
            param([string]$s, [FindSettings]$settings)
            $settings.SortBy = GetSortByFromName($s)
        }
    }
    $BoolFlagActionMap = @{
        "archivesonly" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.SetArchivesOnly($b)
        }
        "debug" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.SetDebug($b)
        }
        "excludearchives" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.IncludeArchives = !$b
        }
        "excludehidden" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.ExcludeHidden = $b
        }
        "help" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.PrintUsage = $b
        }
        "includearchives" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.IncludeArchives = $b
        }
        "includehidden" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.ExcludeHidden = !$b
        }
        "listdirs" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.ListDirs = $b
        }
        "listfiles" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.ListFiles = $b
        }
        "norecursive" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.Recursive = !$b
        }
        "recursive" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.Recursive = $b
        }
        "sort-ascending" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.SortDescending = !$b
        }
        "sort-descending" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.SortDescending = $b
        }
        "verbose" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.Verbose = $b
        }
        "version" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.PrintVersion = $b
        }
    }

    FindOptions() {
        $this.Options = $this.LoadOptionsFromJson()
    }

    [FindOption[]]LoadOptionsFromJson() {
        $optionsHash = Get-Content $script:findOptionsPath | ConvertFrom-Json -AsHashtable
        if (-not $optionsHash.ContainsKey('findoptions')) {
            throw "Missing findoptions in JSON"
        }
        $opts = @(foreach ($optionObj in $optionsHash['findoptions']) {
            $ShortArg = ''
            $LongArg = $optionObj['long']
            $Desc = $optionObj['desc']
            $this.LongArgMap[$LongArg] = $LongArg
            if ($optionObj.ContainsKey('short')) {
                $ShortArg = $optionObj['short']
                $this.LongArgMap[$ShortArg] = $LongArg
            }
            [FindOption]::new($ShortArg, $LongArg, $Desc)
        })
        return $opts | Sort-Object -Property SortArg
    }

    [FindSettings]SettingsFromArgs([string[]]$argList) {
        $settings = [FindSettings]::new()
        # default ListFiles to true since we're using via CLI
        $settings.ListFiles = $true
        $idx = 0
        while ($idx -lt $argList.Count) {
            if ($settings.PrintUsage -or $settings.PrintVersion) {
                return $settings;
            }
            $arg = $argList[$idx]
            if ($arg.StartsWith('-')) {
                while ($arg.StartsWith('-')) {
                    $arg = $arg.Substring(1)
                }
                if (-not $this.LongArgMap.ContainsKey($arg)) {
                    throw "Invalid option: $arg"
                }
                $longArg = $this.LongArgMap[$arg]
                if ($this.ArgActionMap.ContainsKey($longArg)) {
                    $idx++
                    if ($idx -lt $argList.Count) {
                        $this.ArgActionMap[$longArg].Invoke($argList[$idx], $settings)
                    } else {
                        throw "Missing value for $arg"
                    }
                } elseif ($this.BoolFlagActionMap.ContainsKey($longArg)) {
                    $this.BoolFlagActionMap[$longArg].Invoke($true, $settings)
                } else {
                    throw "Invalid option: $arg"
                }
            } else {
                $settings.Paths += $arg
            }
            $idx++
        }
        return $settings
    }

    [string]GetUsageString() {
        $usage = "`nUsage:`n ps1find [options] <path> [<path> ...]`n`nOptions:`n";
        $optStrs = @()
        $optMap = @{}
        $longest = 0
        foreach ($option in $this.Options) {
            $optStr = ''
            if ($option.ShortArg) {
                $optStr = "-$($option.ShortArg),"
            }
            $optStr += "--$($option.LongArg)"
            if ($optStr.Length -gt $longest) {
                $longest = $optStr.Length
            }
            $optStrs += $optStr
            $optMap[$optStr] = $option.Desc
        }
        $formatStr = " {0,-$($longest)}  {1}`n"
        foreach ($optStr in $optStrs) {
            $usage += $formatStr -f $optStr,$optMap[$optStr]
        }
        return $usage;
    }
}
#endregion


#region FileResult
########################################
# FileResult
########################################
class FileResult {
    [string[]]$Containers
    [System.IO.FileInfo]$File
    [FileType]$Type

    FileResult([System.IO.FileInfo]$File, [FileType]$Type) {
        $this.Containers = @()
        $this.File = $File
        $this.Type = $Type
    }
}
#endregion


#region Finder
########################################
# Finder
########################################
class Finder {
    [FindSettings]$settings
    [FileTypes]$fileTypes
    [Scriptblock[]]$dirTests
    [Scriptblock[]]$fileTests
    [Scriptblock[]]$archiveFileTests

    Finder([FindSettings]$settings) {
        $this.settings = $settings
        $this.ValidateSettings()
        $this.fileTypes = [FileTypes]::new()
        $this.dirTests = $this.GetMatchingDirTests()
        $this.fileTests = $this.GetMatchingFileTests()
        $this.archiveFileTests = $this.GetMatchingArchiveFileTests()
    }

    [void]ValidateSettings() {
        if ($null -eq $this.settings.Paths -or $this.settings.Paths.Count -eq 0) {
            throw "Startpath not defined"
        }
        foreach ($path in $this.settings.Paths) {
            if (-not (Test-Path $path)) {
                throw "Startpath not found"
            }
        }
    }

    [bool]MatchesAnyPattern([string]$s, [regex[]]$patterns) {
        return @($patterns | Where-Object { $s -match $_ }).Count -gt 0
    }

    [Scriptblock[]]GetMatchingDirTests() {
        $tests = @()
        if ($this.settings.ExcludeHidden) {
            $tests += {
                param([System.IO.DirectoryInfo]$d)
                return !(IsHiddenFile $d)
            }
        }
        if ($this.settings.InDirPatterns.Count -gt 0) {
            $tests += {
                param([System.IO.DirectoryInfo]$d)
                return $this.MatchesAnyPattern($d.FullName, $this.settings.InDirPatterns)
            }
        } elseif ($this.settings.OutDirPatterns.Count -gt 0) {
            $tests += {
                param([System.IO.DirectoryInfo]$d)
                return !$this.MatchesAnyPattern($d.FullName, $this.settings.OutDirPatterns)
            }
        }
        return $tests
    }

    [bool]IsMatchingDir([System.IO.DirectoryInfo]$d) {
        return @($this.dirTests | Where-Object { $_.Invoke($d) }).Count -eq $this.dirTests.Count
    }

    [Scriptblock[]]GetMatchingArchiveFileTests() {
        $tests = @()
        if ($this.settings.InArchiveExtensions.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.settings.InArchiveExtensions.Contains($f.File.Extension)
            }
        } elseif ($this.settings.OutArchiveExtensions.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.settings.OutArchiveExtensions.Contains($f.File.Extension)
            }
        }
        if ($this.settings.InArchiveFilePatterns.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.MatchesAnyPattern($f.File.Name, $this.settings.InArchiveFilePatterns)
            }
        } elseif ($this.settings.OutArchiveFilePatterns.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.MatchesAnyPattern($f.File.Name, $this.settings.OutArchiveFilePatterns)
            }
        }
        return $tests
    }

    [bool]IsMatchingArchiveFile([FileResult]$f) {
        return @($this.archiveFileTests | Where-Object { $_.Invoke($f) }).Count -eq $this.archiveFileTests.Count
    }

    [Scriptblock[]]GetMatchingFileTests() {
        $tests = @()
        if ($this.settings.InExtensions.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.settings.InExtensions.Contains($f.File.Extension)
            }
        } elseif ($this.settings.OutExtensions.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.settings.OutExtensions.Contains($f.File.Extension)
            }
        }
        if ($this.settings.InFilePatterns.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.MatchesAnyPattern($f.File.Name, $this.settings.InFilePatterns)
            }
        } elseif ($this.settings.OutFilePatterns.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.MatchesAnyPattern($f.File.Name, $this.settings.OutFilePatterns)
            }
        }
        if ($this.settings.InFileTypes.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.settings.InFileTypes.Contains($f.Type)
            }
        } elseif ($this.settings.OutFileTypes.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.settings.OutFileTypes.Contains($f.Type)
            }
        }
        return $tests
    }

    [bool]IsMatchingFile([FileResult]$f) {
        return @($this.fileTests | Where-Object { $_.Invoke($f) }).Count -eq $this.fileTests.Count
    }

    [FileResult]FilterToFileResult([System.IO.FileInfo]$file) {
        $fileResult = [FileResult]::new($file, $this.fileTypes.GetFileType($file))
        if ($fileResult.Type -eq [FileType]::Archive) {
            if ($this.settings.IncludeArchives -and $this.IsMatchingArchiveFile($fileResult)) {
                return $fileResult
            }
            return $null
        }
        if (-not $this.settings.ArchivesOnly -and $this.IsMatchingFile($fileResult)) {
            return $fileResult
        }
        return $null
    }

    [FileResult[]]GetFileResults() {
        $fileResults = @()
        $checkedDirs = @{}
        # define the scriptblock to check each file's directory
        $checkDir = {
            $dir = $_.Directory
            if ($null -eq $dir) {
                return $true
            }
            if ($checkedDirs.ContainsKey($dir.FullName)) {
                return $checkedDirs[$dir.FullName]
            }
            $isMatchingDir = $this.IsMatchingDir($dir)
            $checkedDirs[$dir.FullName] = $isMatchingDir
            return $isMatchingDir
        }
        # define the scriptblock that will get the FileResult objects for each path
        $getPathFileResults = {}
        if ($this.settings.Recursive -and $this.settings.ExcludeHidden) {
            $getPathFileResults = {
                param([string]$path)
                Get-ChildItem -Path $path -File -Recurse:$true |
                Where-Object -FilterScript $checkDir |
                ForEach-Object { $this.FilterToFileResult($_) } |
                Where-Object { $null -ne $_ }
            }
        } elseif ($this.settings.Recursive) {
            $getPathFileResults = {
                param([string]$path)
                Get-ChildItem -Path $path -File -Recurse:$true -Force |
                Where-Object -FilterScript $checkDir |
                ForEach-Object { $this.FilterToFileResult($_) } |
                Where-Object { $null -ne $_ }
            }
        } elseif ($this.settings.ExcludeHidden) {
            $getPathFileResults = {
                param([string]$path)
                Get-ChildItem -Path $path -File -Recurse:$false |
                ForEach-Object { $this.FilterToFileResult($_) } |
                Where-Object { $null -ne $_ }
            }
        } else {
            $getPathFileResults = {
                param([string]$path)
                Get-ChildItem -Path $path -File -Recurse:$false -Force |
                ForEach-Object { $this.FilterToFileResult($_) } |
                Where-Object { $null -ne $_ }
            }
        }

        foreach ($path in $this.settings.Paths) {
            if (Test-Path -Path $path -PathType Container) {
                $fileResults += $getPathFileResults.Invoke($path)
            } elseif (Test-Path -Path $path -PathType Leaf) {
                $pathFile = [System.IO.FileInfo]::new($path)
                $pathFileResult = [FileResult]::new($pathFile, $this.fileTypes.GetFileType($pathFile))
                if ($this.IsMatchingFile($pathFileResult)) {
                    $fileResults += $pathFileResult
                }
            } else {
                Write-Host "Not container or leaf: $path"
            }
        }
        return $fileResults
    }

    [FileResult[]]SortFileResults([FileResult[]]$fileResults) {
        $sorted = @()
        if ($this.settings.SortBy -eq [SortBy]::FileName) {
            $sorted = $fileResults |
                ForEach-Object {[Tuple]::Create($_.File.Name, $_.File.DirectoryName, $_)} |
                Sort-Object |
                ForEach-Object {$_[-1]}
        } elseif ($this.settings.SortBy -eq [SortBy]::FileType) {
            $sorted = $fileResults |
                ForEach-Object {[Tuple]::Create($_.Type, $_.File.DirectoryName, $_.File.Name, $_)} |
                Sort-Object |
                ForEach-Object {$_[-1]}
        } else {
            $sorted = $fileResults |
                ForEach-Object {[Tuple]::Create($_.File.DirectoryName, $_.File.Name, $_)} |
                Sort-Object |
                ForEach-Object {$_[-1]}
        }
        if ($this.settings.SortDescending) {
            [array]::Reverse($sorted)
        }
        return $sorted
    }

    [FileResult[]]Find() {
        return $this.SortFileResults($this.GetFileResults())
    }
}
#endregion
