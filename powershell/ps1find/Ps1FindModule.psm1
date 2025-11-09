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
if (-not (Test-Path env:XFIND_PATH)) {
    $env:XFIND_PATH = Join-Path -Path $HOME -ChildPath 'src' -AdditionalChildPath 'xfind'
}
$xfindPath = $env:XFIND_PATH
$sharedPath = Join-Path -Path $xfindPath -ChildPath 'shared'
$fileTypesPath = Join-Path -Path $sharedPath -ChildPath 'filetypes.json'
$findOptionsPath = Join-Path -Path $sharedPath -ChildPath 'findoptions.json'
#endregion


#region Color
########################################
# Color
########################################
#$esc = "`e" # Escape character
$esc = [char]27
$Reset =  "${esc}[0m";
$Black =  "${esc}[30m";
$Red =    "${esc}[31m";
$Green =  "${esc}[32m";
$Yellow = "${esc}[33m";
$Blue =   "${esc}[34m";
$Purple = "${esc}[35m";
$Cyan =   "${esc}[36m";
$White =  "${esc}[37m";
#endregion


#region Common
########################################
# Common
########################################
function LogMsg {
    param([string]$msg)

    # Write-Output $msg
    Write-Host "$msg"
}

function LogError {
    param([string]$msg)

    # Write-Output "ERROR: $msg`n"
    Write-Host "`nERROR: $msg" -ForegroundColor Red
}
#endregion


#region FileUtil
########################################
# FileUtil
########################################
$dotPaths = @('.', '..')

function GetHomePath {
#    return [Environment]::GetFolderPath([Environment+SpecialFolder]::UserProfile)
    return $HOME
}

function ExpandPath {
    [OutputType([string])]
    param([string]$filePath)
    if ($filePath.StartsWith('~')) {
        $userPath = GetHomePath
        if ($filePath -eq '~' -or $filePath -eq '~/' -or $filePath -eq '~\\') {
            return $userPath
        }
        if ($filePath.StartsWith('~/') -or $filePath.StartsWith('~\\')) {
            return Join-Path -Path $userPath -ChildPath $filePath.Substring(2)
        }
        $homePath = Split-Path -parent $userPath
        return Join-Path -Path $homePath -ChildPath $filePath.Substring(1)
    }
    return $filePath
}

function IsDotDir {
    [OutputType([bool])]
    param([string]$dirName)
    return $dotPaths.Contains($dirName)
}

function IsHiddenName {
    [OutputType([bool])]
    param([string]$name)
    return ($name.Length -gt 1 -and $name.StartsWith('.') -and (-not ($dotPaths.Contains($name))))
}

#function IsHiddenFile {
#    [OutputType([bool])]
#    param([System.IO.FileSystemInfo]$f)
#    return ($f.Attributes.HasFlag([System.IO.FileAttributes]::Hidden)) -or
#            (IsHiddenName $f.Name)
#}

function IsHiddenDirectory {
    [OutputType([bool])]
    param([System.IO.DirectoryInfo]$d)
    if (isHiddenName($d.Name)) {
        return $true
    }
    $parent = $d.Parent
    while ($null -ne $parent) {
        if (isHiddenName($parent.Name)) {
            return $true
        }
        $parent = $parent.Parent
    }
    return $false
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

function PathElems {
    [OutputType([int])]
    param([string]$path)
    ($path -split [System.IO.Path]::DirectorySeparatorChar).Count
}
#endregion


#region FileTypes
########################################
# FileTypes
########################################
enum FileType {
    Unknown
    Archive
    Audio
    Binary
    Code
    Font
    Image
    Text
    Video
    Xml
}

function GetFileTypeFromName {
    [OutputType([FileType])]
    param([string]$name)

    switch ($name.ToLower())
    {
        'archive' {return [FileType]::Archive}
        'audio'   {return [FileType]::Audio}
        'binary'  {return [FileType]::Binary}
        'code'    {return [FileType]::Code}
        'font'    {return [FileType]::Font}
        'image'   {return [FileType]::Image}
        'text'    {return [FileType]::Text}
        'video'   {return [FileType]::Video}
        'xml'     {return [FileType]::Xml}
    }
    return [FileType]::Unknown
}

function FileTypeToName {
    [OutputType([string])]
    param([FileType]$fileType)

    return [FileType].GetEnumName($fileType).ToLower()
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
        # most specific types first
        if ($this.IsCodeFile($fileInfo)) {
            return [FileType]::Code
        }
        if ($this.IsArchiveFile($fileInfo)) {
            return [FileType]::Archive
        }
        if ($this.IsAudioFile($fileInfo)) {
            return [FileType]::Audio
        }
        if ($this.IsFontFile($fileInfo)) {
            return [FileType]::Font
        }
        if ($this.IsImageFile($fileInfo)) {
            return [FileType]::Image
        }
        if ($this.IsVideoFile($fileInfo)) {
            return [FileType]::Video
        }

        # most general types last
        if ($this.IsXmlFile($fileInfo)) {
            return [FileType]::Xml
        }
        if ($this.IsTextFile($fileInfo)) {
            return [FileType]::Text
        }
        if ($this.IsBinaryFile($fileInfo)) {
            return [FileType]::Binary
        }
        return [FileType]::Unknown
    }

    [bool]IsArchiveFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['archive'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['archive']
    }

    [bool]IsAudioFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['audio'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['audio']
    }

    [bool]IsBinaryFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['binary'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['binary']
    }

    [bool]IsCodeFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['code'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['code']
    }

    [bool]IsFontFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['font'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['font']
    }

    [bool]IsImageFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['image'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['image']
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

    [bool]IsVideoFile([System.IO.FileInfo]$fileInfo) {
        return $fileInfo.Extension -in $this.FileTypeExtMap['video'] -or
        $fileInfo.Name -in $this.FileTypeNameMap['video']
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
    FileSize
    FileType
    LastMod
}

function GetSortByFromName {
    [OutputType([SortBy])]
    param([string]$name)

    switch ($name.ToLower())
    {
        'filename' {return [SortBy]::FileName}
        'name' {return [SortBy]::FileName}
        'filesize' {return [SortBy]::FileSize}
        'size' {return [SortBy]::FileSize}
        'filetype' {return [SortBy]::FileType}
        'type' {return [SortBy]::FileType}
        'lastmod' {return [SortBy]::LastMod}
    }
    return [SortBy]::FilePath
}

function SortByToName {
    [OutputType([string])]
    param([SortBy]$sortBy)

    return [SortBy].GetEnumName($sortBy).ToLower()
}

class FindSettings {
    [bool]$ArchivesOnly
    [bool]$Colorize
    [bool]$Debug
    [bool]$FollowSymlinks
    [string[]]$InArchiveExtensions
    [Regex[]]$InArchiveFilePatterns
    [Regex[]]$InDirPatterns
    [string[]]$InExtensions
    [Regex[]]$InFilePatterns
    [FileType[]]$InFileTypes
    [bool]$IncludeArchives
    [bool]$IncludeHidden
    [int]$MaxDepth
    [DateTime]$MaxLastMod
    [long]$MaxSize
    [int]$MinDepth
    [DateTime]$MinLastMod
    [long]$MinSize
    [string[]]$OutArchiveExtensions
    [Regex[]]$OutArchiveFilePatterns
    [Regex[]]$OutDirPatterns
    [string[]]$OutExtensions
    [Regex[]]$OutFilePatterns
    [FileType[]]$OutFileTypes
    [string[]]$Paths
    [bool]$PrintDirs
    [bool]$PrintFiles
    [bool]$PrintUsage
    [bool]$PrintVersion
    [bool]$Recursive
    [SortBy]$SortBy
    [bool]$SortCaseInsensitive
    [bool]$SortDescending
    [bool]$Verbose

    FindSettings() {
		$this.ArchivesOnly = $false
		$this.Colorize = $true
		$this.Debug = $false
		$this.FollowSymlinks = $false
		$this.InArchiveExtensions = @()
		$this.InArchiveFilePatterns = @()
		$this.InDirPatterns = @()
		$this.InExtensions = @()
		$this.InFilePatterns = @()
		$this.InFileTypes = @()
		$this.IncludeArchives = $false
		$this.IncludeHidden = $false
		$this.MaxDepth = -1
		$this.MaxSize = 0
		$this.MinDepth = -1
		$this.MinSize = 0
		$this.OutArchiveExtensions = @()
		$this.OutArchiveFilePatterns = @()
		$this.OutDirPatterns = @()
		$this.OutExtensions = @()
		$this.OutFilePatterns = @()
		$this.OutFileTypes = @()
		$this.Paths = @()
		$this.PrintDirs = $false
		$this.PrintFiles = $false
		$this.PrintUsage = $false
		$this.PrintVersion = $false
		$this.Recursive = $true
		$this.SortBy = [SortBy]::FilePath
		$this.SortCaseInsensitive = $false
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
        $fileTypeNames = $arr | ForEach-Object { FileTypeToName($_) }
        return '[' + ($fileTypeNames -join ', ' ) + ']'
    }

    [string]DateTimeToString([DateTime]$dt) {
        if ($null -eq $dt -or $dt.ToString('yyyy-MM-dd') -eq '0001-01-01') {
            return '0'
        }
        return $dt.ToString('yyyy-MM-dd')
    }

    [string]ToString() {
        return "FindSettings(" +
        "ArchivesOnly=$($this.ArchivesOnly)" +
        ", Colorize=$($this.Colorize)" +
        ", Debug=$($this.Debug)" +
        ", FollowSymlinks=$($this.FollowSymlinks)" +
        ", InArchiveExtensions=$($this.StringArrayToString($this.InArchiveExtensions))" +
        ", InArchiveFilePatterns=$($this.StringArrayToString($this.InArchiveFilePatterns))" +
        ", InDirPatterns=$($this.StringArrayToString($this.InDirPatterns))" +
        ", InExtensions=$($this.StringArrayToString($this.InExtensions))" +
        ", InFilePatterns=$($this.StringArrayToString($this.InFilePatterns))" +
        ", InFileTypes=$($this.FileTypeArrayToString($this.InFileTypes))" +
        ", IncludeArchives=$($this.IncludeArchives)" +
        ", IncludeHidden=$($this.IncludeHidden)" +
        ", MaxDepth=$($this.MaxDepth)" +
        ", MaxLastMod=$($this.DateTimeToString($this.MaxLastMod))" +
        ", MaxSize=$($this.MaxSize)" +
        ", MinDepth=$($this.MinDepth)" +
        ", MinLastMod=$($this.DateTimeToString($this.MinLastMod))" +
        ", MinSize=$($this.MinSize)" +
        ", OutArchiveExtensions=$($this.StringArrayToString($this.OutArchiveExtensions))" +
        ", OutArchiveFilePatterns=$($this.StringArrayToString($this.OutArchiveFilePatterns))" +
        ", OutDirPatterns=$($this.StringArrayToString($this.OutDirPatterns))" +
        ", OutExtensions=$($this.StringArrayToString($this.OutExtensions))" +
        ", OutFilePatterns=$($this.StringArrayToString($this.OutFilePatterns))" +
        ", OutFileTypes=$($this.FileTypeArrayToString($this.OutFileTypes))" +
        ", Paths=$($this.StringArrayToString($this.Paths))" +
        ", PrintDirs=$($this.PrintDirs)" +
        ", PrintFiles=$($this.PrintFiles)" +
        ", PrintUsage=$($this.PrintUsage)" +
        ", PrintVersion=$($this.PrintVersion)" +
        ", Recursive=$($this.Recursive)" +
        ", SortBy=$(SortByToName($this.SortBy))" +
        ", SortCaseInsensitive=$($this.SortCaseInsensitive)" +
        ", SortDescending=$($this.SortDescending)" +
        ", Verbose=$($this.Verbose)" +
        ")"
    }
}
#endregion


#region ArgTokenizer
########################################
# ArgTokenizer
########################################
enum ArgTokenType {
    Unknown
    Bool
    Str
    Int
}

class ArgToken {
    [string]$Name
    [ArgTokenType]$Type
    [object]$Value

    ArgToken([string]$Name, [ArgTokenType]$Type, [object]$Value) {
        $this.Name = $Name
        $this.Type = $Type
        $this.Value = $Value
    }
}

class Option {
    [string]$ShortArg
    [string]$LongArg
    [string]$Desc
    [ArgTokenType]$ArgType
}

class ArgTokenizer {
    # These need to be case-sensitive
    [system.collections.hashtable]$BoolMap
    [system.collections.hashtable]$StrMap
    [system.collections.hashtable]$IntMap

    ArgTokenizer([Option[]]$options) {
        $this.BoolMap = [system.collections.hashtable]::new()
        $this.StrMap = [system.collections.hashtable]::new()
        $this.IntMap = [system.collections.hashtable]::new()
        foreach ($opt in $options) {
            if ($opt.ArgType -eq [ArgTokenType]::Bool) {
                $this.BoolMap[$opt.LongArg] = $opt.LongArg
                if ($opt.ShortArg -ne '') {
                    $this.BoolMap[$opt.ShortArg] = $opt.LongArg
                }
            } elseif ($opt.ArgType -eq [ArgTokenType]::Str) {
                $this.StrMap[$opt.LongArg] = $opt.LongArg
                if ($opt.ShortArg -ne '') {
                    $this.StrMap[$opt.ShortArg] = $opt.LongArg
                }
            } elseif ($opt.ArgType -eq [ArgTokenType]::Int) {
                $this.IntMap[$opt.LongArg] = $opt.LongArg
                if ($opt.ShortArg -ne '') {
                    $this.IntMap[$opt.ShortArg] = $opt.LongArg
                }
            }
        }
    }

    [ArgToken[]]TokenizeArgs([string[]]$argList) {
        [ArgToken[]]$argTokens = @()
        $idx = 0
        while ($idx -lt $argList.Count) {
            $arg = $argList[$idx]
            if ($arg.StartsWith('-')) {
                [string[]]$argNames = @()
                [string]$argVal = ''

                if ($arg.StartsWith('--')) {
                    $arg = $arg.Substring(2)
                    if ($arg.Contains('=')) {
                        $parts = $arg -split '='
                        if ($parts.Length -gt 0) {
                            $arg = $parts[0]
                        }
                        if ($parts.Length -gt 1) {
                            $argVal = $parts[1]
                        }
                    }
                    $argNames += $arg
                } else {
                    $arg = $arg.Substring(1)
                    # TODO: add each char as separate arg
                    foreach ($c in $arg.ToCharArray()) {
                        $c = $c.ToString()
                        if ($this.BoolMap.ContainsKey($c)) {
                            $argNames += $this.BoolMap[$c]
                        } elseif ($this.StrMap.ContainsKey($c)) {
                            $argNames += $this.StrMap[$c]
                        } elseif ($this.IntMap.ContainsKey($c)) {
                            $argNames += $this.IntMap[$c]
                        }
                    }
                }

                foreach ($argName in $argNames) {
                    if ($this.BoolMap.ContainsKey($argName)) {
                        $argTokens += [ArgToken]::new($argName, [ArgTokenType]::Bool, $true)
                        if ($argName -eq 'help' -or $argName -eq 'version') {
                            return $argTokens;
                        }
                    } else {
                        if ($argVal -eq '') {
                            $idx++
                            if ($idx -ge $argList.Count) {
                                throw "Missing value for $arg"
                            }
                            $argVal = $argList[$idx]
                        }
                        if ($this.StrMap.ContainsKey($argName) -or $argName -eq 'settings-file') {
                            $argTokens += [ArgToken]::new($argName, [ArgTokenType]::Str, $argVal)
                        } elseif ($this.IntMap.ContainsKey($argName)) {
                            $argTokens += [ArgToken]::new($argName, [ArgTokenType]::Int, [int]$argVal)
                        } else {
                            throw "Invalid option: $arg"
                        }
                    }
                }
            } else {
                $argTokens += [ArgToken]::new('path', [ArgTokenType]::Str, $arg)
            }
            $idx++
        }
        return $argTokens;
    }

    [ArgToken[]]TokenizeHashtable([Hashtable]$argHash) {
        [ArgToken[]]$argTokens = @()
        # keys are sorted so that output is consistent across all versions
        $keys = $argHash.Keys | Sort-Object
        foreach ($key in $keys) {
            $value = $argHash[$key]
            if ($this.BoolMap.ContainsKey($key)) {
                if ($value -is [bool]) {
                    $argTokens += [ArgToken]::new($key, [ArgTokenType]::Bool, $value)
                } else {
                    throw "Invalid value for option: " + $key
                }
            } elseif ($this.StrMap.ContainsKey($key)) {
                if ($value -is [string])
                {
                    $argTokens += [ArgToken]::new($key, [ArgTokenType]::Str, $value)
                } elseif ($value -is [object]) {
                    foreach ($val in $value) {
                        $argTokens += [ArgToken]::new($key, [ArgTokenType]::Str, $val)
                    }
                } else {
                    throw "Invalid value for option: " + $key
                }
            } elseif ($this.IntMap.ContainsKey($key)) {
                if ($value -is [int] -or $value -is [int64]) {
                    $argTokens += [ArgToken]::new($key, [ArgTokenType]::Int, $value)
                } else {
                    throw "Invalid value for option: " + $key
                }
            } elseif ($key -eq 'settings-file') {
                if ($value -is [string])
                {
                    $argTokens += [ArgToken]::new($key, [ArgTokenType]::Str, $value)
                } elseif ($value -is [object]) {
                    foreach ($val in $value) {
                        $argTokens += [ArgToken]::new($key, [ArgTokenType]::Str, $val)
                    }
                } else {
                    throw "Invalid value for option: " + $key
                }
            } else {
                throw "Invalid option: " + $key
            }
        }
        return $argTokens
    }

    [ArgToken[]]TokenizeJson([string]$json) {
        $argHash = @{}
        try {
            $argHash = $json | ConvertFrom-Json -AsHashtable
        } catch {
            throw "Unable to parse JSON"
        }
        return $this.TokenizeHashtable($argHash)
    }

    [ArgToken[]]TokenizeFilePath([string]$filePath) {
        $expandedPath = ExpandPath($filePath)
        if (-not (Test-Path -Path $expandedPath)) {
            throw "Settings file not found: $filePath"
        }
        if (-not $filePath.EndsWith(".json")) {
            throw "Invalid settings file (must be JSON): $filePath"
        }
        $json = Get-Content -Path $expandedPath -Raw
        try {
            return $this.TokenizeJson($json)
        } catch {
            if ($_.Exception.Message -eq 'Unable to parse JSON') {
                throw "Unable to parse JSON in settings file: $filePath"
            }
            throw $_
        }
    }
}
#endregion


#region FindOptions
########################################
# FindOptions
########################################
class FindOption : Option {
    [string]$SortArg

    FindOption([string]$ShortArg, [string]$LongArg, [string]$Desc, [ArgTokenType]$ArgType) {
        $this.ShortArg = $ShortArg
        $this.LongArg = $LongArg
        $this.Desc = $Desc
        $this.ArgType = $ArgType
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
    [FindOption[]]$FindOptions = @()
    [ArgTokenizer]$ArgTokenizer
    # instantiate this way to get case sensitivity of keys
    $LongArgMap = [system.collections.hashtable]::new()
    $BoolActionMap = @{
        "archivesonly" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.SetArchivesOnly($b)
        }
        "colorize" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.Colorize = $b
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
            $settings.IncludeHidden = !$b
        }
        "followsymlinks" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.FollowSymlinks = $b
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
            $settings.IncludeHidden = $b
        }
        "nocolorize" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.Colorize = !$b
        }
        "nofollowsymlinks" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.FollowSymlinks = !$b
        }
        "noprintdirs" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.PrintDirs = !$b
        }
        "noprintfiles" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.PrintFiles = !$b
        }
        "norecursive" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.Recursive = !$b
        }
        "printdirs" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.PrintDirs = $b
        }
        "printfiles" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.PrintFiles = $b
        }
        "recursive" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.Recursive = $b
        }
        "sort-ascending" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.SortDescending = !$b
        }
        "sort-caseinsensitive" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.SortCaseInsensitive = $b
        }
        "sort-casesensitive" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.SortCaseInsensitive = !$b
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
    $StringActionMap = @{
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
        "maxlastmod" = {
            param([string]$s, [FindSettings]$settings)
            $settings.MaxLastMod = [DateTime]$s
        }
        "minlastmod" = {
            param([string]$s, [FindSettings]$settings)
            $settings.MinLastMod = [DateTime]$s
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
        "settings-file" = {
            param([string]$s, [FindSettings]$settings)
            $this.UpdateSettingsFromFilePath($settings, $s)
        }
        "sort-by" = {
            param([string]$s, [FindSettings]$settings)
            $settings.SortBy = GetSortByFromName($s)
        }
    }
    $IntActionMap = @{
        "maxdepth" = {
            param([int]$i, [FindSettings]$settings)
            $settings.MaxDepth = $i
        }
        "maxsize" = {
            param([int]$i, [FindSettings]$settings)
            $settings.MaxSize = $i
        }
        "mindepth" = {
            param([int]$i, [FindSettings]$settings)
            $settings.MinDepth = $i
        }
        "minsize" = {
            param([int]$i, [FindSettings]$settings)
            $settings.MinSize = $i
        }
    }

    FindOptions() {
        $this.FindOptions = $this.LoadOptionsFromJson()
        $this.ArgTokenizer = [ArgTokenizer]::new($this.FindOptions)
    }

    [FindOption[]]LoadOptionsFromJson() {
        $optionsHash = Get-Content $script:findOptionsPath | ConvertFrom-Json -AsHashtable
        if (-not $optionsHash.ContainsKey('findoptions')) {
            throw "Missing findoptions in JSON"
        }
        # add path manually since it is not in findoptions
        $this.LongArgMap['path'] = 'path'
        $opts = @(foreach ($optionObj in $optionsHash['findoptions']) {
            $ShortArg = ''
            $LongArg = $optionObj['long']
            $Desc = $optionObj['desc']
            $ArgType = [ArgTokenType]::Unknown
            if ($this.BoolActionMap.ContainsKey($LongArg)) {
                $ArgType = [ArgTokenType]::Bool
            } elseif ($this.StringActionMap.ContainsKey($LongArg)) {
                $ArgType = [ArgTokenType]::Str
            } elseif ($this.IntActionMap.ContainsKey($LongArg)) {
                $ArgType = [ArgTokenType]::Int
            }
            $this.LongArgMap[$LongArg] = $LongArg
            if ($optionObj.ContainsKey('short')) {
                $ShortArg = $optionObj['short']
                $this.LongArgMap[$ShortArg] = $LongArg
            }
            [FindOption]::new($ShortArg, $LongArg, $Desc, $ArgType)
        })
        return $opts
    }

    [void]UpdateSettingsFromArgTokens([FindSettings]$settings, [ArgToken[]]$argTokens) {
        foreach ($argToken in $argTokens) {
            if ($argToken.Type -eq [ArgTokenType]::Bool) {
                if ($this.BoolActionMap.ContainsKey($argToken.Name)) {
                    if ($argToken.Value -is [bool]) {
                        $this.BoolActionMap[$argToken.Name].Invoke($argToken.Value, $settings)
                    } else {
                        throw "Invalid value for option: " + $argToken.Name
                    }
                } else {
                    throw "Invalid option: " + $argToken.Name
                }
            } elseif ($argToken.Type -eq [ArgTokenType]::Str) {
                if ($this.StringActionMap.ContainsKey($argToken.Name)) {
                    if ($argToken.Value -is [string]) {
                        $this.StringActionMap[$argToken.Name].Invoke($argToken.Value, $settings)
                    } else {
                        throw "Invalid value for option: " + $argToken.Name
                    }
                } else {
                    throw "Invalid option: " + $argToken.Name
                }
            } elseif ($argToken.Type -eq [ArgTokenType]::Int) {
                if ($this.IntActionMap.ContainsKey($argToken.Name)) {
                    if ($argToken.Value -is [int] -or $argToken.Value -is [int64]) {
                        $this.IntActionMap[$argToken.Name].Invoke($argToken.Value, $settings)
                    } else {
                        throw "Invalid value for option: " + $argToken.Name
                    }
                } else {
                    throw "Invalid option: " + $argToken.Name
                }
            } else {
                throw "Invalid option: " + $argToken.Name
            }
        }
    }

    [void]UpdateSettingsFromJson([FindSettings]$settings, [string]$json) {
        $argTokens = $this.ArgTokenizer.TokenizeJson($json)
        $this.UpdateSettingsFromArgTokens($settings, $argTokens)
    }

    [void]UpdateSettingsFromFilePath([FindSettings]$settings, [string]$filePath) {
        $argTokens = $this.ArgTokenizer.TokenizeFilePath($filePath)
        $this.UpdateSettingsFromArgTokens($settings, $argTokens)
    }

    [void]UpdateSettingsFromArgs([FindSettings]$settings, [string[]]$argList) {
        $argTokens = $this.ArgTokenizer.TokenizeArgs($argList)
        $this.UpdateSettingsFromArgTokens($settings, $argTokens)
    }

    [FindSettings]SettingsFromArgs([string[]]$argList) {
        $settings = [FindSettings]::new()
        # default PrintFiles to true since we're using via CLI
        $settings.PrintFiles = $true
        $this.UpdateSettingsFromArgs($settings, $argList)
        return $settings
    }

    [string]GetUsageString() {
        $usage = "`nUsage:`n ps1find [options] <path> [<path> ...]`n`nOptions:`n";
        $optStrs = @()
        $optMap = @{}
        $longest = 0
        $options = $this.FindOptions | Sort-Object -Property SortArg

        foreach ($option in $options) {
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


#region FileResultFormatter
########################################
# FileResultFormatter
########################################
class FileResultFormatter {
    [FindSettings]$Settings
    [Scriptblock]$FormatDirectoryBlock
    [Scriptblock]$FormatFileNameBlock

    FileResultFormatter([FindSettings]$settings) {
        $this.Settings = $settings
        if ($settings.Colorize -and $settings.InDirPatterns.Length -gt 0) {
            $this.FormatDirectoryBlock = {
                param([System.IO.DirectoryInfo]$dir)
                return $this.FormatDirectoryWithColor($dir)
            }
        } else {
            $this.FormatDirectoryBlock = {
                param([System.IO.DirectoryInfo]$dir)
                if ($null -eq $dir) {
                    return "."
                }
                return $dir.ToString()
            }
        }
        if ($settings.Colorize -and ($settings.InExtensions.Length -gt 0 -or $settings.InFilePatterns.Length -gt 0)) {
            $this.FormatFileNameBlock = {
                param([string]$fileName)
                return $this.FormatFileNameColor($fileName)
            }
        } else {
            $this.FormatFileNameBlock = {
                param([string]$fileName)
                return $fileName
            }
        }
    }

    [string]ColorizeString([string]$s, [int]$matchStartIdx, [int]$matchEndIdx) {
        $prefix = ''
        if ($matchStartIdx -gt 0) {
            $prefix = $s.Substring(0, $matchStartIdx)
        }
        $match = $s.Substring($matchStartIdx, $matchEndIdx - $matchStartIdx)
        $suffix = ''
        if ($matchEndIdx -lt $s.Length) {
            $suffix = $s.Substring($matchEndIdx)
        }
        return "$prefix${script:Green}$match${script:Reset}$suffix"
    }

    [string]FormatDirectoryWithColor([System.IO.DirectoryInfo]$dir) {
        $formattedDir = "."
        if ($null -ne $dir) {
            $formattedDir = $dir.ToString();
            foreach ($dirPattern in $this.Settings.InDirPatterns) {
                $match = $dirPattern.Match($formattedDir)
                if ($match.Success) {
                    $formattedDir = $this.ColorizeString($formattedDir, $match.Index, $match.Index + $match.Length)
                    break
                }
            }
        }
        return $formattedDir
    }

    [string]FormatDirectory([System.IO.DirectoryInfo]$dir) {
        return $this.FormatDirectoryBlock.Invoke($dir)
    }

    [string]FormatFileNameColor([string]$fileName) {
        $formattedFileName = $fileName
        foreach ($filePattern in $this.Settings.InFilePatterns) {
            $match = $filePattern.Match($formattedFileName)
            if ($match.Success) {
                $formattedFileName = $this.ColorizeString($formattedFileName, $match.Index, $match.Index + $match.Length)
                break
            }
        }
        if ($this.Settings.InExtensions.Count -gt 0) {
            $idx = $formattedFileName.LastIndexOf('.')
            if ($idx -gt 0 -and $idx -lt $formattedFileName.Length) {
                $formattedFileName = $this.ColorizeString($formattedFileName, $idx + 1, $formattedFileName.Length)
            }
        }
        return $formattedFileName
    }

    [string]FormatFileName([string]$fileName) {
        return $this.FormatFileNameBlock.Invoke($fileName)
    }

    [string]FormatFile([System.IO.FileInfo]$file) {
        $parent = $this.FormatDirectory($file.Directory)
        $fileName = $this.FormatFileName($file.Name)
        return $parent + [System.IO.Path]::DirectorySeparatorChar + $fileName
    }

    [string]FormatFileResult([FileResult]$result) {
        return $this.FormatFile($result.File)
    }
}
#endregion


#region FileResultSorter
########################################
# FileResultSorter
########################################
class FileResultSorter
{
    [FindSettings]$Settings

    FileResultSorter([FindSettings]$settings)
    {
        $this.Settings = $settings
    }

    [Hashtable[]]GetOrderByPath() {
        return @(
            @{ Expression = { $_.File.DirectoryName } },
            @{ Expression = { $_.File.Name } }
        );
    }

    [Hashtable[]]GetOrderByName() {
        return @(
            @{ Expression = { $_.File.Name } },
            @{ Expression = { $_.File.DirectoryName } }
        );
    }

    [Hashtable[]]GetOrderBySize() {
        return @( @{ Expression = { $_.File.Length } } ) + $this.GetOrderByPath()
    }

    [Hashtable[]]GetOrderByType() {
        return @( @{ Expression = { $_.Type } } ) + $this.GetOrderByPath()
    }

    [Hashtable[]]GetOrderByLastMod() {
        return @( @{ Expression = { $_.File.LastWriteTimeUtc } } ) + $this.GetOrderByPath()
    }

    [FileResult[]]Sort([FileResult[]]$fileResults) {
        $order = @()
        switch (SortByToName($this.Settings.SortBy)) {
            'filename' {
                $order = $this.GetOrderByName()
            }
            'filepath' {
                $order = $this.GetOrderByPath()
            }
            'filesize' {
                $order = $this.GetOrderBySize()
            }
            'filetype' {
                $order = $this.GetOrderByType()
            }
            'lastmod' {
                $order = $this.GetOrderByLastMod()
            }
            Default {
                $order = $this.GetOrderByPath()
            }
        }

        if ($this.Settings.SortDescending) {
            if ($this.Settings.SortCaseInsensitive) {
                return $fileResults |
                        Sort-Object -Descending -Property $order
            }
            return $fileResults |
                    Sort-Object -CaseSensitive -Descending -Property $order
        }

        if ($this.Settings.SortCaseInsensitive) {
            return $fileResults |
                    Sort-Object -Property $order
        }
        return $fileResults |
                Sort-Object -CaseSensitive -Property $order
    }
}
#endregion


#region Finder
########################################
# Finder
########################################
class Finder {
    [FindSettings]$Settings
    [FileTypes]$FileTypes
    [Scriptblock[]]$FilterDirByHiddenTests
    [Scriptblock[]]$FilterDirByInPatternsTests
    [Scriptblock[]]$FilterDirByOutPatternsTests
    [Scriptblock[]]$FileResultTests
    [Scriptblock[]]$ArchiveFileResultTests

    Finder([FindSettings]$settings) {
        $this.Settings = $settings
        $this.ValidateSettings()
        $this.FileTypes = [FileTypes]::new()
        $this.FilterDirByHiddenTests = $this.GetFilterDirByHiddenTests()
        $this.FilterDirByInPatternsTests = $this.GetFilterDirByInPatternsTests()
        $this.FilterDirByOutPatternsTests = $this.GetFilterDirByOutPatternsTests()
        $this.FileResultTests = $this.GetMatchingFileResultTests()
        $this.ArchiveFileResultTests = $this.GetMatchingArchiveFileResultTests()
    }

    [void]ValidateSettings() {
        if ($null -eq $this.Settings.Paths -or $this.Settings.Paths.Count -eq 0) {
            throw "Startpath not defined"
        }
        foreach ($p in $this.Settings.Paths) {
            if (-not (Test-Path $p))
            {
                $p = ExpandPath($p)
            }
            if (-not (Test-Path $p)) {
                throw "Startpath not found"
            }
        }
        if ($this.Settings.MaxDepth -gt -1 -and $this.Settings.MinDepth -gt -1 -and $this.Settings.MaxDepth -lt $this.Settings.MinDepth) {
            throw "Invalid range for mindepth and maxdepth"
        }
        if ($this.Settings.MaxLastMod -gt [DateTime]::MinValue -and $this.Settings.MinLastMod -gt [DateTime]::MinValue -and $this.Settings.MaxLastMod -lt $this.Settings.MinLastMod) {
            throw "Invalid range for minlastmod and maxlastmod"
        }
        if ($this.Settings.MaxSize -gt 0 -and $this.Settings.MinSize -gt 0 -and $this.Settings.MaxSize -lt $this.Settings.MinSize) {
            throw "Invalid range for minsize and maxsize"
        }
    }

    [bool]MatchesAnyPattern([string]$s, [regex[]]$patterns) {
        return @($patterns | Where-Object { $s -match $_ }).Count -gt 0
    }

    [Scriptblock[]]GetFilterDirByHiddenTests() {
        $tests = @()
        if (-not $this.Settings.IncludeHidden) {
            $tests += {
                param([System.IO.DirectoryInfo]$d)
                return !(IsHiddenDirectory $d)
            }
        }
        return $tests
    }

    [bool]FilterDirByHidden([System.IO.DirectoryInfo]$d) {
        foreach ($t in $this.FilterDirByHiddenTests) {
            if (-not $t.Invoke($d)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetFilterDirByInPatternsTests() {
        $tests = @()
        if ($this.Settings.InDirPatterns.Count -gt 0) {
            $tests += {
                param([System.IO.DirectoryInfo]$d)
                return $this.MatchesAnyPattern($d.FullName, $this.Settings.InDirPatterns)
            }
        }
        return $tests
    }

    [bool]FilterDirByInPatterns([System.IO.DirectoryInfo]$d) {
        foreach ($t in $this.FilterDirByInPatternsTests) {
            if (-not $t.Invoke($d)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetFilterDirByOutPatternsTests() {
        $tests = @()
        if ($this.Settings.OutDirPatterns.Count -gt 0) {
            $tests += {
                param([System.IO.DirectoryInfo]$d)
                return !$this.MatchesAnyPattern($d.FullName, $this.Settings.OutDirPatterns)
            }
        }
        return $tests
    }

    [bool]FilterDirByOutPatterns([System.IO.DirectoryInfo]$d) {
        foreach ($t in $this.FilterDirByOutPatternsTests) {
            if (-not $t.Invoke($d)) {
                return $false
            }
        }
        return $true
    }

    [bool]IsMatchingDir([System.IO.DirectoryInfo]$d) {
        return $this.FilterDirByHidden($d) -and $this.FilterDirByInPatterns($d) -and $this.FilterDirByOutPatterns($d)
    }

    [Scriptblock[]]GetMatchingArchiveFileResultTests() {
        $tests = @()
        if ($this.Settings.InArchiveExtensions.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.Settings.InArchiveExtensions.Contains($f.File.Extension)
            }
        } elseif ($this.Settings.OutArchiveExtensions.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.Settings.OutArchiveExtensions.Contains($f.File.Extension)
            }
        }
        if ($this.Settings.InArchiveFilePatterns.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.MatchesAnyPattern($f.File.Name, $this.Settings.InArchiveFilePatterns)
            }
        } elseif ($this.Settings.OutArchiveFilePatterns.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.MatchesAnyPattern($f.File.Name, $this.Settings.OutArchiveFilePatterns)
            }
        }
        return $tests
    }

    [bool]IsMatchingArchiveFileResult([FileResult]$f) {
        foreach ($t in $this.ArchiveFileResultTests) {
            if (-not $t.Invoke($f)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetMatchingFileResultTests() {
        $tests = @()
        if ($this.Settings.InExtensions.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.Settings.InExtensions.Contains($f.File.Extension)
            }
        } elseif ($this.Settings.OutExtensions.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.Settings.OutExtensions.Contains($f.File.Extension)
            }
        }
        if ($this.Settings.InFilePatterns.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.MatchesAnyPattern($f.File.Name, $this.Settings.InFilePatterns)
            }
        } elseif ($this.Settings.OutFilePatterns.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.MatchesAnyPattern($f.File.Name, $this.Settings.OutFilePatterns)
            }
        }
        if ($this.Settings.InFileTypes.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $this.Settings.InFileTypes.Contains($f.Type)
            }
        } elseif ($this.Settings.OutFileTypes.Count -gt 0) {
            $tests += {
                param([FileResult]$f)
                return !$this.Settings.OutFileTypes.Contains($f.Type)
            }
        }
        if ($this.Settings.MaxLastMod -gt [DateTime]::MinValue) {
            $tests += {
                param([FileResult]$f)
                return $f.File.LastWriteTimeUtc -le $this.Settings.MaxLastMod
            }
        }
        if ($this.Settings.MinLastMod -gt [DateTime]::MinValue) {
            $tests += {
                param([FileResult]$f)
                return $f.File.LastWriteTimeUtc -ge $this.Settings.MinLastMod
            }
        }
        if ($this.Settings.MaxSize -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $f.File.Length -le $this.Settings.MaxSize
            }
        }
        if ($this.Settings.MinSize -gt 0) {
            $tests += {
                param([FileResult]$f)
                return $f.File.Length -ge $this.Settings.MinSize
            }
        }
        return $tests
    }

    [bool]IsMatchingFileResult([FileResult]$f) {
        foreach ($t in $this.FileResultTests) {
            if (-not $t.Invoke($f)) {
                # Write-Host "$f did not pass test: $t"
                return $false
            }
        }
        return $true
    }

    [FileResult]FilterToFileResult([System.IO.FileInfo]$file) {
        if (-not $this.IsMatchingDir($file.Directory)) {
            return $null
        }
        if ((-not $this.Settings.IncludeHidden) -and (IsHiddenName($file.Name))) {
            return $null
        }
        $fileResult = [FileResult]::new($file, $this.FileTypes.GetFileType($file))
        if ($fileResult.Type -eq [FileType]::Archive) {
            if ($this.Settings.IncludeArchives -and $this.IsMatchingArchiveFileResult($fileResult)) {
                return $fileResult
            }
            return $null
        }
        if (-not $this.Settings.ArchivesOnly -and $this.IsMatchingFileResult($fileResult)) {
            return $fileResult
        }
        return $null
    }

    [FileResult[]]FilterToFileResults([System.IO.FileInfo[]]$files) {
        $fileResults = @()
        foreach ($file in $files) {
            $fileResult = $this.FilterToFileResult($file)
            if ($null -ne $fileResult) {
                $fileResults += $fileResult
            }
        }
        return $fileResults
    }

    [FileResult[]]RecGetPathResults([System.IO.DirectoryInfo]$dirPath, [int]$minDepth, [int]$maxDepth, [int]$currentDepth) {
        $recurse = $true
        if ($currentDepth -eq $maxDepth) {
            $recurse = $false
        } elseif ($maxDepth -gt -1 -and $currentDepth -gt $maxDepth) {
            return @()
        }

        # Get the dirs and files under file_path
        $pathDirs = @()
        $pathFiles = @()
        $fileResults = @()
        if ($recurse) {
            # Force is needed to get hidden dirs
            $pathDirs = Get-ChildItem -Force -Recurse:$false -Path $dirPath -Directory | Where-Object { $this.FilterDirByHidden($_) -and $this.FilterDirByOutPatterns($_) }
            if (-not $this.Settings.FollowSymlinks) {
                # filter out symlinks
                $pathDirs = $pathDirs | Where-Object { -not $_.Attributes.HasFlag([System.IO.FileAttributes]::ReparsePoint) }
            }
        }
        if ($minDepth -lt 0 -or $currentDepth -ge $minDepth) {
            # Force is needed to get hidden files
            $pathFiles = Get-ChildItem -Force -Recurse:$false -Path $dirPath -File
            if (-not $this.Settings.FollowSymlinks) {
                # filter out symlinks
                $pathFiles = $pathFiles | Where-Object { -not $_.Attributes.HasFlag([System.IO.FileAttributes]::ReparsePoint) }
            }
        }

        # Filter the dirs and files
        $fileResults += $this.FilterToFileResults($pathFiles)
        foreach ($pathDir in $pathDirs) {
            $fileResults += $this.RecGetPathResults($pathDir, $minDepth, $maxDepth, $currentDepth + 1)
        }

        return $fileResults
    }

    [FileResult[]]GetPathResults([string]$path) {
        $fileResults = @()
        if ($path.StartsWith('~')) {
            $path = ExpandPath($path)
        }
        if (Test-Path -Path $path -PathType Container) {
            # if max_depth is zero, we can skip since a directory cannot be a result
            if ($this.Settings.MaxDepth -eq 0) {
                return $fileResults
            }
            $pathDir = [System.IO.DirectoryInfo]::new($path)
            if ($this.FilterDirByHidden($pathDir) -and $this.FilterDirByOutPatterns($pathDir)) {
                $maxDepth = $this.Settings.MaxDepth
                if (-not $this.Settings.Recursive) {
                    $maxDepth = 1
                }
                $fileResults += $this.RecGetPathResults($pathDir, $this.Settings.MinDepth, $maxDepth, 1)
            } else {
                throw "Startpath does not match find settings"
            }
        } else {
            # if min_depth > zero, we can skip since the file is at depth zero
            if ($this.Settings.MinDepth -gt 0) {
                return @()
            }
            $pathFile = [System.IO.FileInfo]::new($path)
            $pathFileResult = [FileResult]::new($pathFile, $this.FileTypes.GetFileType($pathFile))
            if ($this.IsMatchingFileResult($pathFileResult)) {
                $fileResults += $pathFileResult
            } else {
                throw "Startpath does not match find settings"
            }
        }
        return $fileResults
    }

    [FileResult[]]GetFileResults() {
        $fileResults = @()
        foreach ($path in $this.Settings.Paths) {
            $fileResults += $this.GetPathResults($path)
        }
        $fileResultSorter = [FileResultSorter]::new($this.Settings)
        return $fileResultSorter.Sort($fileResults)
    }

    [FileResult[]]Find() {
        return $this.GetFileResults()
    }

    [void]PrintMatchingDirs([FileResult[]]$fileResults, [FileResultFormatter]$formatter) {
        $dirs = @()
        if ($fileResults.Count -gt 0) {
            $dirs = $fileResults |
                    ForEach-Object { $_.File.Directory } |
                    Select-Object -Unique
        }
        if ($dirs.Count -gt 0) {
            LogMsg("`nMatching directories ($($dirs.Count)):")
            foreach ($d in $dirs) {
                LogMsg($formatter.FormatDirectory($d))
            }
        } else {
            LogMsg("`nMatching directories: 0")
        }
    }

    [void]PrintMatchingFiles([FileResult[]]$fileResults, [FileResultFormatter]$formatter) {
        if ($fileResults.Count -gt 0) {
            LogMsg("`nMatching files ($($fileResults.Count)):")
            foreach ($f in $fileResults) {
                LogMsg($formatter.FormatFileResult($f))
            }
        } else {
            LogMsg("`nMatching files: 0")
        }
    }
}
#endregion
