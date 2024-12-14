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


#region Common
########################################
# Common
########################################
function LogMsg {
    param([string]$msg)

    # Write-Output $msg
    Write-Host $msg
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
    return [Environment]::GetFolderPath([Environment+SpecialFolder]::UserProfile);
}

function ExpandPath {
    param([string]$filePath)
    if ($filePath.StartsWith('~')) {
        $userPath = GetHomePath
        if ($filePath -eq '~' -or $filePath -eq '~/' -or $filePath -eq '~\\') {
            return $userPath
        }
        if ($filePath.StartsWith('~/') -or $filePath.StartsWith('~\\')) {
            return Join-Path -Path $userPath -ChildPath $filePath.Substring(2)
        }
        $sepIndex = $filePath.IndexOf([Path]::DirectorySeparatorChar)
        $homePath = [Path]::GetDirectoryName($userPath)
        if ($sepIndex -eq -1) {
            $userName = $filePath.Substring(1)
            return Join-Path -Path $homePath -ChildPath $userName
        }
        $userName = $filePath.Substring(1, $sepIndex - 1)
        $userPath = Join-Path -Path $homePath -ChildPath $userName
        return Join-Path -Path $userPath -ChildPath $filePath.Substring($sepIndex)
    }
    return $filePath
}

function IsDotDir {
    [OutputType([bool])]
    param([string]$dirName)
    return $dotPaths.Contains($dirName)
}

function IsHiddenFileName {
    [OutputType([bool])]
    param([string]$fileName)
    return ($fileName.StartsWith('.') -and (-not ($dotPaths.Contains($fileName))))
}

function IsHiddenFile {
    [OutputType([bool])]
    param([System.IO.FileSystemInfo]$f)
    return ($f.Attributes.HasFlag([System.IO.FileAttributes]::Hidden)) -or
        (IsHiddenFileName $f.Name)
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
    [FindOption[]]$FindOptions = @()
    # $LongArgMap = @{}
    # instantiate this way to get case sensitivity of keys
    $LongArgMap = [system.collections.hashtable]::new()
    $BoolActionMap = @{
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
        # default PrintFiles to true since we're using via CLI
        $settings.PrintFiles = $true
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
                if ($this.BoolActionMap.ContainsKey($longArg)) {
                    $this.BoolActionMap[$longArg].Invoke($true, $settings)

                } elseif ($this.StringActionMap.ContainsKey($longArg) -or $this.IntActionMap.ContainsKey($longArg)) {
                    $idx++
                    if ($idx -lt $argList.Count) {
                        if ($this.StringActionMap.ContainsKey($longArg)) {
                            $this.StringActionMap[$longArg].Invoke($argList[$idx], $settings)
                        } else {
                            $this.IntActionMap[$longArg].Invoke([int]$argList[$idx], $settings)
                        }
                    } else {
                        throw "Missing value for $arg"
                    }
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
        foreach ($option in $this.FindOptions) {
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
    [FindSettings]$Settings
    [FileTypes]$FileTypes
    [Scriptblock[]]$DirTests
    [Scriptblock[]]$FileTests
    [Scriptblock[]]$ArchiveFileTests

    Finder([FindSettings]$settings) {
        $this.Settings = $settings
        $this.ValidateSettings()
        $this.FileTypes = [FileTypes]::new()
        $this.DirTests = $this.GetMatchingDirTests()
        $this.FileTests = $this.GetMatchingFileTests()
        $this.ArchiveFileTests = $this.GetMatchingArchiveFileTests()
    }

    [void]ValidateSettings() {
        if ($null -eq $this.Settings.Paths -or $this.Settings.Paths.Count -eq 0) {
            throw "Startpath not defined"
        }
        foreach ($path in $this.Settings.Paths) {
            if (-not (Test-Path $path) -and -not (Test-Path ExpandPath($path))) {
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

    [Scriptblock[]]GetMatchingDirTests() {
        $tests = @()
        if (-not $this.Settings.IncludeHidden) {
            $tests += {
                param([System.IO.DirectoryInfo]$d)
                return !(IsHiddenFile $d)
            }
        }
        if ($this.Settings.InDirPatterns.Count -gt 0) {
            $tests += {
                param([System.IO.DirectoryInfo]$d)
                return $this.MatchesAnyPattern($d.FullName, $this.Settings.InDirPatterns)
            }
        } elseif ($this.Settings.OutDirPatterns.Count -gt 0) {
            $tests += {
                param([System.IO.DirectoryInfo]$d)
                return !$this.MatchesAnyPattern($d.FullName, $this.Settings.OutDirPatterns)
            }
        }
        return $tests
    }

    [bool]IsMatchingDir([System.IO.DirectoryInfo]$d) {
        return @($this.DirTests | Where-Object { $_.Invoke($d) }).Count -eq $this.DirTests.Count
    }

    [Scriptblock[]]GetMatchingArchiveFileTests() {
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
        return @($this.ArchiveFileTests | Where-Object { $_.Invoke($f) }).Count -eq $this.ArchiveFileTests.Count
    }

    [Scriptblock[]]GetMatchingFileTests() {
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
        foreach ($t in $this.FileTests) {
            if (-not $t.Invoke($f)) {
                # Write-Host "$f did not pass test: $t"
                return $false
            }
        }
        return $true
    }

    [FileResult]FilterToFileResult([System.IO.FileInfo]$file) {
        # Write-Host "FilterToFileResult($file)"
        if ((-not $this.Settings.IncludeHidden) -and (IsHiddenFile($file))) {
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
            $pathDirs = Get-ChildItem -Force -Recurse:$false -Path $dirPath -Directory | Where-Object { $this.IsMatchingDir($_) }
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
        if (-not (Test-Path -Path $path)) {
            $path = ExpandPath($path)
        }
        if (Test-Path -Path $path -PathType Container) {
            # if max_depth is zero, we can skip since a directory cannot be a result
            if ($this.Settings.MaxDepth -eq 0) {
                return $fileResults
            }
            $pathDir = [System.IO.DirectoryInfo]::new($path)
            if ($this.IsMatchingDir($pathDir)) {
                $maxDepth = $this.Settings.MaxDepth
                if (-not $this.Settings.Recursive) {
                    $maxDepth = 1
                }
                $fileResults += $this.RecGetPathResults($pathDir, $this.Settings.MinDepth, $maxDepth, 1)
            } else {
                throw "Startpath does not match find settings"
            }
        } elseif (Test-Path -Path $path -PathType Leaf) {
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
        return $fileResults
    }

    [FileResult[]]SortFileResults([FileResult[]]$fileResults) {
        $listToSort = @()
        if ($this.Settings.SortBy -eq [SortBy]::FileName) {
            $listToSort = $fileResults |
                ForEach-Object {[Tuple]::Create($_.File.Name, $_.File.DirectoryName, $_)}
        } elseif ($this.Settings.SortBy -eq [SortBy]::FileSize) {
            $listToSort = $fileResults |
                ForEach-Object {[Tuple]::Create($_.File.Length, $_.File.DirectoryName, $_.File.Name, $_)}
        } elseif ($this.Settings.SortBy -eq [SortBy]::FileType) {
            $listToSort = $fileResults |
                ForEach-Object {[Tuple]::Create($_.Type, $_.File.DirectoryName, $_.File.Name, $_)}
        } elseif ($this.Settings.SortBy -eq [SortBy]::LastMod) {
            $listToSort = $fileResults |
                ForEach-Object {[Tuple]::Create($_.File.LastWriteTimeUtc, $_.File.DirectoryName, $_.File.Name, $_)}
        } else {
            $listToSort = $fileResults |
                ForEach-Object {[Tuple]::Create($_.File.DirectoryName, $_.File.Name, $_)}
        }
        $sorted = @()
        if ($this.Settings.SortCaseInsensitive -and $this.Settings.SortDescending) {
            $sorted = $listToSort | Sort-Object -Descending | ForEach-Object {$_[-1]}
        } elseif ($this.Settings.SortCaseInsensitive) {
            $sorted = $listToSort | Sort-Object | ForEach-Object {$_[-1]}
        } elseif ($this.Settings.SortDescending) {
            $sorted = $listToSort | Sort-Object -Descending | ForEach-Object {$_[-1]}
        } else {
            $sorted = $listToSort | Sort-Object -CaseSensitive | ForEach-Object {$_[-1]}
        }
        return $sorted
    }

    [FileResult[]]Find() {
        return $this.SortFileResults($this.GetFileResults())
    }
}
#endregion
