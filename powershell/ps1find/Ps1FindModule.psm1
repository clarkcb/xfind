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
$defaultFindSettingsPath = Join-Path $HOME '.config' 'xfind' 'settings.json'
#endregion


#region ConsoleColor
########################################
# ConsoleColor
########################################
#$esc = "`e" # Escape character
$esc = [char]27
$ConsoleReset   = "${esc}[0m";
$ConsoleBlack   = "${esc}[0;30m";
$ConsoleRed     = "${esc}[0;31m";
$ConsoleGreen   = "${esc}[0;32m";
$ConsoleYellow  = "${esc}[0;33m";
$ConsoleBlue    = "${esc}[0;34m";
$ConsoleMagenta = "${esc}[0;35m";
$ConsoleCyan    = "${esc}[0;36m";
$ConsoleWhite   = "${esc}[0;37m";
$BoldBlack      = "${esc}[1;30m";
$BoldRed        = "${esc}[1;31m";
$BoldGreen      = "${esc}[1;32m";
$BoldYellow     = "${esc}[1;33m";
$BoldBlue       = "${esc}[1;34m";
$BoldMagenta    = "${esc}[1;35m";
$BoldCyan       = "${esc}[1;36m";
$BoldWhite      = "${esc}[1;37m";
#endregion


#region Color
########################################
# Color
########################################
enum Color {
    Black
    Red
    Green
    Yellow
    Blue
    Magenta
    Cyan
    White
}

function GetConsoleColorForColor {
    [OutputType([string])]
    param([Color]$color)

    switch ($color)
    {
        Black     {return $ConsoleBlack}
        Red       {return $ConsoleRed}
        Green     {return $ConsoleGreen}
        Yellow    {return $ConsoleYellow}
        Blue      {return $ConsoleBlue}
        Magenta   {return $ConsoleMagenta}
        Cyan      {return $ConsoleCyan}
        White     {return $ConsoleWhite}
    }
    return $ConsoleWhite
}
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
#    [OutputType([string])]
    param([string]$err)

    # Write-Error adds a stack trace, which we don't want
    # Write-Error "`nERROR: ${msg}"
    $host.UI.WriteErrorLine("`nERROR: ${err}")
}

function LogErrorColor {
#    [OutputType([string])]
    param([string]$err)

    # Write-Error adds a stack trace, which we don't want
    # Write-Error "`n${BoldRed}ERROR: ${msg}${Reset}"
    $host.UI.WriteErrorLine("`n${BoldRed}ERROR: ${err}${ConsoleReset}")
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

function IsHiddenPath {
    [OutputType([bool])]
    param([string]$path)
    $segments = PathElems($path)
    foreach ($segment in $segments) {
        if (IsHiddenName($segment)) {
            return $true
        }
    }
    return $false
}

#function IsReadableFile {
#    [OutputType([bool])]
#    param([System.IO.FileSystemInfo]$f)
#
#    $readable = $false
#    try {
#        [System.IO.File]::OpenRead($f).Close()
#        $readable = $true
#    } catch {
#        $readable = $false
#    }
#    return $readable
#}

function GetFileExtension {
    [OutputType([string])]
    param([string]$filePath)
    $fileName = [System.IO.Path]::GetFileName($filePath)
    if (-not $fileName -or -not $fileName.Contains('.') -or $fileName.LastIndexOf('.') -lt 1) {
        return ''
    }
    $ext = [System.IO.Path]::GetExtension($fileName)
    if (-not $ext) {
        return ''
    }
#    return $ext.Substring(1)
    return $ext
}

function PathElems {
    [OutputType([string[]])]
    param([string]$path)
    return ($path -split [System.IO.Path]::DirectorySeparatorChar)
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

    [FileType]GetFileTypeForFilePath([string]$filePath) {
        # most specific types first
        if ($this.IsCodeFilePath($filePath)) {
            return [FileType]::Code
        }
        if ($this.IsArchiveFilePath($filePath)) {
            return [FileType]::Archive
        }
        if ($this.IsAudioFilePath($filePath)) {
            return [FileType]::Audio
        }
        if ($this.IsFontFilePath($filePath)) {
            return [FileType]::Font
        }
        if ($this.IsImageFilePath($filePath)) {
            return [FileType]::Image
        }
        if ($this.IsVideoFilePath($filePath)) {
            return [FileType]::Video
        }

        # most general types last
        if ($this.IsXmlFilePath($filePath)) {
            return [FileType]::Xml
        }
        if ($this.IsTextFilePath($filePath)) {
            return [FileType]::Text
        }
        if ($this.IsBinaryFilePath($filePath)) {
            return [FileType]::Binary
        }
        return [FileType]::Unknown
    }

    [bool]IsFilePathForType([string]$filePath, [string]$typeName) {
        $fileName = [System.IO.Path]::GetFileName($filePath)
        $ext = GetFileExtension($fileName)
        return $ext -in $this.FileTypeExtMap[$typeName] -or
        $fileName -in $this.FileTypeNameMap[$typeName]
    }

    [bool]IsArchiveFilePath([string]$filePath) {
        return $this.IsFilePathForType($filePath, 'archive')
    }

    [bool]IsAudioFilePath([string]$filePath) {
        return $this.IsFilePathForType($filePath, 'audio')
    }

    [bool]IsBinaryFilePath([string]$filePath) {
        return $this.IsFilePathForType($filePath, 'binary')
    }

    [bool]IsCodeFilePath([string]$filePath) {
        return $this.IsFilePathForType($filePath, 'code')
    }

    [bool]IsFontFilePath([string]$filePath) {
        return $this.IsFilePathForType($filePath, 'font')
    }

    [bool]IsImageFilePath([string]$filePath) {
        return $this.IsFilePathForType($filePath, 'image')
    }

    [bool]IsSearchableFilePath([string]$filePath) {
        return $this.GetFileTypeForFilePath($filePath) -ne [FileType]::Unknown
    }

    [bool]IsTextFilePath([string]$filePath) {
        return $this.IsFilePathForType($filePath, 'text')
    }

    [bool]IsUnknownFilePath([string]$filePath) {
        return $this.GetFileTypeForFilePath($filePath) -eq [FileType]::Unknown
    }

    [bool]IsVideoFilePath([string]$filePath) {
        return $this.IsFilePathForType($filePath, 'video')
    }

    [bool]IsXmlFilePath([string]$filePath) {
        return $this.IsFilePathForType($filePath, 'xml')
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
    [bool]$DefaultFiles
    [Color]$DirColor
    [Color]$ExtColor
    [Color]$FileColor
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
		$this.DefaultFiles = $true
		$this.DirColor = [Color]::Cyan
		$this.ExtColor = [Color]::Yellow
		$this.FileColor = [Color]::Magenta
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

    [string]PropertiesToString() {
        $flags = [Reflection.BindingFlags]::Public -bor [Reflection.BindingFlags]::Instance
        $properties = $this.getType().GetProperties($flags) | Sort-Object -Property Name
        $propStrings = @()
        foreach ($prop in $properties)
        {
            if ($prop.PropertyType.Name -eq 'String[]' -or $prop.PropertyType.Name -eq 'Regex[]') {
                $propStrings += "$($prop.Name)=$($this.StringArrayToString($prop.GetValue($this)))"
            } elseif ($prop.PropertyType.Name -eq 'FileType[]') {
                $propStrings += "$($prop.Name)=$($this.FileTypeArrayToString($prop.GetValue($this)))"
            } elseif ($prop.PropertyType.Name -eq 'DateTime') {
                $propStrings += "$($prop.Name)=$($this.DateTimeToString($prop.GetValue($this)))"
            } elseif ($prop.PropertyType.Name -eq 'SortBy') {
                $propStrings += "$($prop.Name)=$(SortByToName($prop.GetValue($this)))"
            } else {
                $propStrings += "$($prop.Name)=$($prop.GetValue($this))"
            }
        }
        return $propStrings -join ', '
    }

    [string]ToString() {
        return $this.GetType().Name + "(" + $this.PropertiesToString() + ")"
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
        "defaultfiles" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.DefaultFiles = $b
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
        "nodefaultfiles" = {
            param([bool]$b, [FindSettings]$settings)
            $settings.DefaultFiles = !$b
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
            if ($optionObj.ContainsKey('short')) {
                $ShortArg = $optionObj['short']
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
                        if ($argToken.Name -eq 'defaultfiles') {
                            $this.UpdateSettingsFromDefaultFiles($settings)
                        }
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

    [void]UpdateSettingsFromDefaultFiles([FindSettings]$settings) {
        if (Test-Path $script:defaultFindSettingsPath) {
            $this.UpdateSettingsFromFilePath($settings, $script:defaultFindSettingsPath)
        }
    }

    [void]UpdateSettingsFromArgs([FindSettings]$settings, [string[]]$argList) {
        $argTokens = $this.ArgTokenizer.TokenizeArgs($argList)
        $this.UpdateSettingsFromArgTokens($settings, $argTokens)
    }

    [FindSettings]SettingsFromArgs([string[]]$argList) {
        $settings = [FindSettings]::new()
        # default PrintFiles to true since we're using via CLI
        $settings.PrintFiles = $true

        # if a defaultfiles option isn't included, go ahead and apply default files now
        $defaultFilesArgs = $argList | Where-Object { $_.EndsWith('defaultfiles') }
        if (-not $defaultFilesArgs) {
            $this.UpdateSettingsFromDefaultFiles($settings)
        }

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
    [string]$FilePath
    [FileType]$Type
    [long]$Size
    [DateTime]$LastMod

    FileResult([string]$FilePath, [FileType]$Type) {
        $this.Containers = @()
        $this.FilePath = $FilePath
        $this.Type = $Type
        $this.Size = 0
        $this.LastMod = [DateTime]::MinValue
    }

    FileResult([string]$FilePath, [FileType]$Type, [long]$Size, [DateTime]$LastMod) {
        $this.Containers = @()
        $this.FilePath = $FilePath
        $this.Type = $Type
        $this.Size = $Size
        $this.LastMod = $LastMod
    }
    
    [string]ToString() {
        return $this.FilePath
    }
}
#endregion


#region FileResultFormatter
########################################
# FileResultFormatter
########################################
class FileResultFormatter {
    [FindSettings]$Settings
    [Scriptblock]$FormatDirPathBlock
    [Scriptblock]$FormatFileNameBlock

    FileResultFormatter([FindSettings]$settings) {
        $this.Settings = $settings
        if ($settings.Colorize -and $settings.InDirPatterns.Length -gt 0) {
            $this.FormatDirPathBlock = {
                param([string]$dirPath)
                return $this.FormatDirPathWithColor($dirPath)
            }
        } else {
            $this.FormatDirPathBlock = {
                param([string]$dirPath)
                if ([string]::IsNullOrEmpty($dirPath)) {
                    return "."
                }
                return $dirPath
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

    [string]ColorizeString([string]$s, [int]$matchStartIdx, [int]$matchEndIdx, [Color]$color) {
        $prefix = ''
        if ($matchStartIdx -gt 0) {
            $prefix = $s.Substring(0, $matchStartIdx)
        }
        $match = $s.Substring($matchStartIdx, $matchEndIdx - $matchStartIdx)
        $suffix = ''
        if ($matchEndIdx -lt $s.Length) {
            $suffix = $s.Substring($matchEndIdx)
        }
        $consoleColor = GetConsoleColorForColor($color)
        return "$prefix${consoleColor}$match${script:ConsoleReset}$suffix"
    }

    [string]FormatDirPathWithColor([string]$dirPath) {
        $formattedDirPath = "."
        if (-not [string]::IsNullOrEmpty($dirPath)) {
            $formattedDirPath = $dirPath;
            foreach ($dirPattern in $this.Settings.InDirPatterns) {
                $match = $dirPattern.Match($formattedDirPath)
                if ($match.Success) {
                    $formattedDirPath = $this.ColorizeString($formattedDirPath, $match.Index, $match.Index + $match.Length, $this.Settings.DirColor)
                    break
                }
            }
        }
        return $formattedDirPath
    }

    [string]FormatDirPath([string]$dirPath) {
        return $this.FormatDirPathBlock.Invoke($dirPath)
    }

    [string]FormatFileNameColor([string]$fileName) {
        $formattedFileName = $fileName
        foreach ($filePattern in $this.Settings.InFilePatterns) {
            $match = $filePattern.Match($formattedFileName)
            if ($match.Success) {
                $formattedFileName = $this.ColorizeString($formattedFileName, $match.Index, $match.Index + $match.Length, $this.Settings.FileColor)
                break
            }
        }
        if ($this.Settings.InExtensions.Count -gt 0) {
            $idx = $formattedFileName.LastIndexOf('.')
            if ($idx -gt 0 -and $idx -lt $formattedFileName.Length) {
                $formattedFileName = $this.ColorizeString($formattedFileName, $idx + 1, $formattedFileName.Length, $this.Settings.ExtColor)
            }
        }
        return $formattedFileName
    }

    [string]FormatFileName([string]$fileName) {
        return $this.FormatFileNameBlock.Invoke($fileName)
    }

    [string]FormatFilePath([string]$filePath) {
        $parent = $this.FormatDirPath([System.IO.Path]::GetDirectoryName($filePath))
        $fileName = $this.FormatFileName([System.IO.Path]::GetFileName($filePath))
        return [System.IO.Path]::Combine($parent, $fileName)
    }

    [string]FormatFileResult([FileResult]$result) {
        return $this.FormatFilePath($result.FilePath)
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
    [Scriptblock[]]$MatchingDirPathByHiddenTests
    [Scriptblock[]]$MatchingDirPathByInPatternsTests
    [Scriptblock[]]$MatchingDirPathByOutPatternsTests
    [Scriptblock[]]$MatchingFileNameByHiddenTests
    [Scriptblock[]]$MatchingArchiveExtensionTests
    [Scriptblock[]]$MatchingArchiveFileNameTests
    [Scriptblock[]]$MatchingArchiveFilePathTests
    [Scriptblock[]]$MatchingArchiveFileResultTests
    [Scriptblock[]]$MatchingExtensionTests
    [Scriptblock[]]$MatchingFileNameTests
    [Scriptblock[]]$MatchingFilePathTests
    [Scriptblock[]]$MatchingFileTypeTests
    [Scriptblock[]]$MatchingFileSizeTests
    [Scriptblock[]]$MatchingLastModTests
    [Scriptblock[]]$MatchingFileResultTests

    Finder([FindSettings]$settings) {
        $this.Settings = $settings
        $this.FileTypes = [FileTypes]::new()
        $this.ValidateSettings()
        $this.MatchingDirPathByHiddenTests = $this.GetIsMatchingDirPathByHiddenTests()
        $this.MatchingDirPathByInPatternsTests = $this.GetIsMatchingDirPathByInPatternsTests()
        $this.MatchingDirPathByOutPatternsTests = $this.GetIsMatchingDirPathByOutPatternsTests()
        $this.MatchingFileNameByHiddenTests = $this.GetIsMatchingFileNameByHiddenTests()
        $this.MatchingArchiveExtensionTests = $this.GetMatchingArchiveExtensionTests()
        $this.MatchingArchiveFileNameTests = $this.GetMatchingArchiveFileNameTests()
        $this.MatchingArchiveFilePathTests = $this.GetMatchingArchiveFilePathTests()
        $this.MatchingArchiveFileResultTests = $this.GetMatchingArchiveFileResultTests()
        $this.MatchingExtensionTests = $this.GetMatchingExtensionTests()
        $this.MatchingFileNameTests = $this.GetMatchingFileNameTests()
        $this.MatchingFilePathTests = $this.GetMatchingFilePathTests()
        $this.MatchingFileTypeTests = $this.GetMatchingFileTypeTests()
        $this.MatchingFileSizeTests = $this.GetMatchingFileSizeTests()
        $this.MatchingLastModTests = $this.GetMatchingLastModTests()
        $this.MatchingFileResultTests = $this.GetMatchingFileResultTests()
    }

    [void]ValidateSettings() {
        if ($null -eq $this.Settings.Paths -or $this.Settings.Paths.Count -eq 0) {
            throw "Startpath not defined"
        }
        foreach ($p in $this.Settings.Paths) {
            if (-not (Test-Path $p)) {
                $p = ExpandPath($p)
                if (-not (Test-Path $p)) {
                    throw "Startpath not found"
                }
            }
            if ((Get-Item $p).LinkType -eq "SymbolicLink") {
                if (-not $this.Settings.FollowSymlinks) {
                    throw "Startpath does not match find settings"
                }
            }
            if (Test-Path -Path $p -PathType Container) {
                if (-not $this.IsTraversableDirPath($p)) {
                    throw "Startpath does not match find settings"
                }
            } elseif (Test-Path -Path $p -PathType Leaf) {
                if ($null -eq $this.FilterFilePathToFileResult($p)) {
                    throw "Startpath does not match find settings"
                }
            } else {
                # TODO: handle start path as symlink
                # TODO: start path is unknown/invalid type
                throw "Startpath does not match find settings"
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
        foreach ($p in $patterns) {
            if ($s -match $p) {
                return $true
            }
        }
        return $false
    }

    [bool]AnyMatchesAnyPattern([string[]]$strs, [regex[]]$patterns) {
        foreach ($s in $strs) {
            if ($this.MatchesAnyPattern($s, $patterns)) {
                return $true
            }
        }
        return $false
    }

    [Scriptblock[]]GetIsMatchingDirPathByHiddenTests() {
        $tests = @()
        if (-not $this.Settings.IncludeHidden) {
            $tests += {
                param([string]$d)
                return !(IsHiddenPath $d)
            }
        }
        return $tests
    }

    [bool]IsMatchingDirPathByHidden([string]$d) {
        foreach ($t in $this.MatchingDirPathByHiddenTests) {
            if (-not $t.Invoke($d)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetIsMatchingDirPathByInPatternsTests() {
        $tests = @()
        if ($this.Settings.InDirPatterns.Count -gt 0) {
            $tests += {
                param([string]$d)
                $elems = PathElems($d)
                return $this.AnyMatchesAnyPattern($elems, $this.Settings.InDirPatterns)
            }
        }
        return $tests
    }

    [bool]IsMatchingDirPathByInPatterns([string]$d) {
        foreach ($t in $this.MatchingDirPathByInPatternsTests) {
            if (-not $t.Invoke($d)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetIsMatchingDirPathByOutPatternsTests() {
        $tests = @()
        if ($this.Settings.OutDirPatterns.Count -gt 0) {
            $tests += {
                param([string]$d)
                $elems = PathElems($d)
                return !$this.MatchesAnyPattern($elems, $this.Settings.OutDirPatterns)
            }
        }
        return $tests
    }

    [bool]IsMatchingDirPathByOutPatterns([string]$d) {
        foreach ($t in $this.MatchingDirPathByOutPatternsTests) {
            if (-not $t.Invoke($d)) {
                return $false
            }
        }
        return $true
    }

    [bool]IsTraversableDirPath([string]$d) {
        return $this.IsMatchingDirPathByHidden($d) -and
                $this.IsMatchingDirPathByOutPatterns($d)
    }

    [bool]IsMatchingDirPath([string]$d) {
        return $this.IsMatchingDirPathByHidden($d) -and 
                $this.IsMatchingDirPathByInPatterns($d) -and
                $this.IsMatchingDirPathByOutPatterns($d)
    }

    [bool]IsNullOrMatchingDirPath([string]$d) {
        return [string]::IsNullOrEmpty($d) -or
                $this.IsMatchingDirPath($d)
    }

    [Scriptblock[]]GetIsMatchingFileNameByHiddenTests() {
        $tests = @()
        if (-not $this.Settings.IncludeHidden) {
            $tests += {
                param([string]$fileName)
                return !(IsHiddenName $fileName)
            }
        }
        return $tests
    }

    [bool]IsMatchingFileNameByHidden([string]$fileName) {
        foreach ($t in $this.MatchingFileNameByHiddenTests) {
            if (-not $t.Invoke($fileName)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetMatchingArchiveExtensionTests() {
        $tests = @()
        if ($this.Settings.InArchiveExtensions.Count -gt 0) {
            $tests += {
                param([string]$ext)
                return $this.Settings.InArchiveExtensions.Contains($ext)
            }
        } elseif ($this.Settings.OutArchiveExtensions.Count -gt 0) {
            $tests += {
                param([string]$ext)
                return !$this.Settings.OutArchiveExtensions.Contains($ext)
            }
        }
        return $tests
    }

    [Scriptblock[]]GetMatchingArchiveFileNameTests() {
        $tests = @()
        if ($this.Settings.InArchiveFilePatterns.Count -gt 0) {
            $tests += {
                param([string]$fileName)
                return $this.MatchesAnyPattern($fileName, $this.Settings.InArchiveFilePatterns)
            }
        } elseif ($this.Settings.OutArchiveFilePatterns.Count -gt 0) {
            $tests += {
                param([string]$fileName)
                return !$this.MatchesAnyPattern($fileName, $this.Settings.OutArchiveFilePatterns)
            }
        }
        return $tests
    }

    [Scriptblock[]]GetMatchingArchiveFilePathTests() {
        $tests = @()
        if ($this.MatchingArchiveExtensionTests.Count -gt 0) {
            $tests += {
                param([string]$filePath)
                $ext = GetFileExtension($filePath)
                foreach ($t in $this.MatchingArchiveExtensionTests) {
                    if (-not $t.Invoke($ext)) {
                        return $false
                    }
                }
                return $true
            }
        }
        if ($this.MatchingArchiveFileNameTests.Count -gt 0) {
            $tests += {
                param([string]$filePath)
                $fileName = [System.IO.Path]::GetFileName($filePath)
                foreach ($t in $this.MatchingArchiveFileNameTests) {
                    if (-not $t.Invoke($fileName)) {
                        return $false
                    }
                }
                return $true
            }
        }
        return $tests
    }

    [bool]IsMatchingArchiveFilePath([string]$filePath) {
        foreach ($t in $this.MatchingArchiveFilePathTests) {
            if (-not $t.Invoke($filePath)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetMatchingArchiveFileResultTests() {
        $tests = @()

        if ($this.MatchingArchiveFilePathTests.Count -gt 0) {
            $tests += {
                param([FileResult]$fr)
                foreach ($t in $this.MatchingArchiveFilePathTests) {
                    if (-not $t.Invoke($fr.FilePath)) {
                        return $false
                    }
                }
                return $true
            }
        }
        return $tests
    }

    [bool]IsMatchingArchiveFileResult([FileResult]$fr) {
        foreach ($t in $this.MatchingArchiveFileResultTests) {
            if (-not $t.Invoke($fr)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetMatchingExtensionTests() {
        $tests = @()
        if ($this.Settings.InExtensions.Count -gt 0) {
            $tests += {
                param([string]$ext)
                return $this.Settings.InExtensions.Contains($ext)
            }
        } elseif ($this.Settings.OutExtensions.Count -gt 0) {
            $tests += {
                param([string]$ext)
                return !$this.Settings.OutExtensions.Contains($ext)
            }
        }
        return $tests
    }

    [Scriptblock[]]GetMatchingFileNameTests() {
        $tests = @()
        if ($this.Settings.InFilePatterns.Count -gt 0) {
            $tests += {
                param([string]$fileName)
                return $this.MatchesAnyPattern($fileName, $this.Settings.InFilePatterns)
            }
        } elseif ($this.Settings.OutFilePatterns.Count -gt 0) {
            $tests += {
                param([string]$fileName)
                return !$this.MatchesAnyPattern($fileName, $this.Settings.OutFilePatterns)
            }
        }
        return $tests
    }

    [Scriptblock[]]GetMatchingFilePathTests() {
        $tests = @()
        if ($this.MatchingExtensionTests.Count -gt 0) {
            $tests += {
                param([string]$filePath)
                $ext = GetFileExtension($filePath)
                foreach ($t in $this.MatchingExtensionTests) {
                    if (-not $t.Invoke($ext)) {
                        return $false
                    }
                }
                return $true
            }
        }
        if ($this.MatchingFileNameTests.Count -gt 0) {
            $tests += {
                param([string]$filePath)
                $fileName = [System.IO.Path]::GetFileName($filePath)
                foreach ($t in $this.MatchingFileNameTests) {
                    if (-not $t.Invoke($fileName)) {
                        return $false
                    }
                }
                return $true
            }
        }
        return $tests
    }

    [bool]IsMatchingFilePath([string]$filePath) {
        foreach ($t in $this.MatchingFilePathTests) {
            if (-not $t.Invoke($filePath)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetMatchingFileTypeTests() {
        $tests = @()
        if ($this.Settings.InFileTypes.Count -gt 0) {
            $tests += {
                param([FileType]$fileType)
                return $this.Settings.InFileTypes.Contains($fileType)
            }
        } elseif ($this.Settings.OutFileTypes.Count -gt 0) {
            $tests += {
                param([FileType]$fileType)
                return !$this.Settings.OutFileTypes.Contains($fileType)
            }
        }
        return $tests
    }

    [bool]IsMatchingFileType([FileType]$fileType) {
        foreach ($t in $this.MatchingFileTypeTests) {
            if (-not $t.Invoke($fileType)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetMatchingFileSizeTests() {
        $tests = @()
        if ($this.Settings.MaxSize -gt 0) {
            $tests += {
                param([long]$fileSize)
                return $fileSize -le $this.Settings.MaxSize
            }
        }
        if ($this.Settings.MinSize -gt 0) {
            $tests += {
                param([long]$fileSize)
                return $fileSize -ge $this.Settings.MinSize
            }
        }
        return $tests
    }

    [bool]IsMatchingFileSize([long]$fileSize) {
        foreach ($t in $this.MatchingFileSizeTests) {
            if (-not $t.Invoke($fileSize)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetMatchingLastModTests() {
        $tests = @()

        if ($this.Settings.MaxLastMod -gt [DateTime]::MinValue) {
            $tests += {
                param([DateTime]$lastMod)
                return $lastMod -le $this.Settings.MaxLastMod
            }
        }
        if ($this.Settings.MinLastMod -gt [DateTime]::MinValue) {
            $tests += {
                param([DateTime]$lastMod)
                return $lastMod -ge $this.Settings.MinLastMod
            }
        }
        return $tests
    }

    [bool]IsMatchingLastMod([DateTime]$lastMod) {
        foreach ($t in $this.MatchingLastModTests) {
            if (-not $t.Invoke($lastMod)) {
                return $false
            }
        }
        return $true
    }

    [Scriptblock[]]GetMatchingFileResultTests() {
        $tests = @()
        if ($this.MatchingFilePathTests.Count -gt 0) {
            $tests += {
                param([FileResult]$fr)
                return $this.IsMatchingFilePath($fr.FilePath)
            }
        }
        if ($this.MatchingFileTypeTests.Count -gt 0) {
            $tests += {
                param([FileResult]$fr)
                return $this.IsMatchingFileType($fr.Type)
            }
        }
        if ($this.MatchingFileSizeTests.Count -gt 0) {
            $tests += {
                param([FileResult]$fr)
                return $this.IsMatchingFileSize($fr.File.Length)
            }
        }
        if ($this.MatchingLastModTests.Count -gt 0) {
            $tests += {
                param([FileResult]$fr)
                return $this.IsMatchingLastMod($fr.File.LastWriteTimeUtc)
            }
        }
        return $tests
    }

    [bool]IsMatchingFileResult([FileResult]$fr) {
        foreach ($t in $this.MatchingFileResultTests) {
            if (-not $t.Invoke($fr)) {
                # Write-Host "$f did not pass test: $t"
                return $false
            }
        }
        return $true
    }

    [FileResult]FilterArchiveFilePathToFileResult([string]$filePath) {
        if (-not $this.Settings.IncludeArchives -and -not $this.Settings.ArchivesOnly) {
            return $null
        }
        if (-not $this.IsMatchingArchiveFilePath($filePath)) {
            return $null
        }

        return [FileResult]::new($filePath, [FileType]::Archive, 0, [DateTime]::MinValue)
    }

    [FileResult]FilterRegularFilePathToFileResult([string]$filePath, [FileType]$fileType) {
        if ($this.Settings.ArchivesOnly) {
            return $null
        }
        if (-not $this.IsMatchingFilePath($filePath) -or -not $this.IsMatchingFileType($fileType)) {
            return $null
        }

        [long]$size = 0
        [DateTime]$lastMod = [DateTime]::MinValue
        if ($this.MatchingFileSizeTests.Count -gt 0 -or $this.MatchingLastModTests.Count -gt 0) {
            $pathFile = [System.IO.FileInfo]::new($filePath)
            $size = $pathFile.Length
            $lastMod = $pathFile.LastWriteTimeUtc
            if (-not $this.IsMatchingFileSize($size) -or -not $this.IsMatchingLastMod($lastMod)) {
                return $null
            }
        }
        
        return [FileResult]::new($filePath, $fileType, $size, $lastMod)
    }

    [FileResult]FilterFilePathToFileResult([string]$filePath) {
        if (-not $this.IsNullOrMatchingDirPath([System.IO.Path]::GetDirectoryName($filePath)) -or 
                -not $this.IsMatchingFileNameByHidden([System.IO.Path]::GetFileName($filePath))) {
            return $null
        }

        $fileType = $this.FileTypes.GetFileTypeForFilePath($filePath)
        if ($fileType -eq [FileType]::Archive) {
            return $this.FilterArchiveFilePathToFileResult($filePath)
        }
        return $this.FilterRegularFilePathToFileResult($filePath, $fileType)
    }

    [FileResult[]]FilterFilePathsToFileResults([string[]]$filePaths) {
        $fileResults = @()
        foreach ($filePath in $filePaths) {
            $fileResult = $this.FilterFilePathToFileResult($filePath)
            if ($null -ne $fileResult) {
                $fileResults += $fileResult
            }
        }
        return $fileResults
    }

    [FileResult[]]RecGetDirPathResults([string]$dirPath, [int]$minDepth, [int]$maxDepth, [int]$currentDepth) {
        $recurse = $true
        if ($currentDepth -eq $maxDepth) {
            $recurse = $false
        } elseif ($maxDepth -gt -1 -and $currentDepth -gt $maxDepth) {
            return @()
        }

        # Get the dirs and files under file_path
        $subDirPaths = @()
        $subFilePaths = @()
        $fileResults = @()
        if ($recurse) {
            # Force is needed to get hidden dirs
            $pathDirs = Get-ChildItem -Force -Recurse:$false -Path $dirPath -Directory
            if (-not $this.Settings.FollowSymlinks) {
                # filter out symlinks
                $pathDirs = $pathDirs | Where-Object { $_.LinkType -ne "SymbolicLink" }
            }
            $dirNames = $pathDirs | Select-Object -ExpandProperty Name |
                    Where-Object { $this.IsTraversableDirPath($_) }
            $subDirPaths = $dirNames | ForEach-Object { [System.IO.Path]::Combine($dirPath, $_) }
        }
        if ($minDepth -lt 0 -or $currentDepth -ge $minDepth) {
            # Force is needed to get hidden files
            $pathFiles = Get-ChildItem -Force -Recurse:$false -Path $dirPath -File
            if (-not $this.Settings.FollowSymlinks) {
                # filter out symlinks
                $pathFiles = $pathFiles | Where-Object { $_.LinkType -ne "SymbolicLink" }
            }
            $fileNames = $pathFiles | Select-Object -ExpandProperty Name |
                    Where-Object { $this.IsMatchingFilePath($_) -and $this.IsMatchingFileType($this.FileTypes.GetFileTypeForFilePath($_)) }
            $subFilePaths = $fileNames | ForEach-Object { [System.IO.Path]::Combine($dirPath, $_) }
        }

        # Filter the dirs and files
        $fileResults += $this.FilterFilePathsToFileResults($subFilePaths)
        foreach ($subDirPath in $subDirPaths) {
            $fileResults += $this.RecGetDirPathResults($subDirPath, $minDepth, $maxDepth, $currentDepth + 1)
        }

        return $fileResults
    }

    [FileResult[]]GetPathResults([string]$path) {
        $fileResults = @()
        # Even though powershell understands ~, dotnet does not, so just start with expanded path
        $path = ExpandPath($path)
        if (-not (Test-Path $path)) {
            throw "Startpath not found"
        }
        if (Test-Path -Path $path -PathType Container) {
            # if max_depth is zero, we can skip since a directory cannot be a result
            if ($this.Settings.MaxDepth -eq 0) {
                return $fileResults
            }
            if ($this.IsTraversableDirPath($path)) {
                $maxDepth = $this.Settings.MaxDepth
                if (-not $this.Settings.Recursive) {
                    $maxDepth = 1
                }
                $fileResults += $this.RecGetDirPathResults($path, $this.Settings.MinDepth, $maxDepth, 1)
            } else {
                throw "Startpath does not match find settings"
            }
        } elseif (Test-Path -Path $path -PathType Leaf) {
            # if min_depth > zero, we can skip since the file is at depth zero
            if ($this.Settings.MinDepth -gt 0) {
                return @()
            }
            $pathFileResult = $this.FilterFilePathToFileResult($path)
            if ($null -ne $pathFileResult) {
                $fileResults += $pathFileResult
            } else {
                throw "Startpath does not match find settings"
            }
        } else {
            throw "Startpath does not match find settings"
        }
        return $fileResults
    }

    [FileResult[]]GetFileResults() {
        $fileResults = @()
        foreach ($path in $this.Settings.Paths) {
            $fileResults += $this.GetPathResults($path)
        }
        if ($fileResults.Count -gt 1) {
            $fileResultSorter = [FileResultSorter]::new($this.Settings)
            return $fileResultSorter.Sort($fileResults)
        }
        return $fileResults
    }

    [FileResult[]]Find() {
        return $this.GetFileResults()
    }
 
    [string[]]GetMatchingDirs([FileResult[]]$fileResults) {
        $dirs = @()
        if ($fileResults.Count -gt 0) {
            $dirs = $fileResults |
                    ForEach-Object { [System.IO.Path]::GetDirectoryName($_) } |
                    Select-Object -Unique
        }
        return $dirs
    }

    [void]PrintMatchingDirs([FileResult[]]$fileResults, [FileResultFormatter]$formatter) {
        $dirs = $this.GetMatchingDirs($fileResults)
        if ($dirs.Count -gt 0) {
            LogMsg("`nMatching directories ($($dirs.Count)):")
            foreach ($d in $dirs) {
                LogMsg($formatter.FormatDirPath($d))
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
