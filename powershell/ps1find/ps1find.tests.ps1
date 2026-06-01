# https://dev.to/omiossec/unit-testing-in-powershell-introduction-to-pester-1de7

using module 'Ps1FindModule'
# Import-Module -Name 'Ps1FindModule' -Verbose

#region FileTypes
Describe -tag "FileTypes" -name "test_is_archive_file" {
    It "file is archive file" {
        $filePath = 'archive.zip'
        $fileTypes = [FileTypes]::new()
        $fileTypes.IsArchiveFilePath($filePath) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_getfiletype_archive_file" {
    It "fileType is archive file" {
        $filePath = 'archive.zip'
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileTypeForFilePath($filePath)
        $fileType | Should -Be Archive
        $fileTypes.IsArchiveFilePath($filePath) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_getfiletype_audio_file" {
    It "fileType is audio file" {
        $filePath = 'music.mp3'
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileTypeForFilePath($filePath)
        $fileType | Should -Be Audio
        $fileTypes.IsAudioFilePath($filePath) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_binary_file" {
    It "file is binary file" {
        $filePath = 'binary.exe'
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileTypeForFilePath($filePath)
        $fileType | Should -Be Binary
        $fileTypes.IsBinaryFilePath($filePath) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_code_file" {
    It "file is code file" {
        $filePath = 'script.ps1'
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileTypeForFilePath($filePath)
        $fileType | Should -Be Code
        $fileTypes.IsCodeFilePath($filePath) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_font_file" {
    It "file is font file" {
        $filePath = 'font.ttf'
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileTypeForFilePath($filePath)
        $fileType | Should -Be Font
        $fileTypes.IsFontFilePath($filePath) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_image_file" {
    It "file is image file" {
        $filePath = 'image.png'
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileTypeForFilePath($filePath)
        $fileType | Should -Be Image
        $fileTypes.IsImageFilePath($filePath) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_text_file" {
    It "file is text file" {
        $filePath = 'text.txt'
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileTypeForFilePath($filePath)
        $fileType | Should -Be Text
        $fileTypes.IsTextFilePath($filePath) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_video_file" {
    It "file is video file" {
        $filePath = 'movie.mp4'
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileTypeForFilePath($filePath)
        $fileType | Should -Be Video
        $fileTypes.IsVideoFilePath($filePath) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_xml_file" {
    It "file is xml file" {
        $filePath = 'content.xml'
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileTypeForFilePath($filePath)
        $fileType | Should -Be Xml
        $fileTypes.IsXmlFilePath($filePath) | Should -BeTrue
    }
}
#endregion


#region FileUtil
Describe -tag "FileUtil" -name "test_is_dot_dir_single_dot" {
    It ". is dot dir" {
        $filePath = '.'
        IsDotDir($filePath) | Should -BeTrue
    }
}

Describe -tag "FileUtil" -name "test_is_dot_dir_double_dot" {
    It ".. is dot dir" {
        $filePath = '..'
        IsDotDir($filePath) | Should -BeTrue
    }
}

Describe -tag "FileUtil" -name "test_is_dot_dir_is_not_dot_dir" {
    It ".git is not dot dir" {
        $filePath = '.git'
        IsDotDir($filePath) | Should -BeFalse
    }
}

Describe -tag "FileUtil" -name "test_expand_tilde_path" {
    It "expand tilde path" {
        $homePath = GetHomePath
        $expandedPath = ExpandPath('~')
        $expandedPath -eq $homePath | Should -BeTrue
    }
}

Describe -tag "FileUtil" -name "test_expand_path_with_tilde" {
    It "expand path with tilde" {
        $homePath = GetHomePath
        $expandedPath = ExpandPath('~/src/xfind')
        $expandedPath -eq "$homePath/src/xfind" | Should -BeTrue
    }
}

Describe -tag "FileUtil" -name "test_expand_path_with_tilde_and_name" {
    It "expand path with tilde and name" {
        $homePath = GetHomePath
        $expandedPath = ExpandPath('~cary/src/xfind')
        $expandedPath -eq "$homePath/src/xfind" | Should -BeTrue
    }
}

#Describe -tag "FileUtil" -name "test_is_hidden_hidden_dir" {
#    It ".git is hidden dir" {
#        $fileInfo = [System.IO.FileInfo]::new('.git')
#        IsHiddenFile($fileInfo) | Should -BeTrue
#    }
#}

Describe -tag "FileUtil" -name "test_is_hidden_name_dot_dir" {
    It ". is not hidden name" {
        IsHiddenName('.') | Should -BeFalse
    }
}

Describe -tag "FileUtil" -name "test_is_hidden_name_double_dot_dir" {
    It ".. is not hidden name" {
        IsHiddenName('..') | Should -BeFalse
    }
}

Describe -tag "FileUtil" -name "test_is_hidden_name_hidden_file_name" {
    It ".gitignore is hidden name" {
        IsHiddenName('.gitignore') | Should -BeTrue
    }
}

Describe -tag "FileUtil" -name "test_is_hidden_name_non_hidden_file_name" {
    It "file.txt is not hidden name" {
        IsHiddenName('file.txt') | Should -BeFalse
    }
}

Describe -tag "FileUtil" -name "test_is_hidden_directory_dot_dir" {
    It ". is not hidden dir" {
        $dir = '.'
        IsHiddenPath($dir) | Should -BeFalse
    }
}

Describe -tag "FileUtil" -name "test_is_hidden_directory_double_dot_dir" {
    It ".. is not hidden dir" {
        $dir = '..'
        IsHiddenPath($dir) | Should -BeFalse
    }
}

Describe -tag "FileUtil" -name "test_is_hidden_directory_dot_git_dir" {
    It ".git is hidden dir" {
        $dir = './.git'
        IsHiddenPath($dir) | Should -BeTrue
    }
}
#endregion


#region FindSettings
Describe -tag "FindSettings" -name "test_default_settings" {
    It "has valid default settings" {
        $settings = [FindSettings]::new()

        $settings.ArchivesOnly | Should -BeFalse
        $settings.Colorize | Should -BeTrue
        $settings.Debug | Should -BeFalse
        $settings.DefaultFiles | Should -BeTrue
        $settings.IncludeHidden | Should -BeFalse
        $settings.IncludeArchives | Should -BeFalse
        $settings.PrintDirs | Should -BeFalse
        $settings.PrintFiles | Should -BeFalse
        $settings.PrintUsage | Should -BeFalse
        $settings.Recursive | Should -BeTrue
        $settings.Verbose | Should -BeFalse
    }
}

Describe -tag "FindSettings" -name "test_add_single_extension" {
    It "has one extension" {
        $settings = [FindSettings]::new()
        $settings.InExtensions += $settings.GetExtensions('ps1')
        $settings.InExtensions.Count | Should -BeExactly 1
    }
}

Describe -tag "FindSettings" -name "test_add_comma_delimited_extensions" {
    It "has two extensions" {
        $settings = [FindSettings]::new()
        $settings.InExtensions += $settings.GetExtensions('ps1,php')
        $settings.InExtensions.Count | Should -BeExactly 2
    }
}

Describe -tag "FindSettings" -name "test_add_extensions_array" {
    It "has two extensions" {
        $settings = [FindSettings]::new()
        $settings.InExtensions += @('ps1' ,'php')
        $settings.InExtensions.Count | Should -BeExactly 2
    }
}

Describe -tag "FindSettings" -name "test_add_patterns_string" {
    It "has one pattern" {
        $settings = [FindSettings]::new()
        $settings.InFilePatterns += [regex]"pattern"
        $settings.InFilePatterns.Count | Should -BeExactly 1
    }
}

Describe -tag "FindSettings" -name "test_add_patterns_array" {
    It "has one pattern" {
        $settings = [FindSettings]::new()
        $settings.InFilePatterns += @([regex]"pat1", [regex]"pat2")
        $settings.InFilePatterns.Count | Should -BeExactly 2
    }
}

Describe -tag "FindSettings" -name "test_set_archives_only" {
    It "archives_only and include_archives are true" {
        $settings = [FindSettings]::new()
        $settings.SetArchivesOnly($true)
        $settings.ArchivesOnly | Should -BeTrue
        $settings.IncludeArchives | Should -BeTrue
    }
}

Describe -tag "FindSettings" -name "test_set_debug" {
    It "debug and verbose are true" {
        $settings = [FindSettings]::new()
        $settings.SetDebug($true)
        $settings.Debug | Should -BeTrue
        $settings.Verbose | Should -BeTrue
    }
}
#endregion


#region FileResult
Describe -tag "FileResult" -name "test_file_result_abs_path" {
    It "matches by default" {
        $path = "/home/user/src/xfind/powershell/ps1find";
        $filename = 'ps1find.ps1';
        $filePath = "$path/$filename"
        $fileresult = [FileResult]::new($filePath, [FileType]::Code, 0, [DateTime]::MinValue);
        $fileResult.FilePath | Should -BeExactly "/home/user/src/xfind/powershell/ps1find/ps1find.ps1"
    }
}
#endregion


#region ArgTokenizer
Describe -tag "ArgTokenizer" -name "test_tokenize_args_no_args" {
    It "equals no tokens" {
        $options = [FindOptions]::new()
        $_args = @()
        $tokens = $options.ArgTokenizer.TokenizeArgs($_args)

        $tokens.Count | Should -BeExactly 0
    }
}

Describe -tag "ArgTokenizer" -name "test_tokenize_args_valid_args" {
    It "has valid tokens" {
        $options = [FindOptions]::new()
        $_args = @('--debug', '-x', 'php,py', '.')
        $tokens = $options.ArgTokenizer.TokenizeArgs($_args)

        $tokens.Count | Should -BeExactly 3
        $tokens[0].Name | Should -BeExactly "debug"
        $tokens[0].Type | Should -BeExactly Bool
        $tokens[0].Value | Should -BeExactly True
        $tokens[1].Name | Should -BeExactly "in-ext"
        $tokens[1].Type | Should -BeExactly Str
        $tokens[1].Value | Should -BeExactly "php,py"
        $tokens[2].Name | Should -BeExactly "path"
        $tokens[2].Type | Should -BeExactly Str
        $tokens[2].Value | Should -BeExactly "."
    }
}
#endregion


#region FindOptions
Describe -tag "FindOptions" -name "test_settings_from_args_no_args" {
    It "equals default settings" {
        $options = [FindOptions]::new()
        $_args = @()
        $settings = $options.SettingsFromArgs($_args)

        $settings.ArchivesOnly | Should -BeFalse
        $settings.Debug | Should -BeFalse
        $settings.IncludeHidden | Should -BeFalse
        $settings.IncludeArchives | Should -BeFalse
        $settings.PrintDirs | Should -BeFalse
        $settings.PrintFiles | Should -BeTrue
        $settings.PrintUsage | Should -BeFalse
        $settings.Recursive | Should -BeTrue
        $settings.Verbose | Should -BeFalse
    }
}

Describe -tag "FindOptions" -name "test_settings_from_args_valid_args" {
    It "has valid settings" {
        $options = [FindOptions]::new()
        $_args = @('-x', 'php,py', '.')
        $settings = $options.SettingsFromArgs($_args)

        $settings.ArchivesOnly | Should -BeFalse
        $settings.Debug | Should -BeFalse
        $settings.IncludeHidden | Should -BeFalse
        $settings.IncludeArchives | Should -BeFalse
        $settings.PrintDirs | Should -BeFalse
        $settings.PrintFiles | Should -BeTrue
        $settings.PrintUsage | Should -BeFalse
        $settings.Recursive | Should -BeTrue
        $settings.Verbose | Should -BeFalse
    }
}
#endregion


#region Finder
Describe -tag "Finder" -name "test_is_matching_dir_no_patterns" {
    It "matches by default" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $finder = [Finder]::new($settings)
        $dirPath = '.'
        $finder.IsMatchingDirPath($dirPath) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_dir_matches_in_pattern" {
    It "matches by pattern" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.InDirPatterns += @([regex]'find')
        $settings.IncludeHidden = $true
        $finder = [Finder]::new($settings)
        $dirPath = './ps1find'
        $finder.IsMatchingDirPath($dirPath) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_file_matches_by_default" {
    It "matches by default" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $finder = [Finder]::new($settings)
        $file = 'ps1find.ps1'
        $fileResult = [FileResult]::new($file, [FileType]::Code);
        $finder.IsMatchingFileResult($fileResult) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_file_matches_in_extension" {
    It "matches by extension" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.InExtensions += @('.ps1')
        $finder = [Finder]::new($settings)
        $file = 'ps1find.ps1'
        $fileResult = [FileResult]::new($file, [FileType]::Code);
        $finder.IsMatchingFileResult($fileResult) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_archive_file_matches_by_default" {
    It "matches by default" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $finder = [Finder]::new($settings)
        $file = 'archive.zip'
        $fileResult = [FileResult]::new($file, [FileType]::Archive);
        $finder.IsMatchingArchiveFileResult($fileResult) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_archive_file_matches_in_extension" {
    It "matches by extension" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.InArchiveExtensions += @('.zip')
        $finder = [Finder]::new($settings)
        $file = 'archive.zip'
        $fileResult = [FileResult]::new($file, [FileType]::Archive);
        $finder.IsMatchingArchiveFileResult($fileResult) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_matches_by_default" {
    It "matches by default" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.IncludeHidden = $true
        $finder = [Finder]::new($settings)
        $file = 'ps1find.ps1'
        $fileResult = $finder.FilterFilePathToFileResult($file)
        $fileResult | Should -Not -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_is_matching_file" {
    It "matches by extension" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.IncludeHidden = $true
        $settings.InExtensions += $settings.GetExtensions('ps1')
        $finder = [Finder]::new($settings)
        $file = 'ps1find.ps1'
        $fileResult = $finder.FilterFilePathToFileResult($file)
        $fileResult | Should -Not -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_not_is_matching_file" {
    It "does not match by extension" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.InExtensions += @('.php')
        $finder = [Finder]::new($settings)
        $file = 'ps1find.ps1'
        $fileResult = $finder.FilterFilePathToFileResult($file)
        $fileResult | Should -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_is_hidden_file" {
    It "does not match because hidden" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $finder = [Finder]::new($settings)
        $file = '.gitignore'
        $fileResult = $finder.FilterFilePathToFileResult($file)
        $fileResult | Should -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_hidden_include_hidden" {
    It "matches because includehidden" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.IncludeHidden = $true
        $finder = [Finder]::new($settings)
        $file = '.gitignore'
        $fileResult = $finder.FilterFilePathToFileResult($file)
        $fileResult | Should -Not -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_archive_no_include_archives" {
    It "does not match because archive" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $finder = [Finder]::new($settings)
        $file = 'archive.zip'
        $fileResult = $finder.FilterFilePathToFileResult($file)
        $fileResult | Should -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_archive_include_archives" {
    It "matches because include_hidden" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.IncludeArchives = $true
        $settings.IncludeHidden = $true
        $finder = [Finder]::new($settings)
        $file = 'archive.zip'
        $fileResult = $finder.FilterFilePathToFileResult($file)
        $fileResult | Should -Not -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_archive_archives_only" {
    It "matches because archive + set_archives_only" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.IncludeHidden = $true
        $settings.SetArchivesOnly($true)
        $finder = [Finder]::new($settings)
        $file = 'archive.zip'
        $fileResult = $finder.FilterFilePathToFileResult($file)
        $fileResult | Should -Not -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_nonarchive_archives_only" {
    It "does not match because non-archive + set_archives_only" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.SetArchivesOnly($true)
        $finder = [Finder]::new($settings)
        $file = 'ps1find.ps1'
        $fileResult = $finder.FilterFilePathToFileResult($file)
        $fileResult | Should -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_follow_symlinks_default_settings" {
    It "excludes symlinks by default" {
        $settings = [FindSettings]::new()
        $binPath = Join-Path -Path "$env:XFIND_PATH" -ChildPath 'bin'
        $settings.Paths += @($binPath)
        $finder = [Finder]::new($settings)
        $fileResults = $finder.Find()
        if ($fileResults.Count -gt 0) {
            $fileResults.Count | Should -BeLessThan 4
        }
    }
}

Describe -tag "Finder" -name "test_follow_symlinks_follow_symlinks" {
    It "includes symlinks with followsymlinks" {
        $settings = [FindSettings]::new()
        $binPath = Join-Path -Path $env:XFIND_PATH -ChildPath 'bin'
        $settings.Paths += @($binPath)
        $settings.FollowSymlinks = $true
        $finder = [Finder]::new($settings)
        $fileResults = $finder.Find()
        if ($fileResults.Count -gt 0) {
            $fileResults.Count | Should -BeGreaterThan 2
        }
    }
}

Describe -tag "Finder" -name "test_follow_symlinks_no_follow_symlinks" {
    It "excludes symlinks with nofollowsymlinks" {
        $settings = [FindSettings]::new()
        $binPath = Join-Path -Path $env:XFIND_PATH -ChildPath 'bin'
        $settings.Paths += @($binPath)
        $settings.FollowSymlinks = $false
        $finder = [Finder]::new($settings)
        $fileResults = $finder.Find()
        if ($fileResults.Count -gt 0) {
            $fileResults.Count | Should -BeLessThan 4
        }
    }
}
#endregion
