# https://dev.to/omiossec/unit-testing-in-powershell-introduction-to-pester-1de7

using module 'Ps1FindModule'
# Import-Module -Name 'Ps1FindModule' -Verbose

#region FileTypes
Describe -tag "FileTypes" -name "test_is_archive_file" {
    It "file is archive file" {
        $fileInfo = [System.IO.FileInfo]::new('archive.zip')
        $fileTypes = [FileTypes]::new()
        $fileTypes.IsArchiveFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_getfiletype_archive_file" {
    It "fileType is archive file" {
        $fileInfo = [System.IO.FileInfo]::new('archive.zip')
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileType($fileInfo)
        $fileType | Should -Be Archive
        $fileTypes.IsArchiveFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_getfiletype_audio_file" {
    It "fileType is audio file" {
        $fileInfo = [System.IO.FileInfo]::new('music.mp3')
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileType($fileInfo)
        $fileType | Should -Be Audio
        $fileTypes.IsAudioFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_binary_file" {
    It "file is binary file" {
        $fileInfo = [System.IO.FileInfo]::new('binary.exe')
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileType($fileInfo)
        $fileType | Should -Be Binary
        $fileTypes.IsBinaryFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_code_file" {
    It "file is code file" {
        $fileInfo = [System.IO.FileInfo]::new('script.ps1')
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileType($fileInfo)
        $fileType | Should -Be Code
        $fileTypes.IsCodeFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_font_file" {
    It "file is font file" {
        $fileInfo = [System.IO.FileInfo]::new('font.ttf')
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileType($fileInfo)
        $fileType | Should -Be Font
        $fileTypes.IsFontFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_image_file" {
    It "file is image file" {
        $fileInfo = [System.IO.FileInfo]::new('image.png')
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileType($fileInfo)
        $fileType | Should -Be Image
        $fileTypes.IsImageFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_text_file" {
    It "file is text file" {
        $fileInfo = [System.IO.FileInfo]::new('text.txt')
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileType($fileInfo)
        $fileType | Should -Be Text
        $fileTypes.IsTextFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_video_file" {
    It "file is video file" {
        $fileInfo = [System.IO.FileInfo]::new('movie.mp4')
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileType($fileInfo)
        $fileType | Should -Be Video
        $fileTypes.IsVideoFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileTypes" -name "test_is_xml_file" {
    It "file is xml file" {
        $fileInfo = [System.IO.FileInfo]::new('content.xml')
        $fileTypes = [FileTypes]::new()
        $fileType = $fileTypes.GetFileType($fileInfo)
        $fileType | Should -Be Xml
        $fileTypes.IsXmlFile($fileInfo) | Should -BeTrue
    }
}
#endregion


#region FileUtil
Describe -tag "FileUtil" -name "test_is_dot_dir_single_dot" {
    It ". is dot dir" {
        $fileInfo = [System.IO.FileInfo]::new('.')
        IsDotDir($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileUtil" -name "test_is_dot_dir_double_dot" {
    It ".. is dot dir" {
        $fileInfo = [System.IO.FileInfo]::new('..')
        IsDotDir($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileUtil" -name "test_is_dot_dir_is_not_dot_dir" {
    It ".git is not dot dir" {
        $fileInfo = [System.IO.FileInfo]::new('.git')
        IsDotDir($fileInfo) | Should -BeFalse
    }
}

Describe -tag "FileUtil" -name "test_is_hidden_hidden_dir" {
    It ".git is hidden dir" {
        $fileInfo = [System.IO.FileInfo]::new('.git')
        IsHiddenFile($fileInfo) | Should -BeTrue
    }
}

Describe -tag "FileUtil" -name "test_is_hidden_dot_dir" {
    It ". is not hidden dir" {
        # $fileInfo = [System.IO.FileInfo]::new('.')
        # IsHiddenFile($fileInfo) | Should -BeFalse
        IsHiddenFileName('.') | Should -BeFalse
    }
}

Describe -tag "FileUtil" -name "test_is_hidden_double_dot_dir" {
    It ".. is not hidden dir" {
        # $fileInfo = [System.IO.FileInfo]::new('..')
        # IsHiddenFile($fileInfo) | Should -BeFalse
        IsHiddenFileName('..') | Should -BeFalse
    }
}
#endregion


#region FindSettings
Describe -tag "FindSettings" -name "test_default_settings" {
    It "has valid default settings" {
        $settings = [FindSettings]::new()

        $settings.ArchivesOnly | Should -BeFalse
        $settings.Debug | Should -BeFalse
        $settings.IncludeHidden | Should -BeFalse
        $settings.IncludeArchives | Should -BeFalse
        $settings.ListDirs | Should -BeFalse
        $settings.ListFiles | Should -BeFalse
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
        $file = [System.IO.FileInfo]::new("$path/$filename")
        $fileresult = [FileResult]::new($file, [FileType]::Code);
        $fileResult.File.ToString() | Should -BeExactly "/home/user/src/xfind/powershell/ps1find/ps1find.ps1"
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
        $settings.ListDirs | Should -BeFalse
        $settings.ListFiles | Should -BeTrue
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
        $settings.ListDirs | Should -BeFalse
        $settings.ListFiles | Should -BeTrue
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
        $dir = [System.IO.DirectoryInfo]::new('.')
        $finder.IsMatchingDir($dir) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_dir_matches_in_pattern" {
    It "matches by pattern" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.InDirPatterns += @([regex]'find')
        $settings.IncludeHidden = $true
        $finder = [Finder]::new($settings)
        $dir = [System.IO.DirectoryInfo]::new('./ps1find')
        $finder.IsMatchingDir($dir) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_file_matches_by_default" {
    It "matches by default" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $finder = [Finder]::new($settings)
        $file = [System.IO.FileInfo]::new('ps1find.ps1')
        $fileResult = [FileResult]::new($file, [FileType]::Code);
        $finder.IsMatchingFileResult($fileResult) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_file_matches_in_extension" {
    It "matches by pattern" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.InExtensions += @('.ps1')
        $finder = [Finder]::new($settings)
        $file = [System.IO.FileInfo]::new('ps1find.ps1')
        $fileResult = [FileResult]::new($file, [FileType]::Code);
        $finder.IsMatchingFileResult($fileResult) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_archive_file_matches_by_default" {
    It "matches by default" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $finder = [Finder]::new($settings)
        $file = [System.IO.FileInfo]::new('archive.zip')
        $fileResult = [FileResult]::new($file, [FileType]::Archive);
        $finder.IsMatchingArchiveFileResult($fileResult) | Should -BeTrue
    }
}

Describe -tag "Finder" -name "test_is_matching_file_matches_in_extension" {
    It "matches by pattern" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.InArchiveExtensions += @('.zip')
        $finder = [Finder]::new($settings)
        $file = [System.IO.FileInfo]::new('archive.zip')
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
        $file = [System.IO.FileInfo]::new('ps1find.ps1')
        $fileResult = $finder.FilterToFileResult($file)
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
        $file = [System.IO.FileInfo]::new('ps1find.ps1')
        $fileResult = $finder.FilterToFileResult($file)
        $fileResult | Should -Not -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_not_is_matching_file" {
    It "does not match by extension" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.InExtensions += @('.php')
        $finder = [Finder]::new($settings)
        $file = [System.IO.FileInfo]::new('ps1find.ps1')
        $fileResult = $finder.FilterToFileResult($file)
        $fileResult | Should -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_is_hidden_file" {
    It "does not match because hidden" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $finder = [Finder]::new($settings)
        $file = [System.IO.FileInfo]::new('.gitignore')
        $fileResult = $finder.FilterToFileResult($file)
        $fileResult | Should -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_hidden_include_hidden" {
    It "matches because includehidden" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.IncludeHidden = $true
        $finder = [Finder]::new($settings)
        $file = [System.IO.FileInfo]::new('.gitignore')
        $fileResult = $finder.FilterToFileResult($file)
        $fileResult | Should -Not -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_archive_no_include_archives" {
    It "does not match because archive" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $finder = [Finder]::new($settings)
        $file = [System.IO.FileInfo]::new('archive.zip')
        $fileResult = $finder.FilterToFileResult($file)
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
        $file = [System.IO.FileInfo]::new('archive.zip')
        $fileResult = $finder.FilterToFileResult($file)
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
        $file = [System.IO.FileInfo]::new('archive.zip')
        $fileResult = $finder.FilterToFileResult($file)
        $fileResult | Should -Not -BeNullOrEmpty
    }
}

Describe -tag "Finder" -name "test_filter_to_file_result_nonarchive_archives_only" {
    It "does not match because non-archive + set_archives_only" {
        $settings = [FindSettings]::new()
        $settings.Paths += @('.')
        $settings.SetArchivesOnly($true)
        $finder = [Finder]::new($settings)
        $file = [System.IO.FileInfo]::new('ps1find.ps1')
        $fileResult = $finder.FilterToFileResult($file)
        $fileResult | Should -BeNullOrEmpty
    }
}
#endregion
