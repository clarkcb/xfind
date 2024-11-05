<?php

declare(strict_types=1);

use PHPUnit\Framework\TestCase;

use phpfind\Config;
use phpfind\FileUtil;
use phpfind\Finder;
use phpfind\FindSettings;

class FinderTest extends TestCase
{
    private function get_settings(): FindSettings
    {
        $settings = new FindSettings();
        $settings->paths = ['.'];
        return $settings;
    }

//    private function get_test_file()
//    {
//        return FileUtil::expand_user_home_path(Config::SHAREDPATH . '/testFiles/testFile2.txt');
//    }

    ################################################################################
    # is_matching_dir tests
    ################################################################################
    public function test_is_matching_dir_no_patterns(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $dir = 'plfind';
        $this->assertTrue($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_matches_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_dir_patterns[] = 'plfind';
        $finder = new Finder($settings);
        $dir = 'plfind';
        $this->assertTrue($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_no_match_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_dir_patterns[] = 'plfind';
        $finder = new Finder($settings);
        $dir = 'pyfind';
        $this->assertFalse($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_matches_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_dir_patterns[] = 'pyfind';
        $finder = new Finder($settings);
        $dir = 'pyfind';
        $this->assertFalse($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_no_match_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_dir_patterns[] = 'pyfind';
        $finder = new Finder($settings);
        $dir = 'plfind';
        $this->assertTrue($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_single_dot(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $dir = '.';
        $this->assertTrue($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_double_dot(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $dir = '..';
        $this->assertTrue($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_hidden_dir(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $dir = '.git';
        $this->assertFalse($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_hidden_dir_include_hidden(): void
    {
        $settings = $this->get_settings();
        $settings->include_hidden = true;
        $finder = new Finder($settings);
        $dir = '.git';
        $this->assertTrue($finder->is_matching_dir($dir));
    }

    ################################################################################
    # is_matching_file tests
    ################################################################################
    public function test_is_matching_file_matches_by_default(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->is_matching_file($file));
    }

    public function test_is_matching_file_matches_in_extension(): void
    {
        $settings = $this->get_settings();
        $settings->in_extensions[] = 'pm';
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->is_matching_file($file));
    }

    public function test_is_matching_file_no_match_in_extension(): void
    {
        $settings = $this->get_settings();
        $settings->in_extensions[] = 'pl';
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($finder->is_matching_file($file));
    }

    public function test_is_matching_file_matches_out_extension(): void
    {
        $settings = $this->get_settings();
        $settings->out_extensions[] = 'pm';
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($finder->is_matching_file($file));
    }

    public function test_is_matching_file_no_match_out_extension(): void
    {
        $settings = $this->get_settings();
        $settings->out_extensions[] = 'py';
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->is_matching_file($file));
    }

    public function test_is_matching_file_matches_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_file_patterns[] = 'Find';
        $finder = new Finder($settings);
        $file = 'Finder.pm';
        $this->assertTrue($finder->is_matching_file($file));
    }

    public function test_is_matching_file_no_match_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_file_patterns[] = 'Find';
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($finder->is_matching_file($file));
    }

    public function test_is_matching_file_matches_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_file_patterns[] = 'Find';
        $finder = new Finder($settings);
        $file = 'Finder.pm';
        $this->assertFalse($finder->is_matching_file($file));
    }

    public function test_is_matching_file_no_match_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_file_patterns[] = 'Find';
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->is_matching_file($file));
    }

    ################################################################################
    # is_matching_archive_file tests
    ################################################################################
    public function test_is_matching_archive_file_matches_by_default(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_matches_in_extension(): void
    {
        $settings = $this->get_settings();
        $settings->in_archive_extensions[] = 'zip';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_no_match_in_extension(): void
    {
        $settings = $this->get_settings();
        $settings->in_archive_extensions[] = 'gz';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_matches_out_extension(): void
    {
        $settings = $this->get_settings();
        $settings->out_archive_extensions[] = 'zip';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_no_match_out_extension(): void
    {
        $settings = $this->get_settings();
        $settings->out_archive_extensions[] = 'gz';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_matches_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_archive_file_patterns[] = 'arch';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_no_match_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_archive_file_patterns[] = 'archives';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_matches_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_archive_file_patterns[] = 'arch';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_no_match_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_archive_file_patterns[] = 'archives';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_matching_archive_file($file));
    }

    ################################################################################
    # filter_to_file_result tests
    ################################################################################
    public function test_filter_to_file_result_matches_by_default(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->filter_to_file_result('.', $file) != null);
    }

    public function test_filter_to_file_result_is_matching_file(): void
    {
        $settings = $this->get_settings();
        $settings->in_extensions[] = 'pm';
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->filter_to_file_result('.', $file) != null);
    }

    public function test_filter_to_file_result_not_is_matching_file(): void
    {
        $settings = $this->get_settings();
        $settings->in_extensions[] = 'pl';
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->filter_to_file_result('.', $file) == null);
    }

    public function test_filter_to_file_result_is_hidden_file(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = '.gitignore';
        $this->assertTrue($finder->filter_to_file_result('.', $file) == null);
    }

    public function test_filter_to_file_result_hidden_include_hidden(): void
    {
        $settings = $this->get_settings();
        $settings->include_hidden = true;
        $finder = new Finder($settings);
        $file = '.gitignore';
        $this->assertTrue($finder->filter_to_file_result('.', $file) != null);
    }

    public function test_filter_to_file_result_archive_no_include_archives(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->filter_to_file_result('.', $file) == null);
    }

    public function test_filter_to_file_result_archive_include_archives(): void
    {
        $settings = $this->get_settings();
        $settings->include_archives = true;
        $finder = new Finder($settings);
        $file = 'archive.zip';
        #print "finder->is_archive_find_file(archive.zip): " . $finder->is_archive_find_file('archive.zip') . "\n";
        $this->assertTrue($finder->filter_to_file_result('.', $file) != null);
    }

    public function test_filter_to_file_result_archive_archives_only(): void
    {
        $settings = $this->get_settings();
        $settings->archives_only = true;
        $settings->include_archives = true;
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->filter_to_file_result('.', $file) != null);
    }

    public function test_filter_to_file_result_non_archive_archives_only(): void
    {
        $settings = $this->get_settings();
        $settings->archives_only = true;
        $settings->include_archives = true;
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->filter_to_file_result('.', $file) == null);
    }

    ################################################################################
    # test filtering symlink files
    ################################################################################
    public function test_default_no_symlinks(): void
    {
        $settings = new FindSettings();
        $bin_path = FileUtil::join_paths(Config::XFIND_PATH, 'bin');
        $settings->paths = [$bin_path];
        $finder = new Finder($settings);
        $file_results = $finder->find();
        $this->assertTrue(count($file_results) < 3);
    }

    public function test_follow_symlinks(): void
    {
        $settings = new FindSettings();
        $bin_path = FileUtil::join_paths(Config::XFIND_PATH, 'bin');
        $settings->paths = [$bin_path];
        $settings->follow_symlinks = true;
        $finder = new Finder($settings);
        $file_results = $finder->find();
        $this->assertTrue(count($file_results) == 0 || count($file_results) > 2);
    }

    public function test_no_follow_symlinks(): void
    {
        $settings = new FindSettings();
        $bin_path = FileUtil::join_paths(Config::XFIND_PATH, 'bin');
        $settings->paths = [$bin_path];
        $settings->follow_symlinks = false;
        $finder = new Finder($settings);
        $file_results = $finder->find();
        $this->assertTrue(count($file_results) < 3);
    }
}
