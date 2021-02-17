<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpfind\Config;
use phpfind\FileUtil;
use phpfind\Finder;
use phpfind\FindSettings;

class FinderTest extends TestCase
{
    private function get_settings()
    {
        $settings = new FindSettings();
        $settings->startpath = '.';
        array_push($settings->findpatterns, "Finder");
        return $settings;
    }

    private function get_test_file()
    {
        return FileUtil::expand_user_home_path(Config::SHAREDPATH . '/testFiles/testFile2.txt');
    }

    ################################################################################
    # is_find_dir tests
    ################################################################################
    public function test_is_find_dir_no_patterns()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $dir = 'plfind';
        $this->assertTrue($finder->is_find_dir($dir));
    }

    public function test_is_find_dir_matches_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_dirpatterns, 'plfind');
        $finder = new Finder($settings);
        $dir = 'plfind';
        $this->assertTrue($finder->is_find_dir($dir));
    }

    public function test_is_find_dir_no_match_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_dirpatterns, 'plfind');
        $finder = new Finder($settings);
        $dir = 'pyfind';
        $this->assertFalse($finder->is_find_dir($dir));
    }

    public function test_is_find_dir_matches_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_dirpatterns, 'pyfind');
        $finder = new Finder($settings);
        $dir = 'pyfind';
        $this->assertFalse($finder->is_find_dir($dir));
    }

    public function test_is_find_dir_no_match_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_dirpatterns, 'pyfind');
        $finder = new Finder($settings);
        $dir = 'plfind';
        $this->assertTrue($finder->is_find_dir($dir));
    }

    public function test_is_find_dir_single_dot()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $dir = '.';
        $this->assertTrue($finder->is_find_dir($dir));
    }

    public function test_is_find_dir_double_dot()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $dir = '..';
        $this->assertTrue($finder->is_find_dir($dir));
    }

    public function test_is_find_dir_hidden_dir()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $dir = '.git';
        $this->assertFalse($finder->is_find_dir($dir));
    }

    public function test_is_find_dir_hidden_dir_include_hidden()
    {
        $settings = $this->get_settings();
        $settings->excludehidden = 0;
        $finder = new Finder($settings);
        $dir = '.git';
        $this->assertTrue($finder->is_find_dir($dir));
    }

    ################################################################################
    # is_find_file tests
    ################################################################################
    public function test_is_find_file_matches_by_default()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->is_find_file($file));
    }

    public function test_is_find_file_matches_in_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->in_extensions, 'pm');
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->is_find_file($file));
    }

    public function test_is_find_file_no_match_in_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->in_extensions, 'pl');
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($finder->is_find_file($file));
    }

    public function test_is_find_file_matches_out_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->out_extensions, 'pm');
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($finder->is_find_file($file));
    }

    public function test_is_find_file_no_match_out_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->out_extensions, 'py');
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->is_find_file($file));
    }

    public function test_is_find_file_matches_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_filepatterns, 'Find');
        $finder = new Finder($settings);
        $file = 'Finder.pm';
        $this->assertTrue($finder->is_find_file($file));
    }

    public function test_is_find_file_no_match_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_filepatterns, 'Find');
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($finder->is_find_file($file));
    }

    public function test_is_find_file_matches_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_filepatterns, 'Find');
        $finder = new Finder($settings);
        $file = 'Finder.pm';
        $this->assertFalse($finder->is_find_file($file));
    }

    public function test_is_find_file_no_match_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_filepatterns, 'Find');
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->is_find_file($file));
    }

    ################################################################################
    # is__archive_find_file tests
    ################################################################################
    public function test_is_archive_find_file_matches_by_default()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_archive_find_file($file));
    }

    public function test_is_archive_find_file_matches_in_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->in_archiveextensions, 'zip');
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_archive_find_file($file));
    }

    public function test_is_archive_find_file_no_match_in_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->in_archiveextensions, 'gz');
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_archive_find_file($file));
    }

    public function test_is_archive_find_file_matches_out_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->out_archiveextensions, 'zip');
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_archive_find_file($file));
    }

    public function test_is_archive_find_file_no_match_out_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->out_archiveextensions, 'gz');
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_archive_find_file($file));
    }

    public function test_is_archive_find_file_matches_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_archivefilepatterns, 'arch');
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_archive_find_file($file));
    }

    public function test_is_archive_find_file_no_match_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_archivefilepatterns, 'archives');
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_archive_find_file($file));
    }

    public function test_is_archive_find_file_matches_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_archivefilepatterns, 'arch');
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_archive_find_file($file));
    }

    public function test_is_archive_find_file_no_match_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_archivefilepatterns, 'archives');
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_archive_find_file($file));
    }

    ################################################################################
    # filter_file tests
    ################################################################################
    public function test_filter_file_matches_by_default()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->filter_file($file));
    }

    public function test_filter_file_is_find_file()
    {
        $settings = $this->get_settings();
        array_push($settings->in_extensions, 'pm');
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->filter_file($file));
    }

    public function test_filter_file_not_is_find_file()
    {
        $settings = $this->get_settings();
        array_push($settings->in_extensions, 'pl');
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($finder->filter_file($file));
    }

    public function test_filter_file_is_hidden_file()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = '.gitignore';
        $this->assertFalse($finder->filter_file($file));
    }

    public function test_filter_file_hidden_includehidden()
    {
        $settings = $this->get_settings();
        $settings->excludehidden = 0;
        $finder = new Finder($settings);
        $file = '.gitignore';
        $this->assertTrue($finder->filter_file($file));
    }

    public function test_filter_file_archive_no_findarchives()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = 'archive.zip';
        #print "finder->is_archive_find_file(archive.zip): " . $finder->is_archive_find_file('archive.zip') . "\n";
        $this->assertFalse($finder->filter_file($file));
    }

    public function test_filter_file_archive_findarchives()
    {
        $settings = $this->get_settings();
        $settings->findarchives = 1;
        $finder = new Finder($settings);
        $file = 'archive.zip';
        #print "finder->is_archive_find_file(archive.zip): " . $finder->is_archive_find_file('archive.zip') . "\n";
        $this->assertTrue($finder->filter_file($file));
    }

    public function test_filter_file_archive_archivesonly()
    {
        $settings = $this->get_settings();
        $settings->archivesonly = 1;
        $settings->findarchives = 1;
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->filter_file($file));
    }

    public function test_filter_file_nonarchive_archivesonly()
    {
        $settings = $this->get_settings();
        $settings->archivesonly = 1;
        $settings->findarchives = 1;
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($finder->filter_file($file));
    }

    ################################################################################
    # find_lines tests
    ################################################################################
    public function test_find_lines()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $testfile = $this->get_test_file();
        $contents = file_get_contents($testfile);
        $results = $finder->find_multiline_string($contents);
        $this->assertCount(2, $results);

        $firstResult = $results[0];
        $this->assertEquals(29, $firstResult->linenum);
        $this->assertEquals(3, $firstResult->match_start_index);
        $this->assertEquals(11, $firstResult->match_end_index);

        $secondResult = $results[1];
        $this->assertEquals(35, $secondResult->linenum);
        $this->assertEquals(24, $secondResult->match_start_index);
        $this->assertEquals(32, $secondResult->match_end_index);
    }

    ################################################################################
    # find_multiline_string tests
    ################################################################################
    public function test_find_multiline_string()
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $testfile = $this->get_test_file();
        $lines = file($testfile);
        $results = $finder->find_lines($lines);
        $this->assertCount(2, $results);

        $firstResult = $results[0];
        $this->assertEquals(29, $firstResult->linenum);
        $this->assertEquals(3, $firstResult->match_start_index);
        $this->assertEquals(11, $firstResult->match_end_index);

        $secondResult = $results[1];
        $this->assertEquals(35, $secondResult->linenum);
        $this->assertEquals(24, $secondResult->match_start_index);
        $this->assertEquals(32, $secondResult->match_end_index);
    }
}
