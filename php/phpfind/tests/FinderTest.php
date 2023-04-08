<?php declare(strict_types=1);

use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

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
        $settings->in_dirpatterns[] = 'plfind';
        $finder = new Finder($settings);
        $dir = 'plfind';
        $this->assertTrue($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_no_match_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_dirpatterns[] = 'plfind';
        $finder = new Finder($settings);
        $dir = 'pyfind';
        $this->assertFalse($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_matches_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_dirpatterns[] = 'pyfind';
        $finder = new Finder($settings);
        $dir = 'pyfind';
        $this->assertFalse($finder->is_matching_dir($dir));
    }

    public function test_is_matching_dir_no_match_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_dirpatterns[] = 'pyfind';
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
        $settings->excludehidden = false;
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
        $settings->in_filepatterns[] = 'Find';
        $finder = new Finder($settings);
        $file = 'Finder.pm';
        $this->assertTrue($finder->is_matching_file($file));
    }

    public function test_is_matching_file_no_match_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_filepatterns[] = 'Find';
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($finder->is_matching_file($file));
    }

    public function test_is_matching_file_matches_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_filepatterns[] = 'Find';
        $finder = new Finder($settings);
        $file = 'Finder.pm';
        $this->assertFalse($finder->is_matching_file($file));
    }

    public function test_is_matching_file_no_match_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_filepatterns[] = 'Find';
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
        $settings->in_archiveextensions[] = 'zip';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_no_match_in_extension(): void
    {
        $settings = $this->get_settings();
        $settings->in_archiveextensions[] = 'gz';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_matches_out_extension(): void
    {
        $settings = $this->get_settings();
        $settings->out_archiveextensions[] = 'zip';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_no_match_out_extension(): void
    {
        $settings = $this->get_settings();
        $settings->out_archiveextensions[] = 'gz';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_matches_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_archivefilepatterns[] = 'arch';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_no_match_in_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->in_archivefilepatterns[] = 'archives';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_matches_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_archivefilepatterns[] = 'arch';
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertFalse($finder->is_matching_archive_file($file));
    }

    public function test_is_matching_archive_file_no_match_out_pattern(): void
    {
        $settings = $this->get_settings();
        $settings->out_archivefilepatterns[] = 'archives';
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

    public function test_filter_to_file_result_hidden_includehidden(): void
    {
        $settings = $this->get_settings();
        $settings->excludehidden = false;
        $finder = new Finder($settings);
        $file = '.gitignore';
        $this->assertTrue($finder->filter_to_file_result('.', $file) != null);
    }

    public function test_filter_to_file_result_archive_no_includearchives(): void
    {
        $settings = $this->get_settings();
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->filter_to_file_result('.', $file) == null);
    }

    public function test_filter_to_file_result_archive_includearchives(): void
    {
        $settings = $this->get_settings();
        $settings->includearchives = true;
        $finder = new Finder($settings);
        $file = 'archive.zip';
        #print "finder->is_archive_find_file(archive.zip): " . $finder->is_archive_find_file('archive.zip') . "\n";
        $this->assertTrue($finder->filter_to_file_result('.', $file) != null);
    }

    public function test_filter_to_file_result_archive_archivesonly(): void
    {
        $settings = $this->get_settings();
        $settings->archivesonly = true;
        $settings->includearchives = true;
        $finder = new Finder($settings);
        $file = 'archive.zip';
        $this->assertTrue($finder->filter_to_file_result('.', $file) != null);
    }

    public function test_filter_to_file_result_nonarchive_archivesonly(): void
    {
        $settings = $this->get_settings();
        $settings->archivesonly = true;
        $settings->includearchives = true;
        $finder = new Finder($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($finder->filter_to_file_result('.', $file) == null);
    }
}
