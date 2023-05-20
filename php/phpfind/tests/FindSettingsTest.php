<?php declare(strict_types=1);

use phpfind\SortBy;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpfind\FindSettings;

class FindSettingsTest extends TestCase
{
    /**
     * @var FindSettings
     */
    private FindSettings $settings;

    public function __construct()
    {
        parent::__construct();
        $this->settings = new FindSettings();
    }

    public function test_default_settings(): void
    {
        $this->assertFalse($this->settings->archives_only);
        $this->assertFalse($this->settings->debug);
        $this->assertTrue($this->settings->exclude_hidden);
        $this->assertFalse($this->settings->include_archives);
        $this->assertFalse($this->settings->list_dirs);
        $this->assertFalse($this->settings->list_files);
        $this->assertFalse($this->settings->print_usage);
        $this->assertFalse($this->settings->print_version);
        $this->assertTrue($this->settings->recursive);
        $this->assertCount(0, $this->settings->paths);
        $this->assertEquals(SortBy::Filepath, $this->settings->sort_by);
        $this->assertFalse($this->settings->sort_case_insensitive);
        $this->assertFalse($this->settings->sort_descending);
        $this->assertFalse($this->settings->verbose);
    }

    public function test_add_single_extension(): void
    {
        $this->settings->add_exts('php', $this->settings->in_extensions);
        $this->assertCount(1, $this->settings->in_extensions);
        $this->assertEquals('php', $this->settings->in_extensions[0]);
    }

    public function test_add_comma_delimited_extensions(): void
    {
        $this->settings->add_exts('php,py', $this->settings->in_extensions);
        $this->assertCount(2, $this->settings->in_extensions);
        $this->assertTrue(in_array('php', $this->settings->in_extensions));
        $this->assertTrue(in_array('py', $this->settings->in_extensions));
    }

    public function test_add_extensions_array(): void
    {
        $this->settings->add_exts(['php','py'], $this->settings->in_extensions);
        $this->assertCount(2, $this->settings->in_extensions);
        $this->assertTrue(in_array('php', $this->settings->in_extensions));
        $this->assertTrue(in_array('py', $this->settings->in_extensions));
    }

    public function test_add_patterns_string(): void
    {
        $this->settings->add_patterns('Finder', $this->settings->in_file_patterns);
        $this->assertCount(1, $this->settings->in_file_patterns);
        $this->assertTrue(in_array('Finder', $this->settings->in_file_patterns));
    }

    public function test_add_patterns_array(): void
    {
        $this->settings->add_patterns(['Finder'], $this->settings->in_file_patterns);
        $this->assertCount(1, $this->settings->in_file_patterns);
        $this->assertTrue(in_array('Finder', $this->settings->in_file_patterns));
    }
}
