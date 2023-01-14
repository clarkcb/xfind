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

    public function test_default_settings()
    {
        $this->assertFalse($this->settings->archivesonly);
        $this->assertFalse($this->settings->debug);
        $this->assertTrue($this->settings->excludehidden);
        $this->assertFalse($this->settings->includearchives);
        $this->assertFalse($this->settings->listdirs);
        $this->assertFalse($this->settings->listfiles);
        $this->assertFalse($this->settings->printusage);
        $this->assertFalse($this->settings->printversion);
        $this->assertTrue($this->settings->recursive);
        $this->assertCount(0, $this->settings->paths);
        $this->assertEquals(SortBy::Filepath, $this->settings->sortby);
        $this->assertFalse($this->settings->sort_caseinsensitive);
        $this->assertFalse($this->settings->sort_descending);
        $this->assertFalse($this->settings->verbose);
    }

    public function test_add_single_extension()
    {
        $this->settings->add_exts('php', $this->settings->in_extensions);
        $this->assertCount(1, $this->settings->in_extensions);
        $this->assertEquals('php', $this->settings->in_extensions[0]);
    }

    public function test_add_comma_delimited_extensions()
    {
        $this->settings->add_exts('php,py', $this->settings->in_extensions);
        $this->assertCount(2, $this->settings->in_extensions);
        $this->assertTrue(in_array('php', $this->settings->in_extensions));
        $this->assertTrue(in_array('py', $this->settings->in_extensions));
    }

    public function test_add_extensions_array()
    {
        $this->settings->add_exts(['php','py'], $this->settings->in_extensions);
        $this->assertCount(2, $this->settings->in_extensions);
        $this->assertTrue(in_array('php', $this->settings->in_extensions));
        $this->assertTrue(in_array('py', $this->settings->in_extensions));
    }

    public function test_add_patterns_string()
    {
        $this->settings->add_patterns('Finder', $this->settings->in_filepatterns);
        $this->assertCount(1, $this->settings->in_filepatterns);
        $this->assertTrue(in_array('Finder', $this->settings->in_filepatterns));
    }

    public function test_add_patterns_array()
    {
        $this->settings->add_patterns(['Finder'], $this->settings->in_filepatterns);
        $this->assertCount(1, $this->settings->in_filepatterns);
        $this->assertTrue(in_array('Finder', $this->settings->in_filepatterns));
    }
}
