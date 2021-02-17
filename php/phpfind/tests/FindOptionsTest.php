<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpfind\FindException;
use phpfind\FindOptions;
use phpfind\FindSettings;

class FindOptionsTest extends TestCase
{
    /**
     * @var FindOptions
     */
    private $findoptions;

    public function __construct()
    {
        parent::__construct();
        $this->findoptions = new FindOptions();
    }

    public function test_no_args()
    {
        $settings = $this->findoptions->settings_from_args([]);
        $this->assertFalse($settings->archivesonly);
        $this->assertTrue($settings->colorize);
        $this->assertFalse($settings->debug);
        $this->assertTrue($settings->excludehidden);
        $this->assertFalse($settings->firstmatch);
        $this->assertEquals(0, $settings->linesafter);
        $this->assertEquals(0, $settings->linesbefore);
        $this->assertFalse($settings->listdirs);
        $this->assertFalse($settings->listfiles);
        $this->assertFalse($settings->listlines);
        $this->assertEquals(150, $settings->maxlinelength);
        $this->assertFalse($settings->multilineoption-REMOVE);
        $this->assertTrue($settings->printresults);
        $this->assertFalse($settings->printusage);
        $this->assertFalse($settings->printversion);
        $this->assertTrue($settings->recursive);
        $this->assertFalse($settings->findarchives);
        $this->assertFalse(isset($settings->startpath));
        $this->assertFalse($settings->uniquelines);
        $this->assertFalse($settings->verbose);
    }

    public function test_valid_args()
    {
        $args = ['-x', 'php,py', '-s', 'Find', '.'];
        $settings = $this->findoptions->settings_from_args($args);
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('php', $settings->in_extensions));
        $this->assertTrue(in_array('py', $settings->in_extensions));
        $this->assertCount(1, $settings->findpatterns);
        $this->assertEquals('Find', $settings->findpatterns[0]);
        $this->assertEquals('.', $settings->startpath);
    }

    public function test_archivesonly_arg()
    {
        $args = ['--archivesonly'];
        $settings = $this->findoptions->settings_from_args($args);
        $this->assertTrue($settings->archivesonly);
        $this->assertTrue($settings->findarchives);
    }

    public function test_debug_arg()
    {
        $args = ['--debug'];
        $settings = $this->findoptions->settings_from_args($args);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
    }

    public function test_missing_arg()
    {
        $this->expectException(FindException::class);
        $args = ['-x', 'php,py', '-s', 'Find', '.', '-D'];
        $this->findoptions->settings_from_args($args);
    }

    public function test_invalid_arg()
    {
        $this->expectException(FindException::class);
        $args = ['-x', 'php,py', '-s', 'Find', '.', '-Q'];
        $this->findoptions->settings_from_args($args);
    }

    public function test_settings_from_json()
    {
        $settings = new FindSettings();
        $json = <<<"END_JSON"
{
  "startpath": "~/src/xfind/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "findpattern": "Finder",
  "linesbefore": 2,
  "linesafter": 2,
  "debug": true,
  "allmatches": false,
  "includehidden": true
}
END_JSON;
        $this->findoptions->settings_from_json($json, $settings);
        $this->assertEquals('~/src/xfind/', $settings->startpath);
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('js', $settings->in_extensions));
        $this->assertTrue(in_array('ts', $settings->in_extensions));
        $this->assertCount(1, $settings->out_dirpatterns);
        $this->assertEquals('node_module', $settings->out_dirpatterns[0]);
        $this->assertCount(1, $settings->out_filepatterns);
        $this->assertEquals('temp', $settings->out_filepatterns[0]);
        $this->assertCount(1, $settings->findpatterns);
        $this->assertEquals('Finder', $settings->findpatterns[0]);
        $this->assertEquals(2, $settings->linesbefore);
        $this->assertEquals(2, $settings->linesafter);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
        $this->assertTrue($settings->firstmatch);
        $this->assertTrue(!$settings->excludehidden);
    }
}
