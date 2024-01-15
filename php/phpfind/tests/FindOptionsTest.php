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
    private FindOptions $findoptions;

    public function __construct()
    {
        parent::__construct();
        $this->findoptions = new FindOptions();
    }

    public function test_settings_from_args_no_args(): void
    {
        $settings = $this->findoptions->settings_from_args([]);
        $this->assertFalse($settings->archives_only);
        $this->assertFalse($settings->debug);
        $this->assertFalse($settings->include_archives);
        $this->assertFalse($settings->include_hidden);
        $this->assertFalse($settings->print_dirs);
        $this->assertTrue($settings->print_files);
        $this->assertFalse($settings->print_usage);
        $this->assertFalse($settings->print_version);
        $this->assertCount(0, $settings->paths);
        $this->assertTrue($settings->recursive);
        $this->assertFalse($settings->verbose);
    }

    public function test_settings_from_args_valid_args(): void
    {
        $args = ['-x', 'php,py', '.'];
        $settings = $this->findoptions->settings_from_args($args);
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('php', $settings->in_extensions));
        $this->assertTrue(in_array('py', $settings->in_extensions));
        $this->assertCount(1, $settings->paths);
        $this->assertEquals('.', $settings->paths[0]);
    }

    public function test_archives_only_arg(): void
    {
        $args = ['--archivesonly'];
        $settings = $this->findoptions->settings_from_args($args);
        $this->assertTrue($settings->archives_only);
        $this->assertTrue($settings->include_archives);
    }

    public function test_debug_arg(): void
    {
        $args = ['--debug'];
        $settings = $this->findoptions->settings_from_args($args);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
    }

    public function test_missing_arg(): void
    {
        $this->expectException(FindException::class);
        $args = ['-x', 'php,py', '.', '-D'];
        $this->findoptions->settings_from_args($args);
    }

    public function test_invalid_arg(): void
    {
        $this->expectException(FindException::class);
        $args = ['-x', 'php,py', '.', '-Q'];
        $this->findoptions->settings_from_args($args);
    }

    public function test_settings_from_json(): void
    {
        $settings = new FindSettings();
        $json = <<<"END_JSON"
{
  "path": "~/src/xfind/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "debug": true,
  "includehidden": true
}
END_JSON;
        $this->findoptions->settings_from_json($json, $settings);
        $this->assertCount(1, $settings->paths);
        $this->assertEquals('~/src/xfind/', $settings->paths[0]);
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('js', $settings->in_extensions));
        $this->assertTrue(in_array('ts', $settings->in_extensions));
        $this->assertCount(1, $settings->out_dir_patterns);
        $this->assertEquals('node_module', $settings->out_dir_patterns[0]);
        $this->assertCount(1, $settings->out_file_patterns);
        $this->assertEquals('temp', $settings->out_file_patterns[0]);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
        $this->assertTrue($settings->include_hidden);
    }
}
