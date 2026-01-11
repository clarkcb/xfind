<?php

declare(strict_types=1);

use PHPUnit\Framework\TestCase;

use phpfind\FindException;
use phpfind\FindOptions;
use phpfind\FindSettings;
use phpfind\SortBy;

class FindOptionsTest extends TestCase
{
    /**
     * @var FindOptions
     */
    private FindOptions $find_options;

    function setUp(): void
    {
        $this->find_options = new FindOptions();
    }

    public static function settings_equals_defaults(FindSettings $settings, bool $print_files = false): bool {
        return
            !$settings->archives_only &&
            !$settings->debug &&
            !$settings->follow_symlinks &&
            count($settings->in_archive_extensions) == 0 &&
            count($settings->in_archive_file_patterns) == 0 &&
            count($settings->in_dir_patterns) == 0 &&
            count($settings->in_extensions) == 0 &&
            count($settings->in_file_patterns) == 0 &&
            count($settings->in_file_types) == 0 &&
            !$settings->include_archives &&
            !$settings->include_hidden &&
            $settings->max_depth == -1 &&
            $settings->max_last_mod == null &&
            $settings->max_size == 0 &&
            $settings->min_depth == -1 &&
            $settings->min_last_mod == null &&
            $settings->min_size == 0 &&
            count($settings->out_archive_extensions) == 0 &&
            count($settings->out_archive_file_patterns) == 0 &&
            count($settings->out_dir_patterns) == 0 &&
            count($settings->out_extensions) == 0 &&
            count($settings->out_file_patterns) == 0 &&
            count($settings->out_file_types) == 0 &&
            count($settings->paths) == 0 &&
            !$settings->print_dirs &&
            $settings->print_files == $print_files &&
            !$settings->print_usage &&
            !$settings->print_version &&
            $settings->recursive &&
            $settings->sort_by == SortBy::Filepath &&
            !$settings->sort_case_insensitive &&
            !$settings->sort_descending &&
            !$settings->verbose;
    }

    public function test_settings_from_args_no_args(): void
    {
        $settings = $this->find_options->settings_from_args([]);
        $this->assertTrue(self::settings_equals_defaults($settings, true));
    }

    public function test_settings_from_args_valid_args(): void
    {
        $args = ['-x', 'php,py', '.'];
        $settings = $this->find_options->settings_from_args($args);
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('php', $settings->in_extensions));
        $this->assertTrue(in_array('py', $settings->in_extensions));
        $this->assertCount(1, $settings->paths);
        $this->assertEquals('.', $settings->paths[0]);
    }

    public function test_archives_only_arg(): void
    {
        $args = ['--archivesonly'];
        $settings = $this->find_options->settings_from_args($args);
        $this->assertTrue($settings->archives_only);
        $this->assertTrue($settings->include_archives);
    }

    public function test_debug_arg(): void
    {
        $args = ['--debug'];
        $settings = $this->find_options->settings_from_args($args);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
    }

    public function test_missing_arg(): void
    {
        $this->expectException(FindException::class);
        $args = ['-x', 'php,py', '.', '-D'];
        $this->find_options->settings_from_args($args);
    }

    public function test_invalid_arg(): void
    {
        $this->expectException(FindException::class);
        $args = ['-x', 'php,py', '.', '-Q'];
        $this->find_options->settings_from_args($args);
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
  "followsymlinks": true,
  "includehidden": true
}
END_JSON;
        $this->find_options->update_settings_from_json($settings, $json);
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
        $this->assertTrue($settings->follow_symlinks);
        $this->assertTrue($settings->verbose);
        $this->assertTrue($settings->include_hidden);
    }

    public function test_settings_from_empty_json(): void
    {
        $settings = new FindSettings();
        $json = '';
        $this->find_options->update_settings_from_json($settings, $json);
        $this->assertTrue(self::settings_equals_defaults($settings));
    }

    public function test_settings_from_invalid_json(): void
    {
        $settings = new FindSettings();
        $json = '<this>is invalid</this> JSON';
        $this->expectException(FindException::class);
        $this->find_options->update_settings_from_json($settings, $json);
    }
}
