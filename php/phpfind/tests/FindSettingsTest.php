<?php

declare(strict_types=1);

use PHPUnit\Framework\TestCase;

use phpfind\FindSettings;
use phpfind\SortBy;

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

    public static function settings_equals_defaults(FindSettings $settings): bool {
        return
            !$settings->archives_only &&
            !$settings->debug &&
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
            !$settings->print_files &&
            !$settings->print_usage &&
            !$settings->print_version &&
            $settings->recursive &&
            $settings->sort_by == SortBy::Filepath &&
            !$settings->sort_case_insensitive &&
            !$settings->sort_descending &&
            !$settings->verbose;
    }

    public function test_default_settings(): void
    {
        $this->assertTrue(self::settings_equals_defaults($this->settings));
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
