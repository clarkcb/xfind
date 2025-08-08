<?php

declare(strict_types=1);

namespace phpfind;

require_once __DIR__ . '/../autoload.php';
//include_once __DIR__ . '/common.php';

/**
 * Class FindOptions
 */
class FindOptions
{
    /**
     * @var FindOption[] $options
     */
    private array $options;
    /**
     * @var array<string, \Closure> $bool_action_map
     */
    private readonly array $bool_action_map;
    /**
     * @var array<string, \Closure> $str_action_map
     */
    private readonly array $str_action_map;
    /**
     * @var array<string, \Closure> $int_action_map
     */
    private readonly array $int_action_map;
    /**
     * @var array<string, string> $long_arg_map
     */
    private array $long_arg_map;

    /**
     * @throws FindException
     */
    public function __construct()
    {
        $this->options = [];

        $this->bool_action_map = [
            'archivesonly' => fn(bool $b, FindSettings $fs) => $fs->set_archives_only($b),
            'colorize' => fn(bool $b, FindSettings $fs) => $fs->colorize = $b,
            'debug' => fn(bool $b, FindSettings $fs) => $fs->set_debug($b),
            'followsymlinks' => fn(bool $b, FindSettings $fs) => $fs->follow_symlinks = $b,
            'excludearchives' => fn(bool $b, FindSettings $fs) => $fs->include_archives = !$b,
            'excludehidden' => fn(bool $b, FindSettings $fs) => $fs->include_hidden = !$b,
            'help' => fn(bool $b, FindSettings $fs) => $fs->print_usage = $b,
            'includearchives' => fn(bool $b, FindSettings $fs) => $fs->include_archives = $b,
            'includehidden' => fn(bool $b, FindSettings $fs) => $fs->include_hidden = $b,
            'nocolorize' => fn(bool $b, FindSettings $fs) => $fs->colorize = !$b,
            'nofollowsymlinks' => fn(bool $b, FindSettings $fs) => $fs->follow_symlinks = !$b,
            'noprintdirs' => fn(bool $b, FindSettings $fs) => $fs->print_dirs = !$b,
            'noprintfiles' => fn(bool $b, FindSettings $fs) => $fs->print_files = !$b,
            'norecursive' => fn(bool $b, FindSettings $fs) => $fs->recursive = !$b,
            'printdirs' => fn(bool $b, FindSettings $fs) => $fs->print_dirs = $b,
            'printfiles' => fn(bool $b, FindSettings $fs) => $fs->print_files = $b,
            'recursive' => fn(bool $b, FindSettings $fs) => $fs->recursive = $b,
            'sort-ascending' => fn(bool $b, FindSettings $fs) => $fs->sort_descending = !$b,
            'sort-caseinsensitive' => fn(bool $b, FindSettings $fs) => $fs->sort_case_insensitive = $b,
            'sort-casesensitive' => fn(bool $b, FindSettings $fs) => $fs->sort_case_insensitive = !$b,
            'sort-descending' => fn(bool $b, FindSettings $fs) => $fs->sort_descending = $b,
            'verbose' => fn(bool $b, FindSettings $fs) => $fs->verbose = $b,
            'version' => fn(bool $b, FindSettings $fs) => $fs->print_version = $b,
        ];

        $this->str_action_map = [
            'in-archiveext' => fn(string $s, FindSettings $fs) => $fs->add_exts($s, $fs->in_archive_extensions),
            'in-archivefilepattern' =>
                fn(string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->in_archive_file_patterns),
            'in-dirpattern' => fn(string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->in_dir_patterns),
            'in-ext' => fn(string $s, FindSettings $fs) => $fs->add_exts($s, $fs->in_extensions),
            'in-filepattern' =>
                fn(string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->in_file_patterns),
            'in-filetype' => fn(string $s, FindSettings $fs) => $fs->add_file_types($s, $fs->in_file_types),
            'maxlastmod' => fn(string $s, FindSettings $fs) => $fs->max_last_mod = new \DateTime($s),
            'minlastmod' => fn(string $s, FindSettings $fs) => $fs->min_last_mod = new \DateTime($s),
            'out-archiveext' => fn(string $s, FindSettings $fs) => $fs->add_exts($s, $fs->out_archive_extensions),
            'out-archivefilepattern' =>
                fn(string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->out_archive_file_patterns),
            'out-dirpattern' =>
                fn(string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->out_dir_patterns),
            'out-ext' => fn(string $s, FindSettings $fs) => $fs->add_exts($s, $fs->out_extensions),
            'out-filepattern' =>
                fn(string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->out_file_patterns),
            'out-filetype' => fn(string $s, FindSettings $fs) => $fs->add_file_types($s, $fs->out_file_types),
            'path' => fn(string $s, FindSettings $fs) => $fs->paths[] = $s,
            'settings-file' => fn(string $s, FindSettings $fs) => $this->update_settings_from_file($fs, $s),
            'sort-by' => fn(string $s, FindSettings $fs) => $fs->set_sort_by($s),
        ];

        $this->int_action_map = [
            'maxdepth' => fn(int $i, FindSettings $fs) => $fs->max_depth = $i,
            'maxsize' => fn(int $i, FindSettings $fs) => $fs->max_size = $i,
            'mindepth' => fn(int $i, FindSettings $fs) => $fs->min_depth = $i,
            'minsize' => fn(int $i, FindSettings $fs) => $fs->min_size = $i,
        ];
        $this->long_arg_map = ['path' => 'path'];
        $this->set_options_from_json();
    }

    /**
     * @throws FindException
     */
    private function set_options_from_json(): void
    {
        $find_options_path = FileUtil::expand_path(Config::FIND_OPTIONS_PATH);
        if (file_exists($find_options_path)) {
            $contents = file_get_contents($find_options_path);
            if ($contents === false || trim($contents) === '') {
                return;
            }
            try {
                $json_obj = (array)json_decode(trim($contents), true, 512, JSON_THROW_ON_ERROR);
                $find_options = $json_obj['findoptions'];
                if ($find_options) {
                    foreach ((array)$find_options as $fo) {
                        $fo = (array)$fo;
                        $short = '';
                        $long = (string)$fo['long'];
                        $desc = (string)$fo['desc'];
                        $this->long_arg_map[$long] = $long;
                        if (array_key_exists('short', $fo)) {
                            $short = (string)$fo['short'];
                            $this->long_arg_map[$short] = $long;
                        }
                        $this->options[] = new FindOption($short, $long, $desc);
                    }
                    usort($this->options, array('phpfind\FindOptions', 'cmp_find_options'));
                }
            } catch (\JsonException $e) {
                throw new FindException($e->getMessage());
            }
        } else {
            throw new FindException('File not found: ' . $find_options_path);
        }
    }

    /**
     * @param string[] $args
     * @return array
     * @throws FindException
     */
    public function arg_map_from_args(array $args): array
    {
        $arg_map = [];
        while (count($args) > 0) {
            $arg = array_shift($args);
            if ($arg[0] == '-') {
                $arg_names = array();
                if (strlen($arg) > 1) {
                    if ($arg[1] == '-') {
                        # it's a long arg
                        if (strlen($arg) > 2) {
                            $arg = substr($arg, 2);
                            if (str_contains($arg, '=')) {
                                $elems = explode('=', $arg);
                                $arg = $elems[0];
                                $arg_value = $elems[1];
                                array_unshift($args, $arg_value);
                            }
                            if (!array_key_exists($arg, $this->long_arg_map)) {
                                throw new FindException("Invalid option: $arg");
                            }
                            $arg_names[] = $arg;
                        } else {
                            throw new FindException("Invalid option: $arg");
                        }
                    } else {
                        # it's a short arg, with possibly multiple args
                        $arg = substr($arg, 1);
                        foreach (str_split($arg) as $c) {
                            if (!array_key_exists($c, $this->long_arg_map)) {
                                throw new FindException("Invalid option: $c");
                            }
                            $arg_names[] = $this->long_arg_map[$c];
                        }
                    }
                } else {
                    throw new FindException("Invalid option: $arg");
                }

                foreach ($arg_names as $arg_name) {
                    if (array_key_exists($arg_name, $this->bool_action_map)) {
                        $arg_map[$arg_name] = true;
                        if (in_array($arg_name, array("help", "version"))) {
                            return $arg_map;
                        }
                    } elseif (array_key_exists($arg_name, $this->str_action_map)
                        || array_key_exists($arg_name, $this->int_action_map)) {
                        if (count($args) > 0) {
                            $val = array_shift($args);
                            if (array_key_exists($arg_name, $this->str_action_map)) {
                                if (!array_key_exists($arg_name, $arg_map)) {
                                    $arg_map[$arg_name] = [];
                                }
                                $arg_map[$arg_name][] = $val;
                            } elseif (array_key_exists($arg_name, $this->int_action_map)) {
                                $arg_map[$arg_name] = intval($val);
                            }
                        } else {
                            throw new FindException("Missing value for $arg");
                        }
                    } else {
                        throw new FindException("Invalid option: $arg");
                    }
                }
            } else {
                if (!array_key_exists('path', $arg_map)) {
                    $arg_map['path'] = [];
                }
                $arg_map['path'][] = $arg;
            }
        }
        return $arg_map;
    }

    /**
     * @param FindSettings $settings
     * @param array $arg_map
     * @return void
     * @throws FindException
     */
    public function update_settings_from_arg_map(FindSettings $settings, array $arg_map): void
    {
        $arg_names = array_keys($arg_map);
        # keys are sorted so that output is consistent across all versions
        sort($arg_names);
        $is_invalid_key = fn (string $k) => !array_key_exists($k, $this->long_arg_map);
        $invalid_keys = array_filter($arg_names, $is_invalid_key);
        if ($invalid_keys) {
            throw new FindException("Invalid option: " . array_values($invalid_keys)[0]);
        }
        foreach ($arg_names as $arg_name) {
            $arg_value = $arg_map[$arg_name];
            $long_arg = $this->long_arg_map[$arg_name];
            if (array_key_exists($long_arg, $this->bool_action_map)) {
                $this->bool_action_map[$long_arg]($arg_value, $settings);
                if (in_array($long_arg, array("help", "version"))) {
                    return;
                }
            } elseif (array_key_exists($long_arg, $this->str_action_map)
                || array_key_exists($long_arg, $this->int_action_map)) {
                if (array_key_exists($long_arg, $this->str_action_map)) {
                    if (is_array($arg_value)) {
                        foreach ($arg_value as $val) {
                            $this->str_action_map[$long_arg]($val, $settings);
                        }
                    } elseif (is_string($arg_value)) {
                        $this->str_action_map[$long_arg]($arg_value, $settings);
                    } else {
                        throw new FindException("Invalid value for option: $long_arg");
                    }
                } elseif (array_key_exists($long_arg, $this->int_action_map)) {
                    $this->int_action_map[$long_arg](intval($arg_value), $settings);
                }
            } else {
                throw new FindException("Invalid option: $arg_name");
            }
        }
    }

    /**
     * @param string $json
     * @param FindSettings $settings
     * @return void
     * @throws FindException
     */
    public function update_settings_from_json(string $json, FindSettings $settings): void
    {
        if (trim($json) === '') {
            return;
        }
        try {
            $json_obj = (array)json_decode(trim($json), true, 512, JSON_THROW_ON_ERROR);
            $this->update_settings_from_arg_map($settings, $json_obj);
        } catch (\JsonException $e) {
            throw new FindException($e->getMessage());
        }
    }

    /**
     * @param FindSettings $settings
     * @param string $file_path
     * @return void
     * @throws FindException
     */
    private function update_settings_from_file(FindSettings $settings, string $file_path): void
    {
        $expanded_path = FileUtil::expand_path($file_path);
        if (!file_exists($expanded_path)) {
            throw new FindException('Settings file not found: ' . $file_path);
        }
        if (!str_ends_with($expanded_path, '.json')) {
            throw new FindException('Invalid settings file (must be JSON): ' . $file_path);
        }
        $json = file_get_contents($expanded_path);
        if ($json) {
            $this->update_settings_from_json($json, $settings);
        }
    }

    /**
     * @param FindSettings $settings
     * @param string[] $args
     * @return void
     * @throws FindException
     */
    public function update_settings_from_args(FindSettings $settings, array $args): void
    {
        $arg_map = $this->arg_map_from_args($args);
        $this->update_settings_from_arg_map($settings, $arg_map);
    }

    /**
     * @param string[] $args
     * @return FindSettings
     * @throws FindException
     */
    public function settings_from_args(array $args): FindSettings
    {
        $settings = new FindSettings();
        // default print_files to true since running as cli
        $settings->print_files = true;
        $this->update_settings_from_args($settings, $args);
        return $settings;
    }

    /**
     * @param FindOption $o1
     * @param FindOption $o2
     * @return int
     */
    private static function cmp_find_options(FindOption $o1, FindOption $o2): int
    {
        return strcmp($o1->sort_arg, $o2->sort_arg);
    }

    /**
     * @return string
     */
    private function get_usage_string(): string
    {
        $usage = "Usage:\n phpfind [options] <path> [<path> ...]";
        $usage .= "\n\nOptions:\n";
        $opt_map = [];
        $longest = 0;
        foreach ($this->options as $option) {
            $opt_str = '';
            if ($option->short_arg) {
                $opt_str = '-' . $option->short_arg . ',';
            }
            $opt_str .= '--' . $option->long_arg;
            if (strlen($opt_str) > $longest) {
                $longest = strlen($opt_str);
            }
            $opt_map[$opt_str] = $option->desc;
        }
        $format_str = " %-" . $longest . "s  %s\n";
        foreach ($opt_map as $opt => $desc) {
            $usage .= sprintf($format_str, $opt, $desc);
        }
        return $usage;
    }

    /**
     * @return void
     */
    public function usage(): void
    {
        echo $this->get_usage_string() . "\n";
    }

    /**
     * @param int $exit_code
     * @return never
     */
    public function usage_and_exit(int $exit_code = 0): never
    {
        $this->usage();
        exit($exit_code);
    }
}
