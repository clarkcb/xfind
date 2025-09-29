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
     * @var array<string, string> $bool_map
     */
    private array $bool_map;
    /**
     * @var array<string, string> $str_map
     */
    private array $str_map;
    /**
     * @var array<string, string> $int_map
     */
    private array $int_map;
    /**
     * @var ArgTokenizer $arg_tokenizer
     */
    private ArgTokenizer $arg_tokenizer;

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
        $this->str_map = ['path' => 'path'];
        $this->set_options_from_json();
        $this->arg_tokenizer = new ArgTokenizer($this->bool_map, $this->str_map, $this->int_map);
    }

    private function add_option(FindOption $opt): void
    {
        $this->options[] = $opt;
        if (array_key_exists($opt->long_arg, $this->bool_action_map)) {
            $this->bool_map[$opt->long_arg] = $opt->long_arg;
            if ($opt->short_arg != '') {
                $this->bool_map[$opt->short_arg] = $opt->long_arg;
            }
        } elseif (array_key_exists($opt->long_arg, $this->str_action_map)) {
            $this->str_map[$opt->long_arg] = $opt->long_arg;
            if ($opt->short_arg != '') {
                $this->str_map[$opt->short_arg] = $opt->long_arg;
            }
        } elseif (array_key_exists($opt->long_arg, $this->int_action_map)) {
            $this->int_map[$opt->long_arg] = $opt->long_arg;
            if ($opt->short_arg != '') {
                $this->int_map[$opt->short_arg] = $opt->long_arg;
            }
        }
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
                        if (array_key_exists('short', $fo)) {
                            $short = (string)$fo['short'];
                        }
                        $this->add_option(new FindOption($short, $long, $desc));
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
     * @param FindSettings $settings
     * @param array $arg_tokens
     * @return void
     * @throws FindException
     */
    private function update_settings_from_arg_tokens(FindSettings $settings, array $arg_tokens): void
    {
        foreach ($arg_tokens as $arg_token) {
            if ($arg_token->type == ArgTokenType::Bool) {
                if (is_bool($arg_token->value)) {
                    $this->bool_action_map[$arg_token->name]($arg_token->value, $settings);
                    if (in_array($arg_token->name, array("help", "version"))) {
                        return;
                    }
                } else {
                    throw new FindException("Invalid value for option: $arg_token->name");
                }
            } elseif ($arg_token->type == ArgTokenType::Str) {
                if (is_array($arg_token->value)) {
                    foreach ($arg_token->value as $val) {
                        if (is_string($val)) {
                            $this->str_action_map[$arg_token->name]($val, $settings);
                        } else {
                            throw new FindException("Invalid value for option: $arg_token->name");
                        }
                    }
                } elseif (is_string($arg_token->value)) {
                    $this->str_action_map[$arg_token->name]($arg_token->value, $settings);
                } else {
                    throw new FindException("Invalid value for option: $arg_token->name");
                }
            } elseif ($arg_token->type == ArgTokenType::Int) {
                if (is_int($arg_token->value)) {
                    $this->int_action_map[$arg_token->name]($arg_token->value, $settings);
                } else {
                    throw new FindException("Invalid value for option: $arg_token->name");
                }
            } else {
                throw new FindException("Invalid option: $arg_token->name");
            }
        }
    }

    /**
     * @param string $json
     * @param FindSettings $settings
     * @return void
     * @throws FindException
     */
    public function update_settings_from_json(FindSettings $settings, string $json): void
    {
        $arg_tokens = $this->arg_tokenizer->tokenize_json($json);
        $this->update_settings_from_arg_tokens($settings, $arg_tokens);
    }

    /**
     * @param FindSettings $settings
     * @param string $file_path
     * @return void
     * @throws FindException
     */
    private function update_settings_from_file(FindSettings $settings, string $file_path): void
    {
        $arg_tokens = $this->arg_tokenizer->tokenize_file($file_path);
        $this->update_settings_from_arg_tokens($settings, $arg_tokens);
    }

    /**
     * @param FindSettings $settings
     * @param string[] $args
     * @return void
     * @throws FindException
     */
    public function update_settings_from_args(FindSettings $settings, array $args): void
    {
        $arg_tokens = $this->arg_tokenizer->tokenize_args($args);
        $this->update_settings_from_arg_tokens($settings, $arg_tokens);
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
