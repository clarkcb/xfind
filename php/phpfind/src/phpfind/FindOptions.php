<?php declare(strict_types=1);

namespace phpfind;

require_once __DIR__ . '/../autoload.php';
//include_once __DIR__ . '/common.php';

/**
 * Class FindOptions
 */
class FindOptions
{
    private array $options;
    private readonly array $arg_action_map;
    private readonly array $bool_flag_action_map;
    private array $long_arg_map;

    /**
     * @throws FindException
     */
    public function __construct()
    {
        $this->options = array();

        $this->arg_action_map = [
            'in-archiveext' => fn (string $s, FindSettings $fs) => $fs->add_exts($s, $fs->in_archive_extensions),
            'in-archivefilepattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->in_archive_file_patterns),
            'in-dirpattern' => fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->in_dir_patterns),
            'in-ext' => fn (string $s, FindSettings $fs) => $fs->add_exts($s, $fs->in_extensions),
            'in-filepattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->in_file_patterns),
            'in-filetype' => fn (string $s, FindSettings $fs) => $fs->add_file_types($s, $fs->in_file_types),
            'maxdepth' => fn (string $s, FindSettings $fs) => $fs->max_depth = intval($s),
            'maxlastmod' => fn (string $s, FindSettings $fs) => $fs->max_last_mod = new \DateTime($s),
            'maxsize' => fn (string $s, FindSettings $fs) => $fs->max_size = intval($s),
            'mindepth' => fn (string $s, FindSettings $fs) => $fs->min_depth = intval($s),
            'minlastmod' => fn (string $s, FindSettings $fs) => $fs->min_last_mod = new \DateTime($s),
            'minsize' => fn (string $s, FindSettings $fs) => $fs->min_size = intval($s),
            'out-archiveext' => fn (string $s, FindSettings $fs) => $fs->add_exts($s, $fs->out_archive_extensions),
            'out-archivefilepattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->out_archive_file_patterns),
            'out-dirpattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->out_dir_patterns),
            'out-ext' => fn (string $s, FindSettings $fs) => $fs->add_exts($s, $fs->out_extensions),
            'out-filepattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->out_file_patterns),
            'out-filetype' => fn (string $s, FindSettings $fs) => $fs->add_file_types($s, $fs->out_file_types),
            'path' => fn (string $s, FindSettings $fs) => $fs->paths[] = $s,
            'settings-file' => fn (string $s, FindSettings $fs) => $this->settings_from_file($s, $fs),
            'sort-by' => fn (string $s, FindSettings $fs) => $fs->set_sort_by($s)
        ];

        $this->bool_flag_action_map = [
            'archivesonly' => fn (bool $b, FindSettings $fs) => $fs->set_archives_only($b),
            'debug' => fn (bool $b, FindSettings $fs) => $fs->set_debug($b),
            'excludearchives' => fn (bool $b, FindSettings $fs) => $fs->include_archives = !$b,
            'excludehidden' => fn (bool $b, FindSettings $fs) => $fs->include_hidden = !$b,
            'help' => fn (bool $b, FindSettings $fs) => $fs->print_usage = $b,
            'includearchives' => fn (bool $b, FindSettings $fs) => $fs->include_archives = $b,
            'includehidden' => fn (bool $b, FindSettings $fs) => $fs->include_hidden = $b,
            'noprintdirs' => fn (bool $b, FindSettings $fs) => $fs->print_dirs = !$b,
            'noprintfiles' => fn (bool $b, FindSettings $fs) => $fs->print_files = !$b,
            'norecursive' => fn (bool $b, FindSettings $fs) => $fs->recursive = !$b,
            'printdirs' => fn (bool $b, FindSettings $fs) => $fs->print_dirs = $b,
            'printfiles' => fn (bool $b, FindSettings $fs) => $fs->print_files = $b,
            'recursive' => fn (bool $b, FindSettings $fs) => $fs->recursive = $b,
            'sort-ascending' => fn (bool $b, FindSettings $fs) => $fs->sort_descending = !$b,
            'sort-caseinsensitive' => fn (bool $b, FindSettings $fs) => $fs->sort_case_insensitive = $b,
            'sort-casesensitive' => fn (bool $b, FindSettings $fs) => $fs->sort_case_insensitive = !$b,
            'sort-descending' => fn (bool $b, FindSettings $fs) => $fs->sort_descending = $b,
            'verbose' => fn (bool $b, FindSettings $fs) => $fs->verbose = $b,
            'version' => fn (bool $b, FindSettings $fs) => $fs->print_version = $b
        ];
        $this->long_arg_map = array();
        $this->set_options_from_json();
    }

    /**
     * @throws FindException
     */
    private function set_options_from_json(): void
    {
        $find_options_path = FileUtil::expand_user_home_path(Config::FINDOPTIONSPATH);
        if (file_exists($find_options_path)) {
            $json_obj = json_decode(file_get_contents($find_options_path), true);
            foreach ($json_obj['findoptions'] as $so) {
                $short = '';
                $long = (string)$so['long'];
                $desc = (string)$so['desc'];
                $this->long_arg_map[$long] = $long;
                if (array_key_exists('short', $so)) {
                    $short = (string)$so['short'];
                    $this->long_arg_map[$short] = $long;
                }
                $option = new FindOption($short, $long, $desc);
                $this->options[] = $option;
            }
            usort($this->options, array('phpfind\FindOptions', 'cmp_find_options'));
        } else {
            throw new FindException('File not found: ' . $find_options_path);
        }
    }

    /**
     * @param string $file_path
     * @param FindSettings $settings
     * @return void
     * @throws FindException
     */
    private function settings_from_file(string $file_path, FindSettings $settings): void
    {
        if (!file_exists($file_path)) {
            throw new FindException('Settings file not found');
        }
        $json = file_get_contents($file_path);
        $this->settings_from_json($json, $settings);
    }

    /**
     * @param string $json
     * @param FindSettings $settings
     * @return void
     * @throws FindException
     */
    public function settings_from_json(string $json, FindSettings $settings): void
    {
        $json_obj = json_decode($json, true);
        foreach (array_keys($json_obj) as $k) {
            if (array_key_exists($k, $this->arg_action_map)) {
                if (gettype($json_obj[$k]) == 'string') {
                    $this->arg_action_map[$k]($json_obj[$k], $settings);
                } elseif (gettype($json_obj[$k]) == 'integer') {
                    $this->arg_action_map[$k](sprintf('%d', $json_obj[$k]), $settings);
                } elseif (gettype($json_obj[$k]) == 'array') {
                    foreach ($json_obj[$k] as $s) {
                        $this->arg_action_map[$k]($s, $settings);
                    }
                } else {
                    throw new FindException("Invalid setting type: $k");
                }
            } elseif (array_key_exists($k, $this->bool_flag_action_map)) {
                $this->bool_flag_action_map[$k]($json_obj[$k], $settings);
            } else {
                throw new FindException("Invalid option: $k");
            }
        }
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
        while (count($args) > 0) {
            $arg = array_shift($args);
            if ($arg[0] == '-') {
                while ($arg[0] == '-') {
                    $arg = substr($arg, 1);
                }
                if (!array_key_exists($arg, $this->long_arg_map)) {
                    throw new FindException("Invalid option: $arg");
                }
                $long_arg = $this->long_arg_map[$arg];
                if (array_key_exists($long_arg, $this->arg_action_map)) {
                    if (count($args) > 0) {
                        $val = array_shift($args);
                        $this->arg_action_map[$long_arg]($val, $settings);
                    } else {
                        throw new FindException("Missing value for $arg");
                    }
                } elseif (array_key_exists($long_arg, $this->bool_flag_action_map)) {
                    $this->bool_flag_action_map[$long_arg](true, $settings);
                    if (in_array($long_arg, array("help", "version"))) {
                        break;
                    }
                } else {
                    throw new FindException("Invalid option: $arg");
                }
            } else {
                $settings->paths[] = $arg;
            }
        }
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
        $opt_map = array();
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
