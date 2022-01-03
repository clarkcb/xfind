<?php declare(strict_types=1);

namespace phpfind;

require_once __DIR__ . '/../autoload.php';
//include_once __DIR__ . '/common.php';

/**
 * Class FindOptions
 *
 * @property array options
 * @property array arg_action_map
 * @property array bool_flag_action_map
 * @property array longarg_map
 */
class FindOptions
{
    private array $options;
    private readonly array $arg_action_map;
    private readonly array $bool_flag_action_map;
    private array $longarg_map;

    /**
     * @throws FindException
     */
    public function __construct()
    {
        $this->options = array();

        $this->arg_action_map = [
            'in-archiveext' => fn (string $s, FindSettings $fs) => $fs->add_exts($s, $fs->in_archiveextensions),
            'in-archivefilepattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->in_archivefilepatterns),
            'in-dirpattern' => fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->in_dirpatterns),
            'in-ext' => fn (string $s, FindSettings $fs) => $fs->add_exts($s, $fs->in_extensions),
            'in-filepattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->in_filepatterns),
            'in-filetype' => fn (string $s, FindSettings $fs) => $fs->add_filetypes($s, $fs->in_filetypes),
            'out-archiveext' => fn (string $s, FindSettings $fs) => $fs->add_exts($s, $fs->out_archiveextensions),
            'out-archivefilepattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->out_archivefilepatterns),
            'out-dirpattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->out_dirpatterns),
            'out-ext' => fn (string $s, FindSettings $fs) => $fs->add_exts($s, $fs->out_extensions),
            'out-filepattern' =>
                fn (string $s, FindSettings $fs) => $fs->add_patterns($s, $fs->out_filepatterns),
            'out-filetype' => fn (string $s, FindSettings $fs) => $fs->add_filetypes($s, $fs->out_filetypes),
            'path' => fn (string $s, FindSettings $fs) => $fs->paths[] = $s,
            'settings-file' => fn (string $s, FindSettings $fs) => $this->settings_from_file($s, $fs)
        ];

        $this->bool_flag_action_map = [
            'archivesonly' => fn (bool $b, FindSettings $fs) => $fs->set_archivesonly($b),
            'debug' => fn (bool $b, FindSettings $fs) => $fs->set_debug($b),
            'excludearchives' => fn (bool $b, FindSettings $fs) => $fs->includearchives = !$b,
            'excludehidden' => fn (bool $b, FindSettings $fs) => $fs->excludehidden = $b,
            'help' => fn (bool $b, FindSettings $fs) => $fs->printusage = $b,
            'includearchives' => fn (bool $b, FindSettings $fs) => $fs->includearchives = $b,
            'includehidden' => fn (bool $b, FindSettings $fs) => $fs->excludehidden = !$b,
            'listdirs' => fn (bool $b, FindSettings $fs) => $fs->listdirs = $b,
            'listfiles' => fn (bool $b, FindSettings $fs) => $fs->listfiles = $b,
            'norecursive' => fn (bool $b, FindSettings $fs) => $fs->recursive = !$b,
            'recursive' => fn (bool $b, FindSettings $fs) => $fs->recursive = $b,
            'verbose' => fn (bool $b, FindSettings $fs) => $fs->verbose = $b,
            'version' => fn (bool $b, FindSettings $fs) => $fs->printversion = $b
        ];
        $this->longarg_map = array();
        $this->set_options_from_json();
    }

    /**
     * @throws FindException
     */
    private function set_options_from_json()
    {
        $findoptionspath = FileUtil::expand_user_home_path(Config::FINDOPTIONSPATH);
        if (file_exists($findoptionspath)) {
            $json_obj = json_decode(file_get_contents($findoptionspath), true);
            foreach ($json_obj['findoptions'] as $so) {
                $short = '';
                if (array_key_exists('short', $so)) {
                    $short = (string)$so['short'];
                }
                $long = (string)$so['long'];
                $desc = (string)$so['desc'];
                $func = null;
                if (array_key_exists($long, $this->arg_action_map)) {
                    $func = $this->arg_action_map[$long];
                } elseif (array_key_exists($long, $this->bool_flag_action_map)) {
                    $func = $this->bool_flag_action_map[$long];
                }
                $option = new FindOption($short, $long, $desc, $func);
                $this->options[] = $option;
                $this->longarg_map[$long] = $long;
                if ($short) {
                    $this->longarg_map[$short] = $long;
                }
            }
            usort($this->options, array('phpfind\FindOptions', 'cmp_findoptions'));
        } else {
            throw new FindException('File not found: ' . $findoptionspath);
        }
    }

    /**
     * @throws FindException
     */
    private function settings_from_file(string $filepath, FindSettings $settings): void
    {
        if (!file_exists($filepath)) {
            throw new FindException('Settings file not found');
        }
        $json = file_get_contents($filepath);
        $this->settings_from_json($json, $settings);
    }

    /**
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
                    $this->arg_action_map[$k](sprintf($json_obj[$k]), $settings);
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
     * @throws FindException
     */
    public function settings_from_args(array $args): FindSettings
    {
        $settings = new FindSettings();
        // default listfiles to true since running as cli
        $settings->listfiles = true;
        while (count($args) > 0) {
            $arg = array_shift($args);
            if ($arg[0] == '-') {
                while ($arg[0] == '-') {
                    $arg = substr($arg, 1);
                }
                if (!array_key_exists($arg, $this->longarg_map)) {
                    throw new FindException("Invalid option: $arg");
                }
                $longarg = $this->longarg_map[$arg];
                if (array_key_exists($longarg, $this->arg_action_map)) {
                    if (count($args) > 0) {
                        $val = array_shift($args);
                        $this->arg_action_map[$longarg]($val, $settings);
                    } else {
                        throw new FindException("Missing value for $arg");
                    }
                } elseif (array_key_exists($longarg, $this->bool_flag_action_map)) {
                    $this->bool_flag_action_map[$longarg](true, $settings);
                    if (in_array($longarg, array("help", "version"))) {
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

    private static function cmp_findoptions(FindOption $o1, FindOption $o2): int
    {
        return strcmp($o1->sortarg, $o2->sortarg);
    }

    private function get_usage_string(): string
    {
        $usage = "Usage:\n phpfind [options] <path> [<path> ...]";
        $usage .= "\n\nOptions:\n";
        $opt_map = array();
        $longest = 0;
        foreach ($this->options as $option) {
            $opt_str = '';
            if ($option->shortarg) {
                $opt_str = '-' . $option->shortarg . ',';
            }
            $opt_str .= '--' . $option->longarg;
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

    public function usage(): void
    {
        echo $this->get_usage_string() . "\n";
    }

    public function usage_and_exit(int $exit_code = 0): never
    {
        $this->usage();
        exit($exit_code);
    }
}
