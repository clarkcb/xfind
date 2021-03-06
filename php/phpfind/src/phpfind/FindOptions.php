<?php declare(strict_types=1);

namespace phpfind;

require_once __DIR__ . '/../autoload.php';
//include_once __DIR__ . '/common.php';

/**
 * Class FindOptions
 */
class FindOptions
{
    private $options;

    public function __construct()
    {
        $this->options = array();

        $this->arg_action_map = [
            'in-archiveext' => function (string $s, FindSettings $settings) {
                $settings->add_exts($s, $settings->in_archiveextensions);
            },
            'in-archivefilepattern' => function (string $s, FindSettings $settings) {
                $settings->add_patterns($s, $settings->in_archivefilepatterns);
            },
            'in-dirpattern' => function (string $s, FindSettings $settings) {
                $settings->add_patterns($s, $settings->in_dirpatterns);
            },
            'in-ext' => function (string $s, FindSettings $settings) {
                $settings->add_exts($s, $settings->in_extensions);
            },
            'in-filepattern' => function (string $s, FindSettings $settings) {
                $settings->add_patterns($s, $settings->in_filepatterns);
            },
            'in-filetype' => function (string $s, FindSettings $settings) {
                $settings->add_filetypes($s, $settings->in_filetypes);
            },
            'out-archiveext' => function (string $s, FindSettings $settings) {
                $settings->add_exts($s, $settings->out_archiveextensions);
            },
            'out-archivefilepattern' => function (string $s, FindSettings $settings) {
                $settings->add_patterns($s, $settings->out_archivefilepatterns);
            },
            'out-dirpattern' => function (string $s, FindSettings $settings) {
                $settings->add_patterns($s, $settings->out_dirpatterns);
            },
            'out-ext' => function (string $s, FindSettings $settings) {
                $settings->add_exts($s, $settings->out_extensions);
            },
            'out-filepattern' => function (string $s, FindSettings $settings) {
                $settings->add_patterns($s, $settings->out_filepatterns);
            },
            'out-filetype' => function (string $s, FindSettings $settings) {
                $settings->add_filetypes($s, $settings->out_filetypes);
            },
            'settings-file' => function (string $s, FindSettings $settings) {
                $this->settings_from_file($s, $settings);
            }
        ];

        $this->bool_flag_action_map = [
            'archivesonly' => function (bool $b, FindSettings $settings) {
                $settings->set_archivesonly($b);
            },
            'colorize' => function (bool $b, FindSettings $settings) {
                $settings->colorize = $b;
            },
            'debug' => function (bool $b, FindSettings $settings) {
                $settings->set_debug($b);
            },
            'excludearchives' => function (bool $b, FindSettings $settings) {
                $settings->includearchives = !$b;
            },
            'excludehidden' => function (bool $b, FindSettings $settings) {
                $settings->excludehidden = $b;
            },
            'help' => function (bool $b, FindSettings $settings) {
                $settings->printusage = $b;
            },
            'includearchives' => function (bool $b, FindSettings $settings) {
                $settings->includearchives = $b;
            },
            'includehidden' => function (bool $b, FindSettings $settings) {
                $settings->excludehidden = !$b;
            },
            'listdirs' => function (bool $b, FindSettings $settings) {
                $settings->listdirs = $b;
            },
            'listfiles' => function (bool $b, FindSettings $settings) {
                $settings->listfiles = $b;
            },
            'nocolorize' => function (bool $b, FindSettings $settings) {
                $settings->colorize = !$b;
            },
            'norecursive' => function (bool $b, FindSettings $settings) {
                $settings->recursive = !$b;
            },
            'recursive' => function (bool $b, FindSettings $settings) {
                $settings->recursive = $b;
            },
            'verbose' => function (bool $b, FindSettings $settings) {
                $settings->verbose = $b;
            },
            'version' => function (bool $b, FindSettings $settings) {
                $settings->printversion = $b;
            }
        ];
        $this->longarg_map = array();
        $this->set_options_from_json();
    }

    private function set_options_from_json()
    {
        $findoptionspath = FileUtil::expand_user_home_path(Config::FINDOPTIONSPATH);
        if (file_exists($findoptionspath)) {
            $json_obj = json_decode(file_get_contents($findoptionspath), true);
            foreach ($json_obj['findoptions'] as $so) {
                $short = '';
                if (array_key_exists('short', $so)) {
                    $short = sprintf($so['short']);
                }
                $long = sprintf($so['long']);
                $desc = $so['desc'];
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

    private function settings_from_file(string $filepath, FindSettings $settings)
    {
        if (!file_exists($filepath)) {
            throw new FindException('Settings file not found');
        }
        $json = file_get_contents($filepath);
        $this->settings_from_json($json, $settings);
    }

    public function settings_from_json(string $json, FindSettings $settings)
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
            } elseif ($k == 'path') {
                $settings->paths[] = $json_obj[$k];
            } else {
                throw new FindException("Invalid option: $k");
            }
        }
    }

    public function settings_from_args(array $args): FindSettings
    {
        $settings = new FindSettings();
        // default listfiles to true since running as cli
        $settings->listfiles = true;
        while (count($args) > 0) {
            $arg = array_shift($args);
            if ($arg{0} == '-') {
                while ($arg{0} == '-') {
                    $arg = substr($arg, 1);
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

    public function usage()
    {
        echo $this->get_usage_string() . "\n";
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

    private static function cmp_findoptions(FindOption $o1, FindOption $o2): int
    {
        return strcmp($o1->sortarg, $o2->sortarg);
    }
}
