<?php

declare(strict_types=1);

namespace phpfind;

readonly class ArgTokenizer {
    private array $bool_map;
    private array $str_map;
    private array $int_map;

    public function __construct(array $bool_map, array $str_map, array $int_map)
    {
        $this->bool_map = $bool_map;
        $this->str_map = $str_map;
        $this->int_map = $int_map;
    }

    /**
     * @param string[] $args
     * @return array
     * @throws FindException
     */
    public function tokenize_args(array $args): array
    {
        $arg_tokens = [];
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
                            $arg_names[] = $arg;
                        } else {
                            throw new FindException("Invalid option: $arg");
                        }
                    } else {
                        # it's a short arg, with possibly multiple args
                        $arg = substr($arg, 1);
                        foreach (str_split($arg) as $c) {
                            if (array_key_exists($c, $this->bool_map)) {
                                $arg_names[] = $this->bool_map[$c];
                            } elseif (array_key_exists($c, $this->str_map)) {
                                $arg_names[] = $this->str_map[$c];
                            } elseif (array_key_exists($c, $this->int_map)) {
                                $arg_names[] = $this->int_map[$c];
                            } else {
                                throw new FindException("Invalid option: $c");
                            }
                        }
                    }
                } else {
                    throw new FindException("Invalid option: $arg");
                }

                foreach ($arg_names as $arg_name) {
                    if (array_key_exists($arg_name, $this->bool_map)) {
                        $arg_tokens[] = new ArgToken($arg_name, ArgTokenType::Bool, true);
                        if (in_array($arg_name, array("help", "version"))) {
                            return $arg_tokens;
                        }
                    } elseif (array_key_exists($arg_name, $this->str_map)
                        || array_key_exists($arg_name, $this->int_map)) {
                        if (count($args) > 0) {
                            $val = array_shift($args);
                            if (array_key_exists($arg_name, $this->str_map)) {
                                $arg_tokens[] = new ArgToken($arg_name, ArgTokenType::Str, $val);
                            } elseif (array_key_exists($arg_name, $this->int_map)) {
                                $arg_tokens[] = new ArgToken($arg_name, ArgTokenType::Int, intval($val));
                            }
                        } else {
                            throw new FindException("Missing value for $arg");
                        }
                    } else {
                        throw new FindException("Invalid option: $arg");
                    }
                }
            } else {
                $arg_tokens[] = new ArgToken('path', ArgTokenType::Str, $arg);
            }
        }
        return $arg_tokens;
    }

    /**
     * @param array $arg_map
     * @return array
     * @throws FindException
     */
    public function tokenize_arg_map(array $arg_map): array
    {
        $arg_names = array_keys($arg_map);
        # keys are sorted so that output is consistent across all versions
        sort($arg_names);
        $arg_tokens = [];
        foreach ($arg_names as $arg_name) {
            $arg_value = $arg_map[$arg_name];
            if (array_key_exists($arg_name, $this->bool_map)) {
                $long_arg = $this->bool_map[$arg_name];
                if (is_bool($arg_value)) {
                    $arg_tokens[] = new ArgToken($long_arg, ArgTokenType::Bool, $arg_value);
                    if (in_array($long_arg, array("help", "version"))) {
                        return $arg_tokens;
                    }
                } else {
                    throw new FindException("Invalid value for option: $arg_name");
                }
            } elseif (array_key_exists($arg_name, $this->str_map)) {
                $long_arg = $this->str_map[$arg_name];
                if (is_array($arg_value)) {
                    foreach ($arg_value as $val) {
                        if (is_string($val)) {
                            $arg_tokens[] = new ArgToken($long_arg, ArgTokenType::Str, $val);
                        } else {
                            throw new FindException("Invalid value for option: $arg_name");
                        }
                    }
                } elseif (is_string($arg_value)) {
                    $arg_tokens[] = new ArgToken($long_arg, ArgTokenType::Str, $arg_value);
                } else {
                    throw new FindException("Invalid value for option: $arg_name");
                }
            } elseif (array_key_exists($arg_name, $this->int_map)) {
                $long_arg = $this->int_map[$arg_name];
                if (is_int($arg_value)) {
                    $arg_tokens[] = new ArgToken($long_arg, ArgTokenType::Int, $arg_value);
                } elseif (is_string($arg_value)) {
                    $arg_tokens[] = new ArgToken($long_arg, ArgTokenType::Int, intval($arg_value));
                } else {
                    throw new FindException("Invalid value for option: $arg_name");
                }
            } else {
                throw new FindException("Invalid option: $arg_name");
            }
        }
        return $arg_tokens;
    }

    /**
     * @param string $json
     * @return array
     * @throws FindException
     */
    public function tokenize_json(string $json): array
    {
        if (trim($json) === '') {
            return [];
        }
        try {
            $json_obj = (array)json_decode(trim($json), true, 512, JSON_THROW_ON_ERROR);
            return $this->tokenize_arg_map($json_obj);
        } catch (\JsonException $e) {
            throw new FindException($e->getMessage());
        }
    }

    /**
     * @param string $file_path
     * @return array
     * @throws FindException
     */
    public function tokenize_file(string $file_path): array
    {
        $expanded_path = FileUtil::expand_path($file_path);
        if (!file_exists($expanded_path)) {
            throw new FindException('Settings file not found: ' . $file_path);
        }
        if (!str_ends_with($expanded_path, '.json')) {
            throw new FindException('Invalid settings file (must be JSON): ' . $file_path);
        }
        $json = file_get_contents($expanded_path);
        return $this->tokenize_json($json);
    }
}
