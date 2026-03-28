<?php

declare(strict_types=1);

namespace phpfind;

use DateTime;
use ReflectionClass;
use ReflectionProperty;

/**
 * Class FindSettings
 */
class FindSettings
{
    public bool $archives_only = false;
    public bool $colorize = true;
    public bool $debug = false;
    public bool $default_files = true;
    public Color $dir_color = Color::Cyan;
    public Color $ext_color = Color::Yellow;
    public Color $file_color = Color::Magenta;
    public bool $follow_symlinks = false;
    /**
     * @var string[] $in_archive_extensions
     */
    public array $in_archive_extensions = [];
    /**
     * @var string[] $in_archive_file_patterns
     */
    public array $in_archive_file_patterns = [];
    /**
     * @var string[] $in_dir_patterns
     */
    public array $in_dir_patterns = [];
    /**
     * @var string[] $in_extensions
     */
    public array $in_extensions = [];
    /**
     * @var string[] $in_file_patterns
     */
    public array $in_file_patterns = [];
    /**
     * @var FileType[] $in_file_types
     */
    public array $in_file_types = [];
    public bool $include_archives = false;
    public bool $include_hidden = false;
    public int $max_depth = -1;
    public ?DateTime $max_last_mod = null;
    public int $max_size = 0;
    public int $min_depth = -1;
    public ?DateTime $min_last_mod = null;
    public int $min_size = 0;
    /**
     * @var string[] $out_archive_extensions
     */
    public array $out_archive_extensions = [];
    /**
     * @var string[] $out_archive_file_patterns
     */
    public array $out_archive_file_patterns = [];
    /**
     * @var string[] $out_dir_patterns
     */
    public array $out_dir_patterns = [];
    /**
     * @var string[] $out_extensions
     */
    public array $out_extensions = [];
    /**
     * @var string[] $out_file_patterns
     */
    public array $out_file_patterns = [];
    /**
     * @var FileType[] $out_file_types
     */
    public array $out_file_types = [];
    /**
     * @var string[] $paths
     */
    public array $paths = [];
    public bool $print_dirs = false;
    public bool $print_files = false;
    public bool $print_usage = false;
    public bool $print_version = false;
    public bool $recursive = true;
    public SortBy $sort_by = SortBy::Filepath;
    public bool $sort_case_insensitive = false;
    public bool $sort_descending = false;
    public bool $verbose = false;

    /**
     * @param string|string[] $ext
     * @param string[] $exts
     * @return void
     */
    public function add_exts(string|array $ext, array &$exts): void
    {
        if (gettype($ext) == 'string') {
            $xs = explode(',', $ext);
            foreach ($xs as $x) {
                $exts[] = $x;
            }
        } elseif (gettype($ext) == 'array') {
            foreach ($ext as $x) {
                $exts[] = $x;
            }
        }
    }

    /**
     * @param string|string[] $file_type
     * @param FileType[] $file_types
     * @return void
     */
    public function add_file_types(string|array $file_type, array &$file_types): void
    {
        if (gettype($file_type) == 'string') {
            $fts = explode(',', $file_type);
            foreach ($fts as $ft) {
                $file_types[] = FileTypes::from_name($ft);
            }
        } elseif (gettype($file_type) == 'array') {
            foreach ($file_type as $ft) {
                $file_types[] = FileTypes::from_name($ft);
            }
        }
    }

    /**
     * @param string|string[] $pattern
     * @param string[] $patterns
     * @return void
     */
    public function add_patterns(string|array $pattern, array &$patterns): void
    {
        if (gettype($pattern) == 'string') {
            $patterns[] = $pattern;
        } elseif (gettype($pattern) == 'array') {
            foreach ($pattern as $p) {
                $patterns[] = $p;
            }
        }
    }

    /**
     * @return bool
     */
    public function need_last_mod(): bool
    {
        return $this->sort_by == SortBy::LastMod ||
            $this->max_last_mod != null || $this->min_last_mod != null;
    }

    /**
     * @return bool
     */
    public function need_size(): bool
    {
        return $this->sort_by == SortBy::Filesize ||
            $this->max_size > 0 || $this->min_size > 0;
    }

    /**
     * @param bool $b
     * @return void
     */
    public function set_archives_only(bool $b): void
    {
        $this->archives_only = $b;
        if ($b) {
            $this->include_archives = $b;
        }
    }

    /**
     * @param bool $b
     * @return void
     */
    public function set_debug(bool $b): void
    {
        $this->debug = $b;
        if ($b) {
            $this->verbose = $b;
        }
    }

    public function set_sort_by(string $sort_by_name): void
    {
        $this->sort_by = match (preg_replace("/[^A-Z]/", '', strtoupper($sort_by_name))) {
            'NAME' => SortBy::Filename,
            'SIZE' => SortBy::Filesize,
            'TYPE' => SortBy::Filetype,
            'LASTMOD' => SortBy::LastMod,
            default => SortBy::Filepath,
        };
    }

    private function __getPropertiesString($reflector): string
    {
        $properties = $reflector->getProperties(ReflectionProperty::IS_PUBLIC);

        $property_map = [];
        foreach ($properties as $property) {
            if ($property->hasType()) {
                $name = $property->getName();
                $property_map[$name] = $property;
            }
        }
        ksort($property_map);

        $property_strings = [];
        foreach ($property_map as $name => $property) {
            $type = $property->getType();
            $type_name = $type->getName();
            $value = $property->getValue($this);
            if ($type_name == 'array') {
                if (str_ends_with($name, 'file_types')) {
                    $value = StringUtil::file_type_array_to_string($value);
                } else {
                    $value = StringUtil::string_array_to_string($value);
                }
            } elseif ($type_name == 'bool') {
                $value = StringUtil::bool_to_string($value);
            } elseif ($type_name == 'DateTime') {
                $value = StringUtil::datetime_to_string($value);
            } elseif ($type_name == 'phpfind\\Color') {
                $value = $value->value;
            } elseif ($type_name == 'phpfind\\SortBy') {
                $value = $value->value;
            }
            $property_strings[] = $name . '=' . $value;
        }
        return join(', ', $property_strings);
    }

    /**
     * @return string
     */
    public function __toString(): string
    {
        $reflector = new ReflectionClass($this);
        return $reflector->getShortName() . '(' .
            $this->__getPropertiesString($reflector) .
            ')';
    }
}
