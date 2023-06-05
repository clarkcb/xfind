<?php declare(strict_types=1);

namespace phpfind;

use DateTime;

/**
 * Class FindSettings
 */
class FindSettings
{
    public bool $archives_only = false;
    public bool $debug = false;
    public bool $exclude_hidden = true;
    public array $in_archive_extensions = array();
    public array $in_archive_file_patterns = array();
    public array $in_dir_patterns = array();
    public array $in_extensions = array();
    public array $in_file_patterns = array();
    public array $in_file_types = array();
    public bool $include_archives = false;
    public bool $list_dirs = false;
    public bool $list_files = false;
    public ?DateTime $max_last_mod = null;
    public int $max_size = 0;
    public ?DateTime $min_last_mod = null;
    public int $min_size = 0;
    public array $out_archive_extensions = array();
    public array $out_archive_file_patterns = array();
    public array $out_dir_patterns = array();
    public array $out_extensions = array();
    public array $out_file_patterns = array();
    public array $out_file_types = array();
    public array $paths = array();
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

    public function need_stat(): bool
    {
        return $this->sort_by == SortBy::Filesize || $this->sort_by == SortBy::LastMod ||
            $this->max_last_mod != null || $this->min_last_mod != null ||
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
        switch (strtoupper($sort_by_name)) {
            case 'NAME':
                $this->sort_by = SortBy::Filename;
                break;
            case 'SIZE':
                $this->sort_by = SortBy::Filesize;
                break;
            case 'TYPE':
                $this->sort_by = SortBy::Filetype;
                break;
            case 'LASTMOD':
                $this->sort_by = SortBy::LastMod;
                break;
            default:
                $this->sort_by = SortBy::Filepath;
                break;
        }
    }

    /**
     * @return string
     */
    public function __toString(): string
    {
        return sprintf('FindSettings(' .
            'archives_only: %s' .
            ', debug: %s' .
            ', exclude_hidden: %s' .
            ', in_archive_extensions: %s' .
            ', in_archive_file_patterns: %s' .
            ', in_dir_patterns: %s' .
            ', in_extensions: %s' .
            ', in_file_patterns: %s' .
            ', in_file_types: %s' .
            ', include_archives: %s' .
            ', list_dirs: %s' .
            ', list_files: %s' .
            ', max_last_mod: %s' .
            ', max_size: %d' .
            ', min_last_mod: %s' .
            ', min_size: %d' .
            ', out_archive_extensions: %s' .
            ', out_archive_patterns: %s' .
            ', out_dir_patterns: %s' .
            ', out_extensions: %s' .
            ', out_file_patterns: %s' .
            ', out_file_types: %s' .
            ', paths: %s' .
            ', print_usage: %s' .
            ', print_version: %s' .
            ', recursive: %s' .
            ', sort_by: %s' .
            ', sort_case_insensitive: %s' .
            ', sort_descending: %s' .
            ', verbose: %s' .
            ')',
            StringUtil::bool_to_string($this->archives_only),
            StringUtil::bool_to_string($this->debug),
            StringUtil::bool_to_string($this->exclude_hidden),
            StringUtil::string_array_to_string($this->in_archive_extensions),
            StringUtil::string_array_to_string($this->in_archive_file_patterns),
            StringUtil::string_array_to_string($this->in_dir_patterns),
            StringUtil::string_array_to_string($this->in_extensions),
            StringUtil::string_array_to_string($this->in_file_patterns),
            StringUtil::file_type_array_to_string($this->in_file_types),
            StringUtil::bool_to_string($this->include_archives),
            StringUtil::bool_to_string($this->list_dirs),
            StringUtil::bool_to_string($this->list_files),
            StringUtil::datetime_to_string($this->max_last_mod),
            $this->max_size,
            StringUtil::datetime_to_string($this->min_last_mod),
            $this->min_size,
            StringUtil::string_array_to_string($this->out_archive_extensions),
            StringUtil::string_array_to_string($this->out_archive_patterns),
            StringUtil::string_array_to_string($this->out_dir_patterns),
            StringUtil::string_array_to_string($this->out_extensions),
            StringUtil::string_array_to_string($this->out_file_patterns),
            StringUtil::file_type_array_to_string($this->out_file_types),
            StringUtil::string_array_to_string($this->paths),
            StringUtil::bool_to_string($this->print_usage),
            StringUtil::bool_to_string($this->print_version),
            StringUtil::bool_to_string($this->recursive),
            $this->sort_by->name,
            StringUtil::bool_to_string($this->sort_case_insensitive),
            StringUtil::bool_to_string($this->sort_descending),
            StringUtil::bool_to_string($this->verbose)
        );
    }
}
