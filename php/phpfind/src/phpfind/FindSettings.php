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
    public array $in_archive_extensions = array();
    public array $in_archive_file_patterns = array();
    public array $in_dir_patterns = array();
    public array $in_extensions = array();
    public array $in_file_patterns = array();
    public array $in_file_types = array();
    public bool $include_archives = false;
    public bool $include_hidden = false;
    public bool $list_dirs = false;
    public bool $list_files = false;
    public int $max_depth = -1;
    public ?DateTime $max_last_mod = null;
    public int $max_size = 0;
    public int $min_depth = -1;
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
        return 'FindSettings(' .
            'archives_only=' . StringUtil::bool_to_string($this->archives_only) .
            ', debug=' . StringUtil::bool_to_string($this->debug) .
            ', in_archive_extensions=' . StringUtil::string_array_to_string($this->in_archive_extensions) .
            ', in_archive_file_patterns=' . StringUtil::string_array_to_string($this->in_archive_file_patterns) .
            ', in_dir_patterns=' . StringUtil::string_array_to_string($this->in_dir_patterns) .
            ', in_extensions=' . StringUtil::string_array_to_string($this->in_extensions) .
            ', in_file_patterns=' . StringUtil::string_array_to_string($this->in_file_patterns) .
            ', in_file_types=' . StringUtil::file_type_array_to_string($this->in_file_types) .
            ', include_archives=' . StringUtil::bool_to_string($this->include_archives) .
            ', include_hidden=' . StringUtil::bool_to_string($this->include_hidden) .
            ', list_dirs=' . StringUtil::bool_to_string($this->list_dirs) .
            ', list_files=' . StringUtil::bool_to_string($this->list_files) .
            ', max_depth=' . $this->max_depth .
            ', max_last_mod=' . StringUtil::datetime_to_string($this->max_last_mod) .
            ', max_size=' . $this->max_size .
            ', min_depth=' . $this->min_depth .
            ', min_last_mod=' . StringUtil::datetime_to_string($this->min_last_mod) .
            ', min_size=' . $this->min_size .
            ', out_archive_extensions=' . StringUtil::string_array_to_string($this->out_archive_extensions) .
            ', out_archive_file_patterns=' . StringUtil::string_array_to_string($this->out_archive_file_patterns) .
            ', out_dir_patterns=' . StringUtil::string_array_to_string($this->out_dir_patterns) .
            ', out_extensions=' . StringUtil::string_array_to_string($this->out_extensions) .
            ', out_file_patterns=' . StringUtil::string_array_to_string($this->out_file_patterns) .
            ', out_file_types=' . StringUtil::file_type_array_to_string($this->out_file_types) .
            ', paths=' . StringUtil::string_array_to_string($this->paths) .
            ', print_usage=' . StringUtil::bool_to_string($this->print_usage) .
            ', print_version=' . StringUtil::bool_to_string($this->print_version) .
            ', recursive=' . StringUtil::bool_to_string($this->recursive) .
            ', sort_by=' . $this->sort_by->value .
            ', sort_case_insensitive=' . StringUtil::bool_to_string($this->sort_case_insensitive) .
            ', sort_descending=' . StringUtil::bool_to_string($this->sort_descending) .
            ', verbose=' . StringUtil::bool_to_string($this->verbose) .
            ')';
    }
}
