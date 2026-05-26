<?php

declare(strict_types=1);

namespace phpfind;

/**
 * Class FileResult
 *
 * @package phpfind
 * @property string[] $containers
 * @property string $path
 * @property string $file_name
 * @property int $file_size
 * @property int $last_mod
 */

class FileResult
{
    const string CONTAINER_SEPARATOR = '!';

    /**
     * @var string[] $containers
     */
    public array $containers;
    public readonly string $file_path;
    public readonly FileType $file_type;
    public readonly int $file_size;
    public readonly int $last_mod;

    /**
     * @param string $file_path
     * @param FileType $file_type
     * @param int $file_size
     * @param int $last_mod
     */
    public function __construct(string $file_path, FileType $file_type, int $file_size, int $last_mod)
    {
        $this->containers = [];
        $this->file_path = $file_path;
        $this->file_type = $file_type;
        $this->file_size = $file_size;
        $this->last_mod = $last_mod;
    }

    /**
     * @return string
     */
    public function path(): string
    {
        return dirname($this->file_path);
    }

    /**
     * @return string
     */
    public function file_name(): string
    {
        return basename($this->file_path);
    }

    /**
     * @return string
     */
    public function __toString(): string
    {
        $s = "";
        if ($this->containers) {
            $s = join(FileResult::CONTAINER_SEPARATOR, $this->containers) .
                FileResult::CONTAINER_SEPARATOR;
        }
        $s .= FileUtil::join_paths($this->path, $this->file_name);
        return $s;
    }
}
