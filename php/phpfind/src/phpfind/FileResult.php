<?php declare(strict_types=1);

namespace phpfind;

/**
 * Class FindFile
 *
 * @property string[] $containers
 * @property array<string, int>|false $stat
 */
class FileResult
{
    const CONTAINER_SEPARATOR = '!';

    public array $containers;
    public readonly string $path;
    public readonly string $filename;
    public readonly FileType $filetype;
    public readonly array|false $stat;

    /**
     * @param string $path
     * @param string $filename
     * @param FileType $filetype
     * @param array|false $stat
     */
    public function __construct(string $path, string $filename, FileType $filetype, array|false $stat)
    {
        $this->containers = array();
        $this->path = $path;
        $this->filename = $filename;
        $this->filetype = $filetype;
        $this->stat = $stat;
    }

    /**
     * @return string
     */
    public function filepath(): string
    {
        return FileUtil::join_path($this->path, $this->filename);
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
        $s .= FileUtil::join_path($this->path, $this->filename);
        return $s;
    }
}
