<?php declare(strict_types=1);

namespace phpfind;

/**
 * Class FileResult
 */
class FileResult
{
    const CONTAINER_SEPARATOR = '!';

    public array $containers;
    public readonly string $path;
    public readonly string $file_name;
    public readonly FileType $file_type;
    public readonly array|false $stat;

    /**
     * @param string $path
     * @param string $file_name
     * @param FileType $file_type
     * @param array|false $stat
     */
    public function __construct(string $path, string $file_name, FileType $file_type, array|false $stat)
    {
        $this->containers = array();
        $this->path = $path;
        $this->file_name = $file_name;
        $this->file_type = $file_type;
        $this->stat = $stat;
    }

    /**
     * @return string
     */
    public function file_path(): string
    {
        return FileUtil::join_path($this->path, $this->file_name);
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
        $s .= FileUtil::join_path($this->path, $this->file_name);
        return $s;
    }
}
