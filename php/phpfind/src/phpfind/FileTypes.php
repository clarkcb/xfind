<?php declare(strict_types=1);

namespace phpfind;

//require_once __DIR__ . '/../autoload.php';

/**
 * Class FileTypes
 *
 * @package phpfind
 * @property array $file_type_map
 */
class FileTypes
{
    private readonly array $file_type_map;

    /**
     * @throws FindException
     */
    public function __construct()
    {
        $this->file_type_map = $this->get_file_type_map_from_json();
    }

    /**
     * @throws FindException
     */
    private function get_file_type_map_from_json(): array
    {
        $file_type_map = array();
        $filetypespath = FileUtil::expand_user_home_path(Config::FILETYPESPATH);
        if (file_exists($filetypespath)) {
            $json_obj = json_decode(file_get_contents($filetypespath), true);
            foreach ($json_obj['filetypes'] as $ft) {
                $type = (string)$ft['type'];
                $exts = $ft['extensions'];
                $file_type_map[$type] = $exts;
            }
            $file_type_map['text'] = array_merge(
                $file_type_map['text'],
                $file_type_map['code'],
                $file_type_map['xml']
            );
        } else {
            throw new FindException('File not found: ' . $filetypespath);
        }
        return $file_type_map;
    }

    /**
     * @param string $typename
     * @return FileType
     */
    public static function from_name(string $typename): FileType
    {
        return match (strtoupper($typename)) {
            'ARCHIVE' => FileType::Archive,
            'BINARY' => FileType::Binary,
            'CODE' => FileType::Code,
            'TEXT' => FileType::Text,
            'XML' => FileType::Xml,
            default => FileType::Unknown
        };
    }

    /**
     * @param string $filename
     * @return FileType
     */
    public function get_filetype(string $filename): FileType
    {
        if ($this->is_code($filename)) {
            return FileType::Code;
        }
        if ($this->is_xml($filename)) {
            return FileType::Xml;
        }
        if ($this->is_text($filename)) {
            return FileType::Text;
        }
        if ($this->is_binary($filename)) {
            return FileType::Binary;
        }
        if ($this->is_archive($filename)) {
            return FileType::Archive;
        }
        return FileType::Unknown;
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_archive(string $filename): bool
    {
        return in_array(FileUtil::get_extension($filename), $this->file_type_map['archive']);
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_binary(string $filename): bool
    {
        return in_array(FileUtil::get_extension($filename), $this->file_type_map['binary']);
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_code(string $filename): bool
    {
        return in_array(FileUtil::get_extension($filename), $this->file_type_map['code']);
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_text(string $filename): bool
    {
        return in_array(FileUtil::get_extension($filename), $this->file_type_map['text']);
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_xml(string $filename): bool
    {
        return in_array(FileUtil::get_extension($filename), $this->file_type_map['xml']);
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_unknown(string $filename): bool
    {
        return $this->get_filetype($filename) == FileType::Unknown;
    }
}
