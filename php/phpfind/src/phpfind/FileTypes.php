<?php declare(strict_types=1);

namespace phpfind;

//require_once __DIR__ . '/../autoload.php';

/**
 * Class FileTypes
 *
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

    public static function from_name(string $name): FileType
    {
        return match (strtoupper($name)) {
            'ARCHIVE' => FileType::Archive,
            'BINARY' => FileType::Binary,
            'CODE' => FileType::Code,
            'TEXT' => FileType::Text,
            'XML' => FileType::Xml,
            default => FileType::Unknown
        };
    }

    public function get_filetype(string $file): FileType
    {
        if ($this->is_code($file)) {
            return FileType::Code;
        }
        if ($this->is_xml($file)) {
            return FileType::Xml;
        }
        if ($this->is_text($file)) {
            return FileType::Text;
        }
        if ($this->is_binary($file)) {
            return FileType::Binary;
        }
        if ($this->is_archive($file)) {
            return FileType::Archive;
        }
        return FileType::Unknown;
    }

    public function is_archive(string $f): bool
    {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['archive']);
    }

    public function is_binary(string $f): bool
    {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['binary']);
    }

    public function is_code(string $f): bool
    {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['code']);
    }

    public function is_text(string $f): bool
    {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['text']);
    }

    public function is_xml(string $f): bool
    {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['xml']);
    }

    public function is_unknown(string $f): bool
    {
        return $this->get_filetype($f) == FileType::Unknown;
    }
}
