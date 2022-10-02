<?php declare(strict_types=1);

namespace phpfind;

//require_once __DIR__ . '/../autoload.php';

/**
 * Class FileTypes
 *
 * @package phpfind
 * @property array $file_type_ext_map
 * @property array $file_type_name_map
 */
class FileTypes
{
    private readonly array $file_type_ext_map;
    private readonly array $file_type_name_map;

    /**
     * @throws FindException
     */
    public function __construct()
    {
        $file_type_maps = $this->get_file_type_map_from_json();
        $this->file_type_ext_map = $file_type_maps[0];
        $this->file_type_name_map = $file_type_maps[1];
    }

    /**
     * @throws FindException
     */
    private function get_file_type_map_from_json(): array
    {
        $file_type_ext_map = array();
        $file_type_name_map = array();
        $filetypespath = FileUtil::expand_user_home_path(Config::FILETYPESPATH);
        if (file_exists($filetypespath)) {
            $json_obj = json_decode(file_get_contents($filetypespath), true);
            foreach ($json_obj['filetypes'] as $ft) {
                $type = (string)$ft['type'];
                $exts = $ft['extensions'];
                $file_type_ext_map[$type] = $exts;
                $names = $ft['names'];
                $file_type_name_map[$type] = $names;
            }
            $file_type_ext_map['text'] = array_merge(
                $file_type_ext_map['text'],
                $file_type_ext_map['code'],
                $file_type_ext_map['xml']
            );
            $file_type_name_map['text'] = array_merge(
                $file_type_name_map['text'],
                $file_type_name_map['code'],
                $file_type_name_map['xml']
            );
        } else {
            throw new FindException('File not found: ' . $filetypespath);
        }
        return [$file_type_ext_map, $file_type_name_map];
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
        return in_array($filename, $this->file_type_name_map['archive'])
            || in_array(FileUtil::get_extension($filename), $this->file_type_ext_map['archive']);
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_binary(string $filename): bool
    {
        return in_array($filename, $this->file_type_name_map['binary'])
            || in_array(FileUtil::get_extension($filename), $this->file_type_ext_map['binary']);
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_code(string $filename): bool
    {
        return in_array($filename, $this->file_type_name_map['code'])
            || in_array(FileUtil::get_extension($filename), $this->file_type_ext_map['code']);
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_text(string $filename): bool
    {
        return in_array($filename, $this->file_type_name_map['text'])
            || in_array(FileUtil::get_extension($filename), $this->file_type_ext_map['text']);
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_xml(string $filename): bool
    {
        return in_array($filename, $this->file_type_name_map['xml'])
            || in_array(FileUtil::get_extension($filename), $this->file_type_ext_map['xml']);
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
