<?php

declare(strict_types=1);

namespace phpfind;

//require_once __DIR__ . '/../autoload.php';

/**
 * Class FileTypes
 *
 * @package phpfind
 * @property array<string, string[]> $file_type_ext_map
 * @property array<string, string[]> $file_type_name_map
 */
class FileTypes
{
    /**
     * @var array<string, string[]> $file_type_ext_map
     */
    private readonly array $file_type_ext_map;
    /**
     * @var array<string, string[]> $file_type_name_map
     */
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
     * @return array<int, array<string, string[]>>
     * @throws FindException
     */
    private function get_file_type_map_from_json(): array
    {
        $file_type_ext_map = array();
        $file_type_name_map = array();
        $file_types_path = FileUtil::expand_path(Config::FILE_TYPES_PATH);
        if (file_exists($file_types_path)) {
            $contents = file_get_contents($file_types_path);
            if ($contents === false || trim($contents) === '') {
                return [];
            }
            try {
                $json_obj = (array)json_decode(trim($contents), true, 512, JSON_THROW_ON_ERROR);
                $file_types = $json_obj['filetypes'];
                if ($file_types !== null) {
                    foreach ((array)$file_types as $ft) {
                        $ft = (array)$ft;
                        $type = (string)$ft['type'];
                        $exts = (array)$ft['extensions'];
                        $file_type_ext_map[$type] = $exts;
                        $names = (array)$ft['names'];
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
                }
            } catch (\JsonException $e) {
                throw new FindException($e->getMessage());
            }
        } else {
            throw new FindException('File not found: ' . $file_types_path);
        }
        return [$file_type_ext_map, $file_type_name_map];
    }

    /**
     * @param string $typename
     * @return FileType
     */
    public static function from_name(string $typename): FileType
    {
        return FileType::tryFrom(strtolower($typename)) ?? FileType::Unknown;
    }

    /**
     * @param FileType $type
     * @return string
     */
    public static function to_name(FileType $type): string
    {
        return $type->value;
    }

    /**
     * @param string $file_name
     * @return FileType
     */
    public function get_file_type(string $file_name): FileType
    {
        # more specific first
        if ($this->is_code($file_name)) {
            return FileType::Code;
        }
        if ($this->is_archive($file_name)) {
            return FileType::Archive;
        }
        if ($this->is_audio($file_name)) {
            return FileType::Audio;
        }
        if ($this->is_font($file_name)) {
            return FileType::Font;
        }
        if ($this->is_image($file_name)) {
            return FileType::Image;
        }
        if ($this->is_video($file_name)) {
            return FileType::Video;
        }

        # more general last
        if ($this->is_xml($file_name)) {
            return FileType::Xml;
        }
        if ($this->is_text($file_name)) {
            return FileType::Text;
        }
        if ($this->is_binary($file_name)) {
            return FileType::Binary;
        }
        return FileType::Unknown;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_archive(string $file_name): bool
    {
        return in_array($file_name, $this->file_type_name_map['archive'])
            || in_array(FileUtil::get_extension($file_name), $this->file_type_ext_map['archive']);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_audio(string $file_name): bool
    {
        return in_array($file_name, $this->file_type_name_map['audio'])
            || in_array(FileUtil::get_extension($file_name), $this->file_type_ext_map['audio']);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_binary(string $file_name): bool
    {
        return in_array($file_name, $this->file_type_name_map['binary'])
            || in_array(FileUtil::get_extension($file_name), $this->file_type_ext_map['binary']);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_code(string $file_name): bool
    {
        return in_array($file_name, $this->file_type_name_map['code'])
            || in_array(FileUtil::get_extension($file_name), $this->file_type_ext_map['code']);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_font(string $file_name): bool
    {
        return in_array($file_name, $this->file_type_name_map['font'])
            || in_array(FileUtil::get_extension($file_name), $this->file_type_ext_map['font']);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_image(string $file_name): bool
    {
        return in_array($file_name, $this->file_type_name_map['image'])
            || in_array(FileUtil::get_extension($file_name), $this->file_type_ext_map['image']);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_text(string $file_name): bool
    {
        return in_array($file_name, $this->file_type_name_map['text'])
            || in_array(FileUtil::get_extension($file_name), $this->file_type_ext_map['text']);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_video(string $file_name): bool
    {
        return in_array($file_name, $this->file_type_name_map['video'])
            || in_array(FileUtil::get_extension($file_name), $this->file_type_ext_map['video']);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_xml(string $file_name): bool
    {
        return in_array($file_name, $this->file_type_name_map['xml'])
            || in_array(FileUtil::get_extension($file_name), $this->file_type_ext_map['xml']);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_unknown(string $file_name): bool
    {
        return $this->get_file_type($file_name) == FileType::Unknown;
    }
}
