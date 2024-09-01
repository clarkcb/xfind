<?php

declare(strict_types=1);

namespace phpfind;

//require_once __DIR__ . '/../autoload.php';
use SQLite3;
use SQLite3Stmt;


/**
 * Class FileTypes
 *
 * @package phpfind
 * @property SQLite3 $db
 * @property FileType[] $file_types
 * @property array<string, FileType> $ext_type_cache
 */
class FileTypes
{
    private readonly SQLite3 $db;
    /**
     * @var FileType[] $file_types
     */
    private readonly array $file_types;
    /**
     * @var array<string, FileType> $ext_type_cache
     */
    private array $ext_type_cache = [];
    private array $name_type_cache = [];

    /**
     * @throws FindException
     */
    public function __construct()
    {
        $this->db = new \SQLite3(Config::XFIND_DB, SQLITE3_OPEN_READONLY);
        $this->file_types = [
            FileType::Unknown,
            FileType::Archive,
            FileType::Audio,
            FileType::Binary,
            FileType::Code,
            FileType::Font,
            FileType::Image,
            FileType::Text,
            FileType::Video,
            FileType::Xml
        ];
        $this->load_name_file_type_cache();
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

    private function load_name_file_type_cache(): void {
        $query = 'SELECT name, file_type_id FROM file_name';
        if ($statement = $this->db->prepare($query)) {
            if ($db_results = $statement->execute()) {
                while ($row = $db_results->fetchArray(SQLITE3_ASSOC)) {
                    $name = $row['name'];
                    $file_type_id = $row['file_type_id'] - 1;
                    $this->name_type_cache[$name] = $this->file_types[$file_type_id];
                }
            }
        }
    }

    private function get_file_type_for_statement(SQLite3Stmt $statement): FileType
    {
        if ($db_result = $statement->execute()) {
            $row = $db_result->fetchArray(SQLITE3_ASSOC);
            if ($row) {
                $file_type_id = $row['file_type_id'] - 1;
                return $this->file_types[$file_type_id];
            }
        }
        return FileType::Unknown;
    }

    /**
     * @param string $file_name
     * @return FileType
     */
    public function get_file_type_for_file_name(string $file_name): FileType
    {
        if (array_key_exists($file_name, $this->name_type_cache)) {
            return $this->name_type_cache[$file_name];
        }
//        $query = 'SELECT file_type_id FROM file_name WHERE name = :name;';
//        if ($statement = $this->db->prepare($query)) {
//            $statement->bindValue(':name', $file_name);
//            return $this->get_file_type_for_statement($statement);
//        }
        return FileType::Unknown;
    }

    /**
     * @param string $file_ext
     * @return FileType
     */
    public function get_file_type_for_extension(string $file_ext): FileType
    {
        if ($file_ext == null || $file_ext == '') {
            return FileType::Unknown;
        }
        if (array_key_exists($file_ext, $this->ext_type_cache)) {
            return $this->ext_type_cache[$file_ext];
        }
        $query = 'SELECT file_type_id FROM file_extension WHERE extension = :ext;';
        if ($statement = $this->db->prepare($query)) {
            $statement->bindValue(':ext', $file_ext);
            $file_type = $this->get_file_type_for_statement($statement);
            $this->ext_type_cache[$file_ext] = $file_type;
            return $file_type;
        }
        return FileType::Unknown;
    }

    /**
     * @param string $file_name
     * @return FileType
     */
    public function get_file_type(string $file_name): FileType
    {
        $file_type_for_file_name = $this->get_file_type_for_file_name($file_name);
        if ($file_type_for_file_name !== FileType::Unknown) {
            return $file_type_for_file_name;
        }
        return $this->get_file_type_for_extension(FileUtil::get_extension($file_name));
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_archive(string $file_name): bool
    {
        return $this->get_file_type($file_name) === FileType::Archive;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_audio(string $file_name): bool
    {
        return $this->get_file_type($file_name) === FileType::Audio;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_binary(string $file_name): bool
    {
        return $this->get_file_type($file_name) === FileType::Binary;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_code(string $file_name): bool
    {
        return $this->get_file_type($file_name) === FileType::Code;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_font(string $file_name): bool
    {
        return $this->get_file_type($file_name) === FileType::Font;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_image(string $file_name): bool
    {
        return $this->get_file_type($file_name) === FileType::Image;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_text(string $file_name): bool
    {
        $file_type = $this->get_file_type($file_name);
        return $file_type === FileType::Text
            || $file_type === FileType::Code
            || $file_type === FileType::Xml;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_video(string $file_name): bool
    {
        return $this->get_file_type($file_name) === FileType::Video;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_xml(string $file_name): bool
    {
        return $this->get_file_type($file_name) === FileType::Xml;
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
