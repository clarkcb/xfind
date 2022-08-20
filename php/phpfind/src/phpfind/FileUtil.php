<?php declare(strict_types=1);

namespace phpfind;

/**
 * Class FileUtil
 */
class FileUtil
{
    public static array $DOT_PATHS = array('.', '..');

    /**
     * @param string $path
     * @return string
     */
    public static function expand_user_home_path(string $path): string
    {
        if (str_starts_with($path, '~')) {
            return str_replace('~', getenv('HOME'), $path);
        }
        return $path;
    }

    /**
     * @param string $filepath
     * @return string
     */
    public static function get_extension(string $filepath): string
    {
        $f = basename($filepath);
        $ext = '';
        $dot_idx = strrpos($f, '.');
        if ($dot_idx !== false && $dot_idx > 0 && $dot_idx < strlen($f)) {
            $ext = substr($f, $dot_idx+1);
        }
        return $ext;
    }

    /**
     * @param string $dir
     * @return bool
     */
    public static function is_dot_dir(string $dir): bool
    {
        return in_array($dir, self::$DOT_PATHS);
    }

    /**
     * @param string $filepath
     * @return bool
     */
    public static function is_hidden(string $filepath): bool
    {
        $f = basename($filepath);
        return strlen($f) > 1 && $f[0] ==='.' && !self::is_dot_dir($f);
    }

    /**
     * @param string $path
     * @param string $filename
     * @return string
     */
    public static function join_path(string $path, string $filename): string
    {
        return self::normalize_path($path) . self::get_separator($path) . $filename;
    }

    /**
     * @param string $path
     * @return string
     */
    public static function get_separator(string $path): string
    {
        $sep = '/';
        if (!str_contains($path, $sep)) {
            if (str_contains($path, '\\')) {
                return '\\';
            }
        }
        return $sep;
    }

    /**
     * @param string $path
     * @return string
     */
    public static function normalize_path(string $path): string
    {
        $sep = self::get_separator($path);
        return rtrim($path, $sep);
    }

    /**
     * @param string $path
     * @return array
     */
    public static function split_path(string $path): array
    {
        $sep = self::get_separator($path);
        if ($sep == '/') {
            $sep = '\\/';
        }
        return preg_split("/$sep/", $path);
    }

    /**
     * @param string $filepath
     * @return array
     */
    public static function split_to_path_and_filename(string $filepath): array
    {
        $elems = self::split_path($filepath);
        $filename = $elems[count($elems)-1];
        $filepath = '.';
        if (count($elems) > 1) {
            array_pop($elems);
            $filepath = implode(['/'], $elems);
        }
        return [$filepath, $filename];
    }
}
