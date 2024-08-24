<?php

declare(strict_types=1);

namespace phpfind;

/**
 * Class FileUtil
 */
class FileUtil
{
    /**
     * @var string[] $DOT_PATHS
     */
    public static array $DOT_PATHS = array('.', '..');

    /**
     * @param string $path
     * @return string
     */
    public static function expand_user_home_path(string $path): string
    {
        if (str_starts_with($path, '~')) {
            $home = getenv('HOME');
            if ($home !== false) {
                if ($path == '~') {
                    return $home;
                }
                $tilde_prefix = '~' . DIRECTORY_SEPARATOR;
                if (str_starts_with($path, $tilde_prefix)) {
                    return str_replace('~', $home, $path);
                }
                // TODO: if path begins with ~user, we need to find the named user's home path
            }
        }
        return $path;
    }

    /**
     * @param string $file_path
     * @return string
     */
    public static function get_extension(string $file_path): string
    {
        $f = basename($file_path);
        $ext = '';
        $dot_idx = strrpos($f, '.');
        if ($dot_idx !== false && $dot_idx > 0 && $dot_idx < strlen($f)) {
            $ext = substr($f, $dot_idx+1);
            if ($ext != 'Z') {
                $ext = strtolower($ext);
            }
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
     * @param string $file_path
     * @return bool
     */
    public static function is_hidden(string $file_path): bool
    {
        $f = basename($file_path);
        return strlen($f) > 1 && $f[0] ==='.' && !self::is_dot_dir($f);
    }

    /**
     * @param string $path
     * @param string ...$elems
     * @return string
     */
    public static function join_paths(string $path, string ...$elems): string
    {
        $elems = array_map(fn($elem) => self::normalize_path_elem($elem), $elems);
        return self::normalize_path($path) . DIRECTORY_SEPARATOR .
            implode(DIRECTORY_SEPARATOR, $elems);
    }

    /**
     * @param string $path
     * @return string
     */
    public static function normalize_path(string $path): string
    {
        return rtrim($path, DIRECTORY_SEPARATOR);
    }

    /**
     * @param string $path
     * @return string
     */
    public static function normalize_path_elem(string $path): string
    {
        return trim($path, DIRECTORY_SEPARATOR);
    }

    /**
     * @param string $path
     * @return string[]
     */
    public static function split_path(string $path): array
    {
        return explode(DIRECTORY_SEPARATOR, $path);
    }

    /**
     * @param string $file_path
     * @return string[]
     */
    public static function split_to_path_and_file_name(string $file_path): array
    {
        $dir_name = pathinfo($file_path, PATHINFO_DIRNAME);
        $file_name = pathinfo($file_path, PATHINFO_BASENAME);
        if (!$dir_name) $dir_name = '.';
        return [$dir_name, $file_name];
    }
}
