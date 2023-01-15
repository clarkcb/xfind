<?php

namespace phpfind;

class StringUtil
{
    /**
     * @param bool $b
     * @return string
     */
    public static function bool_to_string(bool $b): string
    {
        return $b ? 'true' : 'false';
    }

    public static function datetime_to_string(?\DateTime $dt): string
    {
        if ($dt == null) {
            return '0';
        }
        return '"' . $dt->format('Y-m-d\TH:i:s.u\Z') . '"';
    }

    /**
     * @param array $arr
     * @return string
     */
    public static function string_array_to_string(array $arr): string
    {
        if (count($arr)) {
            return '["' . implode('", "', $arr) . '"]';
        }
        return '[]';
    }

    public static function filetype_array_to_string(array $arr): string
    {
        if (count($arr)) {
            $names = array_map(fn (FileType $ft) => FileTypes::to_name($ft), $arr);
            return '[' . implode(', ', $names) . ']';
        }
        return '[]';
    }
}
