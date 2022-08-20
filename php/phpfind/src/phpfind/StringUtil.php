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
}
