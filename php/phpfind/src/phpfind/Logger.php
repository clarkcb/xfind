<?php


namespace phpfind;


class Logger
{
    /**
     * @param string $msg
     * @return void
     */
    public static function log_msg(string $msg): void
    {
        echo "$msg\n";
    }
}
