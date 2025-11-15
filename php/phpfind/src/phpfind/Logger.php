<?php

declare(strict_types=1);

namespace phpfind;


class Logger
{
    /**
     * @param string $msg
     * @return void
     */
    public static function log_msg(string $msg): void
    {
        fwrite(STDOUT, "$msg\n");
        flush();
    }

    /**
     * @param string $msg
     * @return void
     */
    public static function log_err(string $msg, bool $colorize = true): void
    {
        $err = "ERROR: $msg";
        if ($colorize) {
            $err = ConsoleColor::BoldRed->value . $err . ConsoleColor::Reset->value;
        }
        fwrite(STDERR, "$err\n");
        flush();
    }
}
