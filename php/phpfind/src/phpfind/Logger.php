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
    public static function log_err(string $msg): void
    {
        fwrite(STDERR, "ERROR: $msg\n");
        flush();
    }
}
