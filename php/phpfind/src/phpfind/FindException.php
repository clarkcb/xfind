<?php declare(strict_types=1);

namespace phpfind;

use Exception;

/**
 * Class FindException
 */
class FindException extends Exception
{
    /**
     * @param string $message
     */
    public function __construct(string $message)
    {
        parent::__construct($message);
    }
}
