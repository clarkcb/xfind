<?php

declare(strict_types=1);

namespace phpfind;

/**
 * Class FindOption
 */
readonly class FindOption
{
    public string $short_arg;
    public string $long_arg;
    public string $desc;
    public string $sort_arg;

    /**
     * @param string $short_arg
     * @param string $long_arg
     * @param string $desc
     */
    public function __construct(string $short_arg, string $long_arg, string $desc)
    {
        $this->short_arg = $short_arg;
        $this->long_arg = $long_arg;
        $this->desc = $desc;
        $this->sort_arg = $this->__sort_arg();
    }

    /**
     * @return string
     */
    private function __sort_arg(): string
    {
        if ($this->short_arg) {
            return strtolower($this->short_arg) . 'a' . $this->long_arg;
        }
        return $this->long_arg;
    }
}
