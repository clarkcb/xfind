<?php declare(strict_types=1);

namespace phpfind;

/**
 * Class FindOption
 *
 * @property string shortarg
 * @property string longarg
 * @property string desc
 * @property string sortarg
 */
readonly class FindOption
{
    public string $shortarg;
    public string $longarg;
    public string $desc;
    public string $sortarg;

    /**
     * @param string $shortarg
     * @param string $longarg
     * @param string $desc
     */
    public function __construct(string $shortarg, string $longarg, string $desc)
    {
        $this->shortarg = $shortarg;
        $this->longarg = $longarg;
        $this->desc = $desc;
        $this->sortarg = $this->__sortarg();
    }

    /**
     * @return string
     */
    private function __sortarg(): string
    {
        if ($this->shortarg) {
            return strtolower($this->shortarg) . 'a' . $this->longarg;
        }
        return $this->longarg;
    }
}
