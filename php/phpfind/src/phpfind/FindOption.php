<?php declare(strict_types=1);

namespace phpfind;

/**
 * Class FindOption
 *
 * @property string shortarg
 * @property string longarg
 * @property string desc
 * @property callable func
 * @property string sortarg
 */
class FindOption
{
    /**
     * @param string $shortarg
     * @param string $longarg
     * @param string $desc
     * @param $func
     */
    public function __construct(string $shortarg, string $longarg, string $desc, $func)
    {
        $this->shortarg = $shortarg;
        $this->longarg = $longarg;
        $this->desc = $desc;
        $this->func = $func;
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
