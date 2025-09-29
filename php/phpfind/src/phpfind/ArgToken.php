<?php

declare(strict_types=1);

namespace phpfind;

/**
 * Class ArgToken
 */
readonly class ArgToken
{
    public string $name;
    public ArgTokenType $type;
    public mixed $value;

    /**
     * @param string $name
     * @param ArgTokenType $type
     * @param mixed $value
     */
    public function __construct(string $name, ArgTokenType $type, mixed $value)
    {
        $this->name = $name;
        $this->type = $type;
        $this->value = $value;
    }
}
