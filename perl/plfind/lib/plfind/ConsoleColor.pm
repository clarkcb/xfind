###############################################################################
#
# ConsoleColor.pm
#
# Enum of console color values
#
###############################################################################

package plfind::ConsoleColor;

use strict;
use warnings;

use constant {
    CONSOLE_RESET        => "\033[0m",
    CONSOLE_BLACK        => "\033[0;30m",
    CONSOLE_RED          => "\033[0;31m",
    CONSOLE_GREEN        => "\033[0;32m",
    CONSOLE_YELLOW       => "\033[0;33m",
    CONSOLE_BLUE         => "\033[0;34m",
    CONSOLE_MAGENTA      => "\033[0;35m",
    CONSOLE_CYAN         => "\033[0;36m",
    CONSOLE_WHITE        => "\033[0;37m",
    CONSOLE_BOLD_BLACK   => "\033[1;30m",
    CONSOLE_BOLD_RED     => "\033[1;31m",
    CONSOLE_BOLD_GREEN   => "\033[1;32m",
    CONSOLE_BOLD_YELLOW  => "\033[1;33m",
    CONSOLE_BOLD_BLUE    => "\033[1;34m",
    CONSOLE_BOLD_MAGENTA => "\033[1;35m",
    CONSOLE_BOLD_CYAN    => "\033[1;36m",
    CONSOLE_BOLD_WHITE   => "\033[1;37m",
};

1;

__END__
