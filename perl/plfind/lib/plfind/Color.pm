###############################################################################
#
# SortBy.pm
#
# Enum of sort types
#
###############################################################################

package plfind::Color;

use plfind::ConsoleColor;

use strict;
use warnings;

use constant {
    BLACK   => 'black',
    RED     => 'red',
    GREEN   => 'green',
    YELLOW  => 'yellow',
    BLUE    => 'blue',
    MAGENTA => 'magenta',
    CYAN    => 'cyan',
    WHITE   => 'white'
};

sub color_to_console_color {
    my ($color) = @_;
    return plfind::ConsoleColor::CONSOLE_BLACK if $color =~ /^black$/i;
    return plfind::ConsoleColor::CONSOLE_RED if $color =~ /^red$/i;
    return plfind::ConsoleColor::CONSOLE_GREEN if $color =~ /^green$/i;
    return plfind::ConsoleColor::CONSOLE_YELLOW if $color =~ /^yellow$/i;
    return plfind::ConsoleColor::CONSOLE_BLUE if $color =~ /^blue$/i;
    return plfind::ConsoleColor::CONSOLE_MAGENTA if $color =~ /^magenta$/i;
    return plfind::ConsoleColor::CONSOLE_CYAN if $color =~ /^cyan$/i;
    return plfind::ConsoleColor::CONSOLE_WHITE if $color =~ /^white$/i;
    return plfind::ConsoleColor::CONSOLE_BLACK;
}

1;

__END__
