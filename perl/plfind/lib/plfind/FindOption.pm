###############################################################################
#
# FindOption.pm
#
# Encapsulates a find option
#
###############################################################################

package plfind::FindOption;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        short_arg => shift,
        long_arg => shift,
        desc => shift,
        func => shift,
    };
    bless $self, $class;
    return $self;
}

1;

__END__
