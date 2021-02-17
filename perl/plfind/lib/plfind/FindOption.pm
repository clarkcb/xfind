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
        shortarg => shift,
        longarg => shift,
        desc => shift,
        func => shift,
    };
    bless $self, $class;
    return $self;
}

1;

__END__
