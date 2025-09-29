###############################################################################
#
# ArgToken.pm
#
# Encapsulates an arg token
#
###############################################################################

package plfind::ArgToken;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        name => shift,
        type => shift,
        value => shift,
    };
    bless $self, $class;
    return $self;
}

1;

__END__
