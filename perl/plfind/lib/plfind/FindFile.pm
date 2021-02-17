###############################################################################
#
# FindOption.pm
#
# Encapsulates a file to be found
#
###############################################################################

package plfind::FindFile;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        containers => [],
        path => shift,
        filename => shift,
        filetype => shift,
    };
    bless $self, $class;
    return $self;
}

1;

__END__
