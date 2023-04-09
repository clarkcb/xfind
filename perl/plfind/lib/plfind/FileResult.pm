###############################################################################
#
# FileResult.pm
#
# Encapsulates a found file
#
###############################################################################

package plfind::FileResult;

use strict;
use warnings;

use File::Spec;

sub new {
    my $class = shift;
    my $self = {
        containers => [],
        path => shift,
        filename => shift,
        filetype => shift,
        stat => shift,
    };
    bless $self, $class;
    return $self;
}

sub to_string {
    my $self = shift @_;
    my $s = File::Spec->join($self->{path}, $self->{filename});
    return $s;
}

1;

__END__
