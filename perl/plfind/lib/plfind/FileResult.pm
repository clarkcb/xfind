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
        file_name => shift,
        file_type => shift,
        stat => shift,
    };
    bless $self, $class;
    return $self;
}

sub to_string {
    my $self = shift @_;
    my $s = File::Spec->join($self->{path}, $self->{file_name});
    return $s;
}

1;

__END__
