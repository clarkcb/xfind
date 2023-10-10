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
        mime_type => shift,
        file_size => shift,
        last_mod => shift,
    };
    bless $self, $class;
    return $self;
}

sub to_string {
    my $self = shift @_;
    my $s = File::Spec->join($self->{path}, $self->{file_name});
    if ($self->{mime_type}) {
        $s .= ' (' . $self->{mime_type} . ')';
    }
    return $s;
}

1;

__END__
