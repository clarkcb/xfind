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

my $CONTAINER_SEP = '!';

sub new {
    # file_path is a Path::Class instance
    my $class = shift;
    my $self = {
        containers => [],
        file_path => shift,
        file_type => shift,
        file_size => shift,
        last_mod => shift,
    };
    bless $self, $class;
    return $self;
}

sub to_string {
    my $self = shift @_;
    my $s = '';
    if (scalar @{$self->{containers}}) {
        $s = join($CONTAINER_SEP, @{$self->{containers}});
        $s .= $CONTAINER_SEP;
    }
    $s .=  $self->{file_path}->stringify;
    return $s;
}

1;

__END__
