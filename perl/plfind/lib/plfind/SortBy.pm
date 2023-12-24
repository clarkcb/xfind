###############################################################################
#
# SortBy.pm
#
# Enum of sort types
#
###############################################################################

package plfind::SortBy;

use strict;
use warnings;

use constant {
    FILEPATH => 'filepath',
    FILENAME => 'filename',
    FILESIZE => 'filesize',
    FILETYPE => 'filetype',
    LASTMOD => 'lastmod',
};

sub name_to_sort_by {
    my ($self, $name) = @_;
    my $lname = lc($name);
    if ($lname eq 'filename' || $lname eq 'name') {
        $self->{sort_by} = plfind::SortBy->FILENAME;
    } elsif ($lname eq 'filesize' || $lname eq 'size') {
        $self->{sort_by} = plfind::SortBy->FILESIZE;
    } elsif ($lname eq 'filetype' || $lname eq 'type') {
        $self->{sort_by} = plfind::SortBy->FILETYPE;
    } elsif ($lname eq 'lastmod') {
        return plfind::SortBy->LASTMOD;
    }
    return plfind::SortBy->FILEPATH;
}

1;

__END__
