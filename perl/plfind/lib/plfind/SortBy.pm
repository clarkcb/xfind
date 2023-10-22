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
    MIMETYPE => 'mimetype',
};

sub name_to_sort_by {
    my ($name) = @_;
    my $lname = lc($name);
    if ($lname eq 'filename' || $lname eq 'name') {
        return plfind::SortBy->FILENAME;
    }
    if ($lname eq 'filesize' || $lname eq 'size') {
        return plfind::SortBy->FILESIZE;
    }
    if ($lname eq 'filetype' || $lname eq 'type') {
        return plfind::SortBy->FILETYPE;
    }
    if ($lname eq 'lastmod') {
        return plfind::SortBy->LASTMOD;
    } elsif ($lname eq 'mimetype' || $lname eq 'mime') {
        $self->{sort_by} = plfind::SortBy->MIMETYPE;
    }
    return plfind::SortBy->FILEPATH;
}

1;

__END__
