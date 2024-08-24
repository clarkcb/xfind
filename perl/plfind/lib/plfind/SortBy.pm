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
    my ($name) = @_;
    my $uname = uc($name);
    $uname =~ s/[^A-Z]//g;
    if ($uname eq 'FILENAME' || $uname eq 'NAME') {
        return plfind::SortBy->FILENAME;
    }
    if ($uname eq 'FILESIZE' || $uname eq 'SIZE') {
        return plfind::SortBy->FILESIZE;
    }
    if ($uname eq 'FILETYPE' || $uname eq 'TYPE') {
        return plfind::SortBy->FILETYPE;
    }
    if ($uname eq 'LASTMOD') {
        return plfind::SortBy->LASTMOD;
    }
    return plfind::SortBy->FILEPATH;
}

1;

__END__
