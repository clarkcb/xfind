###############################################################################
#
# FileType.pm
#
# Enum of file types
#
###############################################################################

package plfind::FileType;

use strict;
use warnings;

use constant {
    UNKNOWN => 'unknown',
    ARCHIVE => 'archive',
    AUDIO   => 'audio',
    BINARY  => 'binary',
    CODE    => 'code',
    FONT    => 'font',
    IMAGE   => 'image',
    TEXT    => 'text',
    VIDEO   => 'video',
    XML     => 'xml',
};

sub from_name {
    my ($name) = @_;
    my $lname = lc($name);
    $lname =~ s/[^a-z]//g;
    if ($lname eq 'archive') {
        return plfind::FileType->ARCHIVE;
    }
    if ($lname eq 'audio') {
        return plfind::FileType->AUDIO;
    }
    if ($lname eq 'binary') {
        return plfind::FileType->BINARY;
    }
    if ($lname eq 'code') {
        return plfind::FileType->CODE;
    }
    if ($lname eq 'font') {
        return plfind::FileType->FONT;
    }
    if ($lname eq 'image') {
        return plfind::FileType->IMAGE;
    }
    if ($lname eq 'text') {
        return plfind::FileType->TEXT;
    }
    if ($lname eq 'video') {
        return plfind::FileType->VIDEO;
    }
    if ($lname eq 'xml') {
        return plfind::FileType->XML;
    }
    return plfind::FileType->UNKNOWN;
}

1;

__END__
