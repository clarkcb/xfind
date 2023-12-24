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

1;

__END__
