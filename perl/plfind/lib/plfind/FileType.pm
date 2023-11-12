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
    UNKNOWN => 'Unknown',
    ARCHIVE => 'Archive',
    AUDIO   => 'Audio',
    BINARY  => 'Binary',
    CODE    => 'Code',
    FONT    => 'Font',
    IMAGE   => 'Image',
    TEXT    => 'Text',
    VIDEO   => 'Video',
    XML     => 'Xml',
};

1;

__END__
