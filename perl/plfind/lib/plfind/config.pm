###############################################################################
#
# config.pm
#
# Configuration values
#
###############################################################################

package plfind::config;

use strict;
use warnings;

use parent 'Exporter';

use Path::Class;

use plfind::FileUtil;

our $XFIND_PATH;
if (defined $ENV{XFIND_PATH}) {
    $XFIND_PATH = dir($ENV{'XFIND_PATH'})
} else {
    $XFIND_PATH = dir($ENV{'HOME'}, 'src', 'xfind');
}
our $SHARED_PATH = $XFIND_PATH->subdir('shared');
our $FILE_TYPES_PATH = $SHARED_PATH->file('filetypes.json');
our $FIND_OPTIONS_PATH = $SHARED_PATH->file('findoptions.json');
our $XFIND_DB = $SHARED_PATH->file('xfind.db');

our @EXPORT = qw($XFIND_PATH $SHARED_PATH $FILE_TYPES_PATH $FIND_OPTIONS_PATH $XFIND_DB);

1;

__END__
