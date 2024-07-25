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

use Cwd qw(abs_path);
use File::Basename;
use File::Spec;
use JSON::PP qw(decode_json);

use plfind::FileUtil;

our $XFINDPATH = $ENV{'XFIND_PATH'};
if (!$XFINDPATH) {
    $XFINDPATH = join('/', @{[$ENV{'HOME'}, 'src', 'xfind']});
}
our $SHAREDPATH = "$XFINDPATH/shared";
our $FILETYPESPATH = "$SHAREDPATH/filetypes.json";
our $FINDOPTIONSPATH = "$SHAREDPATH/findoptions.json";
our $XFINDDB = "$SHAREDPATH/xfind.db";

our @EXPORT = qw($XFINDPATH $SHAREDPATH $FILETYPESPATH $FINDOPTIONSPATH $XFINDDB);

1;

__END__
