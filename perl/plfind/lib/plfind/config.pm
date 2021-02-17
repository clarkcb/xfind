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

my $absdir = dirname(abs_path(__FILE__));
my $share_path = File::Spec->join($absdir, '../../share');
my $config_json_path = $share_path . '/config.json';
my $config = decode_json plfind::FileUtil::get_file_contents($config_json_path);

our $XFINDPATH = $config->{xfindpath};
our $SHAREDPATH = "$XFINDPATH/shared";
our $FILETYPESPATH = "$share_path/filetypes.json";
our $FINDOPTIONSPATH = "$share_path/findoptions.json";

our @EXPORT = qw($XFINDPATH $SHAREDPATH $FILETYPESPATH $FINDOPTIONSPATH);

1;

__END__
