###############################################################################
#
# FindConfig.pm
#
# Configuration values
#
###############################################################################

package plfind::FindConfig;

use strict;
use warnings;

use parent 'Exporter';

use Path::Class;

use plfind::FileUtil;

sub new {
    my $class = shift;

    my $xfind_config_dir;
    if (defined $ENV{XFIND_CONFIG_DIR}) {
        $xfind_config_dir = dir($ENV{'XFIND_CONFIG_DIR'})
    } else {
        $xfind_config_dir = dir($ENV{'HOME'}, '.config', 'xfind');
    }

    my $xfind_path;
    if (defined $ENV{XFIND_PATH}) {
        $xfind_path = dir($ENV{'XFIND_PATH'})
    } else {
        $xfind_path = dir($ENV{'HOME'}, 'src', 'xfind');
    }
    my $shared_path = $xfind_path->subdir('shared');
    my $file_types_path = $shared_path->file('filetypes.json');
    my $find_options_path = $shared_path->file('findoptions.json');
    my $default_find_settings_path = file($xfind_config_dir, 'settings.json');

    my $self = {
        xfind_path => $xfind_path,
        file_types_path => $file_types_path,
        find_options_path => $find_options_path,
        default_find_settings_path => $default_find_settings_path,
    };
    bless $self, $class;
    return $self;
}

1;

__END__
