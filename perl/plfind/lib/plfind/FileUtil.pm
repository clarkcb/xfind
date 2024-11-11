###############################################################################
#
# FileUtil.pm
#
# Helper class for files
#
###############################################################################

package plfind::FileUtil;

use strict;
use warnings;

# use Data::Dumper;
use plfind::common;

my @DOT_DIRS = ('.', '..');

sub is_dot_dir {
    my ($dir) = @_;
    if (grep {$_ eq $dir} @DOT_DIRS) {
        return 1;
    }
    return 0;
}

sub expand_path {
    my ($file_path) = @_;
    if (!defined $file_path || chomp($file_path) eq '') {
        return '';
    }
    if (substr($file_path, 0, 1) eq '~') {
        if (substr($file_path, 1, 1) eq '/' || substr($file_path, 1, 1) eq '\\') {
            return $ENV{HOME} . substr($file_path, 1);
        }
    }
    return $file_path;
}

sub get_extension {
    my ($file_name) = @_;
    my $idx = rindex($file_name, '.');
    if ($idx > 0 && $idx < length($file_name) - 1) {
        return lc(substr($file_name, $idx+1));
    }
    return '';
}

sub get_extension_for_path {
    # $path is a Path::Class instance
    my ($path) = @_;
    return get_extension($path->basename);
}

sub is_hidden {
    my ($file_name) = @_;
    if (length($file_name) > 1 && substr($file_name, 0, 1) eq '.' && !is_dot_dir($file_name)) {
        return 1;
    }
    return 0;
}

sub is_hidden_path {
    # $path is a Path::Class instance
    my ($path) = @_;
    return is_hidden($path->basename);
}

sub get_file_handle {
    my ($file) = @_;
    open(FILE, "<$file") || die "File I/O error: $file: $!\n";
    return \*FILE;
}

sub get_file_contents {
    my ($file) = @_;
    my $fh = get_file_handle($file);
    local $/;
    my $delim = undef $/;
    my $contents = <$fh>;
    close($fh);
    $/ = $delim;
    return $contents;
}

sub get_file_lines {
    my ($file) = @_;
    my $fh = get_file_handle($file);
    my @lines = <$fh>;
    close($fh);
    return \@lines;
}

1;

__END__
