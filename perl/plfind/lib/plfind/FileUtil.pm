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

use Data::Dumper;
use File::Basename;
use File::Spec;
use plfind::common;

my @DOT_DIRS = ('.', '..');

sub is_dot_dir {
    my ($dir) = @_;
    if (grep {$_ eq $dir} @DOT_DIRS) {
        return 1;
    }
    return 0;
}

sub get_extension {
    my ($file) = @_;
    my $f = basename($file);
    my $idx = rindex($f, '.');
    if ($idx > 0 && $idx < length($f) - 1) {
        return lc(substr($f, $idx+1));
    }
    return '';
}

sub is_hidden {
    my ($file) = @_;
    my $f = basename($file);
    if (length($f) > 1 && substr($f, 0, 1) eq '.' && !is_dot_dir($f)) {
        return 1;
    }
    return 0;
}

sub join_path {
    my ($dir, $file_name) = @_;
    return File::Spec->catfile($dir, $file_name);
}

sub split_path {
    my ($file_path) = @_;
    my ($vol, $dir, $file_name) = File::Spec->splitpath($file_path);
    if (!$dir) {
        $dir = '.';
    } elsif ($dir =~ m|[/\\]$|) {
        $dir =~ s|[/\\]$||;
    }
    return ($dir, $file_name);
}

sub split_dir {
    my ($file_path) = @_;
    return File::Spec->splitdir($file_path);
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
