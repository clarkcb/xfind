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
use Path::Class;
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
        return $file_path;
    }
    if (substr($file_path, 0, 1) eq '~') {
        my $user_path = dir($ENV{HOME});
        if ($file_path eq "~" || $file_path eq "~/" || $file_path eq "~\\") {
            return $user_path;
        }
        if (substr($file_path, 1, 1) eq '/' || substr($file_path, 1, 1) eq '\\') {
            return $ENV{HOME} . substr($file_path, 1);
        }
        my $home_path = $user_path->parent;
        return join_paths(@{[$home_path->stringify, substr($file_path, 1)]});
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

sub is_hidden_name {
    my ($file_name) = @_;
    if (length($file_name) > 1 && substr($file_name, 0, 1) eq '.' && !is_dot_dir($file_name)) {
        return 1;
    }
    return 0;
}

sub is_hidden_path {
    # $path is a Path::Class instance
    my ($path) = @_;
    # split into parent and file_name since they will work for both Dir and File
    my $parent = $path->parent;
    my $file_name = $path->basename;
    my @path_elems = grep {$_ ne ''} $parent->dir_list;
    foreach my $p (@path_elems) {
        if (is_hidden_name($p)) {
            return 1;
        }
    }
    if (is_hidden_name($file_name)) {
        return 1;
    }
    return 0;
}

sub join_paths {
    my @paths = @_;
    if (scalar @paths == 0) {
        return '';
    }
    for (my $i = 0; $i < scalar @paths; $i++) {
        $paths[$i] =~ s|/$||;
    }
    return join('/', @paths);
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
