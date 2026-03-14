###############################################################################
#
# FindSettings.pm
#
# Encapsulates the find settings
#
###############################################################################

package plfind::FindSettings;

use Path::Class;
use Scalar::Util qw(blessed reftype);

use plfind::common;
use plfind::Color;
use plfind::FileTypes;
use plfind::SortBy;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        archives_only => 0,
        colorize => 1,
        debug => 0,
        default_files => 1,
        dir_color => plfind::Color->CYAN,
        ext_color => plfind::Color->YELLOW,
        file_color => plfind::Color->MAGENTA,
        follow_symlinks => 0,
        in_archive_extensions => [],
        in_archive_file_patterns => [],
        in_dir_patterns => [],
        in_extensions => [],
        in_file_patterns => [],
        in_file_types => [],
        include_archives => 0,
        include_hidden => 0,
        max_depth => -1,
        max_last_mod => 0,
        max_size => 0,
        min_depth => -1,
        min_last_mod => 0,
        min_size => 0,
        out_archive_extensions => [],
        out_archive_file_patterns => [],
        out_dir_patterns => [],
        out_extensions => [],
        out_file_patterns => [],
        out_file_types => [],
        print_dirs => 0,
        print_files => 0,
        print_usage => 0,
        print_version => 0,
        recursive => 1,
        sort_case_insensitive => 0,
        sort_descending => 0,
        sort_by => plfind::SortBy->FILEPATH,
        paths => [],
        verbose => 0,
    };
    bless $self, $class;
    return $self;
}

sub set_property {
    my ($self, $name, $val) = @_;
    $self->{$name} = $val;
    if ($val eq 1) {
        if ($name eq 'archives_only') {
            $self->{include_archives} = 1;
        } elsif ($name eq 'debug') {
            $self->{verbose} = 1;
        }
    }
}

sub set_sort_by {
    my ($self, $name) = @_;
    $self->{sort_by} = plfind::SortBy::name_to_sort_by($name);
}

sub add_exts {
    my ($self, $exts, $extaref) = @_;
    my $xs = [];
    if (ref($exts) eq 'ARRAY') {
        $xs = $exts;
    } else { # treat as a string
        my @split = split(',', $exts);
        $xs = \@split;
    }
    foreach my $x (@$xs) {
        push(@$extaref, $x);
    }
}

sub add_file_types {
    my ($self, $file_types, $ftaref) = @_;
    my $fts = [];
    if (ref($file_types) eq 'ARRAY') {
        $fts = $file_types;
    } else { # treat as a string
        my @split = split(',', $file_types);
        $fts = \@split;
    }
    foreach my $ft (@$fts) {
        push(@$ftaref, plfind::FileType::from_name($ft));
    }
}

sub add_patterns {
    my ($self, $pats, $pataref) = @_;
    if (ref($pats) eq 'ARRAY') {
        foreach my $p (@$pats) {
            push (@$pataref, $p);
        }
    } else { # treat as a string
        push(@$pataref, $pats);
    }
}

sub add_path {
    my ($self, $path) = @_;
    if (-d $path) {
        push(@{$self->{paths}}, dir($path));
    } elsif (-f $path) {
        push(@{$self->{paths}}, file($path));
    } else {
        my $expanded = plfind::FileUtil::expand_path($path);
        if (-d $expanded) {
            push(@{$self->{paths}}, dir($path));
        } elsif (-f $expanded) {
            push(@{$self->{paths}}, file($path));
        }
    }
}

sub add_paths {
    my ($self, $paths) = @_;
    if (ref($paths) eq 'ARRAY') {
        foreach my $p (@$paths) {
            $self->add_path($p);
        }
    } else { # treat as a string
        $self->add_path($paths);
    }
}

sub needs_last_mod {
    my $self = shift;
    return $self->{sort_by} eq plfind::SortBy->LASTMOD ||
        blessed($self->{max_last_mod}) ||
        blessed($self->{min_last_mod});
}

sub needs_size {
    my $self = shift;
    return $self->{sort_by} eq plfind::SortBy->FILESIZE ||
           $self->{max_size} > 0 ||
           $self->{min_size} > 0;
}

sub needs_stat {
    my $self = shift;
    return $self->needs_last_mod || $self->needs_size;
}

sub to_string {
    my $self = shift;
    my @keys = keys(%$self);
    my @sorted_keys = sort @keys;
    my $props = [];
    foreach my $k (@sorted_keys) {
        if (ref($self->{$k}) eq 'ARRAY') {
            if ($k =~ /_file_types$/) {
                push(@$props, $k . '=' . plfind::common::file_types_aref_to_string($self->{$k}));
            } else {
                push(@$props, $k . '=' . plfind::common::strings_aref_to_string($self->{$k}));
            }
        } elsif ($k =~ /_color$/) {
            push(@$props, $k . '=' . $self->{$k});
        } elsif ($k =~ /_(depth|size)$/) {
            push(@$props, $k . '=' . $self->{$k});
        } elsif ($k =~ /_last_mod$/) {
            push(@$props, $k . '=' . plfind::common::datetime_to_string($self->{$k}));
        } elsif ($k =~ /^sort_by$/) {
            push(@$props, $k . '=' . $self->{$k});
        } else {
            push(@$props, $k . '=' . plfind::common::bool_to_string($self->{$k}));
        }
    }
    return 'FindSettingsZZ(' . join(', ', @$props) . ')';
}

1;

__END__
