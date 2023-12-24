###############################################################################
#
# FindSettings.pm
#
# Encapsulates the find settings
#
###############################################################################

package plfind::FindSettings;

use Scalar::Util qw(blessed reftype);

use plfind::common;
use plfind::FileTypes;
use plfind::SortBy;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        archives_only => 0,
        debug => 0,
        in_archive_extensions => [],
        in_archive_file_patterns => [],
        in_dir_patterns => [],
        in_extensions => [],
        in_file_patterns => [],
        in_file_types => [],
        include_archives => 0,
        include_hidden => 0,
        list_dirs => 0,
        list_files => 0,
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
    if ($val == 1) {
        if ($name eq 'archives_only') {
            $self->{include_archives} = 1;
        } elsif ($name eq 'debug') {
            $self->{verbose} = 1;
        }
    }
}

sub set_sort_by {
    my ($self, $name) = @_;
    $self->{sort_by} = plfind::SortBy->name_to_sort_by($name);
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
    foreach my $x (@{$xs}) {
        push(@{$extaref}, $x);
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
    foreach my $ft (@{$fts}) {
        push(@{$ftaref}, plfind::FileTypes::from_name($ft));
    }
}

sub add_patterns {
    my ($self, $pats, $pataref) = @_;
    if (ref($pats) eq 'ARRAY') {
        foreach my $p (@{$pats}) {
            push (@{$pataref}, $p);
        }
    } else { # treat as a string
        push(@{$pataref}, $pats);
    }
}

sub needs_stat {
    my $self = shift;
    return $self->{sort_by} eq plfind::SortBy->FILESIZE ||
           $self->{sort_by} eq plfind::SortBy->LASTMOD ||
           blessed($self->{max_last_mod}) ||
           blessed($self->{min_last_mod}) ||
           $self->{max_size} > 0 ||
           $self->{min_size} > 0;
}

sub to_string {
    my $self = shift @_;
    print "FindSettings\n";
    my $s = "FindSettings(" .
        'archives_only=' . plfind::common::bool_to_string($self->{archives_only}) .
        ', debug=' . plfind::common::bool_to_string($self->{debug}) .
        ', exclude_hidden=' . plfind::common::bool_to_string($self->{exclude_hidden}) .
        ', in_archive_extensions=' . plfind::common::strings_aref_to_string($self->{in_archive_extensions}) .
        ', in_archive_file_patterns=' . plfind::common::strings_aref_to_string($self->{in_archive_file_patterns}) .
        ', in_dir_patterns=' . plfind::common::strings_aref_to_string($self->{in_dir_patterns}) .
        ', in_extensions=' . plfind::common::strings_aref_to_string($self->{in_extensions}) .
        ', in_file_patterns=' . plfind::common::strings_aref_to_string($self->{in_file_patterns}) .
        ', in_file_types=' . plfind::common::file_types_aref_to_string($self->{in_file_types}) .
        ', include_archives=' . plfind::common::bool_to_string($self->{include_archives}) .
        ', list_dirs=' . plfind::common::bool_to_string($self->{list_dirs}) .
        ', list_files=' . plfind::common::bool_to_string($self->{list_files}) .
        ', max_depth=' . $self->{max_depth} .
        ', max_last_mod=' . plfind::common::datetime_to_string($self->{max_last_mod}) .
        ', max_size=' . $self->{max_size} .
        ', min_depth=' . $self->{min_depth} .
        ', min_last_mod=' . plfind::common::datetime_to_string($self->{min_last_mod}) .
        ', min_size=' . $self->{min_size} .
        ', out_archive_extensions=' . plfind::common::strings_aref_to_string($self->{out_archive_extensions}) .
        ', out_archive_file_patterns=' . plfind::common::strings_aref_to_string($self->{out_archive_file_patterns}) .
        ', out_dir_patterns=' . plfind::common::strings_aref_to_string($self->{out_dir_patterns}) .
        ', out_extensions=' . plfind::common::strings_aref_to_string($self->{out_extensions}) .
        ', out_file_patterns=' . plfind::common::strings_aref_to_string($self->{out_file_patterns}) .
        ', out_file_types=' . plfind::common::file_types_aref_to_string($self->{out_file_types}) .
        ', paths=' . plfind::common::strings_aref_to_string($self->{paths}) .
        ', print_usage=' . plfind::common::bool_to_string($self->{print_usage}) .
        ', print_version=' . plfind::common::bool_to_string($self->{print_version}) .
        ', recursive=' . plfind::common::bool_to_string($self->{recursive}) .
        ', sort_by=' . $self->{sort_by} .
        ', sort_case_insensitive=' . plfind::common::bool_to_string($self->{sort_case_insensitive}) .
        ', sort_descending=' . plfind::common::bool_to_string($self->{sort_descending}) .
        ', verbose=' . plfind::common::bool_to_string($self->{verbose}) .
        ')';
    return $s;
}

1;

__END__
