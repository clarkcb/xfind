###############################################################################
#
# FindSettings.pm
#
# Encapsulates the find settings
#
###############################################################################

package plfind::FindSettings;

use Scalar::Util qw(blessed);

use plfind::FileTypes;
use plfind::SortBy;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        archives_only => 0,
        debug => 0,
        exclude_hidden => 1,
        in_archive_extensions => [],
        in_archive_file_patterns => [],
        in_dir_patterns => [],
        in_extensions => [],
        in_file_patterns => [],
        in_file_types => [],
        include_archives => 0,
        list_dirs => 0,
        list_files => 0,
        max_last_mod => 0,
        max_size => 0,
        min_last_mod => 0,
        min_size => 0,
        out_archive_extensions => [],
        out_archive_patterns => [],
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

sub bool_to_string {
    my ($self, $bool) = @_;
    return $bool ? 'true' : 'false';
}

sub datetime_to_string {
    my ($self, $dt) = @_;
    if (blessed($dt)) {
        return '"' . $dt . '"';
    }
    return '0';
}

sub aref_to_string {
    my ($self, $aref, $quoted) = @_;
    my $s = '[';
    if (@$aref) {
        foreach my $i (0..$#{$aref}) {
            if ($i > 0) {
                $s .= ', ';
            }
            if ($quoted) {
                $s .= '"' . $aref->[$i] . '"';
            } else {
                $s .= $aref->[$i];
            }
        }
    }
    $s .= ']';
    return $s;
}

sub strings_aref_to_string {
    my ($self, $aref) = @_;
    return $self->aref_to_string($aref, 1);
}

sub file_types_aref_to_string {
    my ($self, $aref) = @_;
    return $self->aref_to_string($aref, 0);
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
    my $uname = uc($name);
    if ($uname eq 'NAME') {
        $self->{sort_by} = plfind::SortBy->FILENAME;
    } elsif ($uname eq 'SIZE') {
        $self->{sort_by} = plfind::SortBy->FILESIZE;
    } elsif ($uname eq 'TYPE') {
        $self->{sort_by} = plfind::SortBy->FILETYPE;
    } elsif ($uname eq 'LASTMOD') {
        $self->{sort_by} = plfind::SortBy->LASTMOD;
    } else {
        $self->{sort_by} = plfind::SortBy->FILEPATH;
    }
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
    my $s = "FindSettings(" .
        'archives_only=' . $self->bool_to_string($self->{archives_only}) .
        ', debug=' . $self->bool_to_string($self->{debug}) .
        ', exclude_hidden=' . $self->bool_to_string($self->{exclude_hidden}) .
        ', in_archive_extensions=' . $self->strings_aref_to_string($self->{in_archive_extensions}) .
        ', in_archive_file_patterns=' . $self->strings_aref_to_string($self->{in_archive_file_patterns}) .
        ', in_dir_patterns=' . $self->strings_aref_to_string($self->{in_dir_patterns}) .
        ', in_extensions=' . $self->strings_aref_to_string($self->{in_extensions}) .
        ', in_file_patterns=' . $self->strings_aref_to_string($self->{in_file_patterns}) .
        ', in_file_types=' . $self->file_types_aref_to_string($self->{in_file_types}) .
        ', include_archives=' . $self->bool_to_string($self->{include_archives}) .
        ', list_dirs=' . $self->bool_to_string($self->{list_dirs}) .
        ', list_files=' . $self->bool_to_string($self->{list_files}) .
        ', max_last_mod=' . $self->datetime_to_string($self->{max_last_mod}) .
        ', max_size=' . $self->{max_size} .
        ', min_last_mod=' . $self->datetime_to_string($self->{min_last_mod}) .
        ', min_size=' . $self->{min_size} .
        ', out_archive_extensions=' . $self->strings_aref_to_string($self->{out_archive_extensions}) .
        ', out_archive__patterns=' . $self->strings_aref_to_string($self->{out_archive_patterns}) .
        ', out_dir_patterns=' . $self->strings_aref_to_string($self->{out_dir_patterns}) .
        ', out_extensions=' . $self->strings_aref_to_string($self->{out_extensions}) .
        ', out_file_patterns=' . $self->strings_aref_to_string($self->{out_file_patterns}) .
        ', out_file_types=' . $self->file_types_aref_to_string($self->{out_file_types}) .
        ', print_usage=' . $self->bool_to_string($self->{print_usage}) .
        ', print_version=' . $self->bool_to_string($self->{print_version}) .
        ', recursive=' . $self->bool_to_string($self->{recursive}) .
        ', paths=' . $self->strings_aref_to_string($self->{paths}) .
        ', sort_case_insensitive=' . $self->bool_to_string($self->{sort_case_insensitive}) .
        ', sort_descending=' . $self->bool_to_string($self->{sort_descending}) .
        ', sort_by=' . $self->{sort_by} .
        ', verbose=' . $self->bool_to_string($self->{verbose}) .
        ')';
    return $s;
}

1;

__END__
