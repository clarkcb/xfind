###############################################################################
#
# FileResultSorter.pm
#
# Sorts file results
#
###############################################################################

package plfind::FileResultSorter;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        settings => shift,
    };
    bless $self, $class;
    return $self;
}

sub cmp_file_results_by_path {
    my ($self, $fr1, $fr2) = @_;
    my ($path1, $path2) = $self->{settings}->{sort_case_insensitive} ?
        (lc($fr1->{file_path}->parent), lc($fr2->{file_path}->parent)) :
        ($fr1->{file_path}->parent, $fr2->{file_path}->parent);
    if ($path1 eq $path2) {
        my ($file_name1, $file_name2) = $self->{settings}->{sort_case_insensitive} ?
            (lc($fr1->{file_path}->basename), lc($fr2->{file_path}->basename)) :
            ($fr1->{file_path}->basename, $fr2->{file_path}->basename);
        return $file_name1 cmp $file_name2;
    }
    return $path1 cmp $path2;
}

sub cmp_file_results_by_file_name {
    my ($self, $fr1, $fr2) = @_;
    my ($file_name1, $file_name2) = $self->{settings}->{sort_case_insensitive} ?
        (lc($fr1->{file_path}->basename), lc($fr2->{file_path}->basename)) :
        ($fr1->{file_path}->basename, $fr2->{file_path}->basename);
    if ($file_name1 eq $file_name2) {
        my ($path1, $path2) = $self->{settings}->{sort_case_insensitive} ?
            (lc($fr1->{file_path}->parent), lc($fr2->{file_path}->parent)) :
            ($fr1->{file_path}->parent, $fr2->{file_path}->parent);
        return $path1 cmp $path2;
    }
    return $file_name1 cmp $file_name2;
}

sub cmp_file_results_by_file_size {
    my ($self, $fr1, $fr2) = @_;
    if ($fr1->{file_size} == $fr2->{file_size}) {
        return $self->cmp_file_results_by_path($fr1, $fr2);
    }
    return $fr1->{file_size} <=> $fr2->{file_size};
}

sub cmp_file_results_by_file_type {
    my ($self, $fr1, $fr2) = @_;
    if ($fr1->{file_type} eq $fr2->{file_type}) {
        return $self->cmp_file_results_by_path($fr1, $fr2);
    }
    return $fr1->{file_type} cmp $fr2->{file_type};
}

sub cmp_file_results_by_last_mod {
    my ($self, $fr1, $fr2) = @_;
    if ($fr1->{last_mod} == $fr2->{last_mod}) {
        return $self->cmp_file_results_by_path($fr1, $fr2);
    }
    return $fr1->{last_mod} <=> $fr2->{last_mod};
}

sub sort {
    my ($self, $file_results) = @_;
    my @sorted;
    if ($self->{settings}->{sort_descending}) {
        if ($self->{settings}->{sort_by} eq plfind::SortBy->FILENAME) {
            @sorted = sort {$self->cmp_file_results_by_file_name($b, $a)} @$file_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILESIZE) {
            @sorted = sort {$self->cmp_file_results_by_file_size($b, $a)} @$file_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILETYPE) {
            @sorted = sort {$self->cmp_file_results_by_file_type($b, $a)} @$file_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->LASTMOD) {
            @sorted = sort {$self->cmp_file_results_by_last_mod($b, $a)} @$file_results;
        } else {
            @sorted = sort {$self->cmp_file_results_by_path($b, $a)} @$file_results;
        }
    } else {
        if ($self->{settings}->{sort_by} eq plfind::SortBy->FILENAME) {
            @sorted = sort {$self->cmp_file_results_by_file_name($a, $b)} @$file_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILESIZE) {
            @sorted = sort {$self->cmp_file_results_by_file_size($a, $b)} @$file_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILETYPE) {
            @sorted = sort {$self->cmp_file_results_by_file_type($a, $b)} @$file_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->LASTMOD) {
            @sorted = sort {$self->cmp_file_results_by_last_mod($a, $b)} @$file_results;
        } else {
            @sorted = sort {$self->cmp_file_results_by_path($a, $b)} @$file_results;
        }
    }
    return \@sorted;
}

1;

__END__
