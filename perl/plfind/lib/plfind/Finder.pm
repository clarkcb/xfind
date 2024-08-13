###############################################################################
#
# Finder.pm
#
# The file finder
#
###############################################################################

package plfind::Finder;

use strict;
use warnings;

use Scalar::Util qw(blessed);

use plfind::common;
use plfind::FileResult;
use plfind::FileType;
use plfind::FileTypes;
use plfind::FileUtil;

sub new {
    my $class = shift;
    my $self = {
        settings => shift,
        file_types => plfind::FileTypes->new(),
        results => [],
    };
    bless $self, $class;
    my $errs = validate_settings($self->{settings});
    return ($self, $errs);
}

sub validate_settings {
    my $settings = shift;
    my $errs = [];
    if (scalar @{$settings->{paths}} < 1) {
        push(@{$errs}, 'Startpath not defined');
    }
    foreach my $p (@{$settings->{paths}}) {
        unless (-e $p) {
            push(@{$errs}, 'Startpath not found');
        }
        unless (-r $p) {
            push(@{$errs}, 'Startpath not readable');
        }
    }
    if ($settings->{max_depth} > -1 && $settings->{min_depth} > -1
        && $settings->{max_depth} < $settings->{min_depth}) {
        push(@{$errs}, 'Invalid range for mindepth and maxdepth');
    }
    if (blessed($settings->{max_last_mod}) && blessed($settings->{min_last_mod})
        && $settings->{max_last_mod} < $settings->{min_last_mod}) {
        push(@{$errs}, 'Invalid range for minlastmod and maxlastmod');
    }
    if ($settings->{max_size} > 0 && $settings->{min_size} > 0
        && $settings->{max_size} < $settings->{min_size}) {
        push(@{$errs}, 'Invalid range for minsize and maxsize');
    }
    return $errs;
}

sub matches_any_pattern {
    my ($s, $patterns) = @_;
    foreach my $pattern (@{$patterns}) {
        if ($s =~ /$pattern/) {
            return 1;
        }
    }
    return 0;
}

sub any_matches_any_pattern {
    my ($slist, $patterns) = @_;
    foreach my $s (@{$slist}) {
        if (matches_any_pattern($s, $patterns)) {
            return 1;
        }
    }
    return 0;
}

sub is_matching_dir {
    my ($self, $d) = @_;
    if (plfind::FileUtil::is_dot_dir($d)) {
        return 1;
    }
    my @path_elems = grep {$_ ne ''} plfind::FileUtil::split_dir($d);
    if (!$self->{settings}->{include_hidden}) {
        foreach my $p (@path_elems) {
            if (plfind::FileUtil::is_hidden($p)) {
                return 0;
            }
        }
    }
    return ((scalar @{$self->{settings}->{in_dir_patterns}} == 0
        || any_matches_any_pattern(\@path_elems, $self->{settings}->{in_dir_patterns}))
        && (scalar @{$self->{settings}->{out_dir_patterns}} == 0
        || !any_matches_any_pattern(\@path_elems, $self->{settings}->{out_dir_patterns})));
}

sub is_matching_archive_extension {
    my ($self, $ext) = @_;
    return ((scalar @{$self->{settings}->{in_archive_extensions}} == 0
        || (grep {$_ eq $ext} @{$self->{settings}->{in_archive_extensions}}))
        && (scalar @{$self->{settings}->{out_archive_extensions}} == 0
        || !(grep {$_ eq $ext} @{$self->{settings}->{out_archive_extensions}})));
}

sub is_matching_extension {
    my ($self, $ext) = @_;
    return ((scalar @{$self->{settings}->{in_extensions}} == 0
        || (grep {$_ eq $ext} @{$self->{settings}->{in_extensions}}))
        && (scalar @{$self->{settings}->{out_extensions}} == 0
        || !(grep {$_ eq $ext} @{$self->{settings}->{out_extensions}})));
}

sub has_matching_archive_extension {
    my ($self, $fr) = @_;
    if (scalar @{$self->{settings}->{in_archive_extensions}} || scalar @{$self->{settings}->{out_archive_extensions}}) {
        my $ext = plfind::FileUtil::get_extension($fr->{file_name});
        return $self->is_matching_archive_extension($ext);
    }
    return 1;
}

sub has_matching_extension {
    my ($self, $fr) = @_;
    if (scalar @{$self->{settings}->{in_extensions}} || scalar @{$self->{settings}->{out_extensions}}) {
        my $ext = plfind::FileUtil::get_extension($fr->{file_name});
        return $self->is_matching_extension($ext);
    }
    return 1;
}

sub is_matching_archive_file_name {
    my ($self, $file_name) = @_;
    return ((scalar @{$self->{settings}->{in_archive_file_patterns}} == 0
        || matches_any_pattern($file_name, $self->{settings}->{in_archive_file_patterns}))
        && (scalar @{$self->{settings}->{out_archive_file_patterns}} == 0
        || !matches_any_pattern($file_name, $self->{settings}->{out_archive_file_patterns})));
}

sub is_matching_file_name {
    my ($self, $file_name) = @_;
    return ((scalar @{$self->{settings}->{in_file_patterns}} == 0
        || matches_any_pattern($file_name, $self->{settings}->{in_file_patterns}))
        && (scalar @{$self->{settings}->{out_file_patterns}} == 0
        || !matches_any_pattern($file_name, $self->{settings}->{out_file_patterns})));
}

sub is_matching_file_type {
    my ($self, $file_type) = @_;
    return ((scalar @{$self->{settings}->{in_file_types}} == 0
        || (grep {$_ eq $file_type} @{$self->{settings}->{in_file_types}}))
        && (scalar @{$self->{settings}->{out_file_types}} == 0
        || !(grep {$_ eq $file_type} @{$self->{settings}->{out_file_types}})));
}

sub is_matching_file_size {
    my ($self, $file_size) = @_;
    return (($self->{settings}->{max_size} == 0 || $file_size <= $self->{settings}->{max_size})
        && ($self->{settings}->{min_size} == 0 || $file_size >= $self->{settings}->{min_size}));
}

sub is_matching_last_mod {
    my ($self, $last_mod) = @_;
    return ((!blessed($self->{settings}->{max_last_mod})
        || $last_mod <= $self->{settings}->{max_last_mod}->epoch)
        && (!blessed($self->{settings}->{min_last_mod})
        || $last_mod >= $self->{settings}->{min_last_mod}->epoch));
}

sub is_matching_archive_file_result {
    my ($self, $fr) = @_;
    return $self->has_matching_archive_extension($fr)
        && $self->is_matching_archive_file_name($fr->{file_name});
}

sub is_matching_file_result {
    my ($self, $fr) = @_;
    return $self->has_matching_extension($fr)
        && $self->is_matching_file_name($fr->{file_name})
        && $self->is_matching_file_type($fr->{file_type})
        && $self->is_matching_file_size($fr->{file_size})
        && $self->is_matching_last_mod($fr->{last_mod});
}

sub filter_to_file_result {
    my ($self, $file_path) = @_;
    my ($dir, $file_name) = plfind::FileUtil::split_path($file_path);
    if (!$self->{settings}->{include_hidden} && plfind::FileUtil::is_hidden($file_name)) {
        return;
    }
    my $file_type = $self->{file_types}->get_file_type($file_name);
    if ($file_type eq plfind::FileType->ARCHIVE
        && !$self->{settings}->{include_archives}
        && !$self->{settings}->{archives_only}) {
        return;
    }
    my $file_size = 0;
    my $last_mod = 0;
    if ($self->{settings}->needs_last_mod || $self->{settings}->needs_size) {
        my $fp = plfind::FileUtil::join_path($dir, $file_name);
        my @fpstat = stat($fp);
        # stat index 7 == size
        $file_size = $fpstat[7];
        # stat index 9 == mtime
        $last_mod = $fpstat[9];
    }
    my $file_result = plfind::FileResult->new($dir, $file_name, $file_type, $file_size, $last_mod);
    if ($file_type eq plfind::FileType->ARCHIVE) {
        if ($self->is_matching_archive_file_result($file_result)) {
            return $file_result;
        }
        return;
    }
    if (!$self->{settings}->{archives_only} && $self->is_matching_file_result($file_result)) {
        return $file_result;
    }
    return;
}

sub rec_get_file_results {
    my ($self, $dir, $min_depth, $max_depth, $current_depth) = @_;
    my $recurse = 1;
    if ($current_depth == $max_depth) {
        $recurse = 0;
    } elsif ($max_depth > -1 && $current_depth > $max_depth) {
        return [];
    }
    my $dir_results = [];
    my $file_results = [];
    opendir(DIR, $dir) or die $!;
    while (my $f = readdir(DIR)) {
        if (plfind::FileUtil::is_dot_dir($f)) {
            next;
        }
        my $sub_file = plfind::FileUtil::join_path($dir, $f);
        if (-d $sub_file && $recurse && $self->is_matching_dir($f)) {
            push(@{$dir_results}, $sub_file);
        } elsif (-f $sub_file && ($min_depth < 0 || $current_depth >= $min_depth)) {
            my $file_result = $self->filter_to_file_result($sub_file);
            if (defined $file_result) {
                push(@{$file_results}, $file_result);
            }
        }
    }
    closedir(DIR);
    foreach my $dir_result (@{$dir_results}) {
        my $sub_file_results = $self->rec_get_file_results($dir_result, $min_depth, $max_depth, $current_depth + 1);
        push(@{$file_results}, @{$sub_file_results});
    }
    return $file_results;
}

sub get_file_results {
    my ($self, $file_path) = @_;
    my $file_results = [];
    if (-d $file_path) {
        # if max_depth is zero, we can skip since a directory cannot be a result
        if ($self->{settings}->{max_depth} == 0) {
            return [];
        }
        if ($self->is_matching_dir($file_path)) {
            my $max_depth = $self->{settings}->{max_depth};
            if (!$self->{settings}->{recursive}) {
                $max_depth = 1;
            }
            push(@{$file_results}, @{$self->rec_get_file_results($file_path, $self->{settings}->{min_depth},
                $max_depth, 1)});
        } else {
            plfind::common::log_err("Startpath does not match find settings");
        }
    } elsif (-f $file_path) {
        # if min_depth > zero, we can skip since the file is at depth zero
        if ($self->{settings}->{min_depth} > 0) {
            return [];
        }
        my $file_result = $self->filter_to_file_result($file_path);
        if (defined $file_result) {
            push(@{$file_results}, $file_result);
        } else {
            plfind::common::log_err("Startpath does not match find settings");
        }
    }
    return $file_results;
}

sub find {
    my $self = shift;
    my $file_results = [];
    foreach my $p (@{$self->{settings}->{paths}}) {
        push(@{$file_results}, @{$self->get_file_results($p)});
    }
    return $self->sort_file_results($file_results);
}

sub cmp_file_results_by_path {
    my ($self, $fr1, $fr2) = @_;
    my ($path1, $path2) = $self->{settings}->{sort_case_insensitive} ?
        (lc($fr1->{path}), lc($fr2->{path})) :
        ($fr1->{path}, $fr2->{path});
    if ($path1 eq $path2) {
        my ($file_name1, $file_name2) = $self->{settings}->{sort_case_insensitive} ?
            (lc($fr1->{file_name}), lc($fr2->{file_name})) :
            ($fr1->{file_name}, $fr2->{file_name});
        return $file_name1 cmp $file_name2;
    }
    return $path1 cmp $path2;
}

sub cmp_file_results_by_file_name {
    my ($self, $fr1, $fr2) = @_;
    my ($file_name1, $file_name2) = $self->{settings}->{sort_case_insensitive} ?
        (lc($fr1->{file_name}), lc($fr2->{file_name})) :
        ($fr1->{file_name}, $fr2->{file_name});
    if ($file_name1 eq $file_name2) {
        my ($path1, $path2) = $self->{settings}->{sort_case_insensitive} ?
            (lc($fr1->{path}), lc($fr2->{path})) :
            ($fr1->{path}, $fr2->{path});
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

sub sort_file_results {
    my ($self, $file_results) = @_;
    my @sorted;
    if ($self->{settings}->{sort_by} eq plfind::SortBy->FILENAME) {
        @sorted = sort {$self->cmp_file_results_by_file_name($a, $b)} @{$file_results};
    } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILESIZE) {
        @sorted = sort {$self->cmp_file_results_by_file_size($a, $b)} @{$file_results};
    } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILETYPE) {
        @sorted = sort {$self->cmp_file_results_by_file_type($a, $b)} @{$file_results};
    } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->LASTMOD) {
        @sorted = sort {$self->cmp_file_results_by_last_mod($a, $b)} @{$file_results};
    } else {
        @sorted = sort {$self->cmp_file_results_by_path($a, $b)} @{$file_results};
    }
    if ($self->{settings}->{sort_descending}) {
        @sorted = reverse @sorted;
    }
    return \@sorted;
}

1;

__END__
