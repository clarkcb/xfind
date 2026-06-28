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

use Path::Class;
use Scalar::Util qw(blessed);

use plfind::common;
use plfind::FileResult;
use plfind::FileResultSorter;
use plfind::FileType;
use plfind::FileTypes;
use plfind::FileUtil;
use plfind::FindError;

sub new {
    my $class = shift;
    my $self = {
        settings => shift,
        file_types => plfind::FileTypes->new(),
        results => [],
    };
    bless $self, $class;
    my $errs = $self->validate_settings();
    return ($self, $errs);
}

sub validate_settings {
    my $self = shift;
    my $settings = $self->{settings};
    my $errs = [];
    if (!defined($settings->{paths}) || scalar @{$settings->{paths}} < 1) {
        push(@$errs, plfind::FindError->STARTPATH_NOT_DEFINED);
        return $errs;
    }
    foreach my $path (@{$settings->{paths}}) {
        my $p = $path;
        # 1) expand if not found
        unless (-e $p) {
            $p = plfind::FileUtil::expand_path($p);
        }
        if (-l $p) {
            unless ($self->{settings}->{follow_symlinks}) {
                push(@$errs, plfind::FindError->STARTPATH_NOT_MATCH_FIND_SETTINGS);
                return $errs;
            }
        }
        if (-d $p) {
            if (ref($p) ne 'Path::Class::Dir') {
                $p = dir($p);
            }
        } elsif (-f $p) {
            if (ref($p) ne 'Path::Class::File') {
                $p = file($p);
            }
        } else {
            push(@$errs, plfind::FindError->STARTPATH_NOT_FOUND);
            return $errs;
        }
        unless (-r $p) {
            push(@$errs, plfind::FindError->STARTPATH_NOT_READABLE);
            return $errs;
        }
        if ($p->is_dir) {
            unless ($self->is_traversable_dir_path($p)) {
                push(@$errs, plfind::FindError->STARTPATH_NOT_MATCH_FIND_SETTINGS);
                return $errs;
            }
        } elsif (-f $p) {
            my $fr = $self->filter_to_file_result($p);
            unless (defined $fr) {
                push(@$errs, plfind::FindError->STARTPATH_NOT_MATCH_FIND_SETTINGS);
                return $errs;
            }
        } else {
            # TODO: handle start path as symlink
            # TODO: start path is unknown/invalid type
            push(@$errs, plfind::FindError->STARTPATH_NOT_MATCH_FIND_SETTINGS);
            return $errs;
        }
    }
    if ($settings->{max_depth} > -1 && $settings->{min_depth} > -1
        && $settings->{max_depth} < $settings->{min_depth}) {
        push(@$errs, plfind::FindError->INVALID_RANGE_MINDEPTH_MAXDEPTH);
        return $errs;
    }
    if (blessed($settings->{max_last_mod}) && blessed($settings->{min_last_mod})
        && $settings->{max_last_mod} < $settings->{min_last_mod}) {
        push(@$errs, plfind::FindError->INVALID_RANGE_MINLASTMOD_MAXLASTMOD);
        return $errs;
    }
    if ($settings->{max_size} > 0 && $settings->{min_size} > 0
        && $settings->{max_size} < $settings->{min_size}) {
        push(@$errs, plfind::FindError->INVALID_RANGE_MINSIZE_MAXSIZE);
    }
    return $errs;
}

sub matches_any_pattern {
    my ($s, $patterns) = @_;
    foreach my $pattern (@$patterns) {
        if ($s =~ /$pattern/) {
            return 1;
        }
    }
    return 0;
}

sub any_matches_any_pattern {
    my ($strings, $patterns) = @_;
    foreach my $s (@$strings) {
        if (matches_any_pattern($s, $patterns)) {
            return 1;
        }
    }
    return 0;
}

sub empty_or_matches_any_pattern {
    my ($s, $patterns) = @_;
    return scalar(@$patterns) == 0 || matches_any_pattern($s, $patterns);
}

sub empty_or_not_matches_any_pattern {
    my ($s, $patterns) = @_;
    return scalar(@$patterns) == 0 || !matches_any_pattern($s, $patterns);
}

sub empty_or_any_matches_any_pattern {
    my ($strings, $patterns) = @_;
    return scalar(@$patterns) == 0 || any_matches_any_pattern($strings, $patterns);
}

sub empty_or_not_any_matches_any_pattern {
    my ($strings, $patterns) = @_;
    return scalar(@$patterns) == 0 || !any_matches_any_pattern($strings, $patterns);
}

sub empty_or_matches_any_string {
    my ($s, $strings) = @_;
    return scalar @$strings == 0 || (grep {$_ eq $s} @$strings);
}

sub empty_or_not_matches_any_string {
    my ($s, $strings) = @_;
    return scalar @$strings == 0 || !(grep {$_ eq $s} @$strings);
}

sub empty_or_matches_any_file_type {
    my ($file_type, $in_file_types) = @_;
    return (scalar @$in_file_types == 0 || (grep {$_ eq $file_type} @$in_file_types));
}

sub empty_or_not_matches_any_file_type {
    my ($file_type, $out_file_types) = @_;
    return (scalar @$out_file_types == 0 || !(grep {$_ eq $file_type} @$out_file_types));
}

sub is_matching_path_by_symlink {
    my ($self, $path) = @_;
    return $self->{settings}->{follow_symlinks} || !(-l $path);
}

sub is_matching_dir_path_by_hidden {
    # $d is an instance of Path::Class::Dir
    my ($self, $d) = @_;
    return $self->{settings}->{include_hidden} || !plfind::FileUtil::is_hidden_path($d);
}

sub is_matching_dir_path_by_in_patterns {
    # $dir_path is an instance of Path::Class::Dir
    my ($self, $dir_path) = @_;
    my @path_elems = grep {$_ ne ''} $dir_path->dir_list;
    return empty_or_any_matches_any_pattern(\@path_elems, $self->{settings}->{in_dir_patterns});
}

sub is_matching_dir_path_by_out_patterns {
    # $dir_path is an instance of Path::Class::Dir
    my ($self, $dir_path) = @_;
    my @path_elems = grep {$_ ne ''} $dir_path->dir_list;
    return empty_or_not_any_matches_any_pattern(\@path_elems, $self->{settings}->{out_dir_patterns});
}

sub is_traversable_dir_path {
    # $dir_path is an instance of Path::Class::Dir
    my ($self, $dir_path) = @_;
    return $self->is_matching_dir_path_by_hidden($dir_path)
        && $self->is_matching_dir_path_by_out_patterns($dir_path);
}

sub is_matching_dir_path {
    # $dir_path is an instance of Path::Class::Dir
    my ($self, $dir_path) = @_;
    return $self->is_matching_dir_path_by_hidden($dir_path)
        && $self->is_matching_dir_path_by_in_patterns($dir_path)
        && $self->is_matching_dir_path_by_out_patterns($dir_path);
}

sub is_null_or_matching_dir_path {
    # $dir_path is an instance of Path::Class::Dir
    my ($self, $dir_path) = @_;
    if (!defined $dir_path || !$dir_path) {
        return 1;
    }
    return $self->is_matching_dir_path($dir_path);
}


sub is_matching_file_name_by_hidden {
    my ($self, $file_name) = @_;
    return $self->{settings}->{include_hidden} || !plfind::FileUtil::is_hidden_name($file_name);
}

sub is_matching_archive_extension {
    my ($self, $ext) = @_;
    return (empty_or_matches_any_string($ext, $self->{settings}->{in_archive_extensions})
        && empty_or_not_matches_any_string($ext, $self->{settings}->{out_archive_extensions}));
}

sub is_matching_archive_extension_for_file_path {
    # $file_path is an instance of Path::Class::File
    my ($self, $file_path) = @_;
    if (scalar @{$self->{settings}->{in_archive_extensions}} || scalar @{$self->{settings}->{out_archive_extensions}}) {
        my $ext = plfind::FileUtil::get_extension($file_path->basename);
        return $self->is_matching_archive_extension($ext);
    }
    return 1;
}

sub is_matching_archive_file_name {
    my ($self, $file_name) = @_;
    return $self->is_matching_file_name_by_hidden($file_name)
        && empty_or_matches_any_pattern($file_name, $self->{settings}->{in_archive_file_patterns})
        && empty_or_not_matches_any_pattern($file_name, $self->{settings}->{out_archive_file_patterns});
}

sub is_matching_archive_file_name_for_file_path {
    # $file_path is an instance of Path::Class::File
    my ($self, $file_path) = @_;
    if (scalar @{$self->{settings}->{in_archive_file_patterns}} || scalar @{$self->{settings}->{out_archive_file_patterns}}) {
        return $self->is_matching_archive_file_name($file_path->basename);
    }
    return 1;
}

sub is_matching_archive_file_path {
    # $file_path is an instance of Path::Class::File
    my ($self, $file_path) = @_;
    return $self->is_matching_archive_extension_for_file_path($file_path)
        && $self->is_matching_archive_file_name_for_file_path($file_path);
}

sub is_matching_archive_file_result {
    my ($self, $fr) = @_;
    return $self->is_matching_archive_file_path($fr->{file_path});
}

sub is_matching_extension {
    my ($self, $ext) = @_;
    return (empty_or_matches_any_string($ext, $self->{settings}->{in_extensions})
        && empty_or_not_matches_any_string($ext, $self->{settings}->{out_extensions}));
}

sub is_matching_extension_for_file_path {
    # $file_path is an instance of Path::Class::File
    my ($self, $file_path) = @_;
    if (scalar @{$self->{settings}->{in_extensions}} || scalar @{$self->{settings}->{out_extensions}}) {
        my $ext = plfind::FileUtil::get_extension($file_path->basename);
        return $self->is_matching_extension($ext);
    }
    return 1;
}

sub is_matching_file_name {
    my ($self, $file_name) = @_;
    return $self->is_matching_file_name_by_hidden($file_name)
        && empty_or_matches_any_pattern($file_name, $self->{settings}->{in_file_patterns})
        && empty_or_not_matches_any_pattern($file_name, $self->{settings}->{out_file_patterns});
}

sub is_matching_file_name_for_file_path {
    # $file_path is an instance of Path::Class::File
    my ($self, $file_path) = @_;
    if (scalar @{$self->{settings}->{in_file_patterns}} || scalar @{$self->{settings}->{out_file_patterns}}) {
        return $self->is_matching_file_name($file_path->basename);
    }
    return 1;
}

sub is_matching_file_path {
    # $file_path is an instance of Path::Class::File
    my ($self, $file_path) = @_;
    return $self->is_matching_extension_for_file_path($file_path)
        && $self->is_matching_file_name_for_file_path($file_path);
}

sub is_matching_file_type {
    my ($self, $file_type) = @_;
    return empty_or_matches_any_file_type($file_type, $self->{settings}->{in_file_types})
        && empty_or_not_matches_any_file_type($file_type, $self->{settings}->{out_file_types});
}

sub is_matching_file_size {
    my ($self, $file_size) = @_;
    return (($self->{settings}->{max_size} <= 0 || $file_size <= $self->{settings}->{max_size})
        && ($self->{settings}->{min_size} <= 0 || $file_size >= $self->{settings}->{min_size}));
}

sub is_matching_last_mod {
    my ($self, $last_mod) = @_;
    return ((!blessed($self->{settings}->{max_last_mod}) || $last_mod <= $self->{settings}->{max_last_mod}->epoch)
        && (!blessed($self->{settings}->{min_last_mod}) || $last_mod >= $self->{settings}->{min_last_mod}->epoch));
}

sub is_matching_file_result {
    my ($self, $fr) = @_;
    return $self->is_matching_file_path($fr->{file_path})
        && $self->is_matching_file_type($fr->{file_type})
        && $self->is_matching_file_size($fr->{file_size})
        && $self->is_matching_last_mod($fr->{last_mod});
}

sub filter_archive_file_path_to_file_result {
    # $file_path is an instance of Path::Class::File
    my ($self, $file_path) = @_;

    if (!$self->{settings}->{include_archives} && !$self->{settings}->{archives_only}) {
        return;
    }

    if (!$self->is_matching_archive_file_path($file_path)) {
        return;
    }

    my $file_size = 0;
    my $last_mod = 0;
    return plfind::FileResult->new($file_path, plfind::FileType->ARCHIVE, $file_size, $last_mod);
}

sub filter_reg_file_path_to_file_result {
    # $file_path is an instance of Path::Class::File
    my ($self, $file_path, $file_type) = @_;

    if ($self->{settings}->{archives_only}) {
        return;
    }

    if (!$self->is_matching_file_path($file_path) || !$self->is_matching_file_type($file_type)) {
        return;
    }

    my $file_size = 0;
    my $last_mod = 0;
    if ($self->{settings}->needs_last_mod || $self->{settings}->needs_size) {
        my $fpstat = $file_path->stat;
        # stat array elements
        # 0 	Device number of file system
        # 1 	Inode number
        # 2 	File mode (type and permissions)
        # 3 	Number of (hard) links to the file
        # 4 	Numeric user ID of file.s owner
        # 5 	Numeric group ID of file.s owner
        # 6 	The device identifier (special files only)
        # 7 	File size, in bytes
        # 8 	Last access time since the epoch
        # 9 	Last modify time since the epoch
        # 10 	Inode change time (not creation time!) since the epoch
        # 11 	Preferred block size for file system I/O
        # 12	Actual number of blocks allocated
        $file_size = $fpstat->[7];
        $last_mod = $fpstat->[9];

        if (!$self->is_matching_file_size($file_size) || !$self->is_matching_last_mod($last_mod)) {
            return;
        }
    }

    return plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
}

sub filter_to_file_result {
    # $file_path is an instance of Path::Class::File
    my ($self, $file_path) = @_;
    my $dir = $file_path->dir;
    if (!$self->is_null_or_matching_dir_path($dir)) {
        return;
    }
    my $file_name = $file_path->basename;
    if (!$self->{settings}->{include_hidden} && plfind::FileUtil::is_hidden_name($file_name)) {
        return;
    }
    my $file_type = $self->{file_types}->get_file_type($file_name);
    if ($file_type eq plfind::FileType->ARCHIVE) {
        return $self->filter_archive_file_path_to_file_result($file_path);
    }
    return $self->filter_reg_file_path_to_file_result($file_path, $file_type);
}

sub rec_get_file_results {
    # $dir_path is an instance of Path::Class::Dir
    my ($self, $dir_path, $min_depth, $max_depth, $current_depth) = @_;
    my $recurse = 1;
    if ($current_depth == $max_depth) {
        $recurse = 0;
    } elsif ($max_depth > -1 && $current_depth > $max_depth) {
        return [];
    }
    my $dir_results = [];
    my $file_results = [];
    while (my $f = $dir_path->next) {
        if (!$self->is_matching_path_by_symlink($f)) {
            next;
        }
        if ($f->is_dir) {
            if (plfind::FileUtil::is_dot_dir($f->basename)) {
                next;
            }
            if ($recurse && $self->is_traversable_dir_path($f)) {
                push(@$dir_results, $f);
            }
        } elsif ($min_depth < 0 || $current_depth >= $min_depth) {
            my $file_result = $self->filter_to_file_result($f);
            if (defined $file_result) {
                push(@$file_results, $file_result);
            }
        }
    }
    foreach my $dir_result (@$dir_results) {
        my $sub_file_results = $self->rec_get_file_results($dir_result, $min_depth, $max_depth, $current_depth + 1);
        push(@$file_results, @$sub_file_results);
    }
    return $file_results;
}

sub get_file_results {
    my ($self, $path) = @_;
    my $file_results = [];
    my @errs;

    unless (-e $path) {
        my $expanded = plfind::FileUtil::expand_path($path);
        unless (-e $expanded) {
            push(@errs, plfind::FindError->STARTPATH_NOT_FOUND);
            return ($file_results, \@errs);
        }
        if ($path->is_dir) {
            $path = dir($expanded);
        } else {
            $path = file($expanded);
        }
    }

    if (-l $path && !$self->{settings}->{follow_symlinks}) {
        push(@errs, plfind::FindError->STARTPATH_NOT_MATCH_FIND_SETTINGS);
        return ($file_results, \@errs);
    }

    if ($path->is_dir) {
        # if max_depth is zero, we can skip since a directory cannot be a result
        if ($self->{settings}->{max_depth} == 0) {
            return ($file_results, []);
        }
        if ($self->is_traversable_dir_path($path)) {
            my $max_depth = $self->{settings}->{max_depth};
            if (!$self->{settings}->{recursive}) {
                $max_depth = 1;
            }
            push(@$file_results, @{$self->rec_get_file_results($path, $self->{settings}->{min_depth},
                $max_depth, 1)});
        } else {
            push(@errs, plfind::FindError->STARTPATH_NOT_MATCH_FIND_SETTINGS);
        }
    } elsif (-f $path) {
        # if min_depth > zero, we can skip since the file is at depth zero
        if ($self->{settings}->{min_depth} > 0) {
            return ($file_results, []);
        }
        my $file_result = $self->filter_to_file_result($path);
        if (defined $file_result) {
            push(@$file_results, $file_result);
        } else {
            push(@errs, plfind::FindError->STARTPATH_NOT_MATCH_FIND_SETTINGS);
        }
    } else {
        push(@errs, plfind::FindError->STARTPATH_NOT_MATCH_FIND_SETTINGS);
    }
    return ($file_results, \@errs);
}

sub find {
    my $self = shift;
    my $file_results = [];
    foreach my $p (@{$self->{settings}->{paths}}) {
        my ($path_file_results, $path_errs) = $self->get_file_results($p);
        if (scalar @$path_errs) {
            return ($file_results, $path_errs);
        }
        push(@$file_results, @$path_file_results);
    }
    if (scalar @$file_results > 1) {
        my $file_result_sorter = plfind::FileResultSorter->new($self->{settings});
        return ($file_result_sorter->sort($file_results), []);
    }
    return ($file_results, []);
}

sub get_matching_dirs {
    my ($file_results) = @_;
    my @dirs = map {$_->{file_path}->parent} @$file_results;
    my $uniq = plfind::common::uniq(\@dirs);
    return $uniq;
}

sub print_matching_dirs {
    my ($file_results, $formatter) = @_;
    my $dirs = get_matching_dirs($file_results);
    if (scalar @$dirs) {
        plfind::common::log_msg(sprintf("\nMatching directories (%d):", scalar @$dirs));
        foreach my $d (@$dirs) {
            plfind::common::log_msg($formatter->format_dir($d));
        }
    } else {
        plfind::common::log_msg("\nMatching directories: 0");
    }
}

sub print_matching_files {
    my ($file_results, $formatter) = @_;
    if (scalar @$file_results) {
        plfind::common::log_msg(sprintf("\nMatching files (%d):", scalar @$file_results));
        foreach my $fr (@$file_results) {
            plfind::common::log_msg($formatter->format_file_result($fr));
        }
    } else {
        plfind::common::log_msg("\nMatching files: 0");
    }
}

1;

__END__
