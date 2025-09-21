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

use constant INVALID_RANGE_MINDEPTH_MAXDEPTH => 'Invalid range for mindepth and maxdepth';
use constant INVALID_RANGE_MINLASTMOD_MAXLASTMOD => 'Invalid range for minlastmod and maxlastmod';
use constant INVALID_RANGE_MINSIZE_MAXSIZE => 'Invalid range for minsize and maxsize';
use constant STARTPATH_NOT_DEFINED => 'Startpath not defined';
use constant STARTPATH_NOT_FOUND => 'Startpath not found';
use constant STARTPATH_NOT_MATCH_FIND_SETTINGS => 'Startpath does not match find settings';
use constant STARTPATH_NOT_READABLE => 'Startpath not readable';

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
        push(@$errs, STARTPATH_NOT_DEFINED);
    }
    foreach my $p (@{$settings->{paths}}) {
        unless (-e $p) {
            my $expanded = plfind::FileUtil::expand_path($p);
            unless (-e $expanded) {
                push(@$errs, STARTPATH_NOT_FOUND);
                return $errs;
            }
        }
        unless (-r $p) {
            my $expanded = plfind::FileUtil::expand_path($p);
            unless (-r $expanded) {
                push(@$errs, STARTPATH_NOT_READABLE);
                return $errs;
            }
        }
    }
    if ($settings->{max_depth} > -1 && $settings->{min_depth} > -1
        && $settings->{max_depth} < $settings->{min_depth}) {
        push(@$errs, INVALID_RANGE_MINDEPTH_MAXDEPTH);
        return $errs;
    }
    if (blessed($settings->{max_last_mod}) && blessed($settings->{min_last_mod})
        && $settings->{max_last_mod} < $settings->{min_last_mod}) {
        push(@$errs, INVALID_RANGE_MINLASTMOD_MAXLASTMOD);
        return $errs;
    }
    if ($settings->{max_size} > 0 && $settings->{min_size} > 0
        && $settings->{max_size} < $settings->{min_size}) {
        push(@$errs, INVALID_RANGE_MINSIZE_MAXSIZE);
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
    my ($slist, $patterns) = @_;
    foreach my $s (@$slist) {
        if (matches_any_pattern($s, $patterns)) {
            return 1;
        }
    }
    return 0;
}

sub filter_dir_by_hidden {
    # $d is an instance of Path::Class::Dir
    my ($self, $d) = @_;
    if (!$self->{settings}->{include_hidden} && plfind::FileUtil::is_hidden_path($d)) {
        return 0;
    }
    return 1;
}

sub filter_dir_by_in_patterns {
    # $d is an instance of Path::Class::Dir
    my ($self, $d) = @_;
    my @path_elems = grep {$_ ne ''} $d->dir_list;
    return scalar @{$self->{settings}->{in_dir_patterns}} == 0
        || any_matches_any_pattern(\@path_elems, $self->{settings}->{in_dir_patterns});
}

sub filter_dir_by_out_patterns {
    # $d is an instance of Path::Class::Dir
    my ($self, $d) = @_;
    my @path_elems = grep {$_ ne ''} $d->dir_list;
    return scalar @{$self->{settings}->{out_dir_patterns}} == 0
        || !any_matches_any_pattern(\@path_elems, $self->{settings}->{out_dir_patterns});
}

sub is_matching_dir {
    # $d is an instance of Path::Class::Dir
    my ($self, $d) = @_;
    return $self->filter_dir_by_hidden($d)
        && $self->filter_dir_by_in_patterns($d)
        && $self->filter_dir_by_out_patterns($d);
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
        my $ext = plfind::FileUtil::get_extension($fr->{file_path}->basename);
        return $self->is_matching_archive_extension($ext);
    }
    return 1;
}

sub has_matching_extension {
    my ($self, $fr) = @_;
    if (scalar @{$self->{settings}->{in_extensions}} || scalar @{$self->{settings}->{out_extensions}}) {
        my $ext = plfind::FileUtil::get_extension($fr->{file_path}->basename);
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
        && $self->is_matching_archive_file_name($fr->{file_path}->basename);
}

sub is_matching_file_result {
    my ($self, $fr) = @_;
    return $self->has_matching_extension($fr)
        && $self->is_matching_file_name($fr->{file_path}->basename)
        && $self->is_matching_file_type($fr->{file_type})
        && $self->is_matching_file_size($fr->{file_size})
        && $self->is_matching_last_mod($fr->{last_mod});
}

sub filter_to_file_result {
    my ($self, $file_path) = @_;
    my $dir = $file_path->dir;
    if (!$self->is_matching_dir($dir)) {
        return;
    }
    my $file_name = $file_path->basename;
    if (!$self->{settings}->{include_hidden} && plfind::FileUtil::is_hidden_name($file_name)) {
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
    }
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
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
    while (my $f = $dir->next) {
        if (-l $f && !$self->{settings}->{follow_symlinks}) {
            next;
        }
        if ($f->is_dir) {
            if (plfind::FileUtil::is_dot_dir($f->basename)) {
                next;
            }
            if ($recurse && $self->filter_dir_by_hidden($f) && $self->filter_dir_by_out_patterns($f)) {
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
    my ($self, $file_path) = @_;
    my $file_results = [];

    unless (-e $file_path) {
        my $expanded = plfind::FileUtil::expand_path($file_path);
        unless (-e $expanded) {
            plfind::common::log_err(STARTPATH_NOT_FOUND);
            return [];
        }
        if ($file_path->is_dir) {
            $file_path = dir($expanded);
        } else {
            $file_path = file($expanded);
        }
    }

    if ($file_path->is_dir) {
        # if max_depth is zero, we can skip since a directory cannot be a result
        if ($self->{settings}->{max_depth} == 0) {
            return [];
        }
        if ($self->filter_dir_by_hidden($file_path) && $self->filter_dir_by_out_patterns($file_path)) {
            my $max_depth = $self->{settings}->{max_depth};
            if (!$self->{settings}->{recursive}) {
                $max_depth = 1;
            }
            push(@$file_results, @{$self->rec_get_file_results($file_path, $self->{settings}->{min_depth},
                $max_depth, 1)});
        } else {
            plfind::common::log_err(STARTPATH_NOT_MATCH_FIND_SETTINGS);
            return [];
        }
    } else {
        # if min_depth > zero, we can skip since the file is at depth zero
        if ($self->{settings}->{min_depth} > 0) {
            return [];
        }
        my $file_result = $self->filter_to_file_result($file_path);
        if (defined $file_result) {
            push(@$file_results, $file_result);
        } else {
            plfind::common::log_err(STARTPATH_NOT_MATCH_FIND_SETTINGS);
            return [];
        }
    }
    return $file_results;
}

sub find {
    my $self = shift;
    my $file_results = [];
    foreach my $p (@{$self->{settings}->{paths}}) {
        push(@$file_results, @{$self->get_file_results($p)});
    }
    my $file_result_sorter = plfind::FileResultSorter->new($self->{settings});
    return $file_result_sorter->sort($file_results);
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
