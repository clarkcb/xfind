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

use File::Spec;
use File::Basename;

use plfind::common;
use plfind::FileResult;
use plfind::FileType;
use plfind::FileTypes;
use plfind::FileUtil;

sub new {
    my $class = shift;
    my $self = {
        settings => shift,
        filetypes => new plfind::FileTypes(),
        results => [],
    };
    bless $self, $class;
    my $errs = $self->validate_settings();
    return ($self, $errs);
}

sub validate_settings {
    my $self = shift;
    my $errs = [];
    if (scalar @{$self->{settings}->{paths}} < 1) {
        push(@{$errs}, 'Startpath not defined');
    }
    foreach my $p (@{$self->{settings}->{paths}}) {
        unless (-e $p) {
            push(@{$errs}, 'Startpath not found');
        }
        unless (-r $p) {
            push(@{$errs}, 'Startpath not readable');
        }
    }
    return $errs;
}

sub matches_any_pattern {
    my ($self, $s, $patterns) = @_;
    foreach my $pattern (@{$patterns}) {
        if ($s =~ /$pattern/) {
            return 1;
        }
    }
    return 0;
}

sub any_matches_any_pattern {
    my ($self, $slist, $patterns) = @_;
    foreach my $s (@{$slist}) {
        if ($self->matches_any_pattern($s, $patterns)) {
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
    my @path_elems = grep {$_ ne ''} File::Spec->splitdir($d);
    if ($self->{settings}->{excludehidden}) {
        foreach my $p (@path_elems) {
            if (plfind::FileUtil::is_hidden($p)) {
                return 0;
            }
        }
    }
    if (scalar @{$self->{settings}->{in_dirpatterns}} &&
        !$self->any_matches_any_pattern(\@path_elems, $self->{settings}->{in_dirpatterns})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_dirpatterns}} &&
        $self->any_matches_any_pattern(\@path_elems, $self->{settings}->{out_dirpatterns})) {
        return 0;
    }
    return 1;
}

sub is_matching_file {
    my ($self, $f) = @_;
    my $ext = plfind::FileUtil::get_extension($f);
    if (scalar @{$self->{settings}->{in_extensions}} &&
        !(grep {$_ eq $ext} @{$self->{settings}->{in_extensions}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_extensions}} &&
        (grep {$_ eq $ext} @{$self->{settings}->{out_extensions}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{in_filepatterns}} &&
        !$self->matches_any_pattern($f, $self->{settings}->{in_filepatterns})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_filepatterns}} &&
        $self->matches_any_pattern($f, $self->{settings}->{out_filepatterns})) {
        return 0;
    }
    my $type = $self->{filetypes}->get_filetype($f);
    if (scalar @{$self->{settings}->{in_filetypes}} &&
        !(grep {$_ eq $type} @{$self->{settings}->{in_filetypes}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_filetypes}} &&
        (grep {$_ eq $type} @{$self->{settings}->{out_filetypes}})) {
        return 0;
    }
    return 1;
}

sub is_matching_archive_file {
    my ($self, $f) = @_;
    my $ext = plfind::FileUtil::get_extension($f);
    if (scalar @{$self->{settings}->{in_archiveextensions}} &&
        !(grep {$_ eq $ext} @{$self->{settings}->{in_archiveextensions}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_archiveextensions}} &&
        (grep {$_ eq $ext} @{$self->{settings}->{out_archiveextensions}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{in_archivefilepatterns}} &&
        !$self->matches_any_pattern($f, $self->{settings}->{in_archivefilepatterns})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_archivefilepatterns}} &&
        $self->matches_any_pattern($f, $self->{settings}->{out_archivefilepatterns})) {
        return 0;
    }
    return 1;
}

sub filter_to_file_result {
    my ($self, $fp) = @_;
    my $d = dirname($fp);
    my $f = basename($fp);
    if ($self->{settings}->{excludehidden} && plfind::FileUtil::is_hidden($f)) {
        return;
    }
    my $ft = $self->{filetypes}->get_filetype($f);
    my $fileresult = new plfind::FileResult($d, $f, $ft);
    if ($ft eq plfind::FileType->ARCHIVE) {
        if ($self->{settings}->{includearchives} && $self->is_matching_archive_file($f)) {
            return $fileresult;
        }
        return;
    }
    if (!$self->{settings}->{archivesonly} && $self->is_matching_file($f)) {
        return $fileresult;
    }
    return;
}

sub get_dir_dir_results {
    # print "get_dir_dir_results\n";
    my ($self, $d) = @_;
    # print "d: $d\n";
    my $dirresults = [];
    opendir(DIR, $d) or die $!;
    while (my $f = readdir(DIR)) {
        my $subfile = File::Spec->join($d, $f);
        if (-d $subfile && !plfind::FileUtil::is_dot_dir($f) && $self->is_matching_dir($subfile)) {
            push(@{$dirresults}, $subfile);
        }
    }
    closedir(DIR);
    return $dirresults;
}

sub get_dir_file_results {
    # print "get_dir_file_results\n";
    my ($self, $d) = @_;
    # print "d: $d\n";
    my $fileresults = [];
    opendir(DIR, $d) or die $!;
    while (my $f = readdir(DIR)) {
        my $subfile = File::Spec->join($d, $f);
        if (-f $subfile) {
            my $fileresult = $self->filter_to_file_result($subfile);
            if (defined $fileresult) {
                push(@{$fileresults}, $fileresult);
            }
        }
    }
    closedir(DIR);
    return $fileresults;
}

sub rec_get_file_results {
    # print "rec_get_file_results\n";
    my ($self, $d) = @_;
    # print "d: $d\n";
    my $dirresults = $self->get_dir_dir_results($d);
    my $fileresults = $self->get_dir_file_results($d);
    foreach my $dirresult (@{$dirresults}) {
        my $subfileresults = $self->rec_get_file_results($dirresult);
        push(@{$fileresults}, @{$subfileresults});
    }
    return $fileresults;
}

sub find {
    my $self = shift;
    my $fileresults = [];
    foreach my $p (@{$self->{settings}->{paths}}) {
        if (-d $p) {
            if ($self->{settings}->{recursive}) {
                push(@{$fileresults}, @{$self->rec_get_file_results($p)});
            } else {
                push(@{$fileresults}, @{$self->get_dir_file_results($p)});
            }
        } elsif (-f $p) {
            my $fileresult = $self->filter_to_file_result($p);
            if (defined $fileresult) {
                push(@{$fileresults}, $fileresult);
            } else {
                plfind::common::log("ERROR: Startpath does not match find settings");
            }
        }
    }
    return $self->sort_file_results($fileresults);
}

sub cmp_file_results_by_path {
    my ($self, $fr1, $fr2) = @_;
    if ($fr1->{path} eq $fr2->{path}) {
        return $fr1->{filename} cmp $fr2->{filename};
    }
    return $fr1->{path} cmp $fr2->{path};
}

sub cmp_file_results_by_filename {
    my ($self, $fr1, $fr2) = @_;
    if ($fr1->{filename} eq $fr2->{filename}) {
        return $fr1->{path} cmp $fr2->{path};
    }
    return $fr1->{filename} cmp $fr2->{filename};
}

sub cmp_file_results_by_filetype {
    my ($self, $fr1, $fr2) = @_;
    if ($fr1->{filetype} eq $fr2->{filetype}) {
        return $self->cmp_file_results_by_path($fr1, $fr2);
    }
    return $fr1->{filetype} cmp $fr2->{filetype};
}

sub sort_file_results {
    my ($self, $fileresults) = @_;
    my @sorted;
    if ($self->{settings}->{sortby} eq plfind::SortBy->FILENAME) {
        @sorted = sort {$self->cmp_file_results_by_filename($a, $b)} @{$fileresults};
    } elsif ($self->{settings}->{sortby} eq plfind::SortBy->FILETYPE) {
        @sorted = sort {$self->cmp_file_results_by_filetype($a, $b)} @{$fileresults};
    } else {
        @sorted = sort {$self->cmp_file_results_by_path($a, $b)} @{$fileresults};
    }
    if ($self->{settings}->{sort_descending}) {
        @sorted = reverse @sorted;
    }
    return \@sorted;
}

sub get_matching_dirs {
    my ($self, $fileresults) = @_;
    my @dirs = map {$_->{path}} @{$fileresults};
    my $uniq = plfind::common::uniq(\@dirs);
    return $uniq;
}

sub print_matching_dirs {
    my ($self, $fileresults) = @_;
    my $dirs = $self->get_matching_dirs($fileresults);
    if (scalar @{$dirs}) {
        plfind::common::log(sprintf("\nMatching directories (%d):", scalar @{$dirs}));
        foreach my $d (@{$dirs}) {
            plfind::common::log($d);
        }
    } else {
        plfind::common::log("\nMatching directories: 0");
    }
}

sub get_matching_files {
    my ($self, $fileresults) = @_;
    my @files = map {File::Spec->join($_->{path}, $_->{filename})} @{$fileresults};
    return \@files;
}

sub print_matching_files {
    my ($self, $fileresults) = @_;
    my $files = $self->get_matching_files($fileresults);
    if (scalar @{$files}) {
        plfind::common::log(sprintf("\nMatching files (%d):", scalar @{$files}));
        foreach my $f (@{$files}) {
            plfind::common::log($f);
        }
    } else {
        plfind::common::log("\nMatching files: 0");
    }
}

1;

__END__
