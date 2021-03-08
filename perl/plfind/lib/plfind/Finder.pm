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

sub is_find_dir {
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

sub is_find_file {
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

sub is_archive_find_file {
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

sub rec_get_find_dirs {
    my ($self, $d) = @_;
    my $finddirs = [];
    opendir(DIR, $d) or die $!;
    while (my $f = readdir(DIR)) {
        my $subfile = File::Spec->join($d, $f);
        if (-d $subfile && !plfind::FileUtil::is_dot_dir($f) && $self->is_find_dir($subfile)) {
            push(@{$finddirs}, $subfile);
        }
    }
    closedir(DIR);
    foreach my $finddir (@{$finddirs}) {
        my @merged = (@{$finddirs}, @{$self->rec_get_find_dirs($finddir)});
        $finddirs = \@merged;
    }
    return $finddirs;
}

sub filter_file {
    my ($self, $f) = @_;
    if ($self->{settings}->{excludehidden} && plfind::FileUtil::is_hidden(basename($f))) {
        return 0;
    }
    if ($self->{filetypes}->is_archive($f)) {
        if ($self->{settings}->{includearchives} && $self->is_archive_find_file($f)) {
            return 1;
        }
        return 0;
    }
    return !$self->{settings}->{archivesonly} && $self->is_find_file($f);
}

sub rec_get_find_files {
    # print "rec_get_find_files\n";
    my ($self, $d) = @_;
    # print "d: $d\n";
    my $finddirs = [];
    my $findfiles = [];
    opendir(DIR, $d) or die $!;
    while (my $f = readdir(DIR)) {
        my $subfile = File::Spec->join($d, $f);
        if ($subfile !~ /\/\.{1,2}$/) {
            if (-d $subfile && $self->is_find_dir($f)) {
                # print "-d $subfile\n";
                push(@{$finddirs}, $subfile);
            } elsif (-f $subfile && $self->filter_file($f)) {
                # print "-f $subfile\n";
                push(@{$findfiles}, $subfile);
            }
        }
    }
    closedir(DIR);
    foreach my $finddir (@{$finddirs}) {
        my $subfindfiles = $self->rec_get_find_files($finddir);
        push(@{$findfiles}, @{$subfindfiles});
    }

    return $findfiles;
}

sub find {
    my $self = shift;
    my $findfiles = [];
    foreach my $p (@{$self->{settings}->{paths}}) {
        if (-d $p) {
            push(@{$findfiles}, @{$self->rec_get_find_files($p)});
        } elsif (-f $p) {
            if ($self->filter_file($p)) {
                push(@{$findfiles}, $p);
            } else {
                plfind::common::log("ERROR: Startpath does not match find settings");
            }
        }
    }
    return $findfiles;
}

sub get_matching_dirs {
    my ($self, $findfiles) = @_;
    my $dir_hash = {};
    foreach my $ff (@{$findfiles}) {
        my $d = dirname($ff);
        $dir_hash->{$d}++;
    }
    my @dirs = keys %{$dir_hash};
    @dirs = sort(@dirs);
    return \@dirs;
}

sub print_matching_dirs {
    my ($self, $findfiles) = @_;
    my $dirs = $self->get_matching_dirs($findfiles);
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
    my ($self, $findfiles) = @_;
    my $file_hash = {};
    foreach my $ff (@{$findfiles}) {
        $file_hash->{$ff}++;
    }
    my @files = keys %{$file_hash};
    @files = sort(@files);
    return \@files;
}

sub print_matching_files {
    my ($self, $findfiles) = @_;
    my $files = $self->get_matching_files($findfiles);
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
