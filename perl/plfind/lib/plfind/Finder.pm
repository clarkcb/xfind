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
use plfind::FindResult;
use plfind::FindResultFormatter;

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
    if (!defined($self->{settings}->{startpath}) || length($self->{settings}->{startpath}) == 0) {
        push(@{$errs}, 'Startpath not defined');
    }
    unless (-e $self->{settings}->{startpath}) {
        push(@{$errs}, 'Startpath not found');
    }
    unless (-r $self->{settings}->{startpath}) {
        push(@{$errs}, 'Startpath not readable');
    }
    unless (scalar @{$self->{settings}->{findpatterns}}) {
        push(@{$errs}, 'No find patterns defined');
    }
    if ($self->{settings}->{linesafter} < 0) {
        push(@{$errs}, 'Invalid linesafter');
    }
    if ($self->{settings}->{linesbefore} < 0) {
        push(@{$errs}, 'Invalid linesbefore');
    }
    if ($self->{settings}->{maxlinelength} < 0) {
        push(@{$errs}, 'Invalid maxlinelength');
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
        if ($self->{settings}->{findarchives} && $self->is_archive_find_file($f)) {
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
        if ($subfile !~ m[/\.{1,2}$]) {
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

sub get_find_files {
    my $self = shift;
    my $findfiles = [];
    if (-d $self->{settings}->{startpath}) {
        push(@{$findfiles}, @{$self->rec_get_find_files($self->{settings}->{startpath})});
    } elsif (-f $self->{settings}->{startpath}) {
        if ($self->filter_file($self->{settings}->{startpath})) {
            push(@{$findfiles}, $self->{settings}->{startpath});
        } else {
            plfind::common::log("ERROR: Startpath does not match find settings");
        }
    }
    return $findfiles;
}

sub find {
    my $self = shift;
    my $findfiles = $self->get_find_files();
    if ($self->{settings}->{verbose}) {
        my @sortedfiles = sort @{$findfiles};
        my $sorted_dir_hash = {};
        foreach my $f (@sortedfiles) {
            $sorted_dir_hash->{dirname($f)} = 1;
        }
        my @sorteddirs = sort (keys %{$sorted_dir_hash});
        plfind::common::log(sprintf("\nDirectories to be found (%d):", scalar @sorteddirs));
        foreach my $d (@sorteddirs) {
            plfind::common::log($d);
        }
        plfind::common::log(sprintf("\nFiles to be found (%d):", scalar @sortedfiles));
        foreach my $f (@sortedfiles) {
            plfind::common::log($f);
        }
        plfind::common::log('');
    }

    # find the files
    foreach my $f (@{$findfiles}) {
        $self->find_file($f);
    }
}

sub find_file {
    my ($self, $f) = @_;
    my $type = $self->{filetypes}->get_filetype($f);
    if ($type eq plfind::FileType->TEXT || $type eq plfind::FileType->CODE || $type eq plfind::FileType->XML) {
        $self->find_text_file($f);
    } elsif ($type eq plfind::FileType->BINARY) {
        $self->find_binary_file($f);
    }
}

sub find_text_file {
    my ($self, $f) = @_;
    if ($self->{settings}->{debug}) {
        plfind::common::log("Finding text file $f");
    }
    if ($self->{settings}->{multilineoption-REMOVE}) {
        $self->find_text_file_contents($f);
    } else {
        $self->find_text_file_lines($f);
    }
}

sub find_text_file_contents {
    my ($self, $f) = @_;
    my $contents = plfind::FileUtil::get_file_contents($f);
    my $results = $self->find_multiline_string($contents);
    foreach my $r (@{$results}) {
        $r->{file} = $f;
        $self->add_find_result($r);
    }
}

sub get_new_line_indices {
    my ($self, $s) = @_;
    my @chars = split(//, $s);
    my $indices = [];
    my $i = 0;
    while ($i < scalar @chars) {
        my $c = $chars[$i];
        if ($c eq "\n") {
            push(@{$indices}, $i);
        }
        $i++;
    }
    return $indices;
}

sub print_array {
    my ($self, $aname, $aref) = @_;
    my $len = scalar @{$aref};
    print $aname . ' (' . $len . '): ';
    print "@{$aref}\n";
}

sub firstindex {
    my ($self, $val, $aref) = @_;
    my $i = 0;
    my $len = scalar @{$aref};
    while ($i < $len) {
        if ($aref->[$i] == $val) {
            return $i;
        }
        $i++;
    }
    return -1;
}

sub get_before_indices {
    my ($self, $indices, $after_index) = @_;
    my @before_indices = grep { $_ <= $after_index } @{$indices};
    return \@before_indices;
}

sub get_after_indices {
    my ($self, $indices, $before_index) = @_;
    my @after_indices = grep { $_ > $before_index } @{$indices};
    return \@after_indices;
}

sub get_lines_before {
    my ($self, $s, $before_start_indices, $start_line_indices, $end_line_indices) = @_;
    my $lines_before = [];
    my $i = 0;
    while ($i < scalar @{$before_start_indices} && $i < $self->{settings}->{linesbefore}) {
        my $start_index = $before_start_indices->[$i];
        my $end_index = $end_line_indices->[$self->firstindex($start_index, $start_line_indices)];
        my $line = substr($s, $start_index, $end_index - $start_index);
        push(@{$lines_before}, $line);
        $i++;
    }
    return $lines_before;
}

sub get_lines_after {
    my ($self, $s, $after_start_indices, $start_line_indices, $end_line_indices) = @_;
    my $lines_after = [];
    my $i = 0;
    while ($i < scalar @{$after_start_indices} && $i < $self->{settings}->{linesafter}) {
        my $start_index = $after_start_indices->[$i];
        my $end_index = $end_line_indices->[$self->firstindex($start_index, $start_line_indices)];
        my $line = substr($s, $start_index, $end_index - $start_index);
        push(@{$lines_after}, $line);
        $i++;
    }
    return $lines_after;
}

sub find_multiline_string {
    my ($self, $s) = @_;
    my $results = [];
    my $new_line_indices = $self->get_new_line_indices($s);
    #$self->print_array('new_line_indices', $new_line_indices);
    my @start_line_indices = map { $_ + 1 } @{$new_line_indices};
    unshift(@start_line_indices, 0);
    #$self->print_array('start_line_indices', \@start_line_indices);
    my @end_line_indices = (@{$new_line_indices}, (length($s)-1));
    #$self->print_array('end_line_indices', \@end_line_indices);
    foreach my $pattern (@{$self->{settings}->{findpatterns}}) {
        while ($s =~ /$pattern/go) {
            my $start_index = $-[0];
            my $end_index = $+[0];
            #print "match from $start_index to $end_index\n";
            my @before_start_indices = grep { $_ <= $start_index } @start_line_indices;
            #$self->print_array('before_start_indices', \@before_start_indices);
            my $m_line_start_index = 0;
            my $before_line_count = 0;
            if (@before_start_indices) {
                $before_line_count = (scalar @before_start_indices) - 1;
                $m_line_start_index = $before_start_indices[$before_line_count];
            }
            #print "before_line_count: $before_line_count\n";
            #print "m_line_start_index: $m_line_start_index\n";
            my $m_line_end_index = $end_line_indices[$self->firstindex($m_line_start_index, \@start_line_indices)];
            #print "m_line_end_index: $m_line_end_index\n";
            my $line = substr($s, $m_line_start_index, $m_line_end_index - $m_line_start_index);
            my $lines_before = [];
            if ($self->{settings}->{linesbefore}) {
                my $before_first_index = ($self->{settings}->{linesbefore} + 1) * -1;
                my @before_indices = @before_start_indices[$before_first_index .. -1];
                #$self->print_array('before_indices', \@before_indices);
                $lines_before = $self->get_lines_before(
                    $s,
                    \@before_indices,
                    \@start_line_indices,
                    \@end_line_indices);
            }
            #print "start_index: $start_index\n";
            my $lines_after = [];
            if ($self->{settings}->{linesafter}) {
                my $after_start_indices = $self->get_after_indices(\@start_line_indices, $start_index);
                my @after_indices = @{$after_start_indices}[0 .. $self->{settings}->{linesafter}];
                #$self->print_array('after_indices', \@after_indices);
                $lines_after = $self->get_lines_after(
                    $s,
                    \@after_indices,
                    \@start_line_indices,
                    \@end_line_indices);
            }
            if (($lines_before && !$self->lines_before_match($lines_before))
                ||
                ($lines_after && !$self->lines_after_match($lines_after))) {
                next;
            }
            my $r = new plfind::FindResult(
                $pattern,
                '',
                $before_line_count + 1,
                $start_index - $m_line_start_index + 1,
                $end_index - $m_line_start_index + 1,
                $line,
                $lines_before,
                $lines_after);
            push(@{$results}, $r);
            if ($self->{settings}->{firstmatch}) {
                last;
            }
        }
    }
    return $results;
}

sub find_text_file_lines {
    my ($self, $f) = @_;
    my $lines = plfind::FileUtil::get_file_lines($f);
    my $results = $self->find_lines($lines);
    foreach my $r (@{$results}) {
        $r->{file} = $f;
        $self->add_find_result($r);
    }
}

sub lines_match {
    my ($self, $lines, $in_patterns, $out_patterns) = @_;
    if ((scalar @{$in_patterns} == 0 || $self->any_matches_any_pattern($lines, $in_patterns))
        &&
        (scalar @{$out_patterns} == 0 || !$self->any_matches_any_pattern($lines, $out_patterns))) {
        return 1;
    }
    return 0;
}

sub lines_before_match {
    my ($self, $lines_before) = @_;
    return $self->lines_match($lines_before, $self->{settings}->{in_linesbeforepatterns},
        $self->{settings}->{out_linesbeforepatterns});
}

sub lines_after_match {
    my ($self, $lines_after) = @_;
    return $self->lines_match($lines_after, $self->{settings}->{in_linesafterpatterns},
        $self->{settings}->{out_linesafterpatterns});
}

sub find_lines {
    my ($self, $lines) = @_;
    my $linenum = 0;
    my $line = '';
    my $lines_before = [];
    my $lines_after = [];
    my $pattern_match_hash = {};
    my $results = [];
    my $find = 1;
    FINDLINES:
    while ($find) {
        if (scalar @{$lines_after} > 0) {
            $line = shift(@{$lines_after});
        } else {
            $line = shift(@{$lines});
        }
        if (!$line) {
            last FINDLINES;
        }
        $linenum++;
        if ($self->{settings}->{linesafter}) {
            while(scalar @{$lines_after} < $self->{settings}->{linesafter}) {
                my $line_after = shift(@{$lines});
                if (!$line_after) {
                    last FINDLINES;
                } else {
                    push(@{$lines_after}, $line_after);
                }
            }
        }
        foreach my $pattern (@{$self->{settings}->{findpatterns}}) {
            if ($self->{settings}->{firstmatch} && $pattern_match_hash->{$pattern}) {
                next;
            }
            if ($line =~ /$pattern/) {
                if ((scalar @{$lines_before} > 0 && !$self->lines_before_match($lines_before))
                    ||
                    (scalar @{$lines_after} > 0  && !$self->lines_after_match($lines_after))) {
                    next;
                }
                while ($line =~ /$pattern/go) {
                    my $start_index = $-[0];
                    my $end_index = $+[0];
                    # make copies of lines_before and lines_after
                    my @lb = (@{$lines_before});
                    my @la = (@{$lines_after});
                    my $r = new plfind::FindResult(
                        $pattern,
                        '',
                        $linenum,
                        $start_index + 1,
                        $end_index + 1,
                        $line,
                        \@lb,
                        \@la);
                    push(@{$results}, $r);
                    $pattern_match_hash->{$pattern} = 1;
                }
            }
        }
        if ($self->{settings}->{linesbefore}) {
            if (scalar @{$lines_before} == $self->{settings}->{linesbefore}) {
                shift(@{$lines_before});
            }
            if (scalar @{$lines_before} < $self->{settings}->{linesbefore}) {
                push(@{$lines_before}, $line);
            }
        }

        if (scalar @{$lines} == 0) {
            last;
            $find = 0;
        }
    }
    return $results;
}

sub find_binary_file {
    my ($self, $f) = @_;
    if ($self->{settings}->{debug}) {
        plfind::common::log("Finding binary file $f");
    }
    my $contents = plfind::FileUtil::get_file_contents($f);
    my $results = $self->find_binary_string($contents);
    foreach my $r (@{$results}) {
        $r->{file} = $f;
        $self->add_find_result($r);
    }
}

sub find_binary_string {
    my ($self, $s) = @_;
    my $results = [];
    foreach my $pattern (@{$self->{settings}->{findpatterns}}) {
        if ($s =~ /$pattern/s) {
            while ($s =~ /$pattern/go) {
                my $start_index = $-[0];
                my $end_index = $+[0];
                my $r = new plfind::FindResult(
                    $pattern,
                    '',
                    0,
                    $start_index + 1,
                    $end_index + 1,
                    '',
                    [],
                    []);
                push(@{$results}, $r);
                if ($self->{settings}->{firstmatch}) {
                    last;
                }
            }
        }
    }
    return $results;
}


sub add_find_result {
    my ($self, $r) = @_;
    push(@{$self->{results}}, $r);
}

sub sort_results ($$) {
    my $a = $_[0];
    my $b = $_[1];
    if ($a->{file} eq $b->{file}) {
        if ($a->{linenum} == $b->{linenum}) {
            $a->{match_start_index} <=> $b->{match_start_index}
        } else {
            $a->{linenum} <=> $b->{linenum}
        }
    } else {
        $a->{file} cmp $b->{file}
    }
}

sub print_results {
    my ($self) = @_;
    my $len = scalar @{$self->{results}};
    my @sorted = sort sort_results @{$self->{results}};
    my $formatter = new plfind::FindResultFormatter($self->{settings});

    plfind::common::log("Find results ($len):");
    # foreach my $r (@{$self->{results}}) {
    foreach my $r (@sorted) {
        # plfind::common::log($r->to_string());
        plfind::common::log($formatter->format($r));
    }
}

sub get_matching_dirs {
    my ($self) = @_;
    my $dir_hash = {};
    foreach my $r (@{$self->{results}}) {
        my $d = dirname($r->{file});
        $dir_hash->{$d}++;
    }
    my @dirs = keys %{$dir_hash};
    @dirs = sort(@dirs);
    return \@dirs;
}

sub print_matching_dirs {
    my ($self) = @_;
    my $dirs = $self->get_matching_dirs();
    plfind::common::log(sprintf("\nDirectories with matches (%d):", scalar @{$dirs}));
    foreach my $d (@{$dirs}) {
        plfind::common::log($d);
    }
}

sub get_matching_files {
    my ($self) = @_;
    my $file_hash = {};
    foreach my $r (@{$self->{results}}) {
        my $f = $r->{file};
        $file_hash->{$f}++;
    }
    my @files = keys %{$file_hash};
    @files = sort(@files);
    return \@files;
}

sub print_matching_files {
    my ($self) = @_;
    my $files = $self->get_matching_files();
    plfind::common::log(sprintf("\nFiles with matches (%d):", scalar @{$files}));
    foreach my $f (@{$files}) {
        plfind::common::log($f);
    }
}

sub get_matching_lines {
    my ($self) = @_;
    my $line_hash = {};
    my @lines;
    foreach my $r (@{$self->{results}}) {
        my $l = plfind::common::trim($r->{line});
        $line_hash->{$l}++;
        push(@lines, $l);
    }
    if ($self->{settings}->{uniquelines}) {
        @lines = keys %{$line_hash};
    }
    @lines = sort {uc($a) cmp uc($b)} @lines;
    return \@lines;
}

sub print_matching_lines {
    my ($self) = @_;
    my $lines = $self->get_matching_lines();
    my $msg = "\nLines with matches (%d):";
    if ($self->{settings}->{uniquelines}) {
        $msg = "\nUnique lines with matches (%d):";
    }
    plfind::common::log(sprintf($msg, scalar @{$lines}));
    foreach my $l (@{$lines}) {
        plfind::common::log($l);
    }
}

1;

__END__
