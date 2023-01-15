###############################################################################
#
# FindSettings.pm
#
# Encapsulates the find settings
#
###############################################################################

package plfind::FindSettings;

use plfind::FileTypes;
use plfind::SortBy;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        archivesonly => 0,
        debug => 0,
        excludehidden => 1,
        in_archiveextensions => [],
        in_archivefilepatterns => [],
        in_dirpatterns => [],
        in_extensions => [],
        in_filepatterns => [],
        in_filetypes => [],
        includearchives => 0,
        listdirs => 0,
        listfiles => 0,
        out_archiveextensions => [],
        out_archivefilepatterns => [],
        out_dirpatterns => [],
        out_extensions => [],
        out_filepatterns => [],
        out_filetypes => [],
        printusage => 0,
        printversion => 0,
        recursive => 1,
        sort_caseinsensitive => 0,
        sort_descending => 0,
        sortby => plfind::SortBy->FILEPATH,
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

sub aref_to_string {
    my ($self, $aref) = @_;
    my $s = '[';
    if (@$aref) {
        foreach my $i (0..$#{$aref}) {
            if ($i > 0) {
                $s .= ', ';
            }
            $s .= '"' . $aref->[$i] . '"';
        }
    }
    $s .= ']';
    return $s;
}

sub set_property {
    my ($self, $name, $val) = @_;
    $self->{$name} = $val;
    if ($val == 1) {
        if ($name eq 'archivesonly') {
            $self->{includearchives} = 1;
        } elsif ($name eq 'debug') {
            $self->{verbose} = 1;
        }
    }
}

sub set_sort_by {
    my ($self, $name) = @_;
    my $uname = uc($name);
    if ($uname eq 'NAME') {
        $self->{sortby} = plfind::SortBy->FILENAME;
    } elsif ($uname eq 'TYPE') {
        $self->{sortby} = plfind::SortBy->FILETYPE;
    } else {
        $self->{sortby} = plfind::SortBy->FILEPATH;
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

sub add_filetypes {
    my ($self, $filetypes, $ftaref) = @_;
    my $fts = [];
    if (ref($filetypes) eq 'ARRAY') {
        $fts = $filetypes;
    } else { # treat as a string
        my @split = split(',', $filetypes);
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

sub to_string {
    my $self = shift @_;
    my $s = "FindSettings(";
    $s .= 'archivesonly=' . $self->bool_to_string($self->{archivesonly});
    $s .= ', debug=' . $self->bool_to_string($self->{debug});
    $s .= ', excludehidden=' . $self->bool_to_string($self->{excludehidden});
    $s .= ', in_archiveextensions=' . $self->aref_to_string($self->{in_archiveextensions});
    $s .= ', in_archivefilepatterns=' . $self->aref_to_string($self->{in_archivefilepatterns});
    $s .= ', in_dirpatterns=' . $self->aref_to_string($self->{in_dirpatterns});
    $s .= ', in_extensions=' . $self->aref_to_string($self->{in_extensions});
    $s .= ', in_filepatterns=' . $self->aref_to_string($self->{in_filepatterns});
    $s .= ', in_filetypes=' . $self->aref_to_string($self->{in_filetypes});
    $s .= ', includearchives=' . $self->bool_to_string($self->{includearchives});
    $s .= ', listdirs=' . $self->bool_to_string($self->{listdirs});
    $s .= ', listfiles=' . $self->bool_to_string($self->{listfiles});
    $s .= ', out_archiveextensions=' . $self->aref_to_string($self->{out_archiveextensions});
    $s .= ', out_archivefilepatterns=' . $self->aref_to_string($self->{out_archivefilepatterns});
    $s .= ', out_dirpatterns=' . $self->aref_to_string($self->{out_dirpatterns});
    $s .= ', out_extensions=' . $self->aref_to_string($self->{out_extensions});
    $s .= ', out_filepatterns=' . $self->aref_to_string($self->{out_filepatterns});
    $s .= ', out_filetypes=' . $self->aref_to_string($self->{out_filetypes});
    $s .= ', printusage=' . $self->bool_to_string($self->{printusage});
    $s .= ', printversion=' . $self->bool_to_string($self->{printversion});
    $s .= ', recursive=' . $self->bool_to_string($self->{recursive});
    $s .= ', paths=' . $self->aref_to_string($self->{paths});
    $s .= ', sort_caseinsensitive=' . $self->bool_to_string($self->{sort_caseinsensitive});
    $s .= ', sort_descending=' . $self->bool_to_string($self->{sort_descending});
    $s .= ', sortby=' . $self->{sortby};
    $s .= ', verbose=' . $self->bool_to_string($self->{verbose});
    $s .= ')';
    return $s;
}

1;

__END__
