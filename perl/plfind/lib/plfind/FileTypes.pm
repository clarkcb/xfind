###############################################################################
#
# FileTypes.pm
#
# Helper class for file types
#
###############################################################################

package plfind::FileTypes;

use strict;
use warnings;

# use XML::Simple;
use Data::Dumper;
use JSON::PP qw(decode_json);
use plfind::common;
use plfind::config;
use plfind::FileType;
use plfind::FileUtil;

sub get_xml_file_type_hash {
    my $file_type_hash = {};
    my $file_type_xml_hash = XMLin($FILETYPESPATH);
    $file_type_xml_hash = $file_type_xml_hash->{filetype};
    my @types = keys %{$file_type_xml_hash};
    foreach my $t (@types) {
        my @exts = split(/\s+/, $file_type_xml_hash->{$t}->{extensions});
        $file_type_hash->{$t} = \@exts;
    }
    my @text = (@{$file_type_hash->{text}}, @{$file_type_hash->{code}},
        @{$file_type_hash->{xml}});
    $file_type_hash->{text} = \@text;
    return $file_type_hash;
}

sub get_json_file_type_hash {
    # print "get_json_file_type_hash\n";
    my $file_type_hash = {};
    my $json_file_type_hash = decode_json plfind::FileUtil::get_file_contents($FILETYPESPATH);
    foreach my $file_type (@{$json_file_type_hash->{filetypes}}) {
        $file_type_hash->{$file_type->{type}} = $file_type->{extensions};
    }
    my @text = (@{$file_type_hash->{text}}, @{$file_type_hash->{code}},
        @{$file_type_hash->{xml}});
    $file_type_hash->{text} = \@text;
    return $file_type_hash;
}

sub new {
    my $class = shift;
    my $self = {
        # file_types => get_xml_file_type_hash(),
        file_types => get_json_file_type_hash(),
    };
    bless $self, $class;
    return $self;
}

sub from_name {
    my ($name) = @_;
    my $uname = uc($name);
    if ($uname eq 'CODE') {
        return plfind::FileType->CODE;
    }
    if ($uname eq 'XML') {
        return plfind::FileType->XML;
    }
    if ($uname eq 'TEXT') {
        return plfind::FileType->TEXT;
    }
    if ($uname eq 'BINARY') {
        return plfind::FileType->BINARY;
    }
    if ($uname eq 'ARCHIVE') {
        return plfind::FileType->ARCHIVE;
    }
    return plfind::FileType->UNKNOWN;
}

sub get_filetype {
    my ($self, $file) = @_;
    if ($self->is_code($file)) {
        return plfind::FileType->CODE;
    }
    if ($self->is_xml($file)) {
        return plfind::FileType->XML;
    }
    if ($self->is_text($file)) {
        return plfind::FileType->TEXT;
    }
    if ($self->is_binary($file)) {
        return plfind::FileType->BINARY;
    }
    if ($self->is_archive($file)) {
        return plfind::FileType->ARCHIVE;
    }
    return plfind::FileType->UNKNOWN;
}

sub is_archive {
    my ($self, $file) = @_;
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{archive}}) {
        return 1;
    }
    return 0;
}

sub is_binary {
    my ($self, $file) = @_;
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{binary}}) {
        return 1;
    }
    return 0;
}

sub is_code {
    my ($self, $file) = @_;
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{code}}) {
        return 1;
    }
    return 0;
}

sub is_text {
    my ($self, $file) = @_;
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{text}}) {
        return 1;
    }
    return 0;
}

sub is_xml {
    my ($self, $file) = @_;
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{xml}}) {
        return 1;
    }
    return 0;
}

sub is_unknown {
    my ($self, $file) = @_;
    my $file_type = $self->get_filetype($file);
    if ($file_type eq plfind::FileType->UNKNOWN) {
        return 1;
    }
    return 0;
}

1;

__END__
