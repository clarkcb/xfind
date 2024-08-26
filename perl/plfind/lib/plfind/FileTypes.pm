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

# use Data::Dumper;
use JSON::PP qw(decode_json);
use plfind::common;
use plfind::config;
use plfind::FileType;
use plfind::FileUtil;

sub get_json_file_type_hashes {
    my $file_type_ext_hash = {};
    my $file_type_name_hash = {};
    my $contents = $FILE_TYPES_PATH->slurp;
    my $json_file_type_hash = decode_json $contents;
    foreach my $file_type (@{$json_file_type_hash->{filetypes}}) {
        $file_type_ext_hash->{$file_type->{type}} = $file_type->{extensions};
        $file_type_name_hash->{$file_type->{type}} = $file_type->{names};
    }
    my @text_exts = (@{$file_type_ext_hash->{text}}, @{$file_type_ext_hash->{code}},
        @{$file_type_ext_hash->{xml}});
    $file_type_ext_hash->{text} = \@text_exts;
    my @text_names = (@{$file_type_name_hash->{text}}, @{$file_type_name_hash->{code}},
        @{$file_type_name_hash->{xml}});
    $file_type_name_hash->{text} = \@text_names;
    my $hashes = [];
    push (@{$hashes}, $file_type_ext_hash);
    push (@{$hashes}, $file_type_name_hash);

    return $hashes;
}

sub new {
    my $class = shift;
    my $hashes = get_json_file_type_hashes();
    my $self = {
        file_type_exts => $hashes->[0],
        file_type_names => $hashes->[1],
    };
    bless $self, $class;
    return $self;
}

sub get_file_type {
    my ($self, $file) = @_;
    # more specific first
    if ($self->is_code($file)) {
        return plfind::FileType->CODE;
    }
    if ($self->is_archive($file)) {
        return plfind::FileType->ARCHIVE;
    }
    if ($self->is_audio($file)) {
        return plfind::FileType->AUDIO;
    }
    if ($self->is_font($file)) {
        return plfind::FileType->FONT;
    }
    if ($self->is_image($file)) {
        return plfind::FileType->IMAGE;
    }
    if ($self->is_video($file)) {
        return plfind::FileType->VIDEO;
    }
    # more general last
    if ($self->is_xml($file)) {
        return plfind::FileType->XML;
    }
    if ($self->is_text($file)) {
        return plfind::FileType->TEXT;
    }
    if ($self->is_binary($file)) {
        return plfind::FileType->BINARY;
    }
    return plfind::FileType->UNKNOWN;
}

sub is_archive {
    my ($self, $file) = @_;
    if (grep {$_ eq $file} @{$self->{file_type_names}->{archive}}) {
        return 1;
    }
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_type_exts}->{archive}}) {
        return 1;
    }
    return 0;
}

sub is_audio {
    my ($self, $file) = @_;
    if (grep {$_ eq $file} @{$self->{file_type_names}->{audio}}) {
        return 1;
    }
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_type_exts}->{audio}}) {
        return 1;
    }
    return 0;
}

sub is_binary {
    my ($self, $file) = @_;
    if (grep {$_ eq $file} @{$self->{file_type_names}->{binary}}) {
        return 1;
    }
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_type_exts}->{binary}}) {
        return 1;
    }
    return 0;
}

sub is_code {
    my ($self, $file) = @_;
    if (grep {$_ eq $file} @{$self->{file_type_names}->{code}}) {
        return 1;
    }
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_type_exts}->{code}}) {
        return 1;
    }
    return 0;
}

sub is_font {
    my ($self, $file) = @_;
    if (grep {$_ eq $file} @{$self->{file_type_names}->{font}}) {
        return 1;
    }
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_type_exts}->{font}}) {
        return 1;
    }
    return 0;
}

sub is_image {
    my ($self, $file) = @_;
    if (grep {$_ eq $file} @{$self->{file_type_names}->{image}}) {
        return 1;
    }
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_type_exts}->{image}}) {
        return 1;
    }
    return 0;
}

sub is_text {
    my ($self, $file) = @_;
    if (grep {$_ eq $file} @{$self->{file_type_names}->{text}}) {
        return 1;
    }
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_type_exts}->{text}}) {
        return 1;
    }
    return 0;
}

sub is_video {
    my ($self, $file) = @_;
    if (grep {$_ eq $file} @{$self->{file_type_names}->{video}}) {
        return 1;
    }
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_type_exts}->{video}}) {
        return 1;
    }
    return 0;
}

sub is_xml {
    my ($self, $file) = @_;
    if (grep {$_ eq $file} @{$self->{file_type_names}->{xml}}) {
        return 1;
    }
    my $ext = plfind::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_type_exts}->{xml}}) {
        return 1;
    }
    return 0;
}

sub is_unknown {
    my ($self, $file) = @_;
    my $file_type = $self->get_file_type($file);
    if ($file_type eq plfind::FileType->UNKNOWN) {
        return 1;
    }
    return 0;
}

1;

__END__
