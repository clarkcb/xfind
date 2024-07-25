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

use Data::Dumper;
use DBD::SQLite;
use DBI qw(:sql_types);
use plfind::common;
use plfind::config;
use plfind::FileType;
use plfind::FileUtil;

sub new {
    my $class = shift;
    my $db = DBI->connect("dbi:SQLite:dbname=$XFIND_DB","","", {
        sqlite_open_flags => DBD::SQLite::OPEN_READONLY,
        ReadOnly => 1
    });
    my $file_types = [
        plfind::FileType->UNKNOWN,
        plfind::FileType->ARCHIVE,
        plfind::FileType->AUDIO,
        plfind::FileType->BINARY,
        plfind::FileType->CODE,
        plfind::FileType->FONT,
        plfind::FileType->IMAGE,
        plfind::FileType->TEXT,
        plfind::FileType->VIDEO,
        plfind::FileType->XML,
    ];
    my $self = {
        db => $db,
        file_types => $file_types,
        ext_type_cache => {},
    };
    bless $self, $class;
    return $self;
}

sub from_name {
    my ($name) = @_;
    my $lname = lc($name);
    if ($lname eq 'archive') {
        return plfind::FileType->ARCHIVE;
    }
    if ($lname eq 'audio') {
        return plfind::FileType->AUDIO;
    }
    if ($lname eq 'binary') {
        return plfind::FileType->BINARY;
    }
    if ($lname eq 'code') {
        return plfind::FileType->CODE;
    }
    if ($lname eq 'font') {
        return plfind::FileType->FONT;
    }
    if ($lname eq 'image') {
        return plfind::FileType->IMAGE;
    }
    if ($lname eq 'text') {
        return plfind::FileType->TEXT;
    }
    if ($lname eq 'video') {
        return plfind::FileType->VIDEO;
    }
    if ($lname eq 'xml') {
        return plfind::FileType->XML;
    }
    return plfind::FileType->UNKNOWN;
}

sub get_file_type_for_statement {
    my ($self, $statement) = @_;
    $statement->execute();
    my $row = $statement->fetch;
    if ($row) {
        my $file_type_id = $row->[0] - 1;
        return $self->{file_types}->[$file_type_id];
    }
    return plfind::FileType->UNKNOWN;
}

sub get_file_type_from_file_name {
    my ($self, $file_name) = @_;
    my $statement = $self->{db}->prepare("SELECT file_type_id FROM file_name WHERE name = ?");
    $statement->bind_param(1, $file_name, SQL_VARCHAR);
    return $self->get_file_type_for_statement($statement);
}

sub get_file_type_from_extension {
    my ($self, $file_ext) = @_;
    if (exists $self->{ext_type_cache}->{$file_ext}) {
        return $self->{ext_type_cache}->{$file_ext};
    }
    my $statement = $self->{db}->prepare("SELECT file_type_id FROM file_extension WHERE extension = ?");
    $statement->bind_param(1, $file_ext, SQL_VARCHAR);
    return $self->get_file_type_for_statement($statement);
}

sub get_file_type {
    my ($self, $file_name) = @_;
    my $file_type_for_file_name = $self->get_file_type_from_file_name($file_name);
    if ($file_type_for_file_name ne plfind::FileType->UNKNOWN) {
        return $file_type_for_file_name;
    }
    my $file_type_for_extension = $self->get_file_type_from_extension(plfind::FileUtil::get_extension($file_name));
    return $file_type_for_extension;
}

sub is_archive {
    my ($self, $file) = @_;
    return $self->get_file_type($file) == plfind::FileType->ARCHIVE ? 1 : 0;
}

sub is_audio {
    my ($self, $file) = @_;
    return $self->get_file_type($file) == plfind::FileType->AUDIO ? 1 : 0;
}

sub is_binary {
    my ($self, $file) = @_;
    return $self->get_file_type($file) == plfind::FileType->BINARY ? 1 : 0;
}

sub is_code {
    my ($self, $file) = @_;
    return $self->get_file_type($file) == plfind::FileType->CODE ? 1 : 0;
}

sub is_font {
    my ($self, $file) = @_;
    return $self->get_file_type($file) == plfind::FileType->FONT ? 1 : 0;
}

sub is_image {
    my ($self, $file) = @_;
    return $self->get_file_type($file) == plfind::FileType->IMAGE ? 1 : 0;
}

sub is_text {
    my ($self, $file) = @_;
    my $file_type = $self->get_file_type($file);
    if ($file_type == plfind::FileType->TEXT
        || $file_type == plfind::FileType->CODE
        || $file_type == plfind::FileType->XML) {
        return 1;
    }
    return 0;
}

sub is_video {
    my ($self, $file) = @_;
    return $self->get_file_type($file) == plfind::FileType->VIDEO ? 1 : 0;
}

sub is_xml {
    my ($self, $file) = @_;
    return $self->get_file_type($file) == plfind::FileType->XML ? 1 : 0;
}

sub is_unknown {
    my ($self, $file) = @_;
    return $self->get_file_type($file) == plfind::FileType->UNKNOWN ? 1 : 0;
}

1;

__END__
