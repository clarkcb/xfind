###############################################################################
#
# FileResultFormatter.pm
#
# Formats file results
#
###############################################################################

package plfind::FileResultFormatter;

use strict;
use warnings;

use Path::Class;

use plfind::Color;

sub new {
    my $class = shift;
    my $self = {
        settings => shift,
    };
    bless $self, $class;
    if ($self->{settings}->{colorize} && scalar @{$self->{settings}->{in_dir_patterns}} > 0) {
        $self->{format_dir} = sub { my ($_self, $dir) = @_; return $_self->format_dir_with_color($dir) };
    } else {
        $self->{format_dir} = sub { my ($_self, $dir) = @_; return $_self->format_dir_default($dir) };
    }
    if ($self->{settings}->{colorize} && (scalar @{$self->{settings}->{in_extensions}} > 0
        || scalar @{$self->{settings}->{in_file_patterns}} > 0)) {
        $self->{format_file_name} = sub { my ($_self, $file_name) = @_; return $_self->format_file_name_with_color($file_name) };
    } else {
        $self->{format_file_name} = sub { my ($_self, $file_name) = @_; return $_self->format_file_name_default($file_name) };
    }
    return $self;
}

sub colorize {
    my ($s, $match_start_index, $match_end_index) = @_;
    my $prefix = '';
    if ($match_start_index > 0) {
        $prefix = substr($s, 0, $match_start_index);
    }
    my $match_length = $match_end_index - $match_start_index;
    my $colorized = plfind::Color->GREEN . substr($s, $match_start_index, $match_length) . plfind::Color->RESET;
    my $suffix = '';
    # if ($match_end_index < length($s) - 1) {
    if ($match_end_index < length($s)) {
        $suffix = substr($s, $match_end_index);
    }
    return $prefix . $colorized . $suffix;
}

sub format_dir_with_color {
    # dir is a Path::Class instance
    my ($self, $dir) = @_;
    my $formatted_dir = '.';
    if (defined $dir && length($dir) > 0) {
        $formatted_dir = $dir->stringify;
        foreach my $p (@{$self->{settings}->{in_dir_patterns}}) {
            if ($formatted_dir =~ /$p/) {
                my $start_index = $-[0];
                my $end_index = $+[0];
                $formatted_dir = colorize($formatted_dir, $start_index, $end_index);
                last;
            }
        }
    }
    return $formatted_dir;
}

sub format_dir_default {
    # dir is a Path::Class instance
    my ($self, $dir) = @_;
    return $dir;
}

sub format_dir {
    my ($self, $dir) = @_;
    $self->{format_dir}->($self, $dir);
}

sub format_file_name_with_color {
    my ($self, $file_name) = @_;
    my $formatted_file_name = $file_name;
    foreach my $p (@{$self->{settings}->{in_file_patterns}}) {
        if ($formatted_file_name =~ /$p/) {
            my $start_index = $-[0];
            my $end_index = $+[0];
            $formatted_file_name = colorize($formatted_file_name, $start_index, $end_index);
            last;
        }
    }
    if (scalar @{$self->{settings}->{in_extensions}}) {
        my $idx = rindex($formatted_file_name, '.');
        my $file_name_len = length($formatted_file_name);
        if ($idx > 0 && $idx < $file_name_len - 1) {
            $formatted_file_name = colorize($formatted_file_name, $idx + 1, $file_name_len);
        }
    }
    return $formatted_file_name;
}

sub format_file_name_default {
    my ($self, $file_name) = @_;
    return $file_name;
}

sub format_file_name {
    my ($self, $file_name) = @_;
    $self->{format_file_name}->($self, $file_name);
}

sub format_path {
    # path is a Path::Class instance
    my ($self, $path) = @_;
    my $parent = $self->format_dir($path->parent);
    my $file_name = $self->format_file_name($path->basename);
    return file($parent, $file_name)->stringify;
}

sub format_file_result {
    my ($self, $result) = @_;
    return $self->format_path($result->{file_path});
}

1;

__END__
