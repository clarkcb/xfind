###############################################################################
#
# ArgTokenizer.pm
#
# Tokenize args into tokens
#
###############################################################################

package plfind::ArgTokenizer;

use strict;
use warnings;

use JSON::PP qw(decode_json);
use Path::Class;

use plfind::ArgToken;
use plfind::ArgTokenType;

sub new {
    my ($class, $bool_hash, $str_hash, $int_hash) = @_;
    my $self = {
        bool_hash => $bool_hash,
        str_hash => $str_hash,
        int_hash => $int_hash
    };
    bless $self, $class;
    return $self;
}

sub tokenize_args {
    my ($self, $args) = @_;
    my @arg_tokens;
    my @errs;
    while (scalar @$args && !(scalar @errs)) {
        my $arg = shift @$args;
        if ($arg =~ /^\-+/) {
            my @arg_names;
            if ($arg =~ /^\-{2}/) {
                if (length($arg) > 2) {
                    $arg =~ s/^\-+//;
                    if ($arg =~ /\=/) {
                        my @elems = split('=', $arg);
                        $arg = $elems[0];
                        unshift(@$args, $elems[1]);
                    }
                    push(@arg_names, $arg);
                } else {
                    push(@errs, "Invalid option: $arg");
                    last;
                }
            } elsif (length($arg) > 1) {
                $arg =~ s/^\-+//;
                for my $c (split //, $arg) {
                    if (exists $self->{bool_hash}->{$c}) {
                        my $long_arg = $self->{bool_hash}->{$c};
                        push(@arg_names, $long_arg);
                    } elsif (exists $self->{str_hash}->{$c}) {
                        my $long_arg = $self->{str_hash}->{$c};
                        push(@arg_names, $long_arg);
                    } elsif (exists $self->{int_hash}->{$c}) {
                        my $long_arg = $self->{int_hash}->{$c};
                        push(@arg_names, $long_arg);
                    } else {
                        push(@errs, "Invalid option: $c");
                        last;
                    }
                }
            } else {
                push(@errs, "Invalid option: $arg");
                last;
            }

            foreach my $arg_name (@arg_names) {
                if (exists $self->{bool_hash}->{$arg_name}) {
                    my $arg_token = plfind::ArgToken->new($self->{bool_hash}->{$arg_name}, plfind::ArgTokenType->BOOL, 1);
                    push(@arg_tokens, $arg_token);
                } elsif (exists $self->{str_hash}->{$arg_name}
                    || exists $self->{int_hash}->{$arg_name}
                    || $arg_name eq 'settings-file') {
                    if (scalar @$args) {
                        my $val = shift @$args;
                        if (exists $self->{str_hash}->{$arg_name}) {
                            my $arg_token = plfind::ArgToken->new($self->{str_hash}->{$arg_name}, plfind::ArgTokenType->STR, $val);
                            push(@arg_tokens, $arg_token);
                        } elsif (exists $self->{int_hash}->{$arg_name}) {
                            if ($val =~ /^\d+$/) {
                                my $arg_token = plfind::ArgToken->new($self->{int_hash}->{$arg_name}, plfind::ArgTokenType->INT, int($val));
                                push(@arg_tokens, $arg_token);
                            } else {
                                push(@errs, 'Invalid value for option: ' . $arg_name);
                            }
                        } else {
                            my $arg_token = plfind::ArgToken->new('settings-file', plfind::ArgTokenType->STR, $val);
                            push(@arg_tokens, $arg_token);
                        }
                    } else {
                        push(@errs, "Missing value for $arg");
                    }
                }
            }
        } else {
            my $arg_token = plfind::ArgToken->new('path', plfind::ArgTokenType->STR, $arg);
            push(@arg_tokens, $arg_token);
        }
    }
    return (\@arg_tokens, \@errs);
}

sub tokenize_arg_hash {
    my ($self, $arg_hash) = @_;
    my @arg_tokens;
    my @errs;
    # keys are sorted so that output is consistent across all versions
    my @arg_names = sort (keys %{$arg_hash});
    while (scalar @arg_names && !(scalar @errs)) {
        my $arg_name = shift @arg_names;
        my $arg_value = $arg_hash->{$arg_name};
        if (exists $self->{bool_hash}->{$arg_name}) {
            if (plfind::common::is_bool($arg_value)) {
                my $arg_token = plfind::ArgToken->new($self->{bool_hash}->{$arg_name}, plfind::ArgTokenType->BOOL, $arg_value);
                push(@arg_tokens, $arg_token);
            } else {
                push(@errs, 'Invalid value for option: ' . $arg_name);
            }
        } elsif (exists $self->{str_hash}->{$arg_name}) {
            if (ref $arg_value eq 'ARRAY') {
                foreach my $val (@$arg_value) {
                    my $arg_token = plfind::ArgToken->new($self->{str_hash}->{$arg_name}, plfind::ArgTokenType->STR, $val);
                    push(@arg_tokens, $arg_token);
                }
            } else {
                # assume scalar
                my $arg_token = plfind::ArgToken->new($self->{str_hash}->{$arg_name}, plfind::ArgTokenType->STR, $arg_value);
                push(@arg_tokens, $arg_token);
            }
        } elsif (exists $self->{int_hash}->{$arg_name}) {
            if ($arg_value =~ /^\d+$/) {
                my $arg_token = plfind::ArgToken->new($self->{int_hash}->{$arg_name}, plfind::ArgTokenType->INT, $arg_value);
                push(@arg_tokens, $arg_token);
            } else {
                push(@errs, 'Invalid value for option: ' . $arg_name);
            }
        } elsif ($arg_name eq 'settings-file') {
            my $arg_token = plfind::ArgToken->new('settings-file', plfind::ArgTokenType->STR, $arg_value);
            push(@arg_tokens, $arg_token);
        } else {
            # should never reach here
            push(@errs, 'Invalid option: ' . $arg_name);
        }
    }
    return (\@arg_tokens, \@errs);
}

sub tokenize_json {
    my ($self, $json) = @_;
    my @arg_tokens;
    my @errs;
    my $json_hash = {};
    my $rc = eval { $json_hash = decode_json $json; 1; };
    if ($rc != 1) {
        push(@errs, 'Unable to decode json');
        return (\@arg_tokens, \@errs);
    }
    return $self->tokenize_arg_hash($json_hash);
}

sub tokenize_file {
    my ($self, $file_path) = @_;
    my @arg_tokens;
    my @errs;
    my $expanded_path = file(plfind::FileUtil::expand_path($file_path));
    unless (-e $expanded_path) {
        push(@errs, 'Settings file not found: ' . $file_path);
        return (\@arg_tokens, \@errs);
    }
    unless ($expanded_path =~ /\.json$/) {
        push(@errs, 'Invalid settings file (must be JSON): ' . $file_path);
        return (\@arg_tokens, \@errs);
    }
    my $json = $expanded_path->slurp;
    return $self->tokenize_json($json);
}

1;

__END__
