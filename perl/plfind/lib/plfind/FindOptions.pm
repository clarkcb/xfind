###############################################################################
#
# FindOptions.pm
#
# Helper class for find options
#
###############################################################################

package plfind::FindOptions;

use strict;
use warnings;

# use Data::Dumper;
use DateTime::Format::DateParse;
use JSON::PP qw(decode_json);
use Path::Class;

use plfind::common;
use plfind::config;
use plfind::FileUtil;
use plfind::FindOption;
use plfind::FindSettings;

my $bool_action_hash = {
    'archivesonly' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('archives_only', $bool);
    },
    'debug' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('debug', $bool);
    },
    'excludearchives' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('include_archives', !$bool);
    },
    'excludehidden' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('include_hidden', !$bool);
    },
    'followsymlinks' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('follow_symlinks', $bool);
    },
    'help' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_usage', $bool);
    },
    'includearchives' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('include_archives', $bool);
    },
    'includehidden' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('include_hidden', $bool);
    },
    'nofollowsymlinks' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('follow_symlinks', !$bool);
    },
    'noprintdirs' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_dirs', !$bool);
    },
    'noprintfiles' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_files', !$bool);
    },
    'norecursive' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('recursive', !$bool);
    },
    'printdirs' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_dirs', $bool);
    },
    'printfiles' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_files', $bool);
    },
    'recursive' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('recursive', $bool);
    },
    'sort-ascending' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('sort_descending', !$bool);
    },
    'sort-caseinsensitive' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('sort_case_insensitive', $bool);
    },
    'sort-casesensitive' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('sort_case_insensitive', !$bool);
    },
    'sort-descending' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('sort_descending', $bool);
    },
    'verbose' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('verbose', $bool);
    },
    'version' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_version', $bool);
    }
};

my $str_action_hash = {
    'in-archiveext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{in_archive_extensions});
    },
    'in-archivefilepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_archive_file_patterns});
    },
    'in-dirpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_dir_patterns});
    },
    'in-ext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{in_extensions});
    },
    'in-filepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_file_patterns});
    },
    'in-filetype' => sub {
        my ($s, $settings) = @_;
        $settings->add_file_types($s, $settings->{in_file_types});
    },
    'maxlastmod' => sub {
        my ($s, $settings) = @_;
        $settings->{max_last_mod} = DateTime::Format::DateParse->parse_datetime($s);
    },
    'minlastmod' => sub {
        my ($s, $settings) = @_;
        $settings->{min_last_mod} = DateTime::Format::DateParse->parse_datetime($s);
    },
    'out-archiveext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{out_archive_extensions});
    },
    'out-archivefilepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_archive_patterns});
    },
    'out-dirpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_dir_patterns});
    },
    'out-ext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{out_extensions});
    },
    'out-filepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_file_patterns});
    },
    'out-filetype' => sub {
        my ($s, $settings) = @_;
        $settings->add_file_types($s, $settings->{out_file_types});
    },
    'path' => sub {
        my ($s, $settings) = @_;
        $settings->add_paths($s);
    },
    'sort-by' => sub {
        my ($s, $settings) = @_;
        $settings->set_sort_by($s);
    },
};

my $int_action_hash = {
    'maxdepth' => sub {
        my ($i, $settings) = @_;
        $settings->{max_depth} = $i;
    },
    'maxsize' => sub {
        my ($i, $settings) = @_;
        $settings->{max_size} = $i;
    },
    'mindepth' => sub {
        my ($i, $settings) = @_;
        $settings->{min_depth} = $i;
    },
    'minsize' => sub {
        my ($i, $settings) = @_;
        $settings->{min_size} = $i;
    },
};

sub new {
    my $class = shift;
    my $self = {
        options => set_options_from_json(),
    };
    bless $self, $class;
    return $self;
}

sub set_options_from_json {
    my $options_hash = {};
    my $contents = $FIND_OPTIONS_PATH->slurp;
    my $options_json_hash = decode_json $contents;
    foreach my $find_option (@{$options_json_hash->{findoptions}}) {
        my $short = $find_option->{short};
        my $long = $find_option->{long};
        my $desc = $find_option->{desc};
        my $opt = plfind::FindOption->new($short, $long, $desc);
        $options_hash->{$long} = $opt;
        if (defined $short) {
            $options_hash->{$short} = $options_hash->{$long};
        }
    }
    return $options_hash;
}

sub update_settings_from_json {
    my ($self, $json, $settings) = @_;
    my $errs = [];
    my $json_hash = decode_json $json;
    # keys are sorted so that output is consistent across all versions
    my @keys = sort (keys %{$json_hash});
    my @invalid_keys = grep { $_ ne 'path' && !exists($self->{options}->{$_}) } @keys;
    if (scalar @invalid_keys) {
        push(@$errs, 'Invalid option: ' . $invalid_keys[0]);
        return $errs;
    }
    foreach my $k (@keys) {
        if (exists $bool_action_hash->{$k}) {
            if (plfind::common::is_bool($json_hash->{$k})) {
                &{$bool_action_hash->{$k}}($json_hash->{$k}, $settings);
            } else {
                push(@$errs, 'Invalid value for option: ' . $k);
            }
        } elsif (exists $str_action_hash->{$k}) {
            &{$str_action_hash->{$k}}($json_hash->{$k}, $settings);
        } elsif (exists $int_action_hash->{$k}) {
            if ($json_hash->{$k} =~ /^\d+$/) {
                &{$int_action_hash->{$k}}($json_hash->{$k}, $settings);
            } else {
                push(@$errs, 'Invalid value for option: ' . $k);
            }
        } else {
            # should never reach here
            push(@$errs, 'Invalid option: ' . $k);
        }
    }
    return $errs;
}

sub update_settings_from_file {
    # $file_path is instance of Path::Class::File
    my ($self, $file_path, $settings) = @_;
    my $errs = [];
    my $expanded_path = file(plfind::FileUtil::expand_path($file_path));
    unless (-e $expanded_path) {
        push(@$errs, 'Settings file not found: ' . $file_path);
        return $errs;
    }
    unless ($expanded_path =~ /\.json$/) {
        push(@$errs, 'Invalid settings file (must be JSON): ' . $file_path);
        return $errs;
    }
    my $json = $expanded_path->slurp;
    my $rc = eval { $errs = $self->update_settings_from_json($json, $settings); 1; };
    if (!$rc) {
        push(@$errs, 'Unable to parse JSON in settings file: ' . $file_path);
    }
    return $errs;
}

sub settings_from_args {
    my ($self, $args) = @_;
    my $settings = plfind::FindSettings->new();
    # default print_files to true since running as cli
    $settings->set_property('print_files', 1);
    my @errs;
    while (scalar @$args && !(scalar @errs)) {
        my $arg = shift @$args;
        if ($arg =~ /^\-+/) {
            $arg =~ s/^\-+//;
            if (exists $self->{options}->{$arg}) {
                my $opt = $self->{options}->{$arg};
                my $long_arg = $opt->{long_arg};
                if (exists $bool_action_hash->{$long_arg}) {
                    &{$bool_action_hash->{$long_arg}}(1, $settings);
                } elsif (exists $str_action_hash->{$long_arg}
                         || exists $int_action_hash->{$long_arg}
                         || $long_arg eq 'settings-file') {
                    if (scalar @$args) {
                        my $val = shift @$args;
                        if (exists $str_action_hash->{$long_arg}) {
                            &{$str_action_hash->{$long_arg}}($val, $settings);
                        } elsif (exists $int_action_hash->{$long_arg}) {
                            &{$int_action_hash->{$long_arg}}(int($val), $settings);
                        } else {
                            my $file_path = file($val);
                            my $settings_file_errors = $self->update_settings_from_file($file_path, $settings);
                            push(@errs, @$settings_file_errors);
                        }
                    } else {
                        push(@errs, "Missing value for $arg");
                    }
                }
            } else {
                push(@errs, "Invalid option: $arg");
            }
        } else {
            $settings->add_path($arg);
        }
    }
    return ($settings, \@errs);
}

sub usage {
    my $self = shift;
    print $self->get_usage_string();
}

sub get_usage_string {
    my $self = shift;
    my $usage = "Usage:\n plfind [options] <path> [<path> ...]\n\nOptions:\n";
    my $longest = 0;
    my $sort_arg_option_hash = {};
    foreach my $opt_key (keys %{$self->{options}}) {
        my $option = $self->{options}->{$opt_key};
        my $long_arg = $option->{long_arg};
        my $short_arg = $option->{short_arg};
        my $sort_arg = $long_arg;
        if (defined $short_arg) {
            $sort_arg = lc($short_arg) . 'a' . $long_arg;
        }
        $sort_arg_option_hash->{$sort_arg} = $option;
    }
    my @sort_args = keys %{$sort_arg_option_hash};
    @sort_args = sort {$plfind::FindOptions::a cmp $plfind::FindOptions::b} @sort_args;
    my $opt_strs_with_key = {};
    my $opt_descs_with_key = {};
    foreach my $sort_arg (@sort_args) {
        my $option = $sort_arg_option_hash->{$sort_arg};
        my $opt_str = '';
        if ($option->{short_arg}) {
            $opt_str = '-' . $option->{short_arg} . ',';
        }
        $opt_str .= '--' . $option->{long_arg};
        if (length($opt_str) > $longest) {
            $longest = length($opt_str);
        }
        $opt_strs_with_key->{$sort_arg} = $opt_str;
        $opt_descs_with_key->{$sort_arg} = $option->{desc};
    }
    my $format_str = " %-" . $longest . "s  %s\n";
    foreach my $sort_arg (@sort_args) {
       $usage .= sprintf($format_str, $opt_strs_with_key->{$sort_arg},
        $opt_descs_with_key->{$sort_arg});
    }
    return $usage;
}

1;

__END__
