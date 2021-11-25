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

# use XML::Simple;
use Data::Dumper;
use JSON::PP qw(decode_json);

use plfind::common;
use plfind::config;
use plfind::FileUtil;
use plfind::FindOption;
use plfind::FindSettings;

my $arg_action_hash = {
    'in-archiveext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{in_archiveextensions});
    },
    'in-archivefilepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{n_archivefilepatterns});
    },
    'in-dirpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_dirpatterns});
    },
    'in-ext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{in_extensions});
    },
    'in-filepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_filepatterns});
    },
    'in-filetype' => sub {
        my ($s, $settings) = @_;
        $settings->add_filetypes($s, $settings->{in_filetypes});
    },
    'out-archiveext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{out_archiveextensions});
    },
    'out-archivefilepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_archivefilepatterns});
    },
    'out-dirpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_dirpatterns});
    },
    'out-ext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{out_extensions});
    },
    'out-filepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_filepatterns});
    },
    'out-filetype' => sub {
        my ($s, $settings) = @_;
        $settings->add_filetypes($s, $settings->{out_filetypes});
    },
    'settings-file' => sub {
        my ($s, $settings) = @_;
        settings_from_file($s, $settings);
    },
    'path' => sub {
        my ($s, $settings) = @_;
        push(@{$settings->{paths}}, $s);
    }
};

my $bool_flag_action_hash = {
    'archivesonly' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('archivesonly', $b);
    },
    'debug' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('debug', $b);
    },
    'excludearchives' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('includearchives', !$b);
    },
    'excludehidden' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('excludehidden', $b);
    },
    'help' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('printusage', $b);
    },
    'includearchives' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('includearchives', $b);
    },
    'includehidden' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('excludehidden', !$b);
    },
    'listdirs' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('listdirs', $b);
    },
    'listfiles' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('listfiles', $b);
    },
    'norecursive' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('recursive', !$b);
    },
    'recursive' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('recursive', $b);
    },
    'verbose' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('verbose', $b);
    },
    'version' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('printversion', $b);
    }
};

sub new {
    my $class = shift;
    my $self = {
        # options => set_options_from_xml(),
        options => set_options_from_json(),
    };
    bless $self, $class;
    return $self;
}

sub set_options_from_xml {
    my $options_hash = {};
    my $options_xml_hash = XMLin($FINDOPTIONSPATH);
    foreach my $i (0..$#{$options_xml_hash->{findoption}}) {
        my $short = $options_xml_hash->{findoption}->[$i]->{short};
        my $long = $options_xml_hash->{findoption}->[$i]->{long};
        my $desc = $options_xml_hash->{findoption}->[$i]->{content};
        $desc = plfind::common::trim($desc);
        my $func = sub {};
        if (exists $arg_action_hash->{$long}) {
            $func = $arg_action_hash->{$long};
        } elsif (exists $bool_flag_action_hash->{$long}) {
            $func = $bool_flag_action_hash->{$long};
        }
        my $opt = new plfind::FindOption($short, $long, $desc, $func);
        $options_hash->{$long} = $opt;
        if ($short) {
            $options_hash->{$short} = $options_hash->{$long};
        }
    }
    return $options_hash;
}

sub set_options_from_json {
    my $options_hash = {};
    my $options_json_hash = decode_json plfind::FileUtil::get_file_contents($FINDOPTIONSPATH);
    foreach my $findoption (@{$options_json_hash->{findoptions}}) {
        my $short = $findoption->{short};
        my $long = $findoption->{long};
        my $desc = $findoption->{desc};
        my $func = sub {};
        if (exists $arg_action_hash->{$long}) {
            $func = $arg_action_hash->{$long};
        } elsif (exists $bool_flag_action_hash->{$long}) {
            $func = $bool_flag_action_hash->{$long};
        }
        my $opt = new plfind::FindOption($short, $long, $desc, $func);
        $options_hash->{$long} = $opt;
        if ($short) {
            $options_hash->{$short} = $options_hash->{$long};
        }
    }
    return $options_hash;
}

sub settings_from_file {
    my ($filepath, $settings) = @_;
    my $errs = [];
    unless (-e $filepath) {
        push(@{$errs}, 'Settings file not found: ' . $filepath);
        return $errs;
    }
    my $json = plfind::FileUtil::get_file_contents($filepath);
    return __from_json($json, $settings);
}

# private function
sub __from_json {
    my ($json, $settings) = @_;
    my $errs = [];
    my $json_hash = decode_json $json;
    my @opt_names = keys %{$json_hash};
    foreach my $o (@opt_names) {
        if (exists $arg_action_hash->{$o}) {
            &{$arg_action_hash->{$o}}($json_hash->{$o}, $settings);
        } elsif (exists $bool_flag_action_hash->{$o}) {
            &{$bool_flag_action_hash->{$o}}($json_hash->{$o}, $settings);
        } elsif ($o eq 'startpath') {
            $settings->{startpath} = $json_hash->{$o};
        } else {
            push(@{$errs}, 'Invalid option: ' . $o);
        }
    }
    return $errs;
}

# public method (made available for unit testing)
sub settings_from_json {
    my ($self, $json, $settings) = @_;
    __from_json($json, $settings);
}

sub settings_from_args {
    my ($self, $args) = @_;
    my $settings = new plfind::FindSettings();
    # default listfiles to true since running as cli
    $settings->set_property('listfiles', 1);
    my @errs;
    while (scalar @{$args}) {
        my $arg = shift @{$args};
        if ($arg =~ /^\-+/) {
            $arg =~ s/^\-+//;
            if (exists $self->{options}->{$arg}) {
                my $opt = $self->{options}->{$arg};
                my $long = $opt->{longarg};
                if (exists $arg_action_hash->{$long}) {
                    if (scalar @{$args}) {
                        my $val = shift @{$args};
                        &{$arg_action_hash->{$long}}($val, $settings);
                    } else {
                        push(@errs, "Missing value for $arg");
                    }
                } elsif (exists $bool_flag_action_hash->{$long}) {
                    &{$bool_flag_action_hash->{$long}}(1, $settings);
                }
            } else {
                push(@errs, "Invalid option: $arg");
            }
        } else {
            push(@{$settings->{paths}}, $arg);
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
    my $options_with_sortkey = {};
    my @opt_strs_with_descs;
    foreach my $opt_key (keys %{$self->{options}}) {
        my $option = $self->{options}->{$opt_key};
        my $long = $option->{longarg};
        my $short = $option->{shortarg};
        my $sortkey = $long;
        if ($short) {
            $sortkey = lc($short) . 'a' . $long;
        }
        $options_with_sortkey->{$sortkey} = $option;
    }
    my @sortkeys = keys %{$options_with_sortkey};
    @sortkeys = sort {$plfind::FindOptions::a cmp $plfind::FindOptions::b} @sortkeys;
    my $opt_strs_with_key = {};
    my $opt_descs_with_key = {};
    foreach my $sortkey (@sortkeys) {
        my $option = $options_with_sortkey->{$sortkey};
        my $opt_str = '';
        if ($option->{shortarg}) {
            $opt_str = '-' . $option->{shortarg} . ',';
        }
        $opt_str .= '--' . $option->{longarg};
        if (length($opt_str) > $longest) {
            $longest = length($opt_str);
        }
        $opt_strs_with_key->{$sortkey} = $opt_str;
        $opt_descs_with_key->{$sortkey} = $option->{desc};
    }
    my $format_str = " %-" . $longest . "s  %s\n";
    foreach my $sortkey (@sortkeys) {
       $usage .= sprintf($format_str, $opt_strs_with_key->{$sortkey},
        $opt_descs_with_key->{$sortkey});
    }
    return $usage;
}

1;

__END__
