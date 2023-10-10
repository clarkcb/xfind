###############################################################################
#
# common.pm
#
# Common functions
#
###############################################################################

package plfind::common;

use Scalar::Util qw(blessed reftype);

use strict;
use warnings;

sub log_msg($) {
    my $msg = shift;
    print STDOUT $msg . "\n";
}

sub log_err($) {
    my $msg = shift;
    print STDERR 'ERROR: ' . $msg . "\n";
}

sub trim($) {
    my $s = shift;
    $s =~ s/^\s+//;
    $s =~ s/\s+$//;
    return $s;
}

# returns the number of leading whitespace characters
sub leading_whitespace_chars($) {
    my $s = shift;
    my $trimmed = 0;

    while ($s =~ /\A\s/) {
        $s = substr($s, 1);
        $trimmed++;
    }
    return $trimmed;
}

# returns the number of trailing whitespace characters
sub trailing_whitespace_chars($) {
    my $s = shift;
    my $trimmed = 0;

    while ($s =~ /\s\Z/) {
        $s = substr($s, 0, length($s) - 1);
        $trimmed++;
    }
    return $trimmed;
}

# returns an array with duplicate values removed
sub uniq($) {
    my $aref = shift;
    my %seen;
    my @uniqs = grep { !$seen{$_}++ } @{$aref};
    return \@uniqs;
}

sub bool_to_string {
    my $bool = shift;
    return $bool ? 'true' : 'false';
}

sub datetime_to_string {
    my $dt = shift;
    if (blessed($dt)) {
        return '"' . $dt . '"';
    }
    return '0';
}

sub aref_to_string {
    my ($aref, $quoted) = @_;
    my $s = '[';
    my $reftype = reftype($aref);
    if ($reftype eq 'ARRAY') {
        foreach my $i (0..$#{$aref}) {
            if ($i > 0) {
                $s .= ', ';
            }
            if ($quoted) {
                $s .= '"' . $aref->[$i] . '"';
            } else {
                $s .= $aref->[$i];
            }
        }
    } else {
        $s .= "'$reftype'";
        # print ref $aref . "\n";
    }
    $s .= ']';
    return $s;
}

sub strings_aref_to_string {
    my $aref = shift;
    return aref_to_string($aref, 1);
}

sub file_types_aref_to_string {
    my $aref = shift;
    return aref_to_string($aref, 0);
}

1;

__END__
