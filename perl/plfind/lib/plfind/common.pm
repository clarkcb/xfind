###############################################################################
#
# common.pm
#
# Common functions
#
###############################################################################

package plfind::common;

use strict;
use warnings;

sub log_msg($) {
    my $msg = shift;
    print $msg . "\n";
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

1;

__END__
