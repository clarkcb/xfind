###############################################################################
#
# SortBy.pm
#
# Enum of sort types
#
###############################################################################

package plfind::FindError;

use strict;
use warnings;

use constant {
    INVALID_RANGE_MINDEPTH_MAXDEPTH => 'Invalid range for mindepth and maxdepth',
    INVALID_RANGE_MINLASTMOD_MAXLASTMOD => 'Invalid range for minlastmod and maxlastmod',
    INVALID_RANGE_MINSIZE_MAXSIZE => 'Invalid range for minsize and maxsize',
    STARTPATH_NOT_DEFINED => 'Startpath not defined',
    STARTPATH_NOT_FOUND => 'Startpath not found',
    STARTPATH_NOT_MATCH_FIND_SETTINGS => 'Startpath does not match find settings',
    STARTPATH_NOT_READABLE => 'Startpath not readable',
};

1;

__END__
