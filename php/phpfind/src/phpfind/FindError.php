<?php

declare(strict_types=1);

namespace phpfind;

/**
 * Enum FindError
 */
enum FindError: string
{
   case STARTPATH_NOT_DEFINED = 'Startpath not defined';
   case STARTPATH_NOT_READABLE = 'Startpath not readable';
   case STARTPATH_NOT_FOUND = 'Startpath not found';
   case INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH = 'Invalid range for mindepth and maxdepth';
   case INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD = 'Invalid range for minlastmod and maxlastmod';
   case INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE = 'Invalid range for minsize and maxsize';
   case STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS = 'Startpath does not match find settings';
}
