#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# pyfind.py
#
# A CLI file find utility implemented in python (>=3.9.x)
#
###############################################################################
import sys
from typing import List

from .common import log
from .config import VERSION
from .fileresult import FileResult
from .finder import Finder
from .findexception import FindException
from .findoptions import FindOptions


def get_found_dirs(found_files: List[FileResult]):
    return sorted(list(set([f.path for f in found_files])))


async def main():
    if sys.version_info < (3, 9):
        sys.exit('Sorry, Python < 3.9 is not supported')

    findoptions = FindOptions()

    settings = None
    try:
        settings = findoptions.find_settings_from_args(sys.argv[1:])
    except FindException as e:
        log('\nERROR: {0!s}\n'.format(e))
        findoptions.usage()

    if settings.debug:
        log('settings: {0!s}'.format(settings))

    if settings.printusage:
        log('')
        findoptions.usage()

    if settings.printversion:
        log('xfind version {}'.format(VERSION))
        sys.exit(0)

    try:
        finder = Finder(settings)
        found_files = await finder.find()

        if settings.listdirs:
            dirs = get_found_dirs(found_files)
            if dirs:
                log('\nMatching directories ({}):'.format(len(dirs)))
                for d in dirs:
                    log(d)
            else:
                log('\nMatching directories: 0')

        if settings.listfiles:
            if found_files:
                log('\nMatching files ({}):'.format(len(found_files)))
                for f in found_files:
                    log(str(f))
            else:
                log('\nMatching files: 0')

    except AssertionError as e:
        log('\nERROR: {0!s}\n'.format(e))
        findoptions.usage()
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
