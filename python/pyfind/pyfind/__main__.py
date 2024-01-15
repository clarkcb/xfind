#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
###############################################################################
#
# pyfind.py
#
# A CLI file find utility implemented in python (>=3.9.x)
#
###############################################################################
"""
import sys
from typing import List

from .common import log, log_error
from .config import VERSION
from .fileresult import FileResult
from .finder import Finder
from .findexception import FindException
from .findoptions import FindOptions


def get_dir_results(file_results: List[FileResult]) -> list[str]:
    """Return unique list of directories from file results"""
    return sorted(list({f.path for f in file_results}))


async def main():
    """main()"""
    if sys.version_info < (3, 9):
        sys.exit('Sorry, Python < 3.9 is not supported')

    findoptions = FindOptions()

    settings = None
    try:
        settings = findoptions.find_settings_from_args(sys.argv[1:])
    except FindException as e:
        log('')
        log_error(f'{e}\n')
        findoptions.usage(1)

    if settings.debug:
        log(f'settings: {settings}')

    if settings.print_usage:
        log('')
        findoptions.usage()

    if settings.print_version:
        log(f'xfind version {VERSION}')
        sys.exit(0)

    try:
        finder = Finder(settings)
        file_results = await finder.find()

        if settings.print_dirs:
            dirs = get_dir_results(file_results)
            if dirs:
                log(f'\nMatching directories ({len(dirs)}):')
                for d in dirs:
                    log(d)
            else:
                log('\nMatching directories: 0')

        if settings.print_files:
            if file_results:
                log(f'\nMatching files ({len(file_results)}):')
                for f in file_results:
                    log(str(f))
            else:
                log('\nMatching files: 0')

    except AssertionError as e:
        log_error(f'{e}\n')
        findoptions.usage(1)
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
