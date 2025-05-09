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
from pathlib import Path
import sys
from typing import List

from . import VERSION
from .common import log, log_error
from .fileresult import FileResult, FileResultFormatter
from .finder import Finder
from .findexception import FindException
from .findoptions import FindOptions


def get_dir_results(file_results: List[FileResult]) -> list[Path]:
    """Return unique list of directories from file results"""
    return sorted(list({f.path.parent for f in file_results if f.path and f.path.parent}))


def print_dir_results(file_results: List[FileResult], settings):
    dirs = get_dir_results(file_results)
    if dirs:
        log(f'\nMatching directories ({len(dirs)}):')

        if settings.colorize and (settings.in_dir_patterns or settings.in_file_patterns):
            formatter = FileResultFormatter(settings)
            for d in dirs:
                log(formatter.format_path(d))
        else:
            for d in dirs:
                log(str(d))
    else:
        log('\nMatching directories: 0')


def print_file_results(file_results: List[FileResult], settings):
    """Print the file results"""
    if file_results:
        log(f'Find results ({len(file_results)}):')
        if settings.colorize and (settings.in_dir_patterns or settings.in_file_patterns):
            formatter = FileResultFormatter(settings)
            for f in file_results:
                log(formatter.format_file_result(f))
        else:
            for f in file_results:
                log(str(f))
    else:
        log('Find results: 0')


async def main():
    """main()"""
    if sys.version_info < (3, 9):
        sys.exit('Sorry, Python < 3.9 is not supported')

    find_options = FindOptions()

    settings = None
    try:
        settings = find_options.find_settings_from_args(sys.argv[1:])
    except FindException as e:
        log('')
        log_error(f'{e}\n')
        find_options.usage(1)

    if settings.debug:
        log(f'settings: {settings}')

    if settings.print_usage:
        log('')
        find_options.usage()

    if settings.print_version:
        log(f'xfind version {VERSION}')
        sys.exit(0)

    try:
        finder = Finder(settings)
        file_results = await finder.find()

        if settings.print_dirs:
            print_dir_results(file_results, settings)

        if settings.print_files:
            print_file_results(file_results, settings)

    except AssertionError as e:
        log('')
        log_error(f'{e}\n')
        find_options.usage(1)
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
