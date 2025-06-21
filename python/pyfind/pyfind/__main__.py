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

from . import VERSION
from .common import log, log_error
from .fileresult import FileResultFormatter
from .finder import Finder, print_dir_results, print_file_results
from .findexception import FindException
from .findoptions import FindOptions


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
        formatter = FileResultFormatter(settings)

        if settings.print_dirs:
            print_dir_results(file_results, formatter)

        if settings.print_files:
            print_file_results(file_results, formatter)

    except AssertionError as e:
        log('')
        log_error(f'{e}\n')
        find_options.usage(1)
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
