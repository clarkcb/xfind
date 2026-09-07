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
from .findconfig import FindConfig
from .finder import Finder, print_matching_dirs, print_matching_files
from .findexception import FindException
from .findoptions import FindOptions


async def main():
    """main()"""
    if sys.version_info < (3, 9):
        sys.exit('Sorry, Python < 3.9 is not supported')

    config = FindConfig()
    find_options = FindOptions(config)

    try:
        settings = find_options.find_settings_from_args(sys.argv[1:])

        if settings.debug:
            log(f'settings: {settings}')

        if settings.print_usage:
            log('')
            find_options.usage()

        if settings.print_version:
            log(f'xfind version {VERSION}')
            sys.exit(0)

        finder = Finder(config, settings)
        file_results = await finder.find()
        formatter = FileResultFormatter(settings)

        if settings.print_dirs:
            print_matching_dirs(file_results, formatter)

        if settings.print_files:
            print_matching_files(file_results, formatter)

    except FindException as e:
        log('')
        log_error(f'{e}\n')
        find_options.usage(1)

    except AssertionError as e:
        log('')
        log_error(f'{e}\n', settings.colorize)
        find_options.usage(1)

    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
