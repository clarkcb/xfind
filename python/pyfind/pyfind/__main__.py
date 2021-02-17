#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# pyfind.py
#
# A CLI file find utility implemented in python (3.x)
#
###############################################################################
import os
import sys
from typing import List

from .common import log
from .config import VERSION
from .finder import Finder
from .findexception import FindException
from .findoptions import FindOptions
from .findresult import FindResult, FindResultFormatter
from .findsettings import FindSettings


def get_sorted_results(results: List[FindResult]):
    return sorted(results, key=lambda r: r.sortkey)


def print_results(results: List[FindResult], settings: FindSettings):
    sorted_results = get_sorted_results(results)
    formatter = FindResultFormatter(settings)
    log('Find results ({}):'.format(len(sorted_results)))
    for r in sorted_results:
        s = formatter.format(r)
        try:
            log(s)
        except UnicodeEncodeError:
            log(repr(s))


def get_matching_dirs(results: List[FindResult]) -> List[str]:
    """Get list of dirs with matches"""
    dirs = set([r.file.path for r in results])
    dirs = sorted(dirs)
    return dirs


def get_matching_files(results: List[FindResult]) -> List[str]:
    """Get list of files with matches"""
    files = set([(r.file.path, r.file.filename) for r in results])
    files = sorted(files)
    return [os.path.join(f[0], f[1]) for f in files]


def get_matching_lines(
        results: List[FindResult], settings: FindSettings) -> List[str]:
    """Get list of lines with matches (unique if settings.uniquelines)"""
    lines = [r.line for r in results if r.line]
    if settings.uniquelines:
        lines = list(set(lines))
    return sorted(lines, key=lambda s: s.upper())


async def main():
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
        results = await finder.find()

        # print the results
        if settings.printresults:
            log('')
            print_results(results, settings)

        if settings.listdirs:
            dirs = get_matching_dirs(results)
            if dirs:
                log('\nDirectories with matches ({}):'.format(len(dirs)))
                for d in dirs:
                    log(d)

        if settings.listfiles:
            files = get_matching_files(results)
            if files:
                log('\nFiles with matches ({}):'.format(len(files)))
                for f in files:
                    log(f)

        if settings.listlines:
            lines = get_matching_lines(results, settings)
            if lines:
                msg = '\nLines with matches ({}):'
                if settings.uniquelines:
                    msg = '\nUnique lines with matches ({}):'
                log(msg.format(len(lines)))
                for line in lines:
                    log(line)

    except AssertionError as e:
        log('\nERROR: {0!s}\n'.format(e))
        findoptions.usage()
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
