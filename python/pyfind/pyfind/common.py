# -*- coding: utf-8 -*-
"""
###############################################################################
#
# common.py
#
# Common functionality
#
###############################################################################
"""
import sys
from dateutil.parser import parse

from .findexception import FindException


def log(message: str):
    """log a message (for now just print to stdout)"""
    print(message)


def log_error(message: str):
    """log an error message (for now just print to stderr)"""
    print(f"ERROR: {message}", file=sys.stderr)


def get_text(nodelist):
    """Get the text from an xml node"""
    rc = []
    for node in nodelist:
        if node.nodeType == node.TEXT_NODE:
            rc.append(node.data)
    return ''.join(rc)


def parse_datetime_str(datetime_str: str):
    """Try to parse datetime string to datetime instance"""
    try:
        return parse(datetime_str)
    except:
        raise FindException(f'Invalid datetime string: {datetime_str}')
