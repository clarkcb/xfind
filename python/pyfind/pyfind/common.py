# -*- coding: utf-8 -*-
###############################################################################
#
# common.py
#
# Common functionality
#
###############################################################################
from dateutil.parser import parse

from .findexception import FindException


def log(message: str):
    """log a message (for now just print to stdout)"""
    print(message)


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
        raise FindException('Invalid datetime string: {}'.format(datetime_str))
