# -*- coding: utf-8 -*-
"""
###############################################################################
#
# config.py
#
# Configuration values
#
###############################################################################
"""
import os

cwd = os.path.dirname(os.path.realpath(__file__))
data_path = os.path.join(cwd, 'data')

XFIND_PATH = os.getenv('XFIND_PATH')
if not XFIND_PATH:
    HOME = os.getenv('HOME')
    XFIND_PATH = os.path.join(HOME, 'src', 'xfind')
SHARED_PATH = os.path.join(XFIND_PATH, 'shared')
FILETYPES_PATH = os.path.join(data_path, 'filetypes.json')
FINDOPTIONS_PATH = os.path.join(data_path, 'findoptions.json')
XFINDDB = os.path.join(SHARED_PATH, 'xfind.db')
