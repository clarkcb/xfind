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

XFINDPATH = os.getenv('XFIND_PATH')
if not XFINDPATH:
    HOME = os.getenv('HOME')
    XFINDPATH = os.path.join(HOME, 'src', 'xfind')
SHAREDPATH = os.path.join(XFINDPATH, 'shared')
FILETYPESPATH = os.path.join(data_path, 'filetypes.json')
FINDOPTIONSPATH = os.path.join(data_path, 'findoptions.json')
