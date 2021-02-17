# -*- coding: utf-8 -*-
###############################################################################
#
# config.py
#
# Configuration values
#
###############################################################################
import json
import os

cwd = os.path.dirname(os.path.realpath(__file__))
config_json_path = os.path.join(cwd, '../data/config.json')
config = json.load(open(config_json_path))

XFINDPATH = config['xfindpath']
SHAREDPATH = os.path.join(XFINDPATH, 'shared')
FILETYPESPATH = os.path.join(cwd, '../data/filetypes.json')
FINDOPTIONSPATH = os.path.join(cwd, '../data/findoptions.json')
VERSION = config['version']
