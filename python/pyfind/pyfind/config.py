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
import json
import os

cwd = os.path.dirname(os.path.realpath(__file__))
data_path = os.path.join(cwd, 'data')
config_json_path = os.path.join(data_path, 'config.json')
with open(config_json_path, encoding='UTF-8') as f:
    config = json.load(f)

XFINDPATH = config['xfindpath']
SHAREDPATH = os.path.join(XFINDPATH, 'shared')
FILETYPESPATH = os.path.join(data_path, 'filetypes.json')
FINDOPTIONSPATH = os.path.join(data_path, 'findoptions.json')
VERSION = config['version']
