# -*- coding: utf-8 -*-
"""
###############################################################################
#
# findconfig.py
#
# Configuration values
#
###############################################################################
"""
import importlib.resources
import os


class FindConfig:
    """FindConfig holds basic configuration."""

    __slots__ = ['file_types_path', 'find_options_path', 'default_find_settings_path']

    def __init__(self):
        """Create a new FindConfig instance."""

        # pyfind data package resources
        _data = importlib.resources.files('pyfind').joinpath('data')

        # FILETYPES_PATH = os.path.join(data_path, 'filetypes.json')
        FILE_TYPES_PATH = _data.joinpath('filetypes.json')

        # FINDOPTIONS_PATH = os.path.join(data_path, 'findoptions.json')
        FIND_OPTIONS_PATH = _data.joinpath('findoptions.json')

        HOME = os.getenv('HOME', '')
        DEFAULT_FIND_CONFIG_DIR = os.path.join(HOME, '.config', 'xfind')
        XFIND_CONFIG_DIR = os.getenv('XFIND_CONFIG_DIR', DEFAULT_FIND_CONFIG_DIR)
        DEFAULT_SETTINGS_PATH = os.path.join(XFIND_CONFIG_DIR, 'settings.json')

        self.file_types_path = FILE_TYPES_PATH
        self.find_options_path = FIND_OPTIONS_PATH
        self.default_find_settings_path = DEFAULT_SETTINGS_PATH
