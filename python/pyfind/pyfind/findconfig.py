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
        _file_types_path = _data.joinpath('filetypes.json')
        _find_options_path = _data.joinpath('findoptions.json')

        _home = os.getenv('HOME', '')
        _default_find_config_dir = os.path.join(_home, '.config', 'xfind')
        _xfind_config_dir = os.getenv('XFIND_CONFIG_DIR', _default_find_config_dir)
        _default_settings_path = os.path.join(_xfind_config_dir, 'settings.json')

        self.file_types_path = _file_types_path
        self.find_options_path = _find_options_path
        self.default_find_settings_path = _default_settings_path
