# -*- coding: utf-8 -*-
"""
###############################################################################
#
# findoptions.py
#
# class FindOptions: defines the available command-line options and
#                    corresponding utility methods
#
###############################################################################
"""
import importlib.resources
import json
import os
import sys
from collections import deque
from io import StringIO
from typing import Any

from .common import parse_datetime_str
from .findexception import FindException
from .findoption import FindOption
from .findsettings import FindSettings


class FindOptions:
    """class to provide usage info and parse command-line arguments into settings."""

    def __init__(self):
        self.options = []
        self.__set_dicts()
        self.__set_options_from_json()

    def __set_dicts(self):
        self.__bool_action_dict = {
            'archivesonly':
                lambda b, settings:
                settings.set_property('archives_only', b),
            'colorize':
                lambda b, settings:
                settings.set_property('colorize', b),
            'debug':
                lambda b, settings:
                settings.set_property('debug', b),
            'followsymlinks':
                lambda b, settings:
                settings.set_property('follow_symlinks', b),
            'excludearchives':
                lambda b, settings:
                settings.set_property('include_archives', not b),
            'excludehidden':
                lambda b, settings:
                settings.set_property('include_hidden', not b),
            'help':
                lambda b, settings:
                settings.set_property('print_usage', b),
            'includearchives':
                lambda b, settings:
                settings.set_property('include_archives', b),
            'includehidden':
                lambda b, settings:
                settings.set_property('include_hidden', b),
            'nocolorize':
                lambda b, settings:
                settings.set_property('colorize', not b),
            'nofollowsymlinks':
                lambda b, settings:
                settings.set_property('follow_symlinks', not b),
            'noincludearchives':
                lambda b, settings:
                settings.set_property('include_archives', not b),
            'noprintdirs':
                lambda b, settings:
                settings.set_property('print_dirs', not b),
            'noprintfiles':
                lambda b, settings:
                settings.set_property('print_files', not b),
            'norecursive':
                lambda b, settings:
                settings.set_property('recursive', not b),
            'printdirs':
                lambda b, settings:
                settings.set_property('print_dirs', b),
            'printfiles':
                lambda b, settings:
                settings.set_property('print_files', b),
            'printmatches':
                lambda b, settings:
                settings.set_property('print_files', b),
            'recursive':
                lambda b, settings:
                settings.set_property('recursive', b),
            'sort-ascending':
                lambda b, settings:
                settings.set_property('sort_descending', not b),
            'sort-caseinsensitive':
                lambda b, settings:
                settings.set_property('sort_case_insensitive', b),
            'sort-casesensitive':
                lambda b, settings:
                settings.set_property('sort_case_insensitive', not b),
            'sort-descending':
                lambda b, settings:
                settings.set_property('sort_descending', b),
            'verbose':
                lambda b, settings:
                settings.set_property('verbose', b),
            'version':
                lambda b, settings:
                settings.set_property('print_version', b)
        }

        self.__str_action_dict = {
            'in-archiveext':
                lambda s, settings:
                settings.add_strs_to_set(s, 'in_archive_extensions'),
            'in-archivefilepattern':
                lambda s, settings:
                settings.add_patterns(s, 'in_archive_file_patterns'),
            'in-dirpattern':
                lambda s, settings:
                settings.add_patterns(s, 'in_dir_patterns'),
            'in-ext':
                lambda s, settings:
                settings.add_strs_to_set(s, 'in_extensions'),
            'in-filepattern':
                lambda s, settings:
                settings.add_patterns(s, 'in_file_patterns'),
            'in-filetype':
                lambda s, settings:
                settings.add_file_types(s, 'in_file_types'),
            'out-archiveext':
                lambda s, settings:
                settings.add_strs_to_set(s, 'out_archive_extensions'),
            'out-archivefilepattern':
                lambda s, settings:
                settings.add_patterns(s, 'out_archive_file_patterns'),
            'out-dirpattern':
                lambda s, settings:
                settings.add_patterns(s, 'out_dir_patterns'),
            'out-ext':
                lambda s, settings:
                settings.add_strs_to_set(s, 'out_extensions'),
            'out-filepattern':
                lambda s, settings:
                settings.add_patterns(s, 'out_file_patterns'),
            'out-filetype':
                lambda s, settings:
                settings.add_file_types(s, 'out_file_types'),
            'path':
                lambda s, settings:
                settings.add_path(s),
            'sort-by':
                lambda s, settings:
                settings.set_sort_by(s),
        }

        self.__dt_action_dict = {
            'lastmod-after':
                lambda dt, settings:
                settings.set_property('lastmod_after', dt),
            'lastmod-before':
                lambda dt, settings:
                settings.set_property('lastmod_before', dt),
            'maxlastmod':
                lambda dt, settings:
                settings.set_property('max_last_mod', dt),
            'minlastmod':
                lambda dt, settings:
                settings.set_property('min_last_mod', dt),
        }

        self.__int_action_dict = {
            'maxdepth':
                lambda i, settings:
                settings.set_property('max_depth', i),
            'maxsize':
                lambda i, settings:
                settings.set_property('max_size', i),
            'mindepth':
                lambda i, settings:
                settings.set_property('min_depth', i),
            'minsize':
                lambda i, settings:
                settings.set_property('min_size', i),
        }

        self.__long_arg_dict = {'path': 'path'}

    def __set_options_from_json(self):
        data = importlib.resources.files('pyfind').joinpath('data')
        find_options_json = data.joinpath('findoptions.json').read_text()
        find_options_dict = json.loads(find_options_json)
        for find_option_obj in find_options_dict['findoptions']:
            long_arg = find_option_obj['long']
            short_arg = ''
            if 'short' in find_option_obj:
                short_arg = find_option_obj['short']
            desc = find_option_obj['desc']
            if long_arg not in self.__bool_action_dict and \
                    long_arg not in self.__str_action_dict and \
                    long_arg not in self.__dt_action_dict and \
                    long_arg not in self.__int_action_dict and \
                    long_arg != 'settings-file':
                raise FindException(f'Unknown find option: {long_arg}')
            self.options.append(FindOption(short_arg, long_arg, desc))
            self.__long_arg_dict[long_arg] = long_arg
            if short_arg:
                self.__long_arg_dict[short_arg] = long_arg

    def update_arg_dict_from_dict(self, arg_dict: dict[str, Any], other_dict: dict[str, Any]):
        """Update arg_dict from another dict"""
        # keys are sorted so that output is consistent across all versions
        key_dict = {self.__long_arg_dict[k]: k for k in sorted(other_dict.keys())}
        invalid_keys = [k for k in key_dict.keys() if k not in self.__long_arg_dict]
        if invalid_keys:
            raise FindException(f'Invalid option: {invalid_keys[0]}')
        for arg in key_dict.keys():
            orig_arg = key_dict[arg]
            if arg in self.__bool_action_dict:
                if other_dict[arg] is True or other_dict[arg] is False:
                    arg_dict[arg] = other_dict[arg]
                else:
                    raise FindException(f'Invalid value for option: {orig_arg}')
            elif arg in self.__str_action_dict:
                if arg not in arg_dict:
                    arg_dict[arg] = []
                if type(other_dict[arg]) == str:
                    arg_dict[arg].append(other_dict[arg])
                elif type(other_dict[arg]) == list:
                    for item in other_dict[arg]:
                        if type(item) == str:
                            arg_dict[arg].append(item)
                        else:
                            raise FindException(f'Invalid value for option: {orig_arg}')
                else:
                    raise FindException(f'Invalid value for option: {orig_arg}')
            elif arg in self.__dt_action_dict:
                if type(other_dict[arg]) == str:
                    arg_dict[arg] = parse_datetime_str(other_dict[arg])
                else:
                    raise FindException(f'Invalid value for option: {orig_arg}')
            elif arg in self.__int_action_dict:
                if type(other_dict[arg]) == int:
                    arg_dict[arg] = other_dict[arg]
                else:
                    raise FindException(f'Invalid value for option: {orig_arg}')
            else:
                raise FindException(f'Invalid option: {orig_arg}')

    def arg_dict_from_dict(self, other_dict: dict[str, Any]):
        arg_dict = {}
        self.update_arg_dict_from_dict(arg_dict, other_dict)
        return arg_dict

    def update_arg_dict_from_json(self, arg_dict: dict[str, Any], json_str: str):
        """Update arg dict from a JSON string"""
        json_dict = json.loads(json_str)
        self.update_arg_dict_from_dict(arg_dict, json_dict)

    def arg_dict_from_json(self, json_str: str) -> dict[str, Any]:
        """Read args from a JSON string into dict"""
        arg_dict = {}
        self.update_arg_dict_from_json(arg_dict, json_str)
        return arg_dict

    def update_arg_dict_from_file(self, arg_dict: dict[str, Any], file_path: str):
        """Read arg dict from a JSON file"""
        expanded_path = os.path.expanduser(file_path)
        if not os.path.exists(expanded_path):
            raise FindException(f'Settings file not found: {file_path}')
        if not file_path.strip().endswith('.json'):
            raise FindException(f'Invalid settings file (must be JSON): {file_path}')
        with open(expanded_path, encoding='UTF-8') as f:
            json_str = f.read()
        try:
            self.update_arg_dict_from_json(arg_dict, json_str)
        except json.JSONDecodeError:
            raise FindException(f'Unable to parse JSON in settings file: {file_path}')

    def arg_dict_from_file(self, file_path: str) -> dict[str, Any]:
        """Read args from a JSON file into dict"""
        arg_dict = {}
        self.update_arg_dict_from_file(arg_dict, file_path)
        return arg_dict

    def update_arg_dict_from_args(self, arg_dict: dict[str, Any], args: list[str]):
        """Update arg dict from a given list of args"""
        arg_deque = deque(args)
        while arg_deque:
            arg = arg_deque.popleft()
            if arg.startswith('-'):
                arg_names = []
                if arg.startswith('--'):
                    if len(arg) > 2:
                        arg_name = arg[2:]
                        if '=' in arg_name:
                            arg_nv = arg_name.split('=', 1)
                            arg_name = arg_nv[0]
                            if arg_nv[1]:
                                arg_deque.appendleft(arg_nv[1])
                        if arg_name in self.__long_arg_dict:
                            arg_names.append(arg_name)
                        else:
                            raise FindException(f'Invalid option: {arg_name}')
                    else:
                        raise FindException(f'Invalid option: {arg}')
                elif len(arg) > 1:
                    for c in arg[1:]:
                        if c in self.__long_arg_dict:
                            arg_names.append(self.__long_arg_dict[c])
                        else:
                            raise FindException(f'Invalid option: {c}')
                else:
                    raise FindException(f'Invalid option: {arg}')
                for arg_name in arg_names:
                    if arg_name in self.__bool_action_dict:
                        arg_dict[arg_name] = True
                        if arg_name in ('help', 'version'):
                            return
                    elif arg_name in self.__str_action_dict or \
                            arg_name in self.__dt_action_dict or \
                            arg_name in self.__int_action_dict or \
                            arg_name == 'settings-file':
                        if arg_deque:
                            arg_val = arg_deque.popleft()
                            if arg_name in self.__str_action_dict:
                                if arg_name not in arg_dict:
                                    arg_dict[arg_name] = []
                                arg_dict[arg_name].append(arg_val)
                            elif arg_name in self.__dt_action_dict:
                                arg_dict[arg_name] = parse_datetime_str(arg_val)
                            elif arg_name in self.__int_action_dict:
                                invalid_int = False
                                i = 0
                                try:
                                    i = int(arg_val)
                                except ValueError:
                                    invalid_int = True
                                else:
                                    if i < 0:
                                        invalid_int = True
                                if invalid_int:
                                    err = f'Invalid value for option {arg}: {arg_val}'
                                    raise FindException(err)
                                arg_dict[arg_name] = i
                            elif arg_name == 'settings-file':
                                self.update_arg_dict_from_file(arg_dict, arg_val)
                        else:
                            raise FindException(f'Missing value for option {arg_name}')
                    else:
                        raise FindException(f'Invalid option: {arg_name}')
            else:
                # if not an option, then it is a path
                if 'path' not in arg_dict:
                    arg_dict['path'] = []
                arg_dict['path'].append(arg)

    def arg_dict_from_args(self, args: list[str]) -> dict[str, Any]:
        """Return a dict of arguments from a given list of args"""
        arg_dict = {}
        self.update_arg_dict_from_args(arg_dict, args)
        return arg_dict

    def update_settings_from_arg_dict(self, settings: FindSettings, arg_dict: dict[str, Any]):
        """Update settings from a dict"""
        for arg in arg_dict.keys():
            if arg in self.__bool_action_dict:
                self.__bool_action_dict[arg](arg_dict[arg], settings)
            elif arg in self.__str_action_dict:
                for item in arg_dict[arg]:
                    self.__str_action_dict[arg](item, settings)
            elif arg in self.__dt_action_dict:
                self.__dt_action_dict[arg](arg_dict[arg], settings)
            elif arg in self.__int_action_dict:
                self.__int_action_dict[arg](arg_dict[arg], settings)
            elif arg == 'settingsfile':
                self.update_settings_from_file(settings, arg_dict[arg])
            else:
                raise FindException(f'Invalid option: {arg}')

    def find_settings_from_arg_dict(self, arg_dict: dict[str, Any]) -> FindSettings:
        """Read settings from a dict"""
        settings = FindSettings()
        self.update_settings_from_arg_dict(settings, arg_dict)
        return settings

    def update_settings_from_json(self, settings: FindSettings, json_str: str):
        """Update settings from a JSON string"""
        json_dict = json.loads(json_str)
        arg_dict = self.arg_dict_from_dict(json_dict)
        self.update_settings_from_arg_dict(settings, arg_dict)

    def find_settings_from_json(self, json_str: str) -> FindSettings:
        """Read settings from a JSON string"""
        settings = FindSettings()
        self.update_settings_from_json(settings, json_str)
        return settings

    def update_settings_from_file(self, settings: FindSettings, file_path: str):
        """Update settings from a JSON file"""
        arg_dict = self.arg_dict_from_file(file_path)
        self.update_settings_from_arg_dict(settings, arg_dict)

    def find_settings_from_file(self, file_path: str) -> FindSettings:
        """Read settings from a JSON file"""
        settings = FindSettings()
        self.update_settings_from_file(settings, file_path)
        return settings

    def update_settings_from_args(self, settings: FindSettings, args: list[str]):
        """Update settings from a given list of args"""
        arg_dict = self.arg_dict_from_args(args)
        self.update_settings_from_arg_dict(settings, arg_dict)

    def find_settings_from_args(self, args: list[str]) -> FindSettings:
        """Read settings from a given list of args"""
        # default print_files to True since running from command line
        settings = FindSettings(print_files=True)
        self.update_settings_from_args(settings, args)
        return settings

    def __get_usage_string(self):
        sio = StringIO()
        sio.write('Usage:\n')
        sio.write(
            ' pyfind [options] <path> [<path> ...]\n\nOptions:\n')
        opt_pairs = []
        longest = 0
        for opt in sorted(self.options, key=lambda o: o.sort_arg):
            opt_string = ''
            if opt.short_arg:
                opt_string += f'-{opt.short_arg},'
            opt_string += f'--{opt.long_arg}'
            if len(opt_string) > longest:
                longest = len(opt_string)
            opt_pairs.append((opt_string, opt.desc))
        format_string = ' {0:<' + str(longest) + 's}  {1:s}\n'
        for opt_pair in opt_pairs:
            sio.write(format_string.format(opt_pair[0], opt_pair[1]))
        usage = sio.getvalue()
        sio.close()
        return usage

    def usage(self, exit_code: int = 0):
        """Print the usage string and exit"""
        print(self.__get_usage_string())
        sys.exit(exit_code)
