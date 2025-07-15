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

    def update_settings_from_json(self, settings: FindSettings, json_str: str):
        """Read settings from a JSON string"""
        json_dict = json.loads(json_str)
        # keys are sorted so that output is consistent across all versions
        keys = sorted(json_dict.keys())
        invalid_keys = [k for k in keys if k not in self.__long_arg_dict]
        if invalid_keys:
            raise FindException(f'Invalid option: {invalid_keys[0]}')
        for arg in keys:
            if arg in self.__bool_action_dict:
                if json_dict[arg] is True or json_dict[arg] is False:
                    self.__bool_action_dict[arg](json_dict[arg], settings)
                else:
                    raise FindException(f'Invalid value for option: {arg}')
            elif arg in self.__str_action_dict:
                if type(json_dict[arg]) == str:
                    self.__str_action_dict[arg](json_dict[arg], settings)
                elif type(json_dict[arg]) == list:
                    for item in json_dict[arg]:
                        if type(item) == str:
                            self.__str_action_dict[arg](item, settings)
                        else:
                            raise FindException(f'Invalid value for option: {arg}')
                else:
                    raise FindException(f'Invalid value for option: {arg}')
            elif arg in self.__dt_action_dict:
                if type(json_dict[arg]) == str:
                    self.__dt_action_dict[arg](parse_datetime_str(json_dict[arg]), settings)
                else:
                    raise FindException(f'Invalid value for option: {arg}')
            elif arg in self.__int_action_dict:
                if type(json_dict[arg]) == int:
                    self.__int_action_dict[arg](json_dict[arg], settings)
                else:
                    raise FindException(f'Invalid value for option: {arg}')
            else:
                raise FindException(f'Invalid option: {arg}')

    def update_settings_from_file(self, settings: FindSettings, file_path: str):
        """Read settings from a JSON file"""
        expanded_path = os.path.expanduser(file_path)
        if not os.path.exists(expanded_path):
            raise FindException(f'Settings file not found: {file_path}')
        if not file_path.strip().endswith('.json'):
            raise FindException(f'Invalid settings file (must be JSON): {file_path}')
        with open(expanded_path, encoding='UTF-8') as f:
            json_str = f.read()
        try:
            self.update_settings_from_json(settings, json_str)
        except json.JSONDecodeError:
            raise FindException(f'Unable to parse JSON in settings file: {file_path}')

    def update_settings_from_args(self, settings: FindSettings, args: list[str]):
        """Updates a FindSettings instance from a given list of args"""
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
                        arg_names.append(arg_name)
                elif len(arg) > 1:
                    arg_names.extend(list(arg[1:]))
                for a in arg_names:
                    if a in self.__long_arg_dict:
                        long_arg = self.__long_arg_dict[a]
                        if long_arg in self.__bool_action_dict:
                            self.__bool_action_dict[long_arg](True, settings)
                            if long_arg in ('help', 'version'):
                                return
                        elif long_arg in self.__str_action_dict or \
                                long_arg in self.__dt_action_dict or \
                                long_arg in self.__int_action_dict or \
                                long_arg == 'settings-file':
                            if arg_deque:
                                arg_val = arg_deque.popleft()
                                if long_arg in self.__str_action_dict:
                                    self.__str_action_dict[long_arg](
                                        arg_val, settings)
                                elif long_arg in self.__dt_action_dict:
                                    self.__dt_action_dict[long_arg](
                                        parse_datetime_str(arg_val), settings)
                                elif long_arg in self.__int_action_dict:
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
                                    self.__int_action_dict[long_arg](i, settings)
                                elif long_arg == 'settings-file':
                                    self.update_settings_from_file(settings, arg_val)
                            else:
                                raise FindException(f'Missing value for option {a}')
                        else:
                            raise FindException(f'Invalid option: {a}')
                    else:
                        raise FindException(f'Invalid option: {a}')
            else:
                settings.add_path(arg)

    def find_settings_from_args(self, args: list[str]) -> FindSettings:
        """Returns a FindSettings instance for a given list of args"""
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
