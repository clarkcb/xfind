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
from typing import List

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
        self.__bool_arg_dict = {
            'archivesonly':
                lambda b, settings:
                settings.set_property('archives_only', b),
            'debug':
                lambda b, settings:
                settings.set_property('debug', b),
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

        self.__coll_arg_dict = {
            'in-archiveext':
                lambda x, settings:
                settings.add_exts(x, 'in_archive_extensions'),
            'in-archivefilepattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_archive_file_patterns'),
            'in-dirpattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_dir_patterns'),
            'in-ext':
                lambda x, settings:
                settings.add_exts(x, 'in_extensions'),
            'in-filepattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_file_patterns'),
            'in-filetype':
                lambda x, settings:
                settings.add_file_types(x, 'in_file_types'),
            'out-archiveext':
                lambda x, settings:
                settings.add_exts(x, 'out_archive_extensions'),
            'out-archivefilepattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_archive_file_patterns'),
            'out-dirpattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_dir_patterns'),
            'out-ext':
                lambda x, settings:
                settings.add_exts(x, 'out_extensions'),
            'out-filepattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_file_patterns'),
            'out-filetype':
                lambda x, settings:
                settings.add_file_types(x, 'out_file_types'),
            'path':
                lambda x, settings:
                settings.paths.add(x),
            'sort-by':
                lambda x, settings:
                settings.set_sort_by(x),
        }

        self.__dt_arg_dict = {
            'lastmod-after':
                lambda x, settings:
                settings.set_property('lastmod_after', x),
            'lastmod-before':
                lambda x, settings:
                settings.set_property('lastmod_before', x),
            'maxlastmod':
                lambda x, settings:
                settings.set_property('max_last_mod', x),
            'minlastmod':
                lambda x, settings:
                settings.set_property('min_last_mod', x),
        }

        self.__int_arg_dict = {
            'maxdepth':
                lambda x, settings:
                settings.set_property('max_depth', int(x)),
            'maxsize':
                lambda x, settings:
                settings.set_property('max_size', int(x)),
            'mindepth':
                lambda x, settings:
                settings.set_property('min_depth', int(x)),
            'minsize':
                lambda x, settings:
                settings.set_property('min_size', int(x)),
        }

        self.__str_arg_dict = {
            'path':
                lambda x, settings:
                settings.paths.add(x),
        }

        self.__long_arg_dict = {}

    def settings_from_file(self, file_path: str, settings: FindSettings):
        """Read settings from a JSON file"""
        assert os.path.exists(file_path), f'Settings file not found: {file_path}'
        with open(file_path, encoding='UTF-8') as f:
            json_str = f.read()
        self.settings_from_json(json_str, settings)

    def settings_from_json(self, json_str: str, settings: FindSettings):
        """Read settings from a JSON string"""
        json_dict = json.loads(json_str)
        for arg in json_dict:
            if arg in self.__bool_arg_dict:
                self.__bool_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__coll_arg_dict:
                self.__coll_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__dt_arg_dict:
                self.__dt_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__int_arg_dict:
                self.__int_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__str_arg_dict:
                self.__str_arg_dict[arg](json_dict[arg], settings)
            else:
                raise FindException(f'Invalid option: {arg}')

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
            if long_arg in self.__bool_arg_dict:
                func = self.__bool_arg_dict[long_arg]
            elif long_arg in self.__coll_arg_dict:
                func = self.__coll_arg_dict[long_arg]
            elif long_arg in self.__dt_arg_dict:
                func = self.__dt_arg_dict[long_arg]
            elif long_arg in self.__int_arg_dict:
                func = self.__int_arg_dict[long_arg]
            elif long_arg in self.__str_arg_dict:
                func = self.__str_arg_dict[long_arg]
            elif long_arg in self.__str_arg_dict:
                func = self.__str_arg_dict[long_arg]
            elif long_arg == 'settings-file':
                func = self.settings_from_file
            else:
                raise FindException(f'Unknown find option: {long_arg}')
            self.options.append(FindOption(short_arg, long_arg, desc, func))
            self.__long_arg_dict[long_arg] = long_arg
            if short_arg:
                self.__long_arg_dict[short_arg] = long_arg

    def find_settings_from_args(self, args: List[str]) -> FindSettings:
        """Returns a FindSettings instance for a given list of args"""
        # default print_files to True since running from command line
        settings = FindSettings(print_files=True)
        return self.update_settings_from_args(settings, args)

    def update_settings_from_args(self, settings: FindSettings, args: List[str]) -> FindSettings:
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
                        arg_names.append(arg[2:])
                elif len(arg) > 1:
                    arg_names.extend(list(arg[1:]))
                for a in arg_names:
                    if a in self.__long_arg_dict:
                        long_arg = self.__long_arg_dict[a]
                        if long_arg in self.__bool_arg_dict:
                            self.__bool_arg_dict[long_arg](True, settings)
                            if long_arg in ('help', 'version'):
                                return settings
                        elif long_arg in self.__coll_arg_dict or \
                                long_arg in self.__dt_arg_dict or \
                                long_arg in self.__int_arg_dict or \
                                long_arg in self.__str_arg_dict or \
                                long_arg == 'settings-file':
                            if arg_deque:
                                arg_val = arg_deque.popleft()
                                if long_arg in self.__coll_arg_dict:
                                    self.__coll_arg_dict[long_arg](
                                        arg_val, settings)
                                elif long_arg in self.__dt_arg_dict:
                                    self.__dt_arg_dict[long_arg](
                                        parse_datetime_str(arg_val), settings)
                                elif long_arg in self.__int_arg_dict:
                                    invalid_int = False
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
                                    self.__int_arg_dict[long_arg](
                                        arg_val, settings)
                                elif long_arg in self.__str_arg_dict:
                                    self.__str_arg_dict[long_arg](
                                        arg_val, settings)
                                elif long_arg == 'settings-file':
                                    self.settings_from_file(arg_val, settings)
                            else:
                                raise FindException(f'Missing value for option {a}')
                        else:
                            raise FindException(f'Invalid option: {a}')
                    else:
                        raise FindException(f'Invalid option: {a}')
            else:
                settings.paths.add(arg)
        return settings

    def usage(self, exit_code: int = 0):
        """Print the usage string and exit"""
        print(self.__get_usage_string())
        sys.exit(exit_code)

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
