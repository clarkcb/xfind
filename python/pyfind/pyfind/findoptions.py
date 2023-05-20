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
import json
import os
import sys
from collections import deque
from io import StringIO
from typing import List

import pkg_resources

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
                settings.set_property('exclude_hidden', b),
            'help':
                lambda b, settings:
                settings.set_property('print_usage', b),
            'includearchives':
                lambda b, settings:
                settings.set_property('include_archives', b),
            'includehidden':
                lambda b, settings:
                settings.set_property('exclude_hidden', not b),
            'listdirs':
                lambda b, settings:
                settings.set_property('list_dirs', b),
            'listfiles':
                lambda b, settings:
                settings.set_property('list_files', b),
            'noincludearchives':
                lambda b, settings:
                settings.set_property('include_archives', not b),
            'noprintmatches':
                lambda b, settings:
                settings.set_property('print_results', not b),
            'norecursive':
                lambda b, settings:
                settings.set_property('recursive', not b),
            'printmatches':
                lambda b, settings:
                settings.set_property('print_results', b),
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
            'sort-by':
                lambda x, settings:
                settings.set_sort_by(x),
            'path':
                lambda x, settings:
                settings.paths.add(x),
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
            'maxsize':
                lambda x, settings:
                settings.set_property('max_size', int(x)),
            'minsize':
                lambda x, settings:
                settings.set_property('min_size', int(x)),
        }

        self.__str_arg_dict = {
            'path':
                lambda x, settings:
                settings.paths.add(x),
        }

        self.__longarg_dict = {}

    def settings_from_file(self, file_path: str, settings: FindSettings):
        """Read settings from a JSON file"""
        assert os.path.exists(file_path), f'Settings file not found: {file_path}'
        with open(file_path) as f:
            jsonstr = f.read()
        self.settings_from_json(jsonstr, settings)

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
        stream = pkg_resources.resource_stream(__name__, 'data/findoptions.json')
        findoptions_dict = json.load(stream)
        for findoption_obj in findoptions_dict['findoptions']:
            longarg = findoption_obj['long']
            shortarg = ''
            if 'short' in findoption_obj:
                shortarg = findoption_obj['short']
            desc = findoption_obj['desc']
            if longarg in self.__bool_arg_dict:
                func = self.__bool_arg_dict[longarg]
            elif longarg in self.__coll_arg_dict:
                func = self.__coll_arg_dict[longarg]
            elif longarg in self.__dt_arg_dict:
                func = self.__dt_arg_dict[longarg]
            elif longarg in self.__int_arg_dict:
                func = self.__int_arg_dict[longarg]
            elif longarg in self.__str_arg_dict:
                func = self.__str_arg_dict[longarg]
            elif longarg in self.__str_arg_dict:
                func = self.__str_arg_dict[longarg]
            elif longarg == 'settings-file':
                func = self.settings_from_file
            else:
                raise FindException(f'Unknown find option: {longarg}')
            self.options.append(FindOption(shortarg, longarg, desc, func))
            self.__longarg_dict[longarg] = longarg
            if shortarg:
                self.__longarg_dict[shortarg] = longarg

    def find_settings_from_args(self, args: List[str]) -> FindSettings:
        """Returns a FindSettings instance for a given list of args"""
        # default list_files to True since running from command line
        settings = FindSettings(list_files=True)
        return self.update_settings_from_args(settings, args)

    def update_settings_from_args(self, settings: FindSettings, args: List[str]) -> FindSettings:
        """Updates a FindSettings instance from a given list of args"""
        argdeque = deque(args)
        while argdeque:
            arg = argdeque.popleft()
            if arg.startswith('-'):
                arg_names = []
                if arg.startswith('--'):
                    if len(arg) > 2:
                        arg_name = arg[2:]
                        if '=' in arg_name:
                            arg_nv = arg_name.split('=', 1)
                            arg_name = arg_nv[0]
                            if arg_nv[1]:
                                argdeque.appendleft(arg_nv[1])
                        arg_names.append(arg[2:])
                elif len(arg) > 1:
                    arg_names.extend(list(arg[1:]))
                for a in arg_names:
                    if a in self.__longarg_dict:
                        longarg = self.__longarg_dict[a]
                        if longarg in self.__bool_arg_dict:
                            self.__bool_arg_dict[longarg](True, settings)
                            if longarg in ('help', 'version'):
                                return settings
                        elif longarg in self.__coll_arg_dict or \
                                longarg in self.__dt_arg_dict or \
                                longarg in self.__int_arg_dict or \
                                longarg in self.__str_arg_dict or \
                                longarg == 'settings-file':
                            if argdeque:
                                argval = argdeque.popleft()
                                if longarg in self.__coll_arg_dict:
                                    self.__coll_arg_dict[longarg](
                                        argval, settings)
                                elif longarg in self.__dt_arg_dict:
                                    self.__dt_arg_dict[longarg](
                                        parse_datetime_str(argval), settings)
                                elif longarg in self.__int_arg_dict:
                                    invalid_int = False
                                    try:
                                        i = int(argval)
                                    except ValueError:
                                        invalid_int = True
                                    else:
                                        if i < 0:
                                            invalid_int = True
                                    if invalid_int:
                                        err = f'Invalid value for option {arg}: {argval}'
                                        raise FindException(err)
                                    self.__int_arg_dict[longarg](
                                        argval, settings)
                                elif longarg in self.__str_arg_dict:
                                    self.__str_arg_dict[longarg](
                                        argval, settings)
                                elif longarg == 'settings-file':
                                    self.settings_from_file(argval, settings)
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
        for opt in sorted(self.options, key=lambda o: o.sortarg):
            opt_string = ''
            if opt.shortarg:
                opt_string += f'-{opt.shortarg},'
            opt_string += f'--{opt.longarg}'
            if len(opt_string) > longest:
                longest = len(opt_string)
            opt_pairs.append((opt_string, opt.desc))
        format_string = ' {0:<' + str(longest) + 's}  {1:s}\n'
        for opt_pair in opt_pairs:
            sio.write(format_string.format(opt_pair[0], opt_pair[1]))
        usage = sio.getvalue()
        sio.close()
        return usage
