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
import sys
from datetime import datetime
from io import StringIO
from typing import Any

from .argtokenizer import ArgToken, ArgTokenType, ArgTokenizer
from .findexception import FindException
from .findoption import FindOption
from .findsettings import FindSettings


class FindOptions:
    """class to provide usage info and parse command-line arguments into settings."""

    def __init__(self):
        self.options = []
        self.__set_dicts()
        self.__set_options_from_json()
        self.arg_tokenizer = ArgTokenizer(options=self.options)

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

        self.__date_action_dict = {
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
            if long_arg in self.__bool_action_dict:
                arg_type = ArgTokenType.BOOL
            elif long_arg in self.__str_action_dict:
                arg_type = ArgTokenType.STR
            elif long_arg in self.__date_action_dict:
                arg_type = ArgTokenType.DATE
            elif long_arg in self.__int_action_dict:
                arg_type = ArgTokenType.INT
            # special case for settings-file which is not in any dict
            elif long_arg == 'settings-file':
                arg_type = ArgTokenType.STR
            else:
                raise FindException(f'Unknown find option: {long_arg}')
            self.options.append(FindOption(short_arg, long_arg, desc, arg_type))
        # Add path option (not in json)
        self.options.append(FindOption('', 'path', '', ArgTokenType.STR))

    def update_settings_from_arg_tokens(self, settings: FindSettings, arg_tokens: list[ArgToken]):
        """Update settings from a list of arg tokens"""
        for arg_token in arg_tokens:
            if arg_token.token_type == ArgTokenType.BOOL:
                if arg_token.name in self.__bool_action_dict:
                    if type(arg_token.value) is bool:
                        self.__bool_action_dict[arg_token.name](arg_token.value, settings)
                    else:
                        raise FindException(f'Invalid value for option: {arg_token.name}')
                else:
                    raise FindException(f'Invalid option: {arg_token.name}')
            elif arg_token.token_type == ArgTokenType.STR:
                if arg_token.name == 'settings-file':
                    self.update_settings_from_file(settings, arg_token.value)
                elif arg_token.name in self.__str_action_dict:
                    if type(arg_token.value) is str:
                        self.__str_action_dict[arg_token.name](arg_token.value, settings)
                    else:
                        raise FindException(f'Invalid value for option: {arg_token.name}')
                else:
                    raise FindException(f'Invalid option: {arg_token.name}')
            elif arg_token.token_type == ArgTokenType.INT:
                if arg_token.name in self.__int_action_dict:
                    if type(arg_token.value) is int:
                        self.__int_action_dict[arg_token.name](arg_token.value, settings)
                    else:
                        raise FindException(f'Invalid value for option: {arg_token.name}')
                else:
                    raise FindException(f'Invalid option: {arg_token.name}')
            elif arg_token.token_type == ArgTokenType.DATE:
                if arg_token.name in self.__date_action_dict:
                    if type(arg_token.value) is datetime:
                        self.__date_action_dict[arg_token.name](arg_token.value, settings)
                    else:
                        raise FindException(f'Invalid value for option: {arg_token.name}')
                else:
                    raise FindException(f'Invalid option: {arg_token.name}')
            else:
                raise FindException(f'Invalid option: {arg_token.name}')

    def update_settings_from_arg_dict(self, settings: FindSettings, arg_dict: dict[str, Any]):
        """Update settings from a dict of args"""
        arg_tokens = self.arg_tokenizer.tokenize_arg_dict(arg_dict)
        self.update_settings_from_arg_tokens(settings, arg_tokens)

    def update_settings_from_json(self, settings: FindSettings, json_str: str):
        """Update settings from a JSON string"""
        arg_tokens = self.arg_tokenizer.tokenize_json(json_str)
        self.update_settings_from_arg_tokens(settings, arg_tokens)

    def find_settings_from_json(self, json_str: str) -> FindSettings:
        """Read settings from a JSON string"""
        settings = FindSettings()
        self.update_settings_from_json(settings, json_str)
        return settings

    def update_settings_from_file(self, settings: FindSettings, file_path: str):
        """Update settings from a JSON file"""
        arg_tokens = self.arg_tokenizer.tokenize_file(file_path)
        self.update_settings_from_arg_tokens(settings, arg_tokens)

    def find_settings_from_file(self, file_path: str) -> FindSettings:
        """Read settings from a JSON file"""
        settings = FindSettings()
        self.update_settings_from_file(settings, file_path)
        return settings

    def update_settings_from_args(self, settings: FindSettings, args: list[str]):
        """Update settings from a given list of args"""
        arg_tokens = self.arg_tokenizer.tokenize_args(args)
        self.update_settings_from_arg_tokens(settings, arg_tokens)

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
        for opt in sorted([o for o in self.options if o.long_arg != 'path'], key=lambda o: o.sort_arg):
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
