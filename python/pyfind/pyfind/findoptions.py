# -*- coding: utf-8 -*-
###############################################################################
#
# findoptions.py
#
# class FindOptions: defines the available command-line options and
#                      corresponding utility methods
#
###############################################################################
import json
import os
import sys
import xml.dom.minidom as minidom
from collections import deque
from io import StringIO
from typing import List

from .common import get_text
from .config import FINDOPTIONSPATH
from .findexception import FindException
from .findoption import FindOption
from .findsettings import FindSettings


class FindOptions(object):
    """class to provide usage info and parse command-line arguments into settings"""

    def __init__(self):
        self.options = []
        self.__set_dicts()
        self.__set_options_from_json()

    def __set_dicts(self):
        self.__bool_arg_dict = {
            'allmatches':
                lambda b, settings:
                settings.set_property('firstmatch', not b),
            'archivesonly':
                lambda b, settings:
                settings.set_property('archivesonly', b),
            'colorize':
                lambda b, settings:
                settings.set_property('colorize', b),
            'debug':
                lambda b, settings:
                settings.set_property('debug', b),
            'excludehidden':
                lambda b, settings:
                settings.set_property('excludehidden', b),
            'firstmatch':
                lambda b, settings:
                settings.set_property('firstmatch', b),
            'help':
                lambda b, settings:
                settings.set_property('printusage', b),
            'includehidden':
                lambda b, settings:
                settings.set_property('excludehidden', not b),
            'listdirs':
                lambda b, settings:
                settings.set_property('listdirs', b),
            'listfiles':
                lambda b, settings:
                settings.set_property('listfiles', b),
            'listlines':
                lambda b, settings:
                settings.set_property('listlines', b),
            'multilineoption-REMOVE':
                lambda b, settings:
                settings.set_property('multilineoption-REMOVE', b),
            'nocolorize':
                lambda b, settings:
                settings.set_property('colorize', not b),
            'noprintmatches':
                lambda b, settings:
                settings.set_property('printresults', not b),
            'norecursive':
                lambda b, settings:
                settings.set_property('recursive', not b),
            'nofindarchives':
                lambda b, settings:
                settings.set_property('findarchives', not b),
            'printmatches':
                lambda b, settings:
                settings.set_property('printresults', b),
            'recursive':
                lambda b, settings:
                settings.set_property('recursive', b),
            'findarchives':
                lambda b, settings:
                settings.set_property('findarchives', b),
            'uniquelines':
                lambda b, settings:
                settings.set_property('uniquelines', b),
            'verbose':
                lambda b, settings:
                settings.set_property('verbose', b),
            'version':
                lambda b, settings:
                settings.set_property('printversion', b)
        }

        self.__coll_arg_dict = {
            'in-archiveext':
                lambda x, settings:
                settings.add_exts(x, 'in_archiveextensions'),
            'in-archivefilepattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_archivefilepatterns'),
            'in-dirpattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_dirpatterns'),
            'in-ext':
                lambda x, settings:
                settings.add_exts(x, 'in_extensions'),
            'in-filepattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_filepatterns'),
            'in-filetype':
                lambda x, settings:
                settings.add_filetypes(x, 'in_filetypes'),
            'in-linesafterpattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_linesafterpatterns'),
            'in-linesbeforepattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_linesbeforepatterns'),
            'linesaftertopattern':
                lambda x, settings:
                settings.add_patterns(x, 'linesaftertopatterns'),
            'linesafteruntilpattern':
                lambda x, settings:
                settings.add_patterns(x, 'linesafteruntilpatterns'),
            'out-archiveext':
                lambda x, settings:
                settings.add_exts(x, 'out_archiveextensions'),
            'out-archivefilepattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_archivefilepatterns'),
            'out-dirpattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_dirpatterns'),
            'out-ext':
                lambda x, settings:
                settings.add_exts(x, 'out_extensions'),
            'out-filepattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_filepatterns'),
            'out-filetype':
                lambda x, settings:
                settings.add_filetypes(x, 'out_filetypes'),
            'out-linesafterpattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_linesafterpatterns'),
            'out-linesbeforepattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_linesbeforepatterns'),
            'findpattern':
                lambda x, settings:
                settings.add_patterns(x, 'findpatterns')
        }

        self.__int_arg_dict = {
            'linesafter':
                lambda x, settings:
                settings.set_property('linesafter', int(x)),
            'linesbefore':
                lambda x, settings:
                settings.set_property('linesbefore', int(x)),
            'maxlinelength':
                lambda x, settings:
                settings.set_property('maxlinelength', int(x)),
        }

        self.__str_arg_dict = {
            'encoding':
                lambda x, settings:
                settings.set_property('textfileencoding', x),
            'startpath':
                lambda x, settings:
                settings.set_property('startpath', x),
        }

        self.__longarg_dict = {}

    def settings_from_file(self, filepath: str, settings: FindSettings):
        assert os.path.exists(filepath), \
            'Settings file not found: {0:s}'.format(filepath)
        with open(filepath) as f:
            jsonstr = f.read()
        self.settings_from_json(jsonstr, settings)

    def settings_from_json(self, jsonstr: str, settings: FindSettings):
        json_dict = json.loads(jsonstr)
        for arg in json_dict:
            if arg in self.__bool_arg_dict:
                self.__bool_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__coll_arg_dict:
                self.__coll_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__int_arg_dict:
                self.__int_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__str_arg_dict:
                self.__str_arg_dict[arg](json_dict[arg], settings)
            else:
                raise FindException('Invalid option: {0}'.format(arg))

    def __set_options_from_json(self):
        with open(FINDOPTIONSPATH, mode='r') as f:
            findoptions_dict = json.load(f)
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
            elif longarg in self.__int_arg_dict:
                func = self.__int_arg_dict[longarg]
            elif longarg in self.__str_arg_dict:
                func = self.__str_arg_dict[longarg]
            elif longarg in self.__str_arg_dict:
                func = self.__str_arg_dict[longarg]
            elif longarg == 'settings-file':
                func = self.settings_from_file
            else:
                raise FindException(
                    'Unknown find option: {0:s}'.format(longarg))
            self.options.append(FindOption(shortarg, longarg, desc, func))
            self.__longarg_dict[longarg] = longarg
            if shortarg:
                self.__longarg_dict[shortarg] = longarg

    def __set_options_from_xml(self):
        findoptionsdom = minidom.parse(FINDOPTIONSPATH)
        findoptionnodes = findoptionsdom.getElementsByTagName(
            'findoption')
        for findoptionnode in findoptionnodes:
            longarg = findoptionnode.getAttribute('long')
            shortarg = findoptionnode.getAttribute('short')
            desc = get_text(findoptionnode.childNodes).strip()
            if longarg in self.__bool_arg_dict:
                func = self.__bool_arg_dict[longarg]
            elif longarg in self.__coll_arg_dict:
                func = self.__coll_arg_dict[longarg]
            elif longarg in self.__int_arg_dict:
                func = self.__int_arg_dict[longarg]
            elif longarg in self.__str_arg_dict:
                func = self.__str_arg_dict[longarg]
            elif longarg == 'settings-file':
                func = self.settings_from_file
            else:
                raise FindException(
                    'Unknown find option: {0:s}'.format(longarg))
            self.options.append(FindOption(shortarg, longarg, desc, func))
            self.__longarg_dict[longarg] = longarg
            if shortarg:
                self.__longarg_dict[shortarg] = longarg

    def find_settings_from_args(self, args: List[str]) -> FindSettings:
        """Returns a FindSettings instance for a given list of args"""
        settings = FindSettings()
        # default printresults to True since running from command line
        settings.printresults = True
        argdeque = deque(args)
        while argdeque:
            arg = argdeque.popleft()
            if arg.startswith('-'):
                while arg and arg.startswith('-'):
                    arg = arg[1:]
                if arg in self.__longarg_dict:
                    longarg = self.__longarg_dict[arg]
                    if longarg in self.__bool_arg_dict:
                        self.__bool_arg_dict[longarg](True, settings)
                        if longarg in ('help', 'version'):
                            return settings
                    elif longarg in self.__coll_arg_dict or \
                            longarg in self.__int_arg_dict or \
                            longarg in self.__str_arg_dict or \
                            longarg == 'settings-file':
                        if argdeque:
                            argval = argdeque.popleft()
                            if longarg in self.__coll_arg_dict:
                                self.__coll_arg_dict[longarg](argval, settings)
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
                                    err = 'Invalid value for option {}: {}'.format(
                                        arg, argval)
                                    raise FindException(err)
                                self.__int_arg_dict[longarg](argval, settings)
                            elif longarg in self.__str_arg_dict:
                                self.__str_arg_dict[longarg](argval, settings)
                            elif longarg == 'settings-file':
                                self.settings_from_file(argval, settings)
                        else:
                            raise FindException('Missing value for option {0}'.
                                                  format(arg))
                    else:
                        raise FindException(
                            'Invalid option: {0}'.format(arg))
                else:
                    raise FindException('Invalid option: {0}'.format(arg))
            else:
                settings.startpath = arg
        return settings

    def usage(self):
        print(self.__get_usage_string())
        sys.exit(1)

    def __get_usage_string(self):
        sio = StringIO()
        sio.write('Usage:\n')
        sio.write(
            ' pyfind [options] -s <findpattern> <startpath>\n\nOptions:\n')
        opt_pairs = []
        longest = 0
        for opt in sorted(self.options, key=lambda o: o.sortarg):
            opt_string = ''
            if opt.shortarg:
                opt_string += '-{0:s},'.format(opt.shortarg)
            opt_string += '--{0:s}'.format(opt.longarg)
            if len(opt_string) > longest:
                longest = len(opt_string)
            opt_pairs.append((opt_string, opt.desc))
        format_string = ' {0:<' + str(longest) + 's}  {1:s}\n'
        for opt_pair in opt_pairs:
            sio.write(format_string.format(opt_pair[0], opt_pair[1]))
        usage = sio.getvalue()
        sio.close()
        return usage
