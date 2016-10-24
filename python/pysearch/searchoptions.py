# -*- coding: utf-8 -*-
################################################################################
#
# searchoptions.py
#
# class SearchOptions: defines the available command-line options and
#                      corresponding utility methods
#
################################################################################
from collections import deque
from cStringIO import StringIO
import json
import os
import platform
import sys
import xml.dom.minidom as minidom

from common import get_text
from config import SEARCHOPTIONSPATH
from searchoption import SearchOption
from searchsettings import SearchSettings

class SearchOptions(object):
    """class to provide usage info and parse command-line arguments into settings"""

    def set_dicts(self):
        self.arg_action_dict = {
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
            'in-linesafterpattern':
                lambda x, settings:
                    settings.add_patterns(x, 'in_linesafterpatterns'),
            'in-linesbeforepattern':
                lambda x, settings:
                    settings.add_patterns(x, 'in_linesbeforepatterns'),
            'linesafter':
                lambda x, settings:
                    settings.set_property('linesafter', int(x)),
            'linesaftertopattern':
                lambda x, settings:
                    settings.add_patterns(x, 'linesaftertopatterns'),
            'linesafteruntilpattern':
                lambda x, settings:
                    settings.add_patterns(x, 'linesafteruntilpatterns'),
            'linesbefore':
                lambda x, settings:
                    settings.set_property('linesbefore', int(x)),
            'maxlinelength':
                lambda x, settings:
                    settings.set_property('maxlinelength', int(x)),
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
            'out-linesafterpattern':
                lambda x, settings:
                    settings.add_patterns(x, 'out_linesafterpatterns'),
            'out-linesbeforepattern':
                lambda x, settings:
                    settings.add_patterns(x, 'out_linesbeforepatterns'),
            'search':
                lambda x, settings:
                    settings.add_patterns(x, 'searchpatterns'),
            'settings-file':
                lambda x, settings:
                    self.settings_from_file(x, settings)
        }

        self.bool_flag_action_dict = {
            'allmatches':
                lambda b, settings:
                    settings.set_property('firstmatch', not b),
            'archivesonly':
                lambda b, settings:
                    settings.set_property('archivesonly', b),
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
            'multilinesearch':
                lambda b, settings:
                    settings.set_property('multilinesearch', b),
            'noprintmatches':
                lambda b, settings:
                    settings.set_property('printresults', not b),
            'norecursive':
                lambda b, settings:
                    settings.set_property('recursive', not b),
            'nosearcharchives':
                lambda b, settings:
                    settings.set_property('searcharchives', not b),
            'printmatches':
                lambda b, settings:
                    settings.set_property('printresults', b),
            'recursive':
                lambda b, settings:
                    settings.set_property('recursive', b),
            'searcharchives':
                lambda b, settings:
                    settings.set_property('searcharchives', b),
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
        self.longarg_dict = {}

    def __init__(self):
        self.options = []
        self.set_dicts()
        self.set_options_from_xml()

    def settings_from_file(self, filepath, settings):
        assert os.path.exists(filepath), 'Settings file not found: %s' % filepath
        with open(filepath) as f:
            jsonstr = f.read()
            # print "jsonstr: '%s'" % jsonstr
            self.settings_from_json(jsonstr, settings)

    def settings_from_json(self, jsonstr, settings):
        json_dict = json.loads(jsonstr)
        for arg in json_dict:
            if arg in self.arg_action_dict:
                self.arg_action_dict[arg](json_dict[arg], settings)
            elif arg in self.bool_flag_action_dict:
                self.bool_flag_action_dict[arg](json_dict[arg], settings)
            elif arg == 'startpath':
                settings.startpath = json_dict[arg]
            else:
                raise Exception('Invalid option: {0}'.format(arg))

    def set_options_from_xml(self):
        searchoptionsdom = minidom.parse(SEARCHOPTIONSPATH)
        searchoptionnodes = searchoptionsdom.getElementsByTagName('searchoption')
        for searchoptionnode in searchoptionnodes:
            name = searchoptionnode.getAttribute('long')
            short = searchoptionnode.getAttribute('short')
            desc = get_text(searchoptionnode.childNodes).strip()
            func = None
            if name in self.arg_action_dict:
                func = self.arg_action_dict[name]
            elif name in self.bool_flag_action_dict:
                func = self.bool_flag_action_dict[name]
            else:
                raise ValueError('Unknown search option: %s' % name)
            option = SearchOption(short, name, desc, func)
            self.longarg_dict[name] = name
            if short:
                self.longarg_dict[short] = name
            self.options.append(option)

    def search_settings_from_args(self, args):
        """Returns a SearchSettings instance for a given list of args"""
        settings = SearchSettings()
        # default printresults to True since running from command line
        settings.printresults = True
        argdeque = deque(args)
        while argdeque:
            arg = argdeque.popleft()
            if arg.startswith('-'):
                while arg and arg.startswith('-'):
                    arg = arg[1:]
                longarg = self.longarg_dict[arg]
                if longarg in self.arg_action_dict:
                    if argdeque:
                        argval = argdeque.popleft()
                        self.arg_action_dict[longarg](argval, settings)
                    else:
                        raise Exception('Missing value for option {0}'.
                                        format(arg))
                elif longarg in self.bool_flag_action_dict:
                    self.bool_flag_action_dict[arg](True, settings)
                    if arg in ('h', 'help', 'V', 'version'):
                        return settings
                else:
                    raise Exception('Invalid option: {0}'.format(arg))
            else:
                settings.startpath = arg
        return settings

    def usage(self):
        print self.get_usage_string()
        sys.exit(1)

    def get_usage_string(self):
        sio = StringIO()
        sio.write('Usage:\n')
        sio.write(' pysearch.py [options] -s <searchpattern> <startpath>\n\nOptions:\n')
        opt_strings = []
        opt_descs = []
        longest = 0
        for opt in sorted(self.options, key=lambda opt: opt.sortarg):
            opt_string = ''
            if opt.shortarg:
                opt_string += '-{0},'.format(opt.shortarg)
            opt_string += '--{0}'.format(opt.longarg)
            if len(opt_string) > longest:
                longest = len(opt_string)
            opt_strings.append(opt_string)
            opt_descs.append(opt.desc)
        format_string = ' %%-%ds  %%s\n' % longest
        for i, opt_string in enumerate(opt_strings):
            sio.write(format_string % (opt_string, opt_descs[i]))
        usage = sio.getvalue()
        sio.close()
        return usage
