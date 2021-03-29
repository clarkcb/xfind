#!/usr/bin/env python3
# -*- coding: utf-8 -*-
################################################################################
#
# xsearch2xfind.py
#
# Performs a (partial) conversion of a copy of xsearch to xfind
#
# Phase 1: rename paths and filenames
# Phase 2: rename namespaces, modules, classes, etc.
#
################################################################################
import csv
import os
import subprocess
import sys
from typing import List, Tuple


class XSearchToXFindConverter(object):
    """a class to convert a copy of xsearch to xfind"""

    def __init__(self, **kwargs):
        self.debug = False
        self.basepath = os.path.join(os.path.expanduser('~'), 'src/xfind')
        self.csvfile = None
        self.langs = []
        self.__dict__.update(kwargs)
        self.lang_config = {
            'clojure': {
                'exts': ['clj', 'md'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'clj',
            },
            'cpp': {
                'exts': ['cpp', 'h', 'sh'],
                'files': ['CMakeLists.txt'],
                'prefix': 'cpp',
            },
            'csharp': {
                'exts': ['cs', 'csproj', 'sln', 'sh', 'bat'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'cs',
            },
            'dart': {
                'exts': ['dart', 'md', 'sh'],
                'files': ['pubspec.yaml'],
                'prefix': 'dart',
            },
            'fsharp': {
                'exts': ['fs', 'fsproj', 'sln', 'sh', 'bat'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'fs',
            },
            'go': {
                'exts': ['go', 'mod'],
                'prefix': 'go',
            },
            'haskell': {
                'exts': ['hs', 'cabal', 'yaml', 'md', 'sh', 'bat'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'hs',
            },
            'java': {
                'exts': ['java'],
                'files': ['pom.xml', 'config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'java',
            },
            'javascript': {
                'exts': ['js', 'bat'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml', 'package.json'],
                'prefix': 'js',
            },
            'kotlin': {
                'exts': ['kt', 'gradle'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'kt',
            },
            'objc': {
                'exts': ['h', 'm', 'sh'],
                'prefix': 'objc',
            },
            'ocaml': {
                'exts': ['ml', 'mli', 'sh'],
                'prefix': 'ml',
            },
            'perl': {
                'exts': ['pl', 'pm', 't', 'bat', 'sh'],
                'files': ['META.json', 'config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'pl',
            },
            'php': {
                'exts': ['php', 'md'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'php',
            },
            'python': {
                'exts': ['py', 'md', 'bat'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'py',
            },
            'ruby': {
                'exts': ['rb', 'gemspec', 'md', 'sh', 'bat'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'rb',
            },
            'rust': {
                'exts': ['rs', 'toml', 'sh'],
                'prefix': 'rs',
            },
            'scala': {
                'exts': ['scala', 'sbt'],
                'files': ['config.json', 'findoptions.json', 'findoptions.xml'],
                'prefix': 'scala',
            },
            'swift': {
                'exts': ['swift', 'md', 'sh'],
                'prefix': 'swift',
            },
            'typescript': {
                'exts': ['ts', 'bat'],
                'files': ['package.json', 'findoptions.json'],
                'prefix': 'ts',
            },
        }
        self.search_repls = {
            'xsearch': 'xfind',
            'Search': 'Find',
            'SEARCH': 'FIND',
            'searchoptions': 'findoptions',
            'searched': 'found',
            'searcher': 'finder',
            'multilinesearch': 'multilineoption-REMOVE',
            'search': 'find',
        }

    def __get_proc_lines(self, proc_args: List[str]) -> Tuple[List[str],List[str]]:
        proc = subprocess.Popen(proc_args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out_lines = []
        err_lines = []
        while True:
            out_line = proc.stdout.readline()
            err_line = proc.stderr.readline()
            if not out_line and not err_line:
                break
            if out_line:
                out_line = out_line.decode('utf-8').rstrip()
                out_lines.append(out_line)
            if err_line:
                err_line = err_line.decode('utf-8').rstrip()
                err_lines.append(err_line)
        proc.terminate()
        return (out_lines, err_lines)

    def get_files(self, name: str) -> List[str]:
        cmd = ['find', '.', '-name', '{}'.format(name)]
        print('{}'.format(' '.join(cmd)))
        (out_lines, err_lines) = self.__get_proc_lines(cmd)
        if err_lines:
            err_msg = 'ERROR:\n{}'.format('\n'.join(err_lines))
            print(err_msg)
            raise KeyboardInterrupt
        return out_lines


    def execute_cmd(self, cmd: List[str]) -> bool:
        print('{}'.format(' '.join(cmd)))
        (out_lines, err_lines) = self.__get_proc_lines(cmd)
        if err_lines:
            print('ERROR:\n{}'.format('\n'.join(err_lines)))
            if err_lines[0].startswith('fatal: renaming') and err_lines[0].endswith('No such file or directory'):
                return False
            raise KeyboardInterrupt
        return True

    def __replace_in_file(self, search_str, replace_str, filepath):
        # sed -i 's/SEARCH_REGEX/REPLACEMENT/g' INPUTFILE
        # sed -i '' -e "s/192.168.20.1/new.domain.com/" {}
        cmd = ['sed', '-i', '', '-e', 's/{}/{}/g'.format(search_str, replace_str), filepath]
        self.execute_cmd(cmd)

    def edit_files(self):
        # for lang in self.langs:
        for lang in [l for l in self.langs if l == 'typescript']:
            currpath = os.path.join(self.basepath, lang)
            print('\ncd {}'.format(currpath))
            os.chdir(currpath)
            name_search = '{}search'.format(self.lang_config[lang]['prefix'])
            name_replace = '{}find'.format(self.lang_config[lang]['prefix'])
            if 'exts' in self.lang_config[lang]:
                for ext in self.lang_config[lang]['exts']:
                    for f in self.get_files('*.{}'.format(ext)):
                        self.__replace_in_file(name_search, name_replace, f)
                        for s in self.search_repls.keys():
                            self.__replace_in_file(s, self.search_repls[s], f)
            if 'files' in self.lang_config[lang]:
                for filename in self.lang_config[lang]['files']:
                    for f in self.get_files(filename):
                        self.__replace_in_file(name_search, name_replace, f)
                        for s in self.search_repls.keys():
                            self.__replace_in_file(s, self.search_repls[s], f)

    def move_files(self):
        with open(self.csvfile) as f:
            reader = csv.reader(f)
            tld = ''
            currpath = self.basepath
            for row in reader:
                if not row or len(row) < 3:
                    continue
                if row[0] == 'lang':
                    continue
                if tld != row[0]:
                    tld = row[0]
                    self.langs.append(tld)
                    currpath = os.path.join(self.basepath, tld)
                    if not os.path.exists(currpath):
                        raise Exception('path not found: {}'.format(currpath))
                    print('\ncd {}'.format(currpath))
                    os.chdir(currpath)
                cmd = row[1]
                if not cmd:
                    raise Exception('invalid command')
                if cmd == 'mkdir -p':
                    newdir = row[2]
                    if not newdir:
                        raise Exception('invalid newdir')
                    newpath = os.path.join(currpath, newdir)
                    if not os.path.exists(newpath):
                        print('{} {}'.format(cmd, newdir))
                        os.makedirs(newdir)
                elif cmd in ('git mv', 'mv'):
                    frompath = row[2]
                    if not frompath:
                        raise Exception('invalid frompath')
                    topath = row[3]
                    if not topath:
                        raise Exception('invalid topath')
                    if os.path.exists(os.path.join(currpath, frompath)):
                        self.execute_cmd(cmd.split(' ') + [frompath, topath])
                elif cmd in ('git rm', 'git rm -r', 'rm -rf', 'rm'):
                    targetpath = row[2]
                    if not targetpath:
                        raise Exception('invalid targetpath')
                    if os.path.exists(os.path.join(currpath, row[2])):
                        self.execute_cmd(cmd.split(' ') + [targetpath])
                elif cmd == 'rmdir':
                    targetpath = row[2]
                    if not targetpath:
                        raise Exception('invalid targetpath')
                    if os.path.exists(os.path.join(currpath, row[2])):
                        if os.path.exists(os.path.join(currpath, row[2], '.DS_Store')):
                            self.execute_cmd(['rm', os.path.join(currpath, row[2], '.DS_Store')])
                        self.execute_cmd(cmd.split(' ') + [targetpath])
                else:
                    raise Exception('unsupported cmd: {}'.format(cmd))

    def convert(self):
        self.move_files()
        self.edit_files()


def main():
    basepath = os.path.join(os.path.expanduser('~'), 'src/xfind')
    csvfile = os.path.join(basepath, 'scripts/xsearch2xfind.csv')
    converter = XSearchToXFindConverter(basepath=basepath, csvfile=csvfile)
    converter.convert()


if __name__ == '__main__':
    main()
