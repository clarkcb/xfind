# -*- coding: utf-8 -*-
################################################################################
#
# xfind.py
#
# Shared domain classes, properties, functions for xfind scripts
#
################################################################################
import os
import platform
import re

########################################
# Configuration
########################################
xfind_dict = {
    'c':          'cfind',
    'clj':        'cljfind',
    'clojure':    'cljfind',
    'cpp':        'cppfind',
    'cs':         'csfind',
    'csharp':     'csfind',
    'dart':       'dartfind',
    'fs':         'fsfind',
    'fsharp':     'fsfind',
    'go':         'gofind',
    'groovy':     'groovyfind',
    'haskell':    'hsfind',
    'hs':         'hsfind',
    'java':       'javafind',
    'javascript': 'jsfind',
    'js':         'jsfind',
    'kotlin':     'ktfind',
    'kt':         'ktfind',
    'objc':       'objcfind',
    # 'ocaml':      'mlfind',
    # 'ml':         'mlfind',
    'perl':       'plfind',
    'pl':         'plfind',
    'php':        'phpfind',
    'powershell': 'ps1find',
    'ps1':        'ps1find',
    'py':         'pyfind',
    'python':     'pyfind',
    'rb':         'rbfind',
    'ruby':       'rbfind',
    'rs':         'rsfind',
    'rust':       'rsfind',
    'scala':      'scalafind',
    'swift':      'swiftfind',
    'ts':         'tsfind',
    'typescript': 'tsfind',
}
win_supported = [
    'cs', 'csharp', 'fs', 'fsharp', 'go', 'haskell', 'javascript', 'js',
    'perl', 'pl', 'ps1', 'py', 'python', 'rb', 'ruby'
]
all_xfind_names = sorted(list(set(xfind_dict.values())))
HOME_NAME = 'HOME'
if platform.system() == 'Windows':
    HOME_NAME = 'USERPROFILE'
    all_xfind_names = sorted([xfind_dict[l] for l in win_supported])

xfind_name_regex = re.compile(r'\b({})(\.exe)?\b'.format('|'.join(all_xfind_names)), re.I | re.S)

default_runs = 10

HOME = os.environ[HOME_NAME]

# set XFINDPATH, default to $HOME/src/xfind but override with env var if defined
XFINDPATH = os.path.join(HOME, 'src', 'xfind')
if 'XFIND_PATH' in os.environ:
    XFINDPATH = os.environ['XFIND_PATH']
elif 'XFINDPATH' in os.environ:
    XFINDPATH = os.environ['XFINDPATH']

default_startpath = XFINDPATH


def non_matching_lens(xfind_output, skip_blanks: bool = True):
    """Examines xfind_output (a dict of {xfind_name : [lines]})
       and returns a dict of xfind instances with non-matching
       output line lengths ({xfind_name: [non_matching_xfind_names]})
    """
    non_matching = {}
    xs = sorted(xfind_output.keys())
    while xs:
        x = xs.pop(0)
        x_output = xfind_output[x]
        if skip_blanks:
            x_output = [l for l in x_output if l.strip() != '']
        x_len = len(x_output)
        for y in xs:
            y_output = xfind_output[y]
            if skip_blanks:
                y_output = [l for l in y_output if l.strip() != '']
            y_len = len(y_output)
            if x_len != y_len:
                non_matching.setdefault(x, []).append(y)
                non_matching.setdefault(y, []).append(x)
    return non_matching


def non_matching_outputs(xfind_output: dict[str, list[str]], sort_lines: bool = True, skip_blanks: bool = True):
    """Examines xfind_output (a dict of {xfind_name : [lines]})
       and returns a dict of xfind instances with non-matching
       output ({xfind_name: [non_matching_xfind_names]})
    """
    non_matching = {}
    xs = sorted(xfind_output.keys())
    while xs:
        x = xs.pop(0)
        x_output = sorted(xfind_output[x]) if sort_lines else xfind_output[x]
        if skip_blanks:
            x_output = [l for l in x_output if l.strip() != '']
        for y in xs:
            y_output = sorted(xfind_output[y]) if sort_lines else xfind_output[y]
            if skip_blanks:
                y_output = [l for l in y_output if l.strip() != '']
            if x_output != y_output:
                # print("\n{}:\n\"{}\"".format(x, x_output))
                # print("\n{}:\n\"{}\"".format(y, y_output))
                non_matching.setdefault(x, []).append(y)
                non_matching.setdefault(y, []).append(x)
    return non_matching
