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
    'perl', 'pl', 'py', 'python', 'rb', 'ruby'
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

def nonmatching_lens(xfind_output):
    """Examines xfind_output (a dict of {xfind_name : [lines]})
       and returns a dict of xfind instances with non-matching
       output line lengths ({xfind_name: [non_matching_xfind_names]})
    """
    nonmatching = {}
    xs = sorted(xfind_output.keys())
    while xs:
        x = xs.pop(0)
        for y in xs:
            x_len = len(xfind_output[x])
            y_len = len(xfind_output[y])
            if x_len != y_len:
                nonmatching.setdefault(x, []).append(y)
                nonmatching.setdefault(y, []).append(x)
    return nonmatching

def nonmatching_outputs(xfind_output):
    """Examines xfind_output (a dict of {xfind_name : [lines]})
       and returns a dict of xfind instances with non-matching
       output ({xfind_name: [non_matching_xfind_names]})
    """
    nonmatching = {}
    xs = sorted(xfind_output.keys())
    while xs:
        x = xs.pop(0)
        for y in xs:
            x_output = xfind_output[x]
            y_output = xfind_output[y]
            if x_output != y_output:
                # print("\n{}:\n\"{}\"".format(x, x_output))
                # print("\n{}:\n\"{}\"".format(y, y_output))
                nonmatching.setdefault(x, []).append(y)
                nonmatching.setdefault(y, []).append(x)
    return nonmatching
