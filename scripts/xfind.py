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
    'elixir':     'exfind',
    'ex':         'exfind',
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


def lines_for_diff(lines: list[str],
                   skip_blanks: bool = False,
                   sort_lines: bool = False,
                   case_insensitive_cmp: bool = False,
                   normalize_field_names: bool = False) -> list[str]:
    """Return lines modified according to different settings"""
    diff_lines = lines[:]
    if skip_blanks:
        diff_lines = [line for line in diff_lines if line.strip() != '']
    if sort_lines:
        diff_lines = list(sorted(diff_lines))
    if case_insensitive_cmp:
        diff_lines = [line.upper() for line in diff_lines]
    if normalize_field_names:
        diff_lines = [
            line.replace('_', '').replace('-', '')
            for line in diff_lines
        ]
    return diff_lines


def non_matching_lens(xfind_output: dict[str, list[str]],
                      skip_blanks: bool = True) -> list[tuple[str, str]]:
    """Examines xfind_output (a dict of {xfind_name : [lines]})
       and returns a list of tuples of non-matching xfind pairs
       ([(xfind_name_1, xfind_name_2)]
    """
    non_matching = []
    xs = sorted(xfind_output.keys())
    while xs:
        x = xs.pop(0)
        x_lines = lines_for_diff(xfind_output[x], skip_blanks=skip_blanks)
        x_len = len(x_lines)
        for y in xs:
            y_lines = lines_for_diff(xfind_output[y], skip_blanks=skip_blanks)
            y_len = len(y_lines)
            if x_len != y_len:
                x_and_y = list(sorted([x, y]))
                x_and_y = (x_and_y[0], x_and_y[1])
                if x_and_y not in non_matching:
                    non_matching.append(x_and_y)
    return non_matching


def non_matching_outputs(xfind_output: dict[str, list[str]],
                         sort_lines: bool = True,
                         skip_blanks: bool = True,
                         case_insensitive_cmp: bool = False,
                         normalize_field_names: bool = False) -> list[tuple[str, str]]:
    """Examines xfind_output (a dict of {xfind_name : [lines]})
       and returns a list of tuples of non-matching xfind pairs
      ([(xfind_name_1, xfind_name_2)]
    """
    non_matching = []
    xs = sorted(xfind_output.keys())
    while xs:
        x = xs.pop(0)
        x_lines = lines_for_diff(xfind_output[x],
                                 skip_blanks=skip_blanks,
                                 sort_lines=sort_lines,
                                 case_insensitive_cmp=case_insensitive_cmp,
                                 normalize_field_names=normalize_field_names)
        for y in xs:
            y_lines = lines_for_diff(xfind_output[y],
                                     skip_blanks=skip_blanks,
                                     sort_lines=sort_lines,
                                     case_insensitive_cmp=case_insensitive_cmp,
                                     normalize_field_names=normalize_field_names)
            if x_lines != y_lines:
                # print("\n{}:\n\"{}\"".format(x, x_output))
                # print("\n{}:\n\"{}\"".format(y, y_output))
                x_and_y = list(sorted([x, y]))
                x_and_y = (x_and_y[0], x_and_y[1])
                if x_and_y not in non_matching:
                    non_matching.append(x_and_y)
    return non_matching
