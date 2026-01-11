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
lang_alias_dict = {
    'bash': 'bash',
    'C': 'c',
    'c': 'c',
    'clj': 'clojure',
    'clojure': 'clojure',
    'c++': 'cpp',
    'cpp': 'cpp',
    'c#': 'csharp',
    'cs': 'csharp',
    'csharp': 'csharp',
    'dart': 'dart',
    'elixir': 'elixir',
    'ex': 'elixir',
    'f#': 'fsharp',
    'fs': 'fsharp',
    'fsharp': 'fsharp',
    'go': 'go',
    'groovy': 'groovy',
    'haskell': 'haskell',
    'hs': 'haskell',
    'java': 'java',
    'javascript': 'javascript',
    'js': 'javascript',
    'kotlin': 'kotlin',
    'kt': 'kotlin',
    'objc': 'objc',
    'ocaml': 'ocaml',
    'ml': 'ocaml',
    'perl': 'perl',
    'pl': 'perl',
    'php': 'php',
    'powershell': 'powershell',
    'ps1': 'powershell',
    'pwsh': 'powershell',
    'py': 'python',
    'python': 'python',
    'rb': 'ruby',
    'ruby': 'ruby',
    'rs': 'rust',
    'rust': 'rust',
    'scala': 'scala',
    'swift': 'swift',
    'ts': 'typescript',
    'typescript': 'typescript'
}
xfind_dict = {
    'bash':       'bashfind',
    'c':          'cfind',
    'clojure':    'cljfind',
    'cpp':        'cppfind',
    'csharp':     'csfind',
    'dart':       'dartfind',
    'elixir':     'exfind',
    'fsharp':     'fsfind',
    'go':         'gofind',
    'groovy':     'groovyfind',
    'haskell':    'hsfind',
    'java':       'javafind',
    'javascript': 'jsfind',
    'kotlin':     'ktfind',
    'objc':       'objcfind',
    # 'ocaml':      'mlfind',
    'perl':       'plfind',
    'php':        'phpfind',
    'powershell': 'ps1find',
    'python':     'pyfind',
    'ruby':       'rbfind',
    'rust':       'rsfind',
    'scala':      'scalafind',
    'swift':      'swiftfind',
    'typescript': 'tsfind',
}
win_supported = [ 'csharp', 'fsharp', 'go', 'haskell', 'javascript', 'perl', 'python', 'ruby']
all_xfind_names = sorted(list(set(xfind_dict.values())))
all_langs = sorted(list(set(xfind_dict.keys())))
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
