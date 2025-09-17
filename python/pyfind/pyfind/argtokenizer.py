# -*- coding: utf-8 -*-
"""
###############################################################################
#
# argtokenizer.py
#
# class ArgTokenizer: parse arguments into tokens
#
###############################################################################
"""
import json
import os
from collections import deque
from enum import Enum
from typing import Any

from .common import parse_datetime_str
from .findexception import FindException


class ArgTokenType(Enum):
    """Enum for argument token types"""
    BOOL = 1
    STR = 2
    INT = 3
    DATE = 4


class ArgToken:
    """class to represent a command-line argument token"""

    __slots__ = [
        'name', 'token_type', 'value'
    ]
    def __init__(self, name: str, token_type: ArgTokenType, value: Any = None):
        self.name: str = name
        self.token_type: ArgTokenType = token_type
        self.value: Any = value


class ArgTokenizer:
    """class to tokenize args in various formats into tokens"""

    __slots__ = [
        'bool_dict', 'date_dict', 'int_dict', 'str_dict'
    ]
    def __init__(self,
                 bool_dict: dict[str, str] = None,
                 date_dict: dict[str, str] = None,
                 str_dict: dict[str, str] = None,
                 int_dict: dict[str, str] = None):
        self.bool_dict: dict[str, str] = {}
        if bool_dict:
            self.bool_dict.update(bool_dict)
        self.date_dict: dict[str, str] = {}
        if date_dict:
            self.date_dict.update(date_dict)
        self.int_dict: dict[str, str] = {}
        if int_dict:
            self.int_dict.update(int_dict)
        self.str_dict: dict[str, str] = {}
        if str_dict:
            self.str_dict.update(str_dict)

    def tokenize_args(self, args: list[str]) -> list[ArgToken]:
        """Tokenize list of args"""
        arg_tokens: list[ArgToken] = []
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
                        arg_names.append((arg_name, arg_name))
                    else:
                        raise FindException(f'Invalid option: {arg}')
                elif len(arg) > 1:
                    for c in arg[1:]:
                        if c in self.bool_dict:
                            arg_names.append((c, self.bool_dict[c]))
                        elif c in self.str_dict:
                            arg_names.append((c, self.str_dict[c]))
                        elif c in self.int_dict:
                            arg_names.append((c, self.int_dict[c]))
                        elif c in self.date_dict:
                            arg_names.append((c, self.date_dict[c]))
                        else:
                            raise FindException(f'Invalid option: {c}')
                else:
                    raise FindException(f'Invalid option: {arg}')
                for (used_name, arg_name) in arg_names:
                    if arg_name in self.bool_dict:
                        arg_tokens.append(ArgToken(arg_name, ArgTokenType.BOOL, True))
                        if arg_name in ('help', 'version'):
                            return arg_tokens
                    elif arg_name in self.str_dict or \
                            arg_name in self.date_dict or \
                            arg_name in self.int_dict or \
                            arg_name == 'settings-file':
                        if arg_deque:
                            arg_val = arg_deque.popleft()
                            if arg_name in self.str_dict:
                                arg_tokens.append(ArgToken(arg_name, ArgTokenType.STR, arg_val))
                            elif arg_name in self.int_dict:
                                invalid_int = False
                                i = 0
                                try:
                                    i = int(arg_val)
                                except ValueError:
                                    invalid_int = True
                                else:
                                    if i < 0:
                                        invalid_int = True
                                if invalid_int:
                                    err = f'Invalid value for option {used_name}: {arg_val}'
                                    raise FindException(err)
                                arg_tokens.append(ArgToken(arg_name, ArgTokenType.INT, i))
                            elif arg_name in self.date_dict:
                                arg_tokens.append(ArgToken(arg_name, ArgTokenType.DATE, parse_datetime_str(arg_val)))
                            elif arg_name == 'settings-file':
                                # TODO: should this be its own type?
                                arg_tokens.append(ArgToken(arg_name, ArgTokenType.STR, arg_val))
                        else:
                            raise FindException(f'Missing value for option {used_name}')
                    else:
                        raise FindException(f'Invalid option: {used_name}')
            else:
                arg_tokens.append(ArgToken('path', ArgTokenType.STR, arg))
        return arg_tokens

    def tokenize_arg_dict(self, arg_dict: dict[str, Any]) -> list[ArgToken]:
        """Tokenize a dict or args"""
        arg_tokens = []
        # keys are sorted so that output is consistent across all versions
        for key in sorted(arg_dict.keys()):
            val = arg_dict[key]
            if key in self.bool_dict:
                if val is True or val is False:
                    arg_tokens.append(ArgToken(self.bool_dict[key], ArgTokenType.BOOL, val))
                else:
                    raise FindException(f'Invalid value for option: {key}')
            elif key in self.str_dict:
                if type(val) == str:
                    arg_tokens.append(ArgToken(self.str_dict[key], ArgTokenType.STR, val))
                elif type(val) == list:
                    for item in val:
                        if type(item) == str:
                            arg_tokens.append(ArgToken(self.str_dict[key], ArgTokenType.STR, item))
                        else:
                            raise FindException(f'Invalid value for option: {key}')
                else:
                    raise FindException(f'Invalid value for option: {key}')
            elif key in self.int_dict:
                if type(val) == int:
                    arg_tokens.append(ArgToken(self.int_dict[key], ArgTokenType.INT, val))
                else:
                    raise FindException(f'Invalid value for option: {key}')
            elif key in self.date_dict:
                if type(val) == str:
                    arg_tokens.append(ArgToken(self.date_dict[key], ArgTokenType.DATE, parse_datetime_str(val)))
                else:
                    raise FindException(f'Invalid value for option: {key}')
            else:
                raise FindException(f'Invalid option: {key}')
        return arg_tokens

    def tokenize_json(self, json_str: str) -> list[ArgToken]:
        """Tokenize a JSON string"""
        json_dict = json.loads(json_str)
        return self.tokenize_arg_dict(json_dict)

    def tokenize_file(self, file_path: str) -> list[ArgToken]:
        """Tokenize a JSON file"""
        expanded_path = os.path.expanduser(file_path)
        if not os.path.exists(expanded_path):
            raise FindException(f'Settings file not found: {file_path}')
        if not file_path.strip().endswith('.json'):
            raise FindException(f'Invalid settings file (must be JSON): {file_path}')
        with open(expanded_path, encoding='UTF-8') as f:
            json_str = f.read()
        try:
            return self.tokenize_json(json_str)
        except json.JSONDecodeError:
            raise FindException(f'Unable to parse JSON in settings file: {file_path}')
