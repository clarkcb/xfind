#!/usr/bin/env python3
# -*- coding: utf-8 -*-
################################################################################
#
# benchmarker.py
#
# The generic classes to provide benchmarking functionality
#
################################################################################
from io import StringIO
import json
import os
import re
import subprocess
import sys

from termcolor import colored

from dataclasses import dataclass
from datetime import datetime
from tabulate import tabulate
from typing import Union


@dataclass
class Scenario:
    """Class to define a test scenario"""
    name: str
    args: list
    replace_exe_name: bool = False
    case_insensitive_cmp: bool = False


@dataclass
class ScenarioGroup:
    """Class for grouping scenarios"""
    name: str
    scenarios: list[Scenario]


# ########################################
# # Configuration
# ########################################
# #exts = ','.join('clj cpp cs dart fs go hs java js kt pl php py rb rs scala swift ts'.split())
# exts = ','.join('py rb'.split())
#
# startpaths = [os.path.join(XSEARCHPATH, d) for d in ('python', 'ruby')]
# scriptpath = os.path.join(XSEARCHPATH, 'scripts')
# sharedpath = os.path.join(XSEARCHPATH, 'shared')

default_runs = 10

default_ignore_dirs = ['build', 'cmake', 'node_modules', 'vendor', 'venv']
ignore_args = [elem for ignore_dir in [['-D', d] for d in default_ignore_dirs] for elem in ignore_dir]

time_keys = {'real', 'sys', 'user', 'total'}


########################################
# Classes
########################################
class LangResult(object):
    def __init__(self, name: str, real: float, sys: float, user: float, **kwargs):
        self.name = name
        self.real = real
        self.sys = sys
        self.user = user
        self.__dict__.update(kwargs)

    @property
    def total(self):
        return self.real + self.sys + self.user


class RunResult(object):
    def __init__(self, scenario: Scenario, run: int, lang_results: list[LangResult], **kwargs):
        self.scenario = scenario
        self.run = run
        self.lang_results = lang_results
        self.__dict__.update(kwargs)
        self.__ranks_dict = {}
        for t in time_keys:
            ranks = []
            if t in {'real', 'sys', 'user'}:
                for lr in self.lang_results:
                    ranks.append((lr.__dict__[t], lr.name))
            elif t == 'total':
                for lr in self.lang_results:
                    ranks.append((lr.total, lr.name))
            self.__ranks_dict[t] = [r[1] for r in sorted(ranks, key=lambda r: r[0])]

    @property
    def real_ranks(self):
        return self.__ranks_dict['real']

    @property
    def sys_ranks(self):
        return self.__ranks_dict['sys']

    @property
    def user_ranks(self):
        return self.__ranks_dict['user']

    @property
    def total_ranks(self):
        return self.__ranks_dict['total']


class ScenarioResult(object):
    def __init__(self, scenario: Scenario, index: int, run_results: list[RunResult], **kwargs):
        self.scenario = scenario
        self.index = index
        self.run_results = run_results
        self.__dict__.update(kwargs)
        self.__lang_results_dict = {}
        for r in self.run_results:
            for lr in r.lang_results:
                self.__lang_results_dict.setdefault(lr.name, []).append(lr)
        self.langs = set(self.__lang_results_dict.keys())
        self.__lang_totals_dict = {l: {} for l in self.langs}
        for l in self.langs:
            for t in {'real', 'sys', 'user'}:
                self.__lang_totals_dict[l][t] = \
                    sum([lr.__dict__[t] for lr in self.__lang_results_dict[l]])
            self.__lang_totals_dict[l]['total'] = \
                sum([lr.total for lr in self.__lang_results_dict[l]])
        # for each time type, a list of the languages in order from fastest to slowest
        self.__ranks_dict = {}
        for t in time_keys:
            type_totals = sorted([(self.__lang_totals_dict[l][t], l) for l in self.langs], key=lambda t: t[0])
            self.__ranks_dict[t] = [t[1] for t in type_totals]

    @property
    def runs(self) -> int:
        return len(self.run_results)

    def __rank_of_type(self, lang: str, time_type: str) -> int:
        return self.__ranks_dict[time_type].index(lang) + 1

    def total_real(self, lang: str) -> float:
        return self.__lang_totals_dict[lang]['real']

    def avg_real(self, lang: str) -> float:
        tr = self.total_real(lang)
        return tr / self.runs

    def rank_real(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'real')

    def total_sys(self, lang: str) -> float:
        return self.__lang_totals_dict[lang]['sys']

    def avg_sys(self, lang: str) -> float:
        ts = self.total_sys(lang)
        return ts / self.runs

    def rank_sys(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'sys')

    def total_user(self, lang: str) -> float:
        return self.__lang_totals_dict[lang]['user']

    def avg_user(self, lang: str) -> float:
        tu = self.total_user(lang)
        return tu / self.runs

    def rank_user(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'user')

    def total_total(self, lang: str) -> float:
        return self.__lang_totals_dict[lang]['total']

    def avg_total(self, lang: str) -> float:
        tt = self.total_total(lang)
        return tt / self.runs

    def rank_total(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'total')


class ScenarioResults(object):
    def __init__(self, scenarios: list[Scenario], exe_names: list[str], **kwargs):
        self.scenario_results = []
        self.scenarios = scenarios
        self.exe_names = exe_names
        self.__dict__.update(kwargs)
        self._update()

    def _update(self):
        self._exe_totals_dict = {x: {t: 0 for t in time_keys} for x in self.exe_names}
        for sr in self.scenario_results:
            for x in self.exe_names:
                self._exe_totals_dict[x]['real'] += sr.total_real(x)
                self._exe_totals_dict[x]['sys'] += sr.total_sys(x)
                self._exe_totals_dict[x]['user'] += sr.total_user(x)
                self._exe_totals_dict[x]['total'] += sr.total_total(x)
        # for each time type, a list of the languages in order from fastest to slowest
        self.__ranks_dict = {}
        for t in time_keys:
            type_totals = sorted([(self._exe_totals_dict[x][t], x) for x in self.exe_names], key=lambda t: t[0])
            self.__ranks_dict[t] = [t[1] for t in type_totals]

    @property
    def runs(self) -> int:
        return sum([sr.runs for sr in self.scenario_results])

    def append(self, scenario_result: ScenarioResult):
        self.scenario_results.append(scenario_result)
        self._update()

    def __len__(self):
        return len(self.scenario_results)

    def __rank_of_type(self, lang: str, time_type: str) -> int:
        return self.__ranks_dict[time_type].index(lang) + 1

    def total_real(self, lang: str) -> float:
        return self._exe_totals_dict[lang]['real']

    def avg_real(self, lang: str) -> float:
        tr = self.total_real(lang)
        if self.runs:
            return tr / self.runs
        return 0

    def rank_real(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'real')

    def total_sys(self, lang: str) -> float:
        return self._exe_totals_dict[lang]['sys']

    def avg_sys(self, lang: str) -> float:
        ts = self.total_sys(lang)
        if self.runs:
            return ts / self.runs
        return 0

    def rank_sys(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'sys')

    def total_user(self, lang: str) -> float:
        return self._exe_totals_dict[lang]['user']

    def avg_user(self, lang: str) -> float:
        tu = self.total_user(lang)
        if self.runs:
            return tu / self.runs
        return 0

    def rank_user(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'user')

    def total_total(self, lang: str) -> float:
        return self._exe_totals_dict[lang]['total']

    def avg_total(self, lang: str) -> float:
        tt = self.total_total(lang)
        if self.runs:
            return tt / self.runs
        return 0

    def rank_total(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'total')


########################################
# Benchmarker class
########################################
class Benchmarker(object):
    def __init__(self, **kwargs):
        self.colorize = True
        self.langs = []
        self.exe_names = []
        self.group_names = []
        self.scenario_names = []
        self.scenarios = []
        self.scenarios_files = []
        self.runs = default_runs
        self.debug = True
        self.exit_on_diff = True
        self.exit_on_sort_diff = True
        self.ignore_blank_lines = True
        self.scenario_diff_dict = {}
        self.include_groups = []
        self.include_scenarios = []
        self.skip_groups = ['settings-only']
        self.skip_scenarios = ['use invalid settings-file']
        self.replace_values = {}
        self.__dict__.update(kwargs)
        # if not self.langs:
        #     self.langs = all_langs
        # self.exe_names = [exe_dict[l] for l in self.langs]
        if not self.langs:
            log_error('No languages specified')
            exit(1)
        if self.group_names and not self.include_groups:
            self.include_groups = self.group_names[:]
            self.group_names = []
        if self.scenario_names and not self.include_scenarios:
            self.include_scenarios = self.scenario_names[:]
            self.scenario_names = []
        self.shell = os.environ.get('SHELL', '/bin/bash')
        self.git_info = get_git_info()
        update_replace_values = {}
        for k in self.replace_values:
            if not k.startswith('$'):
                k = '$' + k
            update_replace_values[k] = self.replace_values[k]
        self.replace_values = update_replace_values
        self.exe_name_regex = re.compile(r'\b({})(\.exe)?\b'.format('|'.join(self.exe_names)), re.I | re.S)
        # read from scenarios files
        if self.scenarios_files and all([os.path.isfile(f) for f in self.scenarios_files]):
            for f in self.scenarios_files:
                self.load_scenarios_file(f)

    def load_scenarios_file(self, scenarios_file: str):
            scenarios_file_group_dict = {}
            if not os.path.isfile(scenarios_file):
                print(f'Error: scenarios file not found: {scenarios_file}')
                return
            with open(scenarios_file, 'r') as sf:
                scenarios_dict = json.load(sf)
            # XSEARCH_PATH = os.environ.get('XSEARCH_PATH', XSEARCHPATH)
            for (rk, rv) in scenarios_dict['ref'].items():
                if rk == 'common_flags':
                    scenarios_dict['ref'][rk] = rv
                if rk == 'common_out_dirpatterns':
                    scenarios_dict['ref'][rk] = [elem for d in [['-D', d] for d in rv] for elem in d]
                elif rk == 'common_in_extensions':
                    scenarios_dict['ref'][rk] = ['-x', ','.join(rv)]
                elif rk == 'common_out_extensions':
                    scenarios_dict['ref'][rk] = ['-X', ','.join(rv)]
                elif rk == 'common_search_patterns':
                    scenarios_dict['ref'][rk] = [elem for s in [['-s', s] for s in rv] for elem in s]
                elif rk == 'common_startpaths':
                    for i, p in enumerate(rv):
                        for replace_value in self.replace_values:
                            rv[i] = rv[i].replace(replace_value, self.replace_values[replace_value])
                        # rv[i] = p.replace('$XSEARCH_PATH', XSEARCH_PATH)
                elif rk == 'scriptpath' or rk == 'sharedpath':
                    for replace_value in self.replace_values:
                        rv = rv.replace(replace_value, self.replace_values[replace_value])
                        scenarios_dict['ref'][rk] = rv
                    # scenarios_dict['ref'][rk] = [rv.replace('$XSEARCH_PATH', XSEARCH_PATH)]

            for s in scenarios_dict['scenarios']:
                if self.include_groups and s['group'] not in self.include_groups:
                    continue
                if self.include_scenarios and s['name'] not in self.include_scenarios:
                    continue
                if self.skip_groups and s['group'] in self.skip_groups:
                    continue
                if self.skip_scenarios and s['name'] in self.skip_scenarios:
                    continue

                sg_name = s['group']
                sg = scenarios_file_group_dict.setdefault(sg_name, ScenarioGroup(sg_name, []))
                args = []
                for a in s['args']:
                    for replace_value in self.replace_values:
                        a = a.replace(replace_value, self.replace_values[replace_value])
                    args.append(a)
                # args = [a.replace('$XSEARCH_PATH', XSEARCH_PATH) for a in s['args']]
                # print(f'args: {args}')
                if 'common_args' in s:
                    for ca in s['common_args']:
                        args.extend(scenarios_dict['ref'][ca]) 
                scenario = Scenario(s['name'], args)
                if 'replace_exe_name' in s:
                    scenario.replace_exe_name = s['replace_exe_name']
                sg.scenarios.append(scenario)
            for g in scenarios_file_group_dict:
                self.group_names.append(g)
                self.scenarios.extend(scenarios_file_group_dict[g].scenarios)

    def __print_data_table(self, title: str, hdr: list[str], data: list[list[Union[float, int]]], col_types: list[type]):
        if self.colorize:
            self.__print_colorized_data_table(title, hdr, data, col_types)
        else:
            print(f'\n{title}')
            print(tabulate(data, headers=hdr))

    def __print_colorized_data_table(self, title: str, hdr: list[str], data: list[list[Union[float, int]]], col_types: list[type]):
        print(f'\n{title}')
        sio = StringIO(tabulate(data, headers=hdr))
        sio.seek(0)
        lines = sio.readlines()
        for hdr in lines[:2]:
            print(colored(hdr.rstrip(), 'white', attrs=['bold']))
        colors = [
            'red', 'green', 'yellow', 'blue', 'magenta', 'cyan',
            'light_red', 'light_green', 'light_yellow', 'light_blue', 'light_magenta', 'light_cyan'
        ]
        c = 0
        for line in lines[2:]:
            print(colored(line.strip(), colors[c]))
            c += 1
            if c >= len(colors):
                c = 0

    def print_scenario_summary(self, scenario_results: ScenarioResults):
        title = "\nScenario results summary for {} out of {} scenarios with {} out of {} total runs\n".\
            format(len(scenario_results), len(self.scenarios),
                   scenario_results.runs, len(self.scenarios) * self.runs)
        hdr = []
        for i in range(len(scenario_results)):
            hdr.extend([f'S{i + 1} total', f'S{i + 1} avg', f'S{i + 1} rank'])
        hdr.extend(['TOTAL', 'AVG', 'RANK'])
        data = []
        col_types = [float, float, int] * (len(scenario_results) + 1)
        for x in self.exe_names:
            row = [x]
            for sr in scenario_results.scenario_results:
                xt = sr.total_total(x)
                xta = sr.avg_total(x)
                xtr = sr.rank_total(x)
                row.extend([xt, xta, xtr])
            xt = scenario_results.total_total(x)
            xta = scenario_results.avg_total(x)
            xtr = scenario_results.rank_total(x)
            row.extend([xt, xta, xtr])
            data.append(row)
        self.__print_data_table(title, hdr, data, col_types)

    def print_scenario_results(self, scenario_results: ScenarioResults):
        title = "\nTotal results for {} out of {} scenarios with {} out of {} total runs\n".\
            format(len(scenario_results.scenario_results), len(self.scenarios),
                   scenario_results.runs, len(self.scenarios) * self.runs)
        title += f'\n\nDate/time:  {datetime.now()}'
        title += '\nGit branch: "{}" ({})\n'.format(self.git_info["branch"],
                                                  self.git_info["commit"])
        hdr = ['real', 'avg', 'rank', 'sys', 'avg', 'rank', 'user', 'avg',
               'rank', 'total', 'avg', 'rank']
        data = []
        col_types = [float, float, int] * 4
        for x in self.exe_names:
            xr = scenario_results.total_real(x)
            xra = scenario_results.avg_real(x)
            xrr = scenario_results.rank_real(x)

            xs = scenario_results.total_sys(x)
            xsa = scenario_results.avg_sys(x)
            xsr = scenario_results.rank_sys(x)

            xu = scenario_results.total_user(x)
            xua = scenario_results.avg_user(x)
            xur = scenario_results.rank_user(x)

            xt = scenario_results.total_total(x)
            xta = scenario_results.avg_total(x)
            xtr = scenario_results.rank_total(x)
            data.append([x, xr, xra, xrr, xs, xsa, xsr, xu, xua, xur, xt, xta, xtr])
        sorted_data = sorted(data, key=lambda d: d[12])
        self.__print_data_table(title, hdr, sorted_data, col_types)

    def print_scenario_results_summary(self, scenario_results: ScenarioResults):
        title = "\nTotal results for {} out of {} scenarios with {} out of {} total runs".\
            format(len(scenario_results.scenario_results), len(self.scenarios),
                   scenario_results.runs, len(self.scenarios) * self.runs)
        title += f'\n\nDate/time:  {datetime.now()}'
        title += '\nGit branch: "{}" ({})\n'.format(self.git_info["branch"],
                                                  self.git_info["commit"])
        hdr = ['total', 'avg', 'rank']
        data = []
        col_types = [float, float, int]
        for x in self.exe_names:
            xt = scenario_results.total_total(x)
            xta = scenario_results.avg_total(x)
            xtr = scenario_results.rank_total(x)
            data.append([x, xt, xta, xtr])
        sorted_data = sorted(data, key=lambda d: d[3])
        self.__print_data_table(title, hdr, sorted_data, col_types)

    def print_scenario_result(self, scenario_result: ScenarioResult):
        now = datetime.now()
        title = '\nTotal results for scenario {} ("{}") with {} runs on {} at {}\n'.\
            format(scenario_result.index, scenario_result.scenario.name, self.runs,
                   now.date(), now.time())
        hdr = ['real', 'avg', 'rank', 'sys', 'avg', 'rank', 'user',
               'avg', 'rank', 'total', 'avg', 'rank']
        data = []
        col_types = [float, float, int] * 4
        for x in self.exe_names:
            xr = scenario_result.total_real(x)
            xra = scenario_result.avg_real(x)
            xrr = scenario_result.rank_real(x)

            xs = scenario_result.total_sys(x)
            xsa = scenario_result.avg_sys(x)
            xsr = scenario_result.rank_sys(x)

            xu = scenario_result.total_user(x)
            xua = scenario_result.avg_user(x)
            xur = scenario_result.rank_user(x)

            xt = scenario_result.total_total(x)
            xta = scenario_result.avg_total(x)
            xtr = scenario_result.rank_total(x)
            data.append([x, xr, xra, xrr, xs, xsa, xsr, xu, xua, xur, xt, xta, xtr])
        self.__print_data_table(title, hdr, data, col_types)

    def print_run_result(self, sn: int, result: RunResult):
        title = f'\nResults for scenario {sn} ("{result.scenario.name}") run {result.run}\n'
        hdr = ['real', 'rank', 'sys', 'rank', 'user', 'rank', 'total', 'rank']
        data = []
        real_ranks = result.real_ranks
        sys_ranks = result.sys_ranks
        user_ranks = result.user_ranks
        total_ranks = result.total_ranks
        col_types = [float, int] * 4
        for x in self.exe_names:
            lang_result = [lr for lr in result.lang_results if lr.name == x][0]
            xr = lang_result.real
            xrr = real_ranks.index(x) + 1
            xs = lang_result.sys
            xsr = sys_ranks.index(x) + 1
            xu = lang_result.user
            xur = user_ranks.index(x) + 1
            xt = lang_result.total
            xtr = total_ranks.index(x) + 1
            data.append([x, xr, xrr, xs, xsr, xu, xur, xt, xtr])
        self.__print_data_table(title, hdr, data, col_types)

    def times_from_lines(self, lines: list[str]) -> dict[str, float]:
        times_lines = [l for l in lines if l.endswith(' sys')]
        time_dict = {
            'real': 0.0,
            'user': 0.0,
            'sys': 0.0,
            'total': 0.0
        }
        if times_lines:
            time_line_re = re.compile(r'^(\d+\.\d+)\s+(real)\s+(\d+\.\d+)\s+(user)\s+(\d+\.\d+)\s+(sys)$')
            time_line_match = time_line_re.match(times_lines[0])
            if time_line_match:
                time_dict = {
                    'real': float(time_line_match.group(1)),
                    'user': float(time_line_match.group(3)),
                    'sys': float(time_line_match.group(5))
                }
                time_dict['total'] = sum(time_dict.values())
            return time_dict
        elif self.shell == '/bin/bash':
            return self.bash_times_from_lines(lines)
        elif self.shell == '/bin/zsh':
            return self.zsh_times_from_lines(lines)
        else:
            return time_dict

    def zsh_times_from_lines(self, lines: list[str]) -> dict[str, float]:
        """The following matches lines like:
           <command> <utime> user <stime> system <pct> cpu <elapsed> total
           Example:
           cppsearch  0.01s user 0.01s system 80% cpu 0.016 total
        """
        print('zsh_times_from_lines')
        times_lines = [l for l in lines if l.endswith(' total')]
        if times_lines:
            time_line_re = re.compile(r'^(\S+)\s+(\d+\.\d+)s user\s+(\d+\.\d+)s system\s+(\d+)% cpu\s+(\d+\.\d+) total$')
            time_dict = {}
            time_line_match = time_line_re.match(times_lines[0])
            if time_line_match:
                time_dict['user'] = float(time_line_match.group(2))
                time_dict['sys'] = float(time_line_match.group(3))
                time_dict['total'] = float(time_line_match.group(5))
                time_dict['real'] = float('0.0')
        else:
            print(f"Times line not found")
            time_dict = {s: 0 for s in time_keys}
        return time_dict

    def bash_times_from_lines(self, lines: list[str]) -> dict[str, float]:
        """The following matches lines like:
           real    0m0.005s
           user    0m0.002s
           sys     0m0.002s
        """
        print('bash_times_from_lines')
        time_dict = {}
        times = lines[0].split()
        time_name_matches = [re.match(r'^(\d+(:\d+)?\.\d+)(user|system|elapsed)', t) for t in times[:3]]
        if time_name_matches and all(time_name_matches):
            for time_name_match in time_name_matches:
                n = time_name_match.group(3)
                t = time_name_match.group(1)
                if n == 'elapsed':
                    colon_idx = t.find(':')
                    time_dict['real'] = float(t[colon_idx+1:])
                else:
                    if n == 'system':
                        n = 'sys'
                    time_dict[n] = float(time_name_match.group(1)[2:])
        else:
            times.reverse()
            try:
                time_dict = {times[i]: float(times[i+1]) for i in range(0, len(times), 2)}
                time_dict['total'] = sum(time_dict.values())
            except Exception as e:
                print(f"Exception: {str(e)}")
                print(f"Invalid times line: \"{lines[0]}\"")
                time_dict = {s: 0 for s in time_keys}
        return time_dict

    def compare_outputs(self, s: Scenario, sn: int, xsearch_output: dict[str, list[str]]) -> bool:
        non_matching = non_matching_outputs(xsearch_output, case_insensitive_cmp=s.case_insensitive_cmp)
        if non_matching:
            print('\nOutputs of these language versions differ:')
            print(non_matching)
            self.scenario_diff_dict[s.name] = non_matching
            for x, y in non_matching:
                print(f'\n{x} output != {y} output for args: {" ".join(s.args)}')
                x_output = xsearch_output[x]
                y_output = xsearch_output[y]
                if self.ignore_blank_lines:
                    x_output = [l for l in x_output if l.strip()]
                    y_output = [l for l in y_output if l.strip()]
                print(f'{x} output ({len(x_output)} lines):\n"{x_output}"')
                print(f'{y} output ({len(y_output)} lines):\n"{y_output}"')
                min_len = min(len(x_output), len(y_output))
                for i in range(min_len):
                    if x_output[i] != y_output[i]:
                        print(f'Line {i+1}: "{x_output[i]}" != "{y_output[i]}"')
            return False
        else:
            print()
            log('Outputs of all versions match')
            return True

    def compare_output_lens(self, s: Scenario, sn: int, xsearch_output: dict[str, list[str]]) -> bool:
        non_matching = non_matching_lens(xsearch_output)
        if non_matching:
            print('\nOutput lengths differ for these language versions:')
            print(non_matching)
            self.scenario_diff_dict[s.name] = non_matching
            for x, y in non_matching:
                print(f'\n{x} output != {y} output for args: {" ".join(s.args)}')
                x_output = xsearch_output[x]
                y_output = xsearch_output[y]
                if self.ignore_blank_lines:
                    x_output = [l for l in x_output if l.strip()]
                    y_output = [l for l in y_output if l.strip()]
                print(f'{x} output ({len(x_output)} lines):\n"{x_output}"')
                print(f'{y} output ({len(y_output)} lines):\n"{y_output}"')
            return False
        else:
            print()
            log('Outputs of all versions match')
            return True

    def do_run(self, s: Scenario, sn: int, rn: int) -> RunResult:
        # return self.do_run_seq(s, sn, rn)
        return self.do_run_concurrent(s, sn, rn)

    def do_run_concurrent(self, s: Scenario, sn: int, rn: int) -> RunResult:
        """This run version starts procs for all language versions before going back and 
           capturing their outputs
        """
        xsearch_procs = {}
        exe_output = {}
        xsearch_times = {}
        lang_results = []
        for x in self.exe_names:
            fullargs = ['time', x] + s.args
            print(' '.join(fullargs[1:]))
            xsearch_procs[x] = subprocess.Popen(fullargs, bufsize=-1, stdout=subprocess.PIPE,
                                                stderr=subprocess.PIPE)

        for x in self.exe_names:
            p = xsearch_procs[x]
            output_lines = []
            error_lines = []
            while True:
                output_line = p.stdout.readline()
                error_line = p.stderr.readline()
                if not output_line and not error_line:
                    break
                if output_line:
                    output_lines.append(output_line.decode().strip())
                if error_line:
                    # error_line = error_line.decode().strip()
                    # if error_line == 'Command exited with non-zero status 1':
                    #     continue
                    error_lines.append(error_line.decode().strip())
                # else:
                #     break
            p.terminate()
            # output = '\n'.join(output_lines)
            # Temporary: sort output lines to reduce mismatches
            # output = '\n'.join(sorted(output_lines))

            # Clean up error_lines
            if error_lines:
                new_output_lines = []
                new_error_lines = []
                i = 0
                while i < len(error_lines):
                    if not error_lines[i]:
                        i += 1
                        continue
                    # If it starts with ERROR: or a console color sequence, add it to output
                    if error_lines[i].startswith('ERROR:'):
                        new_output_lines.append(error_lines[i])
                        i += 1
                    elif error_lines[i].startswith('\x1b['):
                        if error_lines[i].find('ERROR:') > -1 and error_lines[i].endswith('\x1b[0m'):
                            new_output_lines.append(error_lines[i])
                            i += 1
                        elif error_lines[i].find('ERROR:') > -1 and len(error_lines) > i + 1 and \
                                error_lines[i + 1] == '\x1b[0m':
                            new_output_lines.append(error_lines[i] + error_lines[i + 1])
                            i += 2
                        elif len(error_lines) > i + 1 and \
                                error_lines[i + 1].startswith('ERROR:') and \
                                error_lines[i + 1].endswith('\x1b[0m'):
                            new_output_lines.append(error_lines[i] + error_lines[i + 1])
                            i += 2
                        elif len(error_lines) > i + 2 and \
                                error_lines[i + 1].startswith('ERROR:') and \
                                error_lines[i + 2].startswith('\x1b['):
                            new_output_lines.append(error_lines[i] + error_lines[i + 1] + error_lines[i + 2])
                            i += 3
                        else:
                            new_output_lines.append(error_lines[i])
                            i += 1
                    else:
                        new_error_lines.append(error_lines[i])
                        i += 1
                output_lines = new_output_lines + output_lines
                error_lines = new_error_lines

            if s.replace_exe_name:
                output = '\n'.join(output_lines)
                output = self.exe_name_regex.sub('xname', output)
                output_lines = output.split('\n')
            exe_output[x] = output_lines
            if self.debug:
                print('{} output:\n"{}"'.format(x, '\n'.join(output_lines)))
            xsearch_times[x] = self.times_from_lines([e for e in error_lines if e])
            time_dict = xsearch_times[x]
            if 'real' not in time_dict and 'elapsed' not in time_dict:
                raise Exception(f'No real or elapsed time for {x}')
            treal = time_dict['real'] if 'real' in time_dict else time_dict['elapsed']
            if 'sys' not in time_dict and 'system' not in time_dict:
                raise Exception(f'No sys or system time for {x}')
            tsys = time_dict['sys'] if 'sys' in time_dict else time_dict['system']
            lang_results.append(LangResult(x, real=treal, sys=tsys, user=time_dict['user']))
        if not self.compare_outputs(s, sn, exe_output) and self.exit_on_diff:
            if not self.compare_output_lens(s, sn, exe_output) and self.exit_on_sort_diff:
                raise KeyboardInterrupt
        return RunResult(scenario=s, run=rn, lang_results=lang_results)

    def do_run_seq(self, s: Scenario, sn: int, rn: int) -> RunResult:
        """This run version runs each language version subprocess sequentially
        """
        xsearch_output = {}
        xsearch_times = {}
        lang_results = []
        for x in self.exe_names:
            fullargs = ['time', x] + s.args
            print(' '.join(fullargs[1:]))
            p = subprocess.Popen(fullargs, bufsize=-1, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)
            # print('process opened')
            output_lines = []
            time_lines = []
            while True:
                output_line = p.stdout.readline()
                time_line = p.stderr.readline()
                if output_line == '' and time_line == '':
                    break
                if output_line != '':
                    output_lines.append(output_line.decode())
                if time_line != '':
                    time_lines.append(time_line.strip().decode())
            output = ''.join(output_lines)
            if s.replace_exe_name:
                output = self.exe_name_regex.sub('xname', output)
            xsearch_output[x] = output
            if self.debug:
                print(f'output:\n"{output}"')
            xsearch_times[x] = self.times_from_lines(time_lines)
            time_dict = xsearch_times[x]
            lang_results.append(LangResult(x, real=time_dict['real'], sys=time_dict['sys'], user=time_dict['user']))
        if not self.compare_outputs(s, sn, xsearch_output) and self.exit_on_diff:
            if not self.compare_output_lens(s, sn, xsearch_output) and self.exit_on_sort_diff:
                raise KeyboardInterrupt
        return RunResult(scenario=s, run=rn, lang_results=lang_results)

    def activate_pyvenv(self):
        if 'VIRTUAL_ENV' not in os.environ:
            log_error('venv must be activated to run the python version')
            log('Run "source ./pyvenv_setup.sh" before this script')
            sys.exit(1)

    def run(self):
        if 'pyfind' in self.exe_names or 'pysearch' in self.exe_names:
            self.activate_pyvenv()
        scenario_results = ScenarioResults(scenarios=self.scenarios, exe_names=self.exe_names)
        runs = 0
        try:
            for i,s in enumerate(self.scenarios):
                sn = i + 1
                s_results = []
                for r in range(self.runs):
                    rn = r + 1
                    print(f'\nscenario {sn} ("{s.name}") run {rn}\n')
                    result = self.do_run(s, sn, rn)
                    runs += 1
                    s_results.append(result)
                    self.print_run_result(sn, result)
                scenario_result = ScenarioResult(scenario=s, index=sn, run_results=s_results)
                scenario_results.append(scenario_result)
                self.print_scenario_result(scenario_result)
        except KeyboardInterrupt:
            print('\n')

        if self.scenario_diff_dict:
            print('\nThere were output differences in these scenarios:')
            for sn, diffs in self.scenario_diff_dict.items():
                version_diffs = dict()
                print(f"'{sn}': {diffs}")
                for d in diffs:
                    if d[0] not in version_diffs:
                        version_diffs[d[0]] = 0
                    if d[1] not in version_diffs:
                        version_diffs[d[1]] = 0
                    version_diffs[d[0]] += 1
                    version_diffs[d[1]] += 1
                versions = sorted(version_diffs.keys())
                for v in versions:
                    d = version_diffs[v]
                    if d > 1:
                        print(f'{v} had differences with {d} other versions')
        else:
            print('\nOutputs of all versions in all scenarios match')

        self.print_scenario_results(scenario_results)
        # self.print_scenario_summary(scenario_results)
        self.print_scenario_results_summary(scenario_results)


########################################
# Utility functions
########################################
def log(msg: str):
    dt_str = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    print(f'[{dt_str}] {msg}')


def log_error(msg: str):
    dt_str = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    # print(f'[{dt_str}] {msg}')
    print(colored(f'[{dt_str}] ERROR: {msg}', 'red', attrs=['bold']))


def get_git_info():
    git_info = {}
    try:
        git_info['branch'] = subprocess.check_output(['git', 'rev-parse', '--abbrev-ref', 'HEAD']).strip().decode()
        git_info['commit'] = subprocess.check_output(['git', 'rev-parse', 'HEAD']).strip().decode()
        # git_info['status'] = subprocess.check_output(['git', 'status', '--porcelain']).strip().decode()
    except Exception as e:
        print(f'Error getting git info: {str(e)}')
    return git_info


# def fix_arg_list(arg_list: list[str]) -> list[str]:
#     if len(arg_list) < 2:
#         return arg_list
#     if all([len(c) == 1 for c in arg_list]) and any([c not in lang_alias_dict for c in arg_list]):
#         return [''.join(arg_list)]
#     return arg_list


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


def non_matching_lens(exe_output: dict[str, list[str]],
                      skip_blanks: bool = True) -> list[tuple[str, str]]:
    """Examines exe_output (a dict of {exe_name : [lines]})
       and returns a list of tuples of non-matching xsearch pairs
       ([(exe_name_1, exe_name_2)]
    """
    non_matching = []
    xs = sorted(exe_output.keys())
    while xs:
        x = xs.pop(0)
        x_lines = lines_for_diff(exe_output[x], skip_blanks=skip_blanks)
        x_len = len(x_lines)
        for y in xs:
            y_lines = lines_for_diff(exe_output[y], skip_blanks=skip_blanks)
            y_len = len(y_lines)
            if x_len != y_len:
                x_and_y = tuple(sorted([x, y]))
                if x_and_y not in non_matching:
                    non_matching.append(x_and_y)
    return non_matching


def non_matching_outputs(exe_output: dict[str, list[str]],
                         sort_lines: bool = True,
                         skip_blanks: bool = True,
                         case_insensitive_cmp: bool = False,
                         normalize_field_names: bool = False) -> list[tuple[str, str]]:
    """Examines exe_output (a dict of {exe_name : [lines]})
       and returns a list of tuples of non-matching xsearch pairs
      ([(exe_name_1, exe_name_2)]
    """
    non_matching = []
    xs = sorted(exe_output.keys())
    while xs:
        x = xs.pop(0)
        x_lines = lines_for_diff(exe_output[x],
                                 skip_blanks=skip_blanks,
                                 sort_lines=sort_lines,
                                 case_insensitive_cmp=case_insensitive_cmp,
                                 normalize_field_names=normalize_field_names)
        for y in xs:
            y_lines = lines_for_diff(exe_output[y],
                                     skip_blanks=skip_blanks,
                                     sort_lines=sort_lines,
                                     case_insensitive_cmp=case_insensitive_cmp,
                                     normalize_field_names=normalize_field_names)
            if x_lines != y_lines:
                # print("\n{}:\n\"{}\"".format(x, x_output))
                # print("\n{}:\n\"{}\"".format(y, y_output))
                x_and_y = tuple(sorted([x, y]))
                if x_and_y not in non_matching:
                    non_matching.append(x_and_y)
    return non_matching
