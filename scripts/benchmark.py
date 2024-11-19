#!/usr/bin/env python3
# -*- coding: utf-8 -*-
################################################################################
#
# benchmark.py
#
# A simple benchmarking tool for the various xfind language versions
#
################################################################################
import argparse
import json
import subprocess
import sys

from dataclasses import dataclass
from datetime import datetime
from tabulate import tabulate
from typing import Union

from xfind import *


@dataclass
class Scenario:
    """Class to define a test scenario"""
    name: str
    args: list
    replace_xfind_name: bool = False
    case_insensitive_cmp: bool = False


@dataclass
class ScenarioGroup:
    """Class for grouping scenarios"""
    name: str
    scenarios: list[Scenario]


########################################
# Configuration
########################################
#exts = ','.join('clj cpp cs dart fs go hs java js kt pl php py rb rs scala swift ts'.split())
exts = ','.join('py rb'.split())

startpaths = [os.path.join(XFINDPATH, d) for d in ('python', 'ruby')]
scriptpath = os.path.join(XFINDPATH, 'scripts')
sharedpath = os.path.join(XFINDPATH, 'shared')

default_runs = 10

ignore_dirs = ['build', 'cmake', 'node_modules', 'pycache', 'vendor', 'venv']
ignore_args = [elem for ignore_dir in [['-D', d] for d in ignore_dirs] for elem in ignore_dir]
core_args = ignore_args
ext_args = ['-x', exts]
common_args = core_args + ext_args + startpaths

# Scenarios
basic_scenarios = [
    Scenario('no args', [], replace_xfind_name=True),
    Scenario('help', ['-h'], replace_xfind_name=True),
]
extension_scenarios = [
    # match extensions
    Scenario('find matching "{}" extensions'.format(exts), common_args),
    Scenario('find not matching "{}" extensions'.format(exts), core_args + ['-X', exts] + startpaths,
             replace_xfind_name=False),
]
filename_scenarios = [
    # match filename
    Scenario('find with "find" in filename', common_args + ['-f', 'find']),
    Scenario('find with "find" not in filename', common_args + ['-F', 'find']),
    Scenario('find blank filename (should find nothing)', common_args + ['-f', '^$']),
]
filetype_scenarios = [
    # match filetype
    Scenario('find "audio" filetype', core_args + ['-t', 'audio'] + startpaths),
    Scenario('find not "audio" filetype', core_args + ['-T', 'audio'] + startpaths),
    Scenario('find "binary" filetype', core_args + ['-t', 'binary'] + startpaths),
    Scenario('find not "binary" filetype', core_args + ['-T', 'binary'] + startpaths),
    Scenario('find "code" filetype', core_args + ['-t', 'code'] + startpaths),
    Scenario('find not "code" filetype', core_args + ['-T', 'code'] + startpaths),
    Scenario('find "font" filetype', core_args + ['-t', 'font'] + startpaths),
    Scenario('find not "font" filetype', core_args + ['-T', 'font'] + startpaths),
    Scenario('find "image" filetype', core_args + ['-t', 'image'] + startpaths),
    Scenario('find not "image" filetype', core_args + ['-T', 'image'] + startpaths),
    Scenario('find "text" filetype', core_args + ['-t', 'text'] + startpaths),
    Scenario('find not "text" filetype', core_args + ['-T', 'text'] + startpaths),
    Scenario('find "video" filetype', core_args + ['-t', 'video'] + startpaths),
    Scenario('find not "video" filetype', core_args + ['-T', 'video'] + startpaths),
    Scenario('find "xml" filetype', core_args + ['-t', 'xml'] + startpaths),
    Scenario('find not "xml" filetype', core_args + ['-T', 'xml'] + startpaths),
]
print_scenarios = [
    # print dirs
    Scenario('print matching dirs for "{}" extensions'.format(exts), common_args + ['--printdirs']),
    Scenario('print not matching dirs for "{}" extensions'.format(exts), core_args + ['-X', exts, '--printdirs'] + startpaths,
             replace_xfind_name=False),
]
sorting_scenarios = [
    # sorting scenarios
    Scenario('sort shared files by path', ['--sort-by', 'path', sharedpath]),
    Scenario('sort shared files by filename', ['--sort-by', 'name', sharedpath]),
    Scenario('sort scripts files by path case-insensitive', core_args + ['--sort-by', 'path', '--sort-caseinsensitive', scriptpath]),
    Scenario('sort scripts files by filename case-insensitive', core_args + ['--sort-by', 'name', '--sort-caseinsensitive', scriptpath]),
    Scenario('sort shared files by filesize', ['--sort-by', 'size', sharedpath]),
    Scenario('sort shared files by filesize descending', ['--sort-by', 'size', '--sort-descending', sharedpath]),
    Scenario('sort shared files by filetype', ['--sort-by', 'type', sharedpath]),
    Scenario('sort shared files by filetype descending', ['--sort-by', 'type', '--sort-descending', sharedpath]),
    Scenario('sort shared files by lastmod', ['--sort-by', 'lastmod', sharedpath]),
    Scenario('sort shared files by lastmod descending', ['--sort-by', 'lastmod', '--sort-descending', sharedpath]),
]
maxmindepth_scenarios = [
    # filter by maxdepth/mindepth
    Scenario('filter files with depth <= maxdepth', core_args + ['--maxdepth', '3', scriptpath]),
    Scenario('filter files with depth >= mindepth', core_args + ['--mindepth', '2', scriptpath]),
    Scenario('filter files by maxdepth and mindepth', core_args + ['--maxdepth', '3', '--mindepth', '2', scriptpath]),
    Scenario('filter files by invalid range maxdepth and mindepth', ignore_args + ['--maxdepth', '2', '--mindepth', '3', scriptpath], replace_xfind_name=True),
]
maxminlastmod_scenarios = [
    # filter by maxlastmod/minlastmod
    Scenario('filter files with lastmod <= maxlastmod', core_args + ['--maxlastmod', '2022-12-30', scriptpath]),
    Scenario('filter files with lastmod >= minlastmod', core_args + ['--minlastmod', '2020-01-01', scriptpath]),
    Scenario('filter files by maxlastmod and minlastmod', core_args + ['--maxlastmod', '2022-12-30', '--minlastmod', '2020-01-01', scriptpath]),
    Scenario('filter files by invalid range maxlastmod and minlastmod', core_args + ['--maxlastmod', '2020-01-01', '--minlastmod', '2022-12-30', scriptpath], replace_xfind_name=True),
]
maxminsize_scenarios = [
    # filter by maxsize/minsize
    Scenario('filter files with size < maxsize', core_args + ['--maxsize', '10000', scriptpath]),
    Scenario('filter files with size > minsize', core_args + ['--minsize', '1000', scriptpath]),
    Scenario('filter files by maxsize and minsize', core_args + ['--maxsize', '10000', '--minsize', '5000', scriptpath]),
    Scenario('filter files by invalid range maxsize and minsize', core_args + ['--maxsize', '5000', '--minsize', '10000', scriptpath], replace_xfind_name=True),
]
# settingsonly_scenarios = [
#     Scenario('settings-only', core_args + ['-t', 'code'] + ['--settingsonly'], replace_xfind_name=True, case_insensitive_cmp=True),
# ]
scenarios = \
    basic_scenarios + \
    extension_scenarios + \
    filename_scenarios + \
    filetype_scenarios + \
    print_scenarios + \
    sorting_scenarios + \
    maxmindepth_scenarios + \
    maxminlastmod_scenarios + \
    maxminsize_scenarios
# scenarios = \
#     maxminlastmod_scenarios + \
#     maxminsize_scenarios

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
    def __init__(self, scenario_results: list[ScenarioResult] = None, **kwargs):
        self.scenario_results = []
        if scenario_results:
            self.scenario_results = scenario_results
        self.__dict__.update(kwargs)
        self.langs = set()
        self._update()

    def _update(self):
        if self.scenario_results:
            self.langs = self.scenario_results[0].langs
        self._lang_totals_dict = { l: { t: 0 for t in time_keys } for l in self.langs }
        for sr in self.scenario_results:
            for l in self.langs:
                self._lang_totals_dict[l]['real'] += sr.total_real(l)
                self._lang_totals_dict[l]['sys'] += sr.total_sys(l)
                self._lang_totals_dict[l]['user'] += sr.total_user(l)
                self._lang_totals_dict[l]['total'] += sr.total_total(l)
        # for each time type, a list of the languages in order from fastest to slowest
        self.__ranks_dict = {}
        for t in time_keys:
            type_totals = sorted([(self._lang_totals_dict[l][t], l) for l in self.langs], key=lambda t: t[0])
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
        return self._lang_totals_dict[lang]['real']

    def avg_real(self, lang: str) -> float:
        tr = self.total_real(lang)
        return tr / self.runs

    def rank_real(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'real')

    def total_sys(self, lang: str) -> float:
        return self._lang_totals_dict[lang]['sys']

    def avg_sys(self, lang: str) -> float:
        ts = self.total_sys(lang)
        return ts / self.runs

    def rank_sys(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'sys')

    def total_user(self, lang: str) -> float:
        return self._lang_totals_dict[lang]['user']

    def avg_user(self, lang: str) -> float:
        tu = self.total_user(lang)
        return tu / self.runs

    def rank_user(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'user')

    def total_total(self, lang: str) -> float:
        return self._lang_totals_dict[lang]['total']

    def avg_total(self, lang: str) -> float:
        tt = self.total_total(lang)
        return tt / self.runs

    def rank_total(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'total')


########################################
# Benchmarker class
########################################
class Benchmarker(object):
    def __init__(self, **kwargs):
        self.xfind_names = all_xfind_names
        self.group_names = []
        self.scenario_names = []
        self.scenarios = []
        self.scenarios_file = ''
        self.runs = default_runs
        self.debug = True
        self.exit_on_diff = True
        self.exit_on_sort_diff = True
        self.ignore_blank_lines = True
        self.scenario_diff_dict = {}
        self.skip_groups = ['settings-only']
        self.__dict__.update(kwargs)
        self.shell = os.environ.get('SHELL', '/bin/bash')
        self.git_info = get_git_info()
        # read from scenarios file
        if self.scenarios_file and os.path.isfile(self.scenarios_file):
            self.load_scenarios(self.scenarios_file)

    def load_scenarios(self, scenarios_file: str):
            scenario_group_dict = {}
            if not os.path.isfile(scenarios_file):
                print(f'Error: scenarios file not found: {scenarios_file}')
                return
            with open(scenarios_file, 'r') as sf:
                scenarios_dict = json.load(sf)
            self.scenarios = []
            XFIND_PATH = os.environ.get('XFIND_PATH', XFINDPATH)
            for (rk, rv) in scenarios_dict['ref'].items():
                if rk == 'common_out_dirpatterns':
                    scenarios_dict['ref'][rk] = [elem for d in [['-D', d] for d in rv] for elem in d]
                elif rk == 'common_in_extensions':
                    scenarios_dict['ref'][rk] = ['-x', ','.join(rv)]
                elif rk == 'common_out_extensions':
                    scenarios_dict['ref'][rk] = ['-X', ','.join(rv)]
                elif rk == 'common_startpaths':
                    for i, p in enumerate(rv):
                        rv[i] = p.replace('$XFIND_PATH', XFIND_PATH)
                elif rk == 'scriptpath' or rk == 'sharedpath':
                    scenarios_dict['ref'][rk] = [rv.replace('$XFIND_PATH', XFIND_PATH)]

            for s in scenarios_dict['scenarios']:
                if self.scenario_names and s['name'] not in self.scenario_names:
                    continue
                sg_name = s['group']
                sg = scenario_group_dict.setdefault(sg_name, ScenarioGroup(sg_name, []))
                args = s['args']
                if 'common_args' in s:
                    for ca in s['common_args']:
                        args.extend(scenarios_dict['ref'][ca]) 
                scenario = Scenario(s['name'], s['args'])
                if 'replace_xfind_name' in s:
                    scenario.replace_xfind_name = s['replace_xfind_name']
                sg.scenarios.append(scenario)
            groups = self.group_names
            if not groups:
                groups = scenario_group_dict.keys()
            elif any([g not in scenario_group_dict for g in groups]):
                print(f'Error: group not found in scenarios file: {groups}')
                sys.exit(1)
            if self.skip_groups:
                groups = [g for g in groups if g not in self.skip_groups]
            for g in groups:
                sg = scenario_group_dict.get(g)
                if sg:
                    self.scenarios.extend(sg.scenarios)

    def __print_data_table(self, title: str, hdr: list[str], data: list[list[Union[float, int]]], col_types: list[type]):
        print('\n{}'.format(title))
        print(tabulate(data, headers=hdr))

    def print_scenario_summary(self, scenario_results: ScenarioResults):
        title = "\nScenario results summary for {} out of {} scenarios with {} out of {} total runs\n".\
            format(len(scenario_results), len(self.scenarios),
                   scenario_results.runs, len(self.scenarios) * self.runs)
        hdr = []
        for i in range(len(scenario_results)):
            hdr.extend(['S{} total'.format(i + 1), 'S{} avg'.format(i + 1), 'S{} rank'.format(i + 1)])
        hdr.extend(['TOTAL', 'AVG', 'RANK'])
        data = []
        col_types = [float, float, int] * (len(scenario_results) + 1)
        for x in self.xfind_names:
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
        title = "\nTotal results for {} out of {} scenarios with {} out of {} total runs".\
            format(len(scenario_results.scenario_results), len(self.scenarios),
                   scenario_results.runs, len(self.scenarios) * self.runs)
        title += '\n\nDate/time:  {}'.format(datetime.now())
        title += '\nGit branch: "{}" ({})\n'.format(self.git_info["branch"],
                                                  self.git_info["commit"])
        hdr = ['real', 'avg', 'rank', 'sys', 'avg', 'rank', 'user', 'avg',
               'rank', 'total', 'avg', 'rank']
        data = []
        col_types = [float, float, int] * 4
        for x in self.xfind_names:
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
        title += '\n\nDate/time:  {}'.format(datetime.now())
        title += '\nGit branch: "{}" ({})\n'.format(self.git_info["branch"],
                                                  self.git_info["commit"])
        hdr = ['total', 'avg', 'rank']
        data = []
        col_types = [float, float, int]
        for x in self.xfind_names:
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
        for x in self.xfind_names:
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
        for x in self.xfind_names:
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
        if times_lines:
            time_line_re = re.compile(r'^(\d+\.\d+)\s+(real)\s+(\d+\.\d+)\s+(user)\s+(\d+\.\d+)\s+(sys)$')
            time_line_match = time_line_re.match(times_lines[0])
            if time_line_match:
                time_dict = {}
                time_dict['real'] = float(time_line_match.group(1))
                time_dict['user'] = float(time_line_match.group(3))
                time_dict['sys'] = float(time_line_match.group(5))
                time_dict['total'] = sum(time_dict.values())
                return time_dict
        elif self.shell == '/bin/bash':
            return self.bash_times_from_lines(lines)
        elif self.shell == '/bin/zsh':
            return self.zsh_times_from_lines(lines)

    def zsh_times_from_lines(self, lines: list[str]) -> dict[str, float]:
        """The following matches lines like:
           <command> <utime> user <stime> system <pct> cpu <elapsed> total
           Example:
           cppfind  0.01s user 0.01s system 80% cpu 0.016 total
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

    def compare_outputs(self, s: Scenario, sn: int, xfind_output: dict[str, list[str]]) -> bool:
        non_matching = non_matching_outputs(xfind_output, case_insensitive_cmp=s.case_insensitive_cmp)
        if non_matching:
            print('\nOutputs of these language versions differ:')
            print(non_matching)
            self.scenario_diff_dict[s.name] = non_matching
            for x, y in non_matching:
                print(f'\n{x} output != {y} output for args: {" ".join(s.args)}')
                print(f'{x} output ({len(xfind_output[x])} lines):\n"{xfind_output[x]}"')
                print(f'{y} output ({len(xfind_output[y])} lines):\n"{xfind_output[y]}"')
            return False
        else:
            print('\nOutputs of all versions match')
            return True

    def compare_output_lens(self, s: Scenario, sn: int, xfind_output: dict[str, list[str]]) -> bool:
        non_matching = non_matching_lens(xfind_output)
        if non_matching:
            print('\nOutput lengths differ for these language versions:')
            print(non_matching)
            self.scenario_diff_dict[s.name] = non_matching
            for x, y in non_matching:
                print(f'\n{x} output != {y} output for args: {" ".join(s.args)}')
                print(f'{x} output ({len(xfind_output[x])} lines):\n"{xfind_output[x]}"')
                print(f'{y} output ({len(xfind_output[y])} lines):\n"{xfind_output[y]}"')
            return False
        else:
            print('\nOutputs of all versions match')
            return True

    def do_run(self, s: Scenario, sn: int, rn: int) -> RunResult:
        # return self.do_run_seq(s, sn, rn)
        return self.do_run_concurrent(s, sn, rn)

    def do_run_concurrent(self, s: Scenario, sn: int, rn: int) -> RunResult:
        """This run version starts procs for all language versions before going back and 
           capturing their outputs
        """
        xfind_procs = {}
        xfind_output = {}
        xfind_times = {}
        lang_results = []
        for x in self.xfind_names:
            fullargs = ['time', x] + s.args
            print(' '.join(fullargs[1:]))
            xfind_procs[x] = subprocess.Popen(fullargs, bufsize=-1, stdout=subprocess.PIPE,
                                              stderr=subprocess.PIPE)

        for x in self.xfind_names:
            p = xfind_procs[x]
            output_lines = []
            error_lines = []
            while True:
                output_line = p.stdout.readline()
                error_line = p.stderr.readline()
                if not output_line and not error_line:
                    break
                if output_line and error_line:
                    error_line = error_line.decode().strip()
                    if error_line.startswith('ERROR:'):
                        output_lines.append(error_line)
                    else:
                        error_lines.append(error_line)
                    output_lines.append(output_line.decode().strip())
                elif output_line:
                    output_lines.append(output_line.decode().strip())
                elif error_line:
                    error_line = error_line.decode().strip()
                    if error_line == 'Command exited with non-zero status 1':
                        continue
                    # print(f'error_line: "{error_line}"')
                    if error_line.startswith('ERROR:'):
                        output_lines.append(error_line)
                    else:
                        error_lines.append(error_line)
            p.terminate()
            # output = '\n'.join(output_lines)
            # Temporary: sort output lines to reduce mismatches
            # output = '\n'.join(sorted(output_lines))
            if s.replace_xfind_name:
                output = '\n'.join(output_lines)
                output = xfind_name_regex.sub('xfind', output)
                output_lines = output.split('\n')
            xfind_output[x] = output_lines
            if self.debug:
                print('{} output:\n"{}"'.format(x, '\n'.join(output_lines)))
            xfind_times[x] = self.times_from_lines([e for e in error_lines if e])
            time_dict = xfind_times[x]
            if 'real' not in time_dict and 'elapsed' not in time_dict:
                raise Exception('No real or elapsed time for {}'.format(x))
            treal = time_dict['real'] if 'real' in time_dict else time_dict['elapsed']
            if 'sys' not in time_dict and 'system' not in time_dict:
                raise Exception('No sys or system time for {}'.format(x))
            tsys = time_dict['sys'] if 'sys' in time_dict else time_dict['system']
            lang_results.append(LangResult(x, real=treal, sys=tsys, user=time_dict['user']))
        if not self.compare_outputs(s, sn, xfind_output) and self.exit_on_diff:
            if not self.compare_output_lens(s, sn, xfind_output) and self.exit_on_sort_diff:
                raise KeyboardInterrupt
        return RunResult(scenario=s, run=rn, lang_results=lang_results)

    def do_run_seq(self, s: Scenario, sn: int, rn: int) -> RunResult:
        """This run version runs each language version subprocess sequentially
        """
        xfind_output = {}
        xfind_times = {}
        lang_results = []
        for x in self.xfind_names:
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
            if s.replace_xfind_name:
                output = xfind_name_regex.sub('xfind', output)
            xfind_output[x] = output
            if self.debug:
                print(f'output:\n"{output}"')
            xfind_times[x] = self.times_from_lines(time_lines)
            time_dict = xfind_times[x]
            lang_results.append(LangResult(x, real=time_dict['real'], sys=time_dict['sys'], user=time_dict['user']))
        if not self.compare_outputs(s, sn, xfind_output) and self.exit_on_diff:
            if not self.compare_output_lens(s, sn, xfind_output) and self.exit_on_sort_diff:
                raise KeyboardInterrupt
        return RunResult(scenario=s, run=rn, lang_results=lang_results)

    def activate_pyvenv(self):
        if 'VIRTUAL_ENV' not in os.environ:
            print('ERROR: venv must be activated to run pyfind')
            print('Run "source ./pyvenv_setup.sh" before this script')
            sys.exit(1)

    def run(self):
        if 'pyfind' in self.xfind_names:
            self.activate_pyvenv()
        scenario_results = ScenarioResults()
        runs = 0
        try:
            for i, s in enumerate(self.scenarios):
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
                for v, d in version_diffs.items():
                    if d > 1:
                        print(f'{v} had differences with {d} other versions')
        else:
            print('\nOutputs of all versions in all scenarios match')

        self.print_scenario_results(scenario_results)
        # self.print_scenario_summary(scenario_results)
        self.print_scenario_results_summary(scenario_results)


########################################
# Main functions
########################################
def get_git_info():
    git_info = {}
    try:
        git_info['branch'] = subprocess.check_output(['git', 'rev-parse', '--abbrev-ref', 'HEAD']).strip().decode()
        git_info['commit'] = subprocess.check_output(['git', 'rev-parse', 'HEAD']).strip().decode()
        # git_info['status'] = subprocess.check_output(['git', 'status', '--porcelain']).strip().decode()
    except Exception as e:
        print(f'Error getting git info: {str(e)}')
    return git_info


def get_parser():
    parser = argparse.ArgumentParser(description='Run xfind benchmark')
    parser.add_argument('-g', '--group', nargs='*', help='Name of scenario group to run')
    parser.add_argument('-s', '--scenario', nargs='*', help='Name of scenario to run')
    parser.add_argument('-l', '--langs', help='Comma-separated list of languages to include in benchmark')
    parser.add_argument('-L', '--nolangs', help='Comma-separated list of languages to exclude from benchmark')
    parser.add_argument('-r', '--runs', type=int, help='Number of runs for each scenario')
    parser.add_argument('-b', '--exit-on-diff', action='store_true', help='Exit on first output difference')
    parser.add_argument('-f', '--scenarios-file', help='A scenarios json file')
    parser.add_argument('--debug', action='store_true', help='Print debug output')
    return parser


def main():
    # Defaults
    xfind_names = all_xfind_names
    groups = []
    scenarios = []
    runs = default_runs
    debug = False
    exit_on_diff = True
    scenarios_file = 'scenarios.json'

    parser = get_parser()
    parsed_args = parser.parse_args(sys.argv[1:])

    if parsed_args.debug:
        debug = True

    if parsed_args.group:
        groups.extend(parsed_args.group)

    if parsed_args.scenario:
        scenarios.extend(parsed_args.scenario)

    if parsed_args.langs:
        xfind_names = []
        langs = sorted(parsed_args.langs.split(','))
        for lang in langs:
            if lang in xfind_dict:
                xfind_names.append(xfind_dict[lang])
            else:
                print(f'Skipping unknown language: {lang}')

    if parsed_args.nolangs:
        nolangs = sorted(parsed_args.nolangs.split(','))
        for nolang in nolangs:
            if nolang in xfind_dict:
                if xfind_dict[nolang] in xfind_names:
                    del xfind_names[xfind_names.index(xfind_dict[nolang])]
            else:
                print(f'Skipping unknown language: {lang}')

    if parsed_args.runs:
        runs = parsed_args.runs

    if parsed_args.scenarios_file:
        scenarios_file = parsed_args.scenarios_file

    print(f'xfind_names ({len(xfind_names)}): {str(xfind_names)}')
    print(f'debug: {debug}')
    print(f'exit_on_diff: {exit_on_diff}')
    print(f'groups: {groups}')
    print(f'runs: {runs}')
    print(f'scenarios_file: {scenarios_file}')
    benchmarker = Benchmarker(xfind_names=xfind_names, runs=runs,
                              group_names=groups, scenario_names=scenarios,
                              scenarios_file=scenarios_file,
                              exit_on_diff=exit_on_diff,
                              debug=debug)
    benchmarker.run()


if __name__ == '__main__':
    main()
