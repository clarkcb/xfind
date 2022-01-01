/*
 * finder.ts
 *
 * performs the finding based on the given FindSettings instance
 */

'use strict';

import * as assert from 'assert';
import * as fs from 'fs';
import { access, lstat, stat } from 'fs/promises';
import * as path from 'path';

import * as common from './common';
import {FileType} from './filetype';
import {FileTypes} from './filetypes';
import {FileUtil} from './fileutil';
import {FindError} from './finderror';
import {FindFile} from './findfile';
import {FindSettings} from './findsettings';

export class Finder {
    _settings: FindSettings;

    constructor(settings: FindSettings) {
        this._settings = settings;
        this.validateSettings();
    }

    private validateSettings(): void {
        try {
            assert.ok(this._settings.paths.length > 0, 'Startpath not defined');
            for (const p of this._settings.paths) {
                // await access(p, fs.constants.F_OK | fs.constants.R_OK);
                fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                // const stat = await lstat(p);
                const stat = fs.lstatSync(p);
                if (stat.isDirectory()) {
                    assert.ok(this.isFindDir(p),
                        'Startpath does not match find settings');
                } else if (stat.isFile()) {
                    assert.ok(this.filterFile(p),
                        'Startpath does not match find settings');
                } else {
                    assert.ok(false, 'Startpath not findable file type');
                }
            }

        } catch (err: Error | any) {
            let msg = err.message;
            if (err.code === 'ENOENT') {
                msg = 'Startpath not found';
            } else if (err.code === 'EACCES') {
                msg = 'Startpath not readable';
            }
            throw new FindError(msg);
        }
    }

    private static matchesAnyString(s: string, elements: string[]): boolean {
        return elements.indexOf(s) > -1;
    }

    private static matchesAnyPattern(s: string, patterns: RegExp[]): boolean {
        return patterns.some((p: RegExp) => s.search(p) > -1);
    }

    private static matchesAnyFileType(ft: FileType, fileTypes: FileType[]): boolean {
        return fileTypes.indexOf(ft) > -1;
    }

    private static anyMatchesAnyPattern(ss: string[], patterns: RegExp[]) {
        return ss.some((s: string) => this.matchesAnyPattern(s, patterns));
    }

    public isFindDir(dir: string): boolean {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        if (this._settings.excludeHidden) {
            const nonDotElems = dir.split(path.sep).filter((p: string) => !Finder.matchesAnyString(p, ['.','..']));
            if (nonDotElems.length === 0) {
                return true;
            }
            if (nonDotElems.some((p: string) => FileUtil.isHidden(p))) {
                return false;
            }
        }
        if (this._settings.inDirPatterns.length && !Finder.matchesAnyPattern(dir,
                this._settings.inDirPatterns)) {
            return false;
        }
        return !(this._settings.outDirPatterns.length && Finder.matchesAnyPattern(dir,
            this._settings.outDirPatterns));
    }

    public isFindFile(file: string): boolean {
        if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
            return false;
        }
        const ext: string = FileUtil.getExtension(file);
        if ((this._settings.inExtensions.length &&
            !Finder.matchesAnyString(ext, this._settings.inExtensions))
            || (this._settings.outExtensions.length &&
                Finder.matchesAnyString(ext, this._settings.outExtensions))
            || (this._settings.inFilePatterns.length &&
                !Finder.matchesAnyPattern(file, this._settings.inFilePatterns))
            || (this._settings.outFilePatterns.length &&
                Finder.matchesAnyPattern(file, this._settings.outFilePatterns))) {
            return false;
        }
        const filetype: FileType = FileTypes.getFileType(file);
        if ((this._settings.inFileTypes.length &&
            !Finder.matchesAnyFileType(filetype, this._settings.inFileTypes))
            || (this._settings.outFileTypes.length &&
                Finder.matchesAnyFileType(filetype, this._settings.outFileTypes))) {
                    return false;
        }
        return true;
    }

    public isArchiveFindFile(file: string): boolean {
        if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
            return false;
        }
        const ext: string = FileUtil.getExtension(file);
        if (this._settings.inArchiveExtensions.length &&
            !Finder.matchesAnyString(ext, this._settings.inArchiveExtensions)) {
            return false;
        }
        if (this._settings.outArchiveExtensions.length &&
            Finder.matchesAnyString(ext, this._settings.outArchiveExtensions)) {
            return false;
        }
        if (this._settings.inArchiveFilePatterns.length &&
            !Finder.matchesAnyPattern(file, this._settings.inArchiveFilePatterns)) {
            return false;
        }
        return !(this._settings.outArchiveFilePatterns.length &&
        Finder.matchesAnyPattern(file, this._settings.outArchiveFilePatterns));
    }

    public filterFile(f: string): boolean {
        if (FileTypes.isArchiveFile(f)) {
            return (this._settings.includeArchives && this.isArchiveFindFile(f));
        }
        return (!this._settings.archivesOnly && this.isFindFile(f));
    }

    private recGetFindFiles(currentDir: string): FindFile[] {
        const findDirs: string[] = [];
        let findFiles: FindFile[] = [];
        fs.readdirSync(currentDir).map((f: string) => {
            return path.join(currentDir, f);
        }).forEach((f: string) => {
            const stats = fs.statSync(f);
            if (stats.isDirectory() && this._settings.recursive && this.isFindDir(f)) {
                findDirs.push(f);
            } else if (stats.isFile()) {
                const dirname = path.dirname(f) || '.';
                const filename = path.basename(f);
                if (this.filterFile(filename)) {
                    const filetype = FileTypes.getFileType(filename);
                    const sf = new FindFile(dirname, filename, filetype);
                    findFiles.push(sf);
                }
            }
        });
        findDirs.forEach(d => {
            findFiles = findFiles.concat(this.recGetFindFiles(d));
        });
        return findFiles;
    }

    private async getFindFiles(startPath: string): Promise<FindFile[]> {
        let findFiles: FindFile[] = [];
        const stats = await stat(startPath);
        if (stats.isDirectory()) {
            if (this.isFindDir(startPath)) {
                findFiles = findFiles.concat(this.recGetFindFiles(startPath));
            } else {
                throw new FindError('startPath does not match find criteria');
            }
        } else if (stats.isFile()) {
            const dirname = path.dirname(startPath) || '.';
            const filename = path.basename(startPath);
            if (this.isFindDir(dirname) && this.filterFile(filename)) {
                const filetype = FileTypes.getFileType(filename);
                const sf = new FindFile(dirname, filename, filetype);
                findFiles.push(sf);
            } else {
                throw new FindError('startPath does not match find criteria');
            }
        }
        return findFiles;
    }

    public async find(): Promise<FindFile[]> {
        // get the find files
        let findfiles: FindFile[] = [];

        const pathFindFilesArrays = await Promise.all(this._settings.paths.map(d => this.getFindFiles(d)));
        pathFindFilesArrays.forEach(pathFindFiles => {
            findfiles = findfiles.concat(pathFindFiles);
        });

        return findfiles;
    }

    public getMatchingDirs(findfiles: FindFile[]): string[] {
        const dirs: string[] = findfiles.map(f => f.pathname);
        return common.setFromArray(dirs);
    }

    public printMatchingDirs(findfiles: FindFile[]): void {
        const dirs: string[] = this.getMatchingDirs(findfiles);
        if (dirs.length > 0) {
            common.log("\nMatching directories " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(d));
        } else {
            common.log("\nMatching directories: 0");
        }
    }

    public getMatchingFiles(findfiles: FindFile[]): string[] {
        const files: string[] = findfiles.map(f => f.relativePath());
        return common.setFromArray(files);
    }

    public printMatchingFiles(findfiles: FindFile[]): void {
        const files: string[] = this.getMatchingFiles(findfiles);
        if (files.length > 0) {
            common.log("\nMatching files " + `(${files.length}):`);
            files.forEach(f => common.log(f));
        } else {
            common.log("\nMatching files: 0");
        }
    }
}
