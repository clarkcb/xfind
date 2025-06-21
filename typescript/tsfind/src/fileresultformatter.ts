/*
 * searchresults.ts
 *
 * SearchResult class represents a search result
 */

import {COLORS} from './color';
import {FileResult} from './fileresult';
import {FindSettings} from './findsettings';
import path from "path";

"use strict";

export class FileResultFormatter {
    settings: FindSettings;

    constructor(settings: FindSettings) {
        this.settings = settings;
        if (settings.colorize) {
            if (settings.inDirPatterns.length > 0) {
                this.formatPath = function(path: string): string {
                    return this.formatPathWithColor(path);
                };
            }
            if (settings.inExtensions.length > 0 || settings.inFilePatterns.length > 0) {
                this.formatFileName = function(fileName: string): string {
                    return this.formatFileNameWithColor(fileName);
                };
            }
        }
    }

    public colorize(s: string, matchStartIndex: number, matchEndIndex: number): string {
        let prefix = '';
        if (matchStartIndex > 0) {
            prefix = s.slice(0, matchStartIndex);
        }
        let suffix = '';
        if (matchEndIndex < s.length) {
            suffix = s.slice(matchEndIndex);
        }
        return prefix +
            COLORS.GREEN +
            s.slice(matchStartIndex, matchEndIndex) +
            COLORS.RESET +
            suffix;
    }

    private formatPathWithColor(path: string): string {
        let formattedPath = '.';
        if (path) {
            formattedPath = path;
            for (let p of this.settings.inDirPatterns) {
                let m = p.exec(formattedPath);
                if (m) {
                    formattedPath = this.colorize(formattedPath, m.index, m.index + m[0].length);
                    break;
                }
            }
        }
        return formattedPath;
    }

    public formatPath(path: string): string {
        return path;
    }

    private formatFileNameWithColor(fileName: string): string {
        let formattedFileName = fileName;
        for (let p of this.settings.inFilePatterns) {
            let m = p.exec(formattedFileName);
            if (m) {
                formattedFileName = this.colorize(formattedFileName, m.index, m.index + m[0].length);
                break;
            }
        }
        if (this.settings.inExtensions.length > 0) {
            const idx: number = formattedFileName.lastIndexOf('.');
            if (idx > 0 && idx < formattedFileName.length - 1) {
                formattedFileName = this.colorize(formattedFileName, idx + 1, formattedFileName.length);
            }
        }
        return formattedFileName;
    }

    public formatFileName(fileName: string): string {
        return fileName;
    }

    public formatFileResult(result: FileResult): string {
        let parent = this.formatPath(result.path);
        let fileName = this.formatFileName(result.fileName);
        return path.join(parent, fileName);
    }
}
