/*
 * searchresults.ts
 *
 * SearchResult class represents a search result
 */

import {Color, colorToConsoleColor} from './color';
import {ConsoleColor} from './consolecolor';
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
                this.formatDirPath = function(path: string): string {
                    return this.formatDirPathWithColor(path);
                };
            }
            if (settings.inExtensions.length > 0 || settings.inFilePatterns.length > 0) {
                this.formatFileName = function(fileName: string): string {
                    return this.formatFileNameWithColor(fileName);
                };
            }
        }
    }

    public colorize(s: string, matchStartIndex: number, matchEndIndex: number, color: Color): string {
        let prefix = '';
        if (matchStartIndex > 0) {
            prefix = s.slice(0, matchStartIndex);
        }
        let suffix = '';
        if (matchEndIndex < s.length) {
            suffix = s.slice(matchEndIndex);
        }
        return prefix +
            colorToConsoleColor(color) +
            s.slice(matchStartIndex, matchEndIndex) +
            ConsoleColor.RESET +
            suffix;
    }

    private formatDirPathWithColor(dirPath: string): string {
        let formattedPath = '.';
        if (dirPath) {
            formattedPath = dirPath;
            for (let p of this.settings.inDirPatterns) {
                let m = p.exec(formattedPath);
                if (m) {
                    formattedPath = this.colorize(formattedPath, m.index, m.index + m[0].length, this.settings.dirColor);
                    break;
                }
            }
        }
        return formattedPath;
    }

    public formatDirPath(dirPath: string): string {
        return dirPath;
    }

    private formatFileNameWithColor(fileName: string): string {
        let formattedFileName = fileName;
        for (let p of this.settings.inFilePatterns) {
            let m = p.exec(formattedFileName);
            if (m) {
                formattedFileName = this.colorize(formattedFileName, m.index, m.index + m[0].length, this.settings.fileColor);
                break;
            }
        }
        if (this.settings.inExtensions.length > 0) {
            const idx: number = formattedFileName.lastIndexOf('.');
            if (idx > 0 && idx < formattedFileName.length - 1) {
                formattedFileName = this.colorize(formattedFileName, idx + 1, formattedFileName.length, this.settings.extColor);
            }
        }
        return formattedFileName;
    }

    public formatFileName(fileName: string): string {
        return fileName;
    }

    public formatFileResult(result: FileResult): string {
        let parent = this.formatDirPath(result.path);
        let fileName = this.formatFileName(result.fileName);
        return path.join(parent, fileName);
    }
}
