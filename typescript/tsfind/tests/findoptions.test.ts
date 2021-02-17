/*
 * findoptions.test.js
 *
 * Some tests of findoptions.js
 */

import {FindOptions} from '../src/findoptions';
import {FindSettings} from '../src/findsettings';

describe('testing findoptions', () => {
    it('testNoArgs', () => {
        const findOptions: FindOptions = new FindOptions();
        findOptions.settingsFromArgs([], function (err: Error | void, settings: FindSettings) {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
            expect(settings.archivesOnly).toBeFalsy();
            expect(settings.debug).toBeFalsy();
            expect(settings.excludeHidden).toBeTruthy();
            expect(settings.firstMatch).toBeFalsy();
            expect(settings.linesAfter).toEqual(0);
            expect(settings.linesBefore).toEqual(0);
            expect(settings.listDirs).toBeFalsy();
            expect(settings.listFiles).toBeFalsy();
            expect(settings.listLines).toBeFalsy();
            expect(settings.maxLineLength).toEqual(150);
            expect(settings.multilineFind).toBeFalsy();
            expect(settings.printResults).toBeTruthy();
            expect(settings.printUsage).toBeFalsy();
            expect(settings.printVersion).toBeFalsy();
            expect(settings.recursive).toBeTruthy();
            expect(settings.findArchives).toBeFalsy();
            expect(settings.startPath).toEqual('');
            expect(settings.uniqueLines).toBeFalsy();
            expect(settings.verbose).toBeFalsy();
        });
    });

    it('testValidArgs', () => {
        const findOptions: FindOptions = new FindOptions();
        const args: string[] = ['-x', 'js,java', '-s', 'Finder', '.'];
        findOptions.settingsFromArgs(args, function (err: Error | void, settings: FindSettings) {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
            expect(settings.inExtensions.length).toEqual(2);
            expect(settings.inExtensions[0]).toEqual('js');
            expect(settings.inExtensions[1]).toEqual('java');
            expect(settings.findPatterns.length).toEqual(1);
            expect(settings.findPatterns[0].source).toEqual('Finder');
        });
    });

    it('testInvalidArg', () => {
        const findOptions: FindOptions = new FindOptions();
        const args: string[] = ['-Q'];
        findOptions.settingsFromArgs(args, function(err: Error|void) {
            if (err) {
                const expected = "Invalid option: Q";
                expect(err.message).toEqual(expected);
            } else {
                expect(false).toEqual(true);
            }
        });
    });

    it('testSettingsFromJson', () => {
        const findOptions: FindOptions = new FindOptions();
        const settings: FindSettings = new FindSettings();
        const json: string = '{\n' +
            '  "startpath": "~/src/xfind/",\n' +
            '  "in-ext": ["js","ts"],\n' +
            '  "out-dirpattern": "node_module",\n' +
            '  "out-filepattern": ["temp"],\n' +
            '  "findpattern": "Finder",\n' +
            '  "linesbefore": 2,\n' +
            '  "linesafter": 2,\n' +
            '  "debug": true,\n' +
            '  "allmatches": false,\n' +
            '  "includehidden": true\n' +
            '}';
        const err: Error|void = findOptions.settingsFromJson(json, settings);
        expect(err).toBeUndefined();
        expect(settings.startPath).toEqual('~/src/xfind/');
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.outDirPatterns.length).toEqual(1);
        expect(settings.outDirPatterns[0].source).toEqual('node_module');
        expect(settings.outFilePatterns.length).toEqual(1);
        expect(settings.outFilePatterns[0].source).toEqual('temp');
        expect(settings.findPatterns.length).toEqual(1);
        expect(settings.findPatterns[0].source).toEqual('Finder');
        expect(settings.linesBefore).toEqual(2);
        expect(settings.linesAfter).toEqual(2);
        expect(settings.debug).toBeTruthy();
        expect(settings.verbose).toBeTruthy();
        expect(settings.firstMatch).toBeTruthy();
        expect(!settings.excludeHidden).toBeTruthy();
    });
});
