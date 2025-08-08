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
                console.log('There was an error calling settingsFromArgs: ' + err);
                expect(false).toEqual(true);
            }
            expect(settings.archivesOnly).toBeFalsy();
            expect(settings.debug).toBeFalsy();
            expect(settings.followSymlinks).toBeFalsy();
            expect(settings.includeArchives).toBeFalsy();
            expect(settings.includeHidden).toBeFalsy();
            expect(settings.printDirs).toBeFalsy();
            expect(settings.printFiles).toBeTruthy();
            expect(settings.printUsage).toBeFalsy();
            expect(settings.printVersion).toBeFalsy();
            expect(settings.recursive).toBeTruthy();
            expect(settings.paths.length).toEqual(0);
            expect(settings.verbose).toBeFalsy();
        });
    });

    it('testValidArgs', () => {
        const findOptions: FindOptions = new FindOptions();
        const args: string[] = ['-x', 'js,java', '.'];
        findOptions.settingsFromArgs(args, function (err: Error | void, settings: FindSettings) {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
            expect(settings.inExtensions.length).toEqual(2);
            expect(settings.inExtensions[0]).toEqual('js');
            expect(settings.inExtensions[1]).toEqual('java');
            expect(settings.paths.length).toEqual(1);
            expect(settings.paths[0]).toEqual('.');
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
            '  "path": "~/src/xfind/",\n' +
            '  "in-ext": ["js","ts"],\n' +
            '  "out-dirpattern": "node_module",\n' +
            '  "out-filepattern": ["temp"],\n' +
            '  "debug": true,\n' +
            '  "followsymlinks": true,\n' +
            '  "includehidden": true\n' +
            '}';
        const err: Error|void = findOptions.updateSettingsFromJson(settings, json);
        expect(err).toBeUndefined();
        expect(settings.paths.length).toEqual(1);
        expect(settings.paths[0]).toEqual('~/src/xfind/');
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.outDirPatterns.length).toEqual(1);
        expect(settings.outDirPatterns[0].source).toEqual('node_module');
        expect(settings.outFilePatterns.length).toEqual(1);
        expect(settings.outFilePatterns[0].source).toEqual('temp');
        expect(settings.debug).toBeTruthy();
        expect(settings.verbose).toBeTruthy();
        expect(settings.followSymlinks).toBeTruthy();
        expect(settings.includeHidden).toBeTruthy();
    });
});
