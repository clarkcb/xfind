/*
 * findoptions_test.js
 *
 * Some tests of findoptions.js
 */

const FindOptions = require('../src/findoptions').FindOptions;
const FindSettings = require('../src/findsettings').FindSettings;

describe('testing findoptions', () => {
    it('testNoArgs', () => {
        const findOptions = new FindOptions();
        findOptions.settingsFromArgs([], (err, settings) => {
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
        const findOptions = new FindOptions();
        const args = ['-x', 'js,java', '-s', 'Finder', '.'];
        findOptions.settingsFromArgs(args, (err, settings) => {
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

    it('testArchivesOnly', () => {
        const findOptions = new FindOptions();
        const args = ['--archivesonly'];
        findOptions.settingsFromArgs(args, (err, settings) => {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
            expect(settings.archivesOnly).toEqual(true);
            expect(settings.findArchives).toEqual(true);
        });
    });

    it('testDebug', () => {
        const findOptions = new FindOptions();
        const args = ['--debug'];
        findOptions.settingsFromArgs(args, (err, settings) => {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
            expect(settings.debug).toEqual(true);
            expect(settings.verbose).toEqual(true);
        });
    });

    it('testMissingArg', () => {
        const findOptions = new FindOptions();
        const args = ['-x'];
        findOptions.settingsFromArgs(args, (err) => {
            if (err) {
                const expected = "Missing argument for option x";
                expect(err.message).toEqual(expected);
            } else {
                console.log("Did not get expected missing argument error");
                expect(false).toEqual(true);
            }
        });
    });

    it('testIvalidArg', () => {
        const findOptions = new FindOptions();
        const args = ['-Q'];
        findOptions.settingsFromArgs(args, (err) => {
            if (err) {
                const expected = "Invalid option: Q";
                expect(err.message).toEqual(expected);
            } else {
                console.log("Did not get expected unknown option error");
                expect(false).toEqual(true);
            }
        });
    });

    it('testSettingsFromJson', () => {
        const findOptions = new FindOptions();
        const settings = new FindSettings();
        const json = '{\n' +
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
        const err = findOptions.settingsFromJson(json, settings);
        expect(err).toBeNull();
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
        expect(settings.excludeHidden).toBeFalsy();
    });
});
