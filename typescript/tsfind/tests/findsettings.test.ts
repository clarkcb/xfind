/*
 * findsettings.test.js
 *
 * Some tests of findsettings.js
 */

import {FindSettings} from '../src/findsettings';

describe('testing findsettings', () => {
    it('testDefaultSettings', () => {
        const settings: FindSettings = new FindSettings();
        expect(settings.archivesOnly).toBeFalsy();
        expect(settings.debug).toBeFalsy();
        expect(settings.includeArchives).toBeFalsy();
        expect(settings.includeHidden).toBeFalsy();
        expect(settings.paths.length).toEqual(0);
        expect(settings.printDirs).toBeFalsy();
        expect(settings.printFiles).toBeFalsy();
        expect(settings.printUsage).toBeFalsy();
        expect(settings.printVersion).toBeFalsy();
        expect(settings.recursive).toBeTruthy();
        expect(settings.sortCaseInsensitive).toBeFalsy();
        expect(settings.sortDescending).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
    });

    it('testAddExtensionsAsCommaSeparatedString', () => {
        const settings: FindSettings = new FindSettings();
        settings.addInExtensions('js,java');
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddExtensionsAsArray', () => {
        const settings: FindSettings = new FindSettings();
        settings.addInExtensions(['js','java']);
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddFindPattern', () => {
        const settings: FindSettings = new FindSettings();
        settings.addInFilePatterns('Finder');
        expect(settings.inFilePatterns.length).toEqual(1);
        expect(settings.inFilePatterns[0].source).toEqual('Finder');
    });

    it('testAddFindPatterns', () => {
        const settings: FindSettings = new FindSettings();
        settings.addInFilePatterns(['Finder', 'FileTypes']);
        expect(settings.inFilePatterns.length).toEqual(2);
        expect(settings.inFilePatterns[0].source).toEqual('Finder');
        expect(settings.inFilePatterns[1].source).toEqual('FileTypes');
    });

    it('testSetArchivesOnly', () => {
        const settings: FindSettings = new FindSettings();
        expect(settings.archivesOnly).toBeFalsy();
        expect(settings.includeArchives).toBeFalsy();
        settings.archivesOnly = true;
        expect(settings.archivesOnly).toBeTruthy();
        expect(settings.includeArchives).toBeTruthy();
    });

    it('testSetDebug', () => {
        const settings: FindSettings = new FindSettings();
        expect(settings.debug).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
        settings.debug = true;
        expect(settings.debug).toBeTruthy();
        expect(settings.verbose).toBeTruthy();
    });
});
