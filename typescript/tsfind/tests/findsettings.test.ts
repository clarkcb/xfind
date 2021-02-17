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
        expect(settings.excludeHidden).toBeTruthy();
        expect(settings.firstMatch).toBeFalsy();
        expect(settings.linesAfter).toEqual(0);
        expect(settings.linesBefore).toEqual(0);
        expect(settings.listDirs).toBeFalsy();
        expect(settings.listFiles).toBeFalsy();
        expect(settings.listLines).toBeFalsy();
        expect(settings.maxLineLength).toEqual(150);
        expect(settings.multilineFind).toBeFalsy();
        expect(settings.printResults).toBeFalsy();
        expect(settings.printUsage).toBeFalsy();
        expect(settings.printVersion).toBeFalsy();
        expect(settings.recursive).toBeTruthy();
        expect(settings.findArchives).toBeFalsy();
        expect(settings.startPath).toEqual('');
        expect(settings.uniqueLines).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
    });

    it('testAddExtensionsAsCommaSeparatedString', () => {
        const settings: FindSettings = new FindSettings();
        settings.addInExtensions("js,java");
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddExtensionsAsArray', () => {
        const settings: FindSettings = new FindSettings();
        settings.addInExtensions(["js","java"]);
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddFindPattern', () => {
        const settings: FindSettings = new FindSettings();
        settings.addFindPatterns("Finder");
        expect(settings.findPatterns.length).toEqual(1);
        expect(settings.findPatterns[0].source).toEqual('Finder');
    });

    it('testAddFindPatterns', () => {
        const settings: FindSettings = new FindSettings();
        settings.addFindPatterns(["Finder", "FileTypes"]);
        expect(settings.findPatterns.length).toEqual(2);
        expect(settings.findPatterns[0].source).toEqual('Finder');
        expect(settings.findPatterns[1].source).toEqual('FileTypes');
    });

    it('testSetArchivesOnly', () => {
        const settings: FindSettings = new FindSettings();
        expect(settings.archivesOnly).toBeFalsy();
        expect(settings.findArchives).toBeFalsy();
        settings.setArchivesOnly(true);
        expect(settings.archivesOnly).toBeTruthy();
        expect(settings.findArchives).toBeTruthy();
    });

    it('testSetDebug', () => {
        const settings: FindSettings = new FindSettings();
        expect(settings.debug).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
        settings.setDebug(true);
        expect(settings.debug).toBeTruthy();
        expect(settings.verbose).toBeTruthy();
    });
});
