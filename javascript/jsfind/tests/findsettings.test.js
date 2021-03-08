/*
 * findsettings.test.js
 *
 * Some tests of findsettings.js
 */

const FindSettings = require('../src/findsettings').FindSettings;

describe('testing findsettings', () => {
    it('testDefaultSettings', () => {
        const settings = new FindSettings();
        expect(settings.archivesOnly).toBeFalsy();
        expect(settings.debug).toBeFalsy();
        expect(settings.excludeHidden).toBeTruthy();
        expect(settings.includeArchives).toBeFalsy();
        expect(settings.listDirs).toBeFalsy();
        expect(settings.listFiles).toBeFalsy();
        expect(settings.printUsage).toBeFalsy();
        expect(settings.printVersion).toBeFalsy();
        expect(settings.recursive).toBeTruthy();
        expect(settings.paths.length).toEqual(0);
        expect(settings.uniqueLines).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
    });

    it('testAddExtensionsAsCommaSeparatedString', () => {
        let settings = new FindSettings();
        settings.addInExtensions('js,java');
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddExtensionsAsArray', () => {
        let settings = new FindSettings();
        settings.addInExtensions(['js','java']);
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddFindPattern', () => {
        let settings = new FindSettings();
        settings.addInFilePatterns('Finder');
        expect(settings.inFilePatterns.length).toEqual(1);
        expect(settings.inFilePatterns[0].source).toEqual('Finder');
    });

    it('testAddFindPatterns', () => {
        let settings = new FindSettings();
        settings.addInFilePatterns(['Finder', 'FileTypes']);
        expect(settings.inFilePatterns.length).toEqual(2);
        expect(settings.inFilePatterns[0].source).toEqual('Finder');
        expect(settings.inFilePatterns[1].source).toEqual('FileTypes');
    });

    it('testSetArchivesOnly', () => {
        let settings = new FindSettings();
        expect(settings.archivesOnly).toBeFalsy();
        expect(settings.includeArchives).toBeFalsy();
        settings.setArchivesOnly();
        expect(settings.archivesOnly).toBeTruthy();
        expect(settings.includeArchives).toBeTruthy();
    });

    it('testSetDebug', () => {
        let settings = new FindSettings();
        expect(settings.debug).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
        settings.setDebug();
        expect(settings.debug).toBeTruthy();
        expect(settings.verbose).toBeTruthy();
    });
});
