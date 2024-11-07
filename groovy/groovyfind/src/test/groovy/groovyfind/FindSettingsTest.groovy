package groovyfind

import org.junit.jupiter.api.Test

import static org.junit.jupiter.api.Assertions.assertEquals
import static org.junit.jupiter.api.Assertions.assertTrue

class FindSettingsTest {

    FindSettingsTest() {

    }

    @Test
    final void testDefaultSettings() {
        def settings = new FindSettings()
        assertEquals(DefaultFindSettings.ARCHIVES_ONLY, settings.archivesOnly)
        assertEquals(DefaultFindSettings.DEBUG, settings.debug)
        assertEquals(DefaultFindSettings.FOLLOW_SYMLINKS, settings.followSymlinks)
        assertEquals(DefaultFindSettings.INCLUDE_HIDDEN, settings.includeHidden)
        assertEquals(DefaultFindSettings.INCLUDE_ARCHIVES, settings.includeArchives)
        assertEquals(DefaultFindSettings.MAX_SIZE, settings.maxSize)
        assertEquals(DefaultFindSettings.MIN_SIZE, settings.minSize)
        assertEquals(DefaultFindSettings.PRINT_DIRS, settings.printDirs)
        assertEquals(DefaultFindSettings.PRINT_FILES, settings.printFiles)
        assertEquals(DefaultFindSettings.PRINT_USAGE, settings.printUsage)
        assertEquals(DefaultFindSettings.PRINT_VERSION, settings.printVersion)
        assertEquals(DefaultFindSettings.SORT_CASE_INSENSITIVE, settings.sortCaseInsensitive)
        assertEquals(DefaultFindSettings.SORT_DESCENDING, settings.sortDescending)
        assertEquals(DefaultFindSettings.VERBOSE, settings.verbose)
    }

    @Test
    final void testAddExtensions() {
        def settings = new FindSettings()
        settings.addInExtension('java,scala')
        def inExtensions = settings.inExtensions
        assertEquals(2, inExtensions.size())
        assertTrue(inExtensions.contains('java'))
        assertTrue(inExtensions.contains('scala'))
    }

    @Test
    final void testAddPattern() {
        def settings = new FindSettings()
        settings.addInFilePattern('Find')
        def inFilePatterns = settings.inFilePatterns
        assertEquals(1, inFilePatterns.size())
    }

    @Test
    final void testSetArchivesOnly() {
        def settings = new FindSettings()
        settings.setArchivesOnly(true)
        assertTrue(settings.archivesOnly)
        assertTrue(settings.includeArchives)
    }

    @Test
    final void testSetDebug() {
        def settings = new FindSettings()
        settings.setDebug(true)
        assertTrue(settings.debug)
        assertTrue(settings.verbose)
    }

    @Test
    final void testSetLastModFromString() {
        def settings = new FindSettings()
        settings.setMaxLastModFromString('2023-01-01')
        assertEquals(1, settings.maxLastMod.getDayOfMonth())
        assertEquals(1, settings.maxLastMod.getMonth().getValue())
        assertEquals(2023, settings.maxLastMod.getYear())
        settings.setMinLastModFromString('2022-06-01')
        assertEquals(1, settings.minLastMod.getDayOfMonth())
        assertEquals(6, settings.minLastMod.getMonth().getValue())
        assertEquals(2022, settings.minLastMod.getYear())
    }

}
