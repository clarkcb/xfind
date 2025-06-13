package javafind;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class FindSettingsTest {

    public FindSettingsTest() {

    }

    @Test
    public final void testDefaultSettings() {
        var settings = new FindSettings();
        assertEquals(DefaultFindSettings.ARCHIVES_ONLY, settings.getArchivesOnly());
        assertEquals(DefaultFindSettings.DEBUG, settings.getDebug());
        assertEquals(DefaultFindSettings.FOLLOW_SYMLINKS, settings.getFollowSymlinks());
        assertEquals(DefaultFindSettings.INCLUDE_HIDDEN, settings.getIncludeHidden());
        assertEquals(DefaultFindSettings.INCLUDE_ARCHIVES, settings.getIncludeArchives());
        assertEquals(DefaultFindSettings.MAX_SIZE, settings.getMaxSize());
        assertEquals(DefaultFindSettings.MIN_SIZE, settings.getMinSize());
        assertEquals(DefaultFindSettings.PRINT_DIRS, settings.getPrintDirs());
        assertEquals(DefaultFindSettings.PRINT_FILES, settings.getPrintFiles());
        assertEquals(DefaultFindSettings.PRINT_USAGE, settings.getPrintUsage());
        assertEquals(DefaultFindSettings.PRINT_VERSION, settings.getPrintVersion());
        assertEquals(SortBy.FILEPATH, settings.getSortBy());
        assertEquals(DefaultFindSettings.SORT_CASE_INSENSITIVE, settings.getSortCaseInsensitive());
        assertEquals(DefaultFindSettings.SORT_DESCENDING, settings.getSortDescending());
        assertEquals(DefaultFindSettings.VERBOSE, settings.getVerbose());
    }

    @Test
    public final void testAddExtensions() {
        var settings = new FindSettings();
        settings.addInExtension("java,scala");
        var inExtensions = settings.getInExtensions();
        assertEquals(2, inExtensions.size());
        assertTrue(inExtensions.contains("java"));
        assertTrue(inExtensions.contains("scala"));
    }

    @Test
    public final void testAddPattern() {
        var settings = new FindSettings();
        settings.addInFilePattern("Find");
        var inFilePatterns = settings.getInFilePatterns();
        assertEquals(1, inFilePatterns.size());
    }

    @Test
    public final void testSetArchivesOnly() {
        var settings = new FindSettings();
        settings.setArchivesOnly(true);
        assertTrue(settings.getArchivesOnly());
        assertTrue(settings.getIncludeArchives());
    }

    @Test
    public final void testSetDebug() {
        var settings = new FindSettings();
        settings.setDebug(true);
        assertTrue(settings.getDebug());
        assertTrue(settings.getVerbose());
    }

    @Test
    public final void testSetLastModFromString() {
        var settings = new FindSettings();
        settings.setMaxLastMod("2023-01-01");
        var maxLastMod = settings.getMaxLastMod().get();
        assertEquals(1, maxLastMod.getDayOfMonth());
        assertEquals(1, maxLastMod.getMonth().getValue());
        assertEquals(2023, maxLastMod.getYear());
        settings.setMinLastMod("2022-06-01");
        var minLastMod = settings.getMinLastMod().get();
        assertEquals(1, minLastMod.getDayOfMonth());
        assertEquals(6, minLastMod.getMonth().getValue());
        assertEquals(2022, minLastMod.getYear());
    }
}
