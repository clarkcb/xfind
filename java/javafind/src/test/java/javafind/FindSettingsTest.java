package javafind;

import org.junit.Test;

import java.time.temporal.ChronoField;
import java.util.Set;
import java.util.regex.Pattern;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class FindSettingsTest {

    public FindSettingsTest() {

    }

    @Test
    public final void testDefaultSettings() {
        var settings = new FindSettings();
        assertEquals(DefaultFindSettings.ARCHIVES_ONLY, settings.getArchivesOnly());
        assertEquals(DefaultFindSettings.DEBUG, settings.getDebug());
        assertEquals(DefaultFindSettings.EXCLUDE_HIDDEN, settings.getExcludeHidden());
        assertEquals(DefaultFindSettings.INCLUDE_ARCHIVES, settings.getIncludeArchives());
        assertEquals(DefaultFindSettings.LIST_DIRS, settings.getListDirs());
        assertEquals(DefaultFindSettings.LIST_FILES, settings.getListFiles());
        assertEquals(DefaultFindSettings.MAX_SIZE, settings.getMaxSize());
        assertEquals(DefaultFindSettings.MIN_SIZE, settings.getMinSize());
        assertEquals(DefaultFindSettings.PRINT_USAGE, settings.getPrintUsage());
        assertEquals(DefaultFindSettings.PRINT_VERSION, settings.getPrintVersion());
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
        assertEquals(1, settings.getMaxLastMod().getDayOfMonth());
        assertEquals(1, settings.getMaxLastMod().getMonth().getValue());
        assertEquals(2023, settings.getMaxLastMod().getYear());
        settings.setMinLastMod("2022-06-01");
        assertEquals(1, settings.getMinLastMod().getDayOfMonth());
        assertEquals(6, settings.getMinLastMod().getMonth().getValue());
        assertEquals(2022, settings.getMinLastMod().getYear());
    }
}
