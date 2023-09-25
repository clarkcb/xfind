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
        assertEquals(DefaultFindSettings.ARCHIVESONLY, settings.getArchivesOnly());
        assertEquals(DefaultFindSettings.DEBUG, settings.getDebug());
        assertEquals(DefaultFindSettings.EXCLUDEHIDDEN, settings.getExcludeHidden());
        assertEquals(DefaultFindSettings.INCLUDEARCHIVES, settings.getIncludeArchives());
        assertEquals(DefaultFindSettings.LISTDIRS, settings.getListDirs());
        assertEquals(DefaultFindSettings.LISTFILES, settings.getListFiles());
        assertEquals(DefaultFindSettings.MAXSIZE, settings.getMaxSize());
        assertEquals(DefaultFindSettings.MINSIZE, settings.getMinSize());
        assertEquals(DefaultFindSettings.PRINTUSAGE, settings.getPrintUsage());
        assertEquals(DefaultFindSettings.PRINTVERSION, settings.getPrintVersion());
        assertEquals(DefaultFindSettings.SORT_CASEINSENSITIVE, settings.getSortCaseInsensitive());
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
        assertEquals(1, settings.getMaxLastMod().get(ChronoField.DAY_OF_MONTH));
        assertEquals(1, settings.getMaxLastMod().get(ChronoField.MONTH_OF_YEAR));
        assertEquals(2023, settings.getMaxLastMod().get(ChronoField.YEAR));
        settings.setMinLastMod("2022-06-01");
        assertEquals(1, settings.getMinLastMod().get(ChronoField.DAY_OF_MONTH));
        assertEquals(6, settings.getMinLastMod().get(ChronoField.MONTH_OF_YEAR));
        assertEquals(2022, settings.getMinLastMod().get(ChronoField.YEAR));
    }
}
