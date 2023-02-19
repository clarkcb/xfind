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
        FindSettings settings = new FindSettings();
        assertEquals(DefaultSettings.ARCHIVESONLY, settings.getArchivesOnly());
        assertEquals(DefaultSettings.DEBUG, settings.getDebug());
        assertEquals(DefaultSettings.EXCLUDEHIDDEN, settings.getExcludeHidden());
        assertEquals(DefaultSettings.INCLUDEARCHIVES, settings.getIncludeArchives());
        assertEquals(DefaultSettings.LISTDIRS, settings.getListDirs());
        assertEquals(DefaultSettings.LISTFILES, settings.getListFiles());
        assertEquals(DefaultSettings.MAXSIZE, settings.getMaxSize());
        assertEquals(DefaultSettings.MINSIZE, settings.getMinSize());
        assertEquals(DefaultSettings.PRINTUSAGE, settings.getPrintUsage());
        assertEquals(DefaultSettings.PRINTVERSION, settings.getPrintVersion());
        assertEquals(DefaultSettings.SORT_CASEINSENSITIVE, settings.getSortCaseInsensitive());
        assertEquals(DefaultSettings.SORT_DESCENDING, settings.getSortDescending());
        assertEquals(DefaultSettings.VERBOSE, settings.getVerbose());
    }

    @Test
    public final void testAddExtensions() {
        FindSettings settings = new FindSettings();
        settings.addInExtension("java,scala");
        Set<String> inExtensions = settings.getInExtensions();
        assertEquals(2, inExtensions.size());
        assertTrue(inExtensions.contains("java"));
        assertTrue(inExtensions.contains("scala"));
    }

    @Test
    public final void testAddPattern() {
        FindSettings settings = new FindSettings();
        settings.addInFilePattern("Find");
        Set<Pattern> inFilePatterns = settings.getInFilePatterns();
        assertEquals(1, inFilePatterns.size());
    }

    @Test
    public final void testSetArchivesOnly() {
        FindSettings settings = new FindSettings();
        settings.setArchivesOnly(true);
        assertTrue(settings.getArchivesOnly());
        assertTrue(settings.getIncludeArchives());
    }

    @Test
    public final void testSetDebug() {
        FindSettings settings = new FindSettings();
        settings.setDebug(true);
        assertTrue(settings.getDebug());
        assertTrue(settings.getVerbose());
    }

    @Test
    public final void testSetLastModFromString() {
        FindSettings settings = new FindSettings();
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
