package javafind;

import org.junit.Test;

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
        assertEquals(inExtensions.size(), 2);
        assertTrue(inExtensions.contains("java"));
        assertTrue(inExtensions.contains("scala"));
    }

    @Test
    public final void testAddPattern() {
        FindSettings settings = new FindSettings();
        settings.addInFilePattern("Find");
        Set<Pattern> inFilePatterns = settings.getInFilePatterns();
        assertEquals(inFilePatterns.size(), 1);
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
}
