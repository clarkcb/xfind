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
        assertEquals(DefaultSettings.FIRSTMATCH, settings.getFirstMatch());
        assertEquals(DefaultSettings.LINESAFTER, settings.getLinesAfter());
        assertEquals(DefaultSettings.LINESBEFORE, settings.getLinesBefore());
        assertEquals(DefaultSettings.LISTDIRS, settings.getListDirs());
        assertEquals(DefaultSettings.LISTFILES, settings.getListFiles());
        assertEquals(DefaultSettings.LISTLINES, settings.getListLines());
        assertEquals(DefaultSettings.MAXLINELENGTH, settings.getMaxLineLength());
        assertEquals(DefaultSettings.MULTILINEFIND, settings.getMultiLineFind());
        assertEquals(DefaultSettings.PRINTRESULTS, settings.getPrintResults());
        assertEquals(DefaultSettings.PRINTUSAGE, settings.getPrintUsage());
        assertEquals(DefaultSettings.PRINTVERSION, settings.getPrintVersion());
        assertEquals(DefaultSettings.FINDARCHIVES, settings.getFindArchives());
        assertEquals(DefaultSettings.UNIQUELINES, settings.getUniqueLines());
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
        settings.addFindPattern("Finder");
        Set<Pattern> findPatterns = settings.getFindPatterns();
        assertEquals(findPatterns.size(), 1);
    }

    @Test
    public final void testSetArchivesOnly() {
        FindSettings settings = new FindSettings();
        settings.setArchivesOnly(true);
        assertTrue(settings.getArchivesOnly());
        assertTrue(settings.getFindArchives());
    }

    @Test
    public final void testSetDebug() {
        FindSettings settings = new FindSettings();
        settings.setDebug(true);
        assertTrue(settings.getDebug());
        assertTrue(settings.getVerbose());
    }
}
