package javafind;

import org.json.simple.parser.ParseException;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.*;

public class FindOptionsTest {

    public FindOptionsTest() {

    }

    @Test
    public final void testSettingsFromMinimalArgs() {
        String[] args = new String[]{"-s", "Find", "."};
        try {
            FindOptions findOptions = new FindOptions();
            FindSettings settings = findOptions.settingsFromArgs(args);
            assertFalse(settings.getArchivesOnly());
            assertFalse(settings.getDebug());
            assertTrue(settings.getExcludeHidden());
            assertFalse(settings.getFirstMatch());
            assertEquals(settings.getLinesAfter(), 0);
            assertEquals(settings.getLinesBefore(), 0);
            assertFalse(settings.getListDirs());
            assertFalse(settings.getListFiles());
            assertFalse(settings.getListLines());
            assertEquals(settings.getMaxLineLength(), 150);
            assertFalse(settings.getMultiLineFind());
            assertTrue(settings.getPrintResults());
            assertFalse(settings.getPrintUsage());
            assertFalse(settings.getPrintVersion());
            assertFalse(settings.getFindArchives());
            assertFalse(settings.getUniqueLines());
            assertFalse(settings.getVerbose());
        } catch (FindException e) {
            System.out.println("FindException: " + e.getMessage());
            fail();
        } catch (ParseException | IOException e) {
            System.out.println("Exception: " + e.getMessage());
            fail();
        }
    }

    @Test
    public final void testSettingsFromValidArgs() {
        String[] args = new String[]{"-x", "java,scala", "-s", "Find", "."};
        try {
            FindOptions findOptions = new FindOptions();
            FindSettings settings = findOptions.settingsFromArgs(args);
            assertEquals(settings.getInExtensions().size(), 2);
            assertTrue(settings.getInExtensions().contains("java"));
            assertTrue(settings.getInExtensions().contains("scala"));
            assertEquals(settings.getFindPatterns().size(), 1);
            assertEquals("Find", settings.getFindPatterns().toArray()[0].toString());
        } catch (FindException e) {
            System.out.println("FindException: " + e.getMessage());
            fail();
        } catch (ParseException | IOException e) {
            System.out.println("Exception: " + e.getMessage());
            fail();
        }
    }

    @Test
    public final void testSettingsFromJson() {
        StringBuilder json = new StringBuilder("{\n")
                .append("  \"startpath\": \"~/src/xfind/\",\n")
                .append("  \"in-ext\": [\"js\",\"ts\"],\n")
                .append("  \"out-dirpattern\": \"node_module\",\n")
                .append("  \"out-filepattern\": [\"temp\"],\n")
                .append("  \"findpattern\": \"Finder\",\n")
                .append("  \"linesbefore\": 2,\n")
                .append("  \"linesafter\": 2,\n")
                .append("  \"debug\": true,\n")
                .append("  \"allmatches\": false,\n")
                .append("  \"includehidden\": false,\n")
                .append("}");
        try {
            FindOptions findOptions = new FindOptions();
            FindSettings settings = new FindSettings();
            findOptions.settingsFromJson(json.toString(), settings);

            assertEquals("~/src/xfind/", settings.getStartPath());

            assertEquals(2, settings.getInExtensions().size());
            assertTrue(settings.getInExtensions().contains("js"));
            assertTrue(settings.getInExtensions().contains("ts"));

            assertEquals(1, settings.getOutDirPatterns().size());
            assertEquals("node_module", settings.getOutDirPatterns().toArray()[0].toString());

            assertEquals(1, settings.getOutFilePatterns().size());
            assertEquals("temp", settings.getOutFilePatterns().toArray()[0].toString());

            assertEquals(1, settings.getFindPatterns().size());
            assertEquals("Finder", settings.getFindPatterns().toArray()[0].toString());

            assertEquals(2, settings.getLinesBefore());
            assertEquals(2, settings.getLinesAfter());

            assertTrue(settings.getDebug());
            assertTrue(settings.getFirstMatch());
            assertTrue(settings.getExcludeHidden());
        } catch (ParseException e) {
            System.out.println("ParseException: " + e.getMessage());
            fail();
        } catch (IOException e) {
            System.out.println("IOException: " + e.getMessage());
            fail();
        }
    }
}
