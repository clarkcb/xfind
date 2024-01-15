package javafind;

import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

public class FindOptionsTest {

    public FindOptionsTest() {

    }

    @Test
    public final void testSettingsFromMinimalArgs() {
        var args = new String[]{"."};
        try {
            var findOptions = new FindOptions();
            var settings = findOptions.settingsFromArgs(args);
            assertFalse(settings.getArchivesOnly());
            assertFalse(settings.getDebug());
            assertFalse(settings.getIncludeArchives());
            assertFalse(settings.getIncludeHidden());
            assertFalse(settings.getPrintDirs());
            assertTrue(settings.getPrintFiles());
            assertFalse(settings.getPrintUsage());
            assertFalse(settings.getPrintVersion());
            assertFalse(settings.getVerbose());
        } catch (FindException e) {
            System.out.println("FindException: " + e.getMessage());
            fail();
        } catch (IOException e) {
            System.out.println("Exception: " + e.getMessage());
            fail();
        }
    }

    @Test
    public final void testSettingsFromValidArgs() {
        var args = new String[]{"-x", "java,scala", "."};
        try {
            var findOptions = new FindOptions();
            var settings = findOptions.settingsFromArgs(args);
            assertEquals(2, settings.getInExtensions().size());
            assertTrue(settings.getInExtensions().contains("java"));
            assertTrue(settings.getInExtensions().contains("scala"));
            assertEquals(1, settings.getPaths().size());
            assertEquals(".", settings.getPaths().toArray()[0]);
        } catch (FindException e) {
            System.out.println("FindException: " + e.getMessage());
            fail();
        } catch (IOException e) {
            System.out.println("Exception: " + e.getMessage());
            fail();
        }
    }

    @Test
    public final void testSettingsFromJson() {
        var json = new StringBuilder("{\n")
                .append("  \"path\": \"~/src/xfind/\",\n")
                .append("  \"in-ext\": [\"js\",\"ts\"],\n")
                .append("  \"out-dirpattern\": \"node_module\",\n")
                .append("  \"out-filepattern\": [\"temp\"],\n")
                .append("  \"debug\": true,\n")
                .append("  \"includehidden\": false,\n")
                .append("}");
        try {
            var findOptions = new FindOptions();
            var settings = new FindSettings();
            findOptions.settingsFromJson(json.toString(), settings);

            assertEquals(1, settings.getPaths().size());
            assertEquals("~/src/xfind/", settings.getPaths().toArray()[0]);

            assertEquals(2, settings.getInExtensions().size());
            assertTrue(settings.getInExtensions().contains("js"));
            assertTrue(settings.getInExtensions().contains("ts"));

            assertEquals(1, settings.getOutDirPatterns().size());
            assertEquals("node_module", settings.getOutDirPatterns().toArray()[0].toString());

            assertEquals(1, settings.getOutFilePatterns().size());
            assertEquals("temp", settings.getOutFilePatterns().toArray()[0].toString());

            assertTrue(settings.getDebug());
            assertTrue(settings.getVerbose());
            assertFalse(settings.getIncludeHidden());
        } catch (IOException e) {
            System.out.println("IOException: " + e.getMessage());
            fail();
        }
    }
}
