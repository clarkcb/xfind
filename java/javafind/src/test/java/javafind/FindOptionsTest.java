package javafind;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Paths;

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
            assertFalse(settings.getFollowSymlinks());
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
            assertEquals(Paths.get("."), settings.getPaths().toArray()[0]);
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
        var json = """
{
    "path": "~/src/xfind/",
    "in-ext": ["js","ts"],
    "out-dirpattern": "node_module",
    "out-filepattern": ["temp"],
    "debug": true,
    "followsymlinks": true,
    "includehidden": false
}
""";
        try {
            var findOptions = new FindOptions();
            var settings = new FindSettings();
            findOptions.settingsFromJson(json, settings);

            assertEquals(1, settings.getPaths().size());
            assertEquals(Paths.get("~/src/xfind/"), settings.getPaths().toArray()[0]);

            assertEquals(2, settings.getInExtensions().size());
            assertTrue(settings.getInExtensions().contains("js"));
            assertTrue(settings.getInExtensions().contains("ts"));

            assertEquals(1, settings.getOutDirPatterns().size());
            assertEquals("node_module", settings.getOutDirPatterns().toArray()[0].toString());

            assertEquals(1, settings.getOutFilePatterns().size());
            assertEquals("temp", settings.getOutFilePatterns().toArray()[0].toString());

            assertTrue(settings.getDebug());
            assertTrue(settings.getVerbose());
            assertTrue(settings.getFollowSymlinks());
            assertFalse(settings.getIncludeHidden());
        } catch (IOException e) {
            System.out.println("IOException: " + e.getMessage());
            fail();
        } catch (FindException e) {
            System.out.println("FindException: " + e.getMessage());
            fail();
        }
    }
}
