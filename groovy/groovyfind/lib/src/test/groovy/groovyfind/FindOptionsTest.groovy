package groovyfind

import java.nio.file.Paths
import org.junit.jupiter.api.Test

import static org.junit.jupiter.api.Assertions.*

class FindOptionsTest {

    FindOptionsTest() {

    }

    @Test
    final void testSettingsFromMinimalArgs() {
        def args = new String[]{'.'}
        try {
            def findOptions = new FindOptions()
            def settings = findOptions.settingsFromArgs(args)
            assertFalse(settings.archivesOnly)
            assertFalse(settings.debug)
            assertFalse(settings.followSymlinks)
            assertFalse(settings.includeArchives)
            assertFalse(settings.includeHidden)
            assertFalse(settings.printDirs)
            assertTrue(settings.printFiles)
            assertFalse(settings.printUsage)
            assertFalse(settings.printVersion)
            assertFalse(settings.verbose)
        } catch (FindException e) {
            System.out.println("FindException: ${e.message}")
            fail()
        } catch (IOException e) {
            System.out.println("IOException: ${e.message}")
            fail()
        }
    }

    @Test
    final void testSettingsFromValidArgs() {
        def args = new String[]{'-x', 'java,scala', '.'}
        try {
            def findOptions = new FindOptions()
            def settings = findOptions.settingsFromArgs(args)
            assertEquals(2, settings.inExtensions.size())
            assertTrue(settings.inExtensions.contains('java'))
            assertTrue(settings.inExtensions.contains('scala'))
            assertEquals(1, settings.paths.size())
            assertEquals(Paths.get('.'), settings.paths.toArray()[0])
        } catch (FindException e) {
            System.out.println("FindException: ${e.message}")
            fail()
        } catch (IOException e) {
            System.out.println("IOException: ${e.message}")
            fail()
        }
    }

    @Test
    final void testUpdateSettingsFromJson() {
        def json = new StringBuilder('{\n')
                .append('  "path": "~/src/xfind/",\n')
                .append('  "in-ext": ["js","ts"],\n')
                .append('  "out-dirpattern": "node_module",\n')
                .append('  "out-filepattern": ["temp"],\n')
                .append('  "debug": true,\n')
                .append('  "followsymlinks": true,\n')
                .append('  "includehidden": false\n')
                .append('}')
        try {
            def findOptions = new FindOptions()
            def settings = new FindSettings()
            findOptions.updateSettingsFromJson(json.toString(), settings)

            assertEquals(1, settings.paths.size())
            assertEquals(Paths.get('~/src/xfind/'), settings.paths.toArray()[0])

            assertEquals(2, settings.inExtensions.size())
            assertTrue(settings.inExtensions.contains('js'))
            assertTrue(settings.inExtensions.contains('ts'))

            assertEquals(1, settings.outDirPatterns.size())
            assertEquals('node_module', settings.outDirPatterns.toArray()[0].toString())

            assertEquals(1, settings.outFilePatterns.size())
            assertEquals('temp', settings.outFilePatterns.toArray()[0].toString())

            assertTrue(settings.debug)
            assertTrue(settings.verbose)
            assertTrue(settings.followSymlinks)
            assertFalse(settings.includeHidden)
        } catch (IOException e) {
            System.out.println("IOException: ${e.message}")
            fail()
        }
    }

}
