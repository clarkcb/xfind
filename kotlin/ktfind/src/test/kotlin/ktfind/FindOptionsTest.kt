package ktfind

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * @author cary on 7/30/16.
 */
class FindOptionsTest {

    @Test
    fun testSettingsFromMinimalArgs() {
        val args = arrayOf(".")
        val findOptions = FindOptions()
        val settings = findOptions.settingsFromArgs(args)
        assertFalse(settings.archivesOnly)
        assertFalse(settings.debug)
        assertFalse(settings.includeArchives)
        assertFalse(settings.includeHidden)
        assertFalse(settings.listDirs)
        assertTrue(settings.listFiles)
        assertEquals(1, settings.paths.size)
        assertEquals(".", settings.paths.first())
        assertFalse(settings.printUsage)
        assertFalse(settings.printVersion)
        assertFalse(settings.verbose)
    }

    @Test
    fun testSettingsFromValidArgs() {
        val args = arrayOf("-x", "java,scala", ".")
        val findOptions = FindOptions()
        val settings = findOptions.settingsFromArgs(args)
        assertEquals(2, settings.inExtensions.size)
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
        assertEquals(1, settings.paths.size)
        assertEquals(".", settings.paths.first())
    }

    @Test
    fun testSettingsFromJson() {
        val json = """{
                 |  "path": "~/src/xfind/",
                 |  "in-ext": ["js","ts"],
                 |  "out-dirpattern": ["build", "node_module", "tests", "typings"],
                 |  "out-filepattern": ["gulpfile", "\\.min\\."],
                 |  "debug": true,
                 |  "includehidden": false
                 |}""".trimMargin()
        val findOptions = FindOptions()
        val settings = findOptions.settingsFromJson(json, getDefaultSettings())

        assertEquals(1, settings.paths.size)
        assertEquals("~/src/xfind/", settings.paths.first())

        assertEquals(2, settings.inExtensions.size)
        assertTrue(settings.inExtensions.contains("js"))
        assertTrue(settings.inExtensions.contains("ts"))

        assertEquals(4, settings.outDirPatterns.size)
        assertEquals(1, settings.outDirPatterns.count {it.pattern == "node_module"})

        assertEquals(2, settings.outFilePatterns.size)
        assertEquals(1, settings.outFilePatterns.count {it.pattern == "gulpfile"})

        assertTrue(settings.debug)
        assertFalse(settings.includeHidden)
    }
}
