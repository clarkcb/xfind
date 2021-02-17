package ktfind

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

/**
 * @author cary on 7/30/16.
 */
class FindOptionsTest {

    @Test
    fun testSettingsFromMinimalArgs() {
        val args = arrayOf("-s", "Find", ".")
        val findOptions = FindOptions()
        val settings = findOptions.settingsFromArgs(args)
        assertFalse(settings.archivesOnly)
        assertFalse(settings.debug)
        assertTrue(settings.excludeHidden)
        assertFalse(settings.firstMatch)
        assertEquals(0, settings.linesAfter)
        assertEquals(0, settings.linesBefore)
        assertFalse(settings.listDirs)
        assertFalse(settings.listFiles)
        assertFalse(settings.listLines)
        assertEquals(150, settings.maxLineLength)
        assertFalse(settings.multiLineFind)
        assertTrue(settings.printResults)
        assertFalse(settings.printUsage)
        assertFalse(settings.printVersion)
        assertFalse(settings.findArchives)
        assertFalse(settings.uniqueLines)
        assertFalse(settings.verbose)
    }

    @Test
    fun testSettingsFromValidArgs() {
        val args = arrayOf("-x", "java,scala", "-s", "Find", ".")
        val findOptions = FindOptions()
        val settings = findOptions.settingsFromArgs(args)
        assertEquals(2, settings.inExtensions.size)
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
        assertEquals(1, settings.findPatterns.size)
        assertEquals("Find", settings.findPatterns.first().toString())
    }

    @Test
    fun testSettingsFromJson() {
        val json = """{
                 |  "startpath": "~/src/xfind/",
                 |  "in-ext": ["js","ts"],
                 |  "out-dirpattern": ["build", "node_module", "tests", "typings"],
                 |  "out-filepattern": ["gulpfile", "\\.min\\."],
                 |  "findpattern": "Finder",
                 |  "linesbefore": 2,
                 |  "linesafter": 2,
                 |  "debug": true,
                 |  "allmatches": false,
                 |  "includehidden": false
                 |}""".trimMargin()
        val findOptions = FindOptions()
        val settings = findOptions.settingsFromJson(json, getDefaultSettings())

        assertTrue(settings.startPath == "~/src/xfind/")

        assertEquals(2, settings.inExtensions.size)
        assertTrue(settings.inExtensions.contains("js"))
        assertTrue(settings.inExtensions.contains("ts"))

        assertEquals(4, settings.outDirPatterns.size)
        assertEquals(1, settings.outDirPatterns.count {it.pattern == "node_module"})

        assertEquals(2, settings.outFilePatterns.size)
        assertEquals(1, settings.outFilePatterns.count {it.pattern == "gulpfile"})

        assertEquals(1, settings.findPatterns.size)
        assertEquals("Finder", settings.findPatterns.first().pattern)

        assertEquals(2, settings.linesBefore)
        assertEquals(2, settings.linesAfter)

        assertTrue(settings.debug)
        assertTrue(settings.firstMatch)
        assertTrue(settings.excludeHidden)
    }
}
