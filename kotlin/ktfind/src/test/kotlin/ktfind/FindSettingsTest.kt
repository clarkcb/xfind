package ktfind

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * @author cary on 7/30/16.
 */
class FindSettingsTest {
    @Test
    fun testDefaultSettings() {
        val settings = getDefaultSettings()
        assertEquals(false, settings.archivesOnly)
        assertEquals(false, settings.debug)
        assertEquals(false, settings.includeArchives)
        assertEquals(false, settings.includeHidden)
        assertEquals(false, settings.printDirs)
        assertEquals(false, settings.printFiles)
        assertEquals(false, settings.printUsage)
        assertEquals(false, settings.printVersion)
        assertEquals(true, settings.recursive)
        assertEquals(false, settings.verbose)
    }

    @Test
    fun testAddExtensions() {
        val settings = getDefaultSettings().copy(inExtensions = setOf("java", "scala"))
        assertEquals(2, settings.inExtensions.size.toLong())
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
    }

    @Test
    fun testAddExtensionsAsCommaString() {
        val defaultSettings = getDefaultSettings()
        val settings = defaultSettings.copy(inExtensions = addExtensions("java,scala", defaultSettings.inExtensions))
        assertEquals(2, settings.inExtensions.size.toLong())
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
    }

    @Test
    fun testSetArchivesOnly() {
        val settings = setArchivesOnly(getDefaultSettings(), true)
        assertTrue(settings.archivesOnly)
        assertTrue(settings.includeArchives)
    }

    @Test
    fun testSetDebug() {
        val settings = setDebug(getDefaultSettings(), true)
        assertTrue(settings.debug)
        assertTrue(settings.verbose)
    }
}
