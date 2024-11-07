package ktfind

import org.junit.jupiter.api.Assertions.assertFalse
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
        assertFalse(settings.archivesOnly)
        assertFalse(settings.debug)
        assertFalse(settings.followSymlinks)
        assertFalse(settings.includeArchives)
        assertFalse(settings.includeHidden)
        assertFalse(settings.printDirs)
        assertFalse(settings.printFiles)
        assertFalse(settings.printUsage)
        assertFalse(settings.printVersion)
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
