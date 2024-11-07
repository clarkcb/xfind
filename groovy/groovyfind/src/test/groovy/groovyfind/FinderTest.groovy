package groovyfind

import org.junit.jupiter.api.Test

import java.nio.file.Paths

import static org.junit.jupiter.api.Assertions.*

class FinderTest {

    FinderTest() {}

    private static FindSettings getSettings() {
        def settings = new FindSettings()
        settings.addPath('.')
        return settings
    }

    private static String getBinPath() {
        var xfindPath = System.getenv("XFIND_PATH")
        if (xfindPath == null) {
            xfindPath = System.getenv("HOME") + "src/xfind"
        }
        return xfindPath + "/bin"
    }

    /*************************************************************
     * isMatchingDir tests
     *************************************************************/
    @Test
    final void testIsMatchingDir_SingleDot_True() {
        def settings = getSettings()
        def finder = new Finder(settings)
        assertTrue(finder.isMatchingDir(Paths.get('.')))
    }

    @Test
    final void testIsMatchingDir_DoubleDot_True() {
        def settings = getSettings()
        def finder = new Finder(settings)
        assertTrue(finder.isMatchingDir(Paths.get('..')))
    }

    @Test
    final void testIsMatchingDir_IsHidden_False() {
        def settings = getSettings()
        def finder = new Finder(settings)
        assertFalse(finder.isMatchingDir(Paths.get('.git')))
    }

    @Test
    final void testIsMatchingDir_IsHiddenIncludeHidden_True() {
        def settings = getSettings()
        settings.setIncludeHidden(true)
        def finder = new Finder(settings)
        assertTrue(finder.isMatchingDir(Paths.get('.git')))
    }

    @Test
    final void testIsMatchingDir_NoPatterns_True() {
        def settings = getSettings()
        def finder = new Finder(settings)
        assertTrue(finder.isMatchingDir(Paths.get('/Users')))
    }

    @Test
    final void testIsMatchingDir_MatchesInPattern_True() {
        def settings = getSettings()
        settings.addInDirPattern('Find')
        def finder = new Finder(settings)
        assertTrue(finder.isMatchingDir(Paths.get('CsFind')))
    }

    @Test
    final void testIsMatchingDir_MatchesOutPattern_False() {
        def settings = getSettings()
        settings.addOutDirPattern('Find')
        def finder = new Finder(settings)
        assertFalse(finder.isMatchingDir(Paths.get('CsFind')))
    }

    @Test
    final void testIsMatchingDir_DoesNotMatchInPattern_False() {
        def settings = getSettings()
        settings.addInDirPattern('FindFiles')
        def finder = new Finder(settings)
        assertFalse(finder.isMatchingDir(Paths.get('CsFind')))
    }

    @Test
    final void testIsMatchingDir_DoesNotMatchOutPattern_True() {
        def settings = getSettings()
        settings.addOutDirPattern('FindFiles')
        def finder = new Finder(settings)
        def dir = Paths.get('CsFind')
        assertTrue(finder.isMatchingDir(dir))
    }

    @Test
    final void testIsMatchingDir_DoesNotMatchOutPattern2_True() {
        def settings = getSettings()
        settings.addOutDirPattern('FindFiles')
        def finder = new Finder(settings)
        def dir = Paths.get('/Users/cary/src/xfind/java/javafind/ssrc/main/java/javafind')
        assertTrue(finder.isMatchingDir(dir))
    }

    /*************************************************************
     * isMatchingFile tests
     *************************************************************/
    @Test
    final void testIsMatchingFile_NoExtensionsNoPatterns_True() {
        def settings = getSettings()
        def finder = new Finder(settings)
        def path = Paths.get('./FileUtil.cs')
        def fileResult = new FileResult(path, FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    final void testIsMatchingFile_MatchesInExtension_True() {
        def settings = getSettings()
        settings.addInExtension('cs')
        def finder = new Finder(settings)
        def path = Paths.get('./FileUtil.cs')
        def fileResult = new FileResult(path, FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    final void testIsMatchingFile_DoesNotMatchInExtension_False() {
        def settings = getSettings()
        settings.addInExtension('java')
        def finder = new Finder(settings)
        def path = Paths.get('./FileUtil.cs')
        def fileResult = new FileResult(path, FileType.CODE)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }


    @Test
    final void testIsMatchingFile_MatchesOutExtension_False() {
        def settings = getSettings()
        settings.addOutExtension('cs')
        def finder = new Finder(settings)
        def path = Paths.get('./FileUtil.cs')
        def fileResult = new FileResult(path, FileType.CODE)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    final void testIsMatchingFile_DoesNotMatchOutExtension_True() {
        def settings = getSettings()
        settings.addOutExtension('java')
        def finder = new Finder(settings)
        def path = Paths.get('./FileUtil.cs')
        def fileResult = new FileResult(path, FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    final void testIsMatchingFile_MatchesInPattern_True() {
        def settings = getSettings()
        settings.addInFilePattern('Find')
        def finder = new Finder(settings)
        def path = Paths.get('./Finder.cs')
        def fileResult = new FileResult(path, FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    @Test
    final void testIsMatchingFile_DoesNotMatchInPattern_False() {
        def settings = getSettings()
        settings.addInFilePattern('Find')
        def finder = new Finder(settings)
        def path = Paths.get('./FileUtil.cs')
        def fileResult = new FileResult(path, FileType.CODE)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    final void testIsMatchingFile_MatchesOutPattern_False() {
        def settings = getSettings()
        settings.addOutFilePattern('Find')
        def finder = new Finder(settings)
        def path = Paths.get('./Finder.cs')
        def fileResult = new FileResult(path, FileType.CODE)
        assertFalse(finder.isMatchingFileResult(fileResult))
    }

    @Test
    final void testIsMatchingFile_DoesNotMatchOutPattern_True() {
        def settings = getSettings()
        settings.addOutFilePattern('Find')
        def finder = new Finder(settings)
        def path = Paths.get('./FileUtil.cs')
        def fileResult = new FileResult(path, FileType.CODE)
        assertTrue(finder.isMatchingFileResult(fileResult))
    }

    /*************************************************************
     * isMatchingArchiveFile tests
     *************************************************************/
    @Test
    final void testIsMatchingArchiveFile_NoExtensionsNoPatterns_True() {
        def settings = getSettings()
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertTrue(finder.isMatchingArchiveFile(path))
    }

    @Test
    final void testIsMatchingArchiveFile_MatchesInExtension_True() {
        def settings = getSettings()
        settings.addInArchiveExtension('zip')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertTrue(finder.isMatchingArchiveFile(path))
    }

    @Test
    final void testIsMatchingArchiveFile_DoesNotMatchInExtension_False() {
        def settings = getSettings()
        settings.addInArchiveExtension('gz')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertFalse(finder.isMatchingArchiveFile(path))
    }


    @Test
    final void testIsMatchingArchiveFile_MatchesOutExtension_False() {
        def settings = getSettings()
        settings.addOutArchiveExtension('zip')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertFalse(finder.isMatchingArchiveFile(path))
    }

    @Test
    final void testIsMatchingArchiveFile_DoesNotMatchOutExtension_True() {
        def settings = getSettings()
        settings.addOutArchiveExtension('gz')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertTrue(finder.isMatchingArchiveFile(path))
    }

    @Test
    final void testIsMatchingArchiveFile_MatchesInPattern_True() {
        def settings = getSettings()
        settings.addInArchiveFilePattern('arch')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertTrue(finder.isMatchingArchiveFile(path))
    }

    @Test
    final void testIsMatchingArchiveFile_DoesNotMatchInPattern_False() {
        def settings = getSettings()
        settings.addInArchiveFilePattern('archives')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertFalse(finder.isMatchingArchiveFile(path))
    }

    @Test
    final void testIsMatchingArchiveFile_MatchesOutPattern_False() {
        def settings = getSettings()
        settings.addOutArchiveFilePattern('arch')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertFalse(finder.isMatchingArchiveFile(path))
    }

    @Test
    final void testIsMatchingArchiveFile_DoesNotMatchOutPattern_True() {
        def settings = getSettings()
        settings.addOutArchiveFilePattern('archives')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertTrue(finder.isMatchingArchiveFile(path))
    }

    /*************************************************************
     * filterToFileResult tests
     *************************************************************/
    @Test
    final void testFilterToFileResult_IsHidden_Null() {
        def settings = getSettings()
        def finder = new Finder(settings)
        def path = Paths.get('.gitignore')
        assertFalse(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_IsHiddenIncludeHidden_NotNull() {
        def settings = getSettings()
        settings.setIncludeHidden(true)
        def finder = new Finder(settings)
        def path = Paths.get('.gitignore')
        assertTrue(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_ArchiveNoFindArchives_Null() {
        def settings = getSettings()
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertFalse(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_ArchiveFindArchives_NotNull() {
        def settings = getSettings()
        settings.setIncludeArchives(true)
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertTrue(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_IsMatchingArchiveFile_NotNull() {
        def settings = getSettings()
        settings.setIncludeArchives(true)
        settings.addInArchiveExtension('zip')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertTrue(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_NotIsMatchingArchiveFile_Null() {
        def settings = getSettings()
        settings.setIncludeArchives(true)
        settings.addOutArchiveExtension('zip')
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertFalse(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_ArchiveFileArchivesOnly_NotNull() {
        def settings = getSettings()
        settings.setArchivesOnly(true)
        def finder = new Finder(settings)
        def path = Paths.get('archive.zip')
        assertTrue(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_NoExtensionsNoPatterns_NotNull() {
        def settings = getSettings()
        def finder = new Finder(settings)
        def path = Paths.get('FileUtil.cs')
        assertTrue(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_IsMatchingFile_NotNull() {
        def settings = getSettings()
        settings.addInExtension('cs')
        def finder = new Finder(settings)
        def path = Paths.get('FileUtil.cs')
        assertTrue(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_NotIsMatchingFile_Null() {
        def settings = getSettings()
        settings.addOutExtension('cs')
        def finder = new Finder(settings)
        def path = Paths.get('FileUtil.cs')
        assertFalse(finder.filterToFileResult(path).isPresent())
    }

    @Test
    final void testFilterToFileResult_NonArchiveFileArchivesOnly_Null() {
        def settings = getSettings()
        settings.setArchivesOnly(true)
        def finder = new Finder(settings)
        def path = Paths.get('FileUtil.cs')
        assertFalse(finder.filterToFileResult(path).isPresent())
    }

    /*************************************************************
     * followSymlinks tests
     *************************************************************/
    @Test
    final void testFollowSymlinks_Default_Excluded() {
        var settings = new FindSettings()
        settings.addPath(getBinPath())
        var finder = new Finder(settings)
        try {
            var fileResults = finder.find()
            assertTrue(fileResults.size() < 3)
        } catch (FindException e) {
            fail()
        }
    }

    @Test
    final void testFollowSymlinks_FollowSymlinks_Included() {
        var settings = new FindSettings()
        settings.addPath(getBinPath())
        settings.followSymlinks = true
        var finder = new Finder(settings)
        try {
            var fileResults = finder.find()
            assertTrue(fileResults.isEmpty() || fileResults.size() > 2)
        } catch (FindException ignored) {
            fail()
        }
    }

    @Test
    final void testFollowSymlinks_NoFollowSymlinks_Excluded() {
        var settings = new FindSettings()
        settings.addPath(getBinPath())
        settings.followSymlinks = false
        var finder = new Finder(settings)
        try {
            var fileResults = finder.find()
            assertTrue(fileResults.size() < 3)
        } catch (FindException ignored) {
            fail()
        }
    }
}
