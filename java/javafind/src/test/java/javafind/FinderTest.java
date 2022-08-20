package javafind;

import org.junit.Test;

import java.io.File;

import static org.junit.Assert.*;

public class FinderTest {

    public FinderTest() {}

    private static FindSettings getSettings() {
        FindSettings settings = new FindSettings();
        settings.addPath(".");
        return settings;
    }

    /*************************************************************
     * isMatchingDir tests
     *************************************************************/
    @Test
    public final void testIsMatchingDir_SingleDot_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(new File(".")));
    }

    @Test
    public final void testIsMatchingDir_DoubleDot_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(new File("..")));
    }

    @Test
    public final void testIsMatchingDir_IsHidden_False() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        assertFalse(finder.isMatchingDir(new File(".git")));
    }

    @Test
    public final void testIsMatchingDir_IsHiddenIncludeHidden_True() {
        FindSettings settings = getSettings();
        settings.setExcludeHidden(false);
        Finder finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(new File(".git")));
    }

    @Test
    public final void testIsMatchingDir_NoPatterns_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(new File("/Users")));
    }

    @Test
    public final void testIsMatchingDir_MatchesInPattern_True() {
        FindSettings settings = getSettings();
        settings.addInDirPattern("Find");
        Finder finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(new File("CsFind")));
    }

    @Test
    public final void testIsMatchingDir_MatchesOutPattern_False() {
        FindSettings settings = getSettings();
        settings.addOutDirPattern("Find");
        Finder finder = new Finder(settings);
        assertFalse(finder.isMatchingDir(new File("CsFind")));
    }

    @Test
    public final void testIsMatchingDir_DoesNotMatchInPattern_False() {
        FindSettings settings = getSettings();
        settings.addInDirPattern("FindFiles");
        Finder finder = new Finder(settings);
        assertFalse(finder.isMatchingDir(new File("CsFind")));
    }

    @Test
    public final void testIsMatchingDir_DoesNotMatchOutPattern_True() {
        FindSettings settings = getSettings();
        settings.addOutDirPattern("FindFiles");
        Finder finder = new Finder(settings);
        File dir = new File("CsFind");
        assertTrue(finder.isMatchingDir(dir));
    }

    /*************************************************************
     * isMatchingFile tests
     *************************************************************/
    @Test
    public final void testIsMatchingFile_NoExtensionsNoPatterns_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.isMatchingFile(file));
    }

    @Test
    public final void testIsMatchingFile_MatchesInExtension_True() {
        FindSettings settings = getSettings();
        settings.addInExtension("cs");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.isMatchingFile(file));
    }

    @Test
    public final void testIsMatchingFile_DoesNotMatchInExtension_False() {
        FindSettings settings = getSettings();
        settings.addInExtension("java");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertFalse(finder.isMatchingFile(file));
    }


    @Test
    public final void testIsMatchingFile_MatchesOutExtension_False() {
        FindSettings settings = getSettings();
        settings.addOutExtension("cs");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertFalse(finder.isMatchingFile(file));
    }

    @Test
    public final void testIsMatchingFile_DoesNotMatchOutExtension_True() {
        FindSettings settings = getSettings();
        settings.addOutExtension("java");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.isMatchingFile(file));
    }

    @Test
    public final void testIsMatchingFile_MatchesInPattern_True() {
        FindSettings settings = getSettings();
        settings.addInFilePattern("Find");
        Finder finder = new Finder(settings);
        File file = new File("Finder.cs");
        assertTrue(finder.isMatchingFile(file));
    }

    @Test
    public final void testIsMatchingFile_DoesNotMatchInPattern_False() {
        FindSettings settings = getSettings();
        settings.addInFilePattern("Find");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertFalse(finder.isMatchingFile(file));
    }

    @Test
    public final void testIsMatchingFile_MatchesOutPattern_False() {
        FindSettings settings = getSettings();
        settings.addOutFilePattern("Find");
        Finder finder = new Finder(settings);
        File file = new File("Finder.cs");
        assertFalse(finder.isMatchingFile(file));
    }

    @Test
    public final void testIsMatchingFile_DoesNotMatchOutPattern_True() {
        FindSettings settings = getSettings();
        settings.addOutFilePattern("Find");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.isMatchingFile(file));
    }

    /*************************************************************
     * isMatchingArchiveFile tests
     *************************************************************/
    @Test
    public final void testIsMatchingArchiveFile_NoExtensionsNoPatterns_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(file));
    }

    @Test
    public final void testIsMatchingArchiveFile_MatchesInExtension_True() {
        FindSettings settings = getSettings();
        settings.addInArchiveExtension("zip");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(file));
    }

    @Test
    public final void testIsMatchingArchiveFile_DoesNotMatchInExtension_False() {
        FindSettings settings = getSettings();
        settings.addInArchiveExtension("gz");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.isMatchingArchiveFile(file));
    }


    @Test
    public final void testIsMatchingArchiveFile_MatchesOutExtension_False() {
        FindSettings settings = getSettings();
        settings.addOutArchiveExtension("zip");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.isMatchingArchiveFile(file));
    }

    @Test
    public final void testIsMatchingArchiveFile_DoesNotMatchOutExtension_True() {
        FindSettings settings = getSettings();
        settings.addOutArchiveExtension("gz");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(file));
    }

    @Test
    public final void testIsMatchingArchiveFile_MatchesInPattern_True() {
        FindSettings settings = getSettings();
        settings.addInArchiveFilePattern("arch");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(file));
    }

    @Test
    public final void testIsMatchingArchiveFile_DoesNotMatchInPattern_False() {
        FindSettings settings = getSettings();
        settings.addInArchiveFilePattern("archives");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.isMatchingArchiveFile(file));
    }

    @Test
    public final void testIsMatchingArchiveFile_MatchesOutPattern_False() {
        FindSettings settings = getSettings();
        settings.addOutArchiveFilePattern("arch");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.isMatchingArchiveFile(file));
    }

    @Test
    public final void testIsMatchingArchiveFile_DoesNotMatchOutPattern_True() {
        FindSettings settings = getSettings();
        settings.addOutArchiveFilePattern("archives");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(file));
    }

    /*************************************************************
     * filterToFileResult tests
     *************************************************************/
    @Test
    public final void testFilterToFileResult_IsHidden_Null() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File(".gitignore");
        assertNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_IsHiddenIncludeHidden_NotNull() {
        FindSettings settings = getSettings();
        settings.setExcludeHidden(false);
        Finder finder = new Finder(settings);
        File file = new File(".gitignore");
        assertNotNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_ArchiveNoFindArchives_Null() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_ArchiveFindArchives_NotNull() {
        FindSettings settings = getSettings();
        settings.setIncludeArchives(true);
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertNotNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_IsMatchingArchiveFile_NotNull() {
        FindSettings settings = getSettings();
        settings.setIncludeArchives(true);
        settings.addInArchiveExtension("zip");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertNotNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_NotIsMatchingArchiveFile_Null() {
        FindSettings settings = getSettings();
        settings.setIncludeArchives(true);
        settings.addOutArchiveExtension("zip");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_ArchiveFileArchivesOnly_NotNull() {
        FindSettings settings = getSettings();
        settings.setArchivesOnly(true);
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertNotNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_NoExtensionsNoPatterns_NotNull() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertNotNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_IsMatchingFile_NotNull() {
        FindSettings settings = getSettings();
        settings.addInExtension("cs");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertNotNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_NotIsMatchingFile_Null() {
        FindSettings settings = getSettings();
        settings.addOutExtension("cs");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertNull(finder.filterToFileResult(file));
    }

    @Test
    public final void testFilterToFileResult_NonArchiveFileArchivesOnly_Null() {
        FindSettings settings = getSettings();
        settings.setArchivesOnly(true);
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertNull(finder.filterToFileResult(file));
    }
}
