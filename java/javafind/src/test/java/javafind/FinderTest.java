package javafind;

import org.junit.jupiter.api.Test;

import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

public class FinderTest {

    public FinderTest() {}

    private static FindSettings getSettings() {
        var settings = new FindSettings();
        settings.addPath(".");
        return settings;
    }

    /*************************************************************
     * isMatchingDir tests
     *************************************************************/
    @Test
    public final void testIsMatchingDir_SingleDot_True() {
        var settings = getSettings();
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(Paths.get(".")));
    }

    @Test
    public final void testIsMatchingDir_DoubleDot_True() {
        var settings = getSettings();
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(Paths.get("..")));
    }

    @Test
    public final void testIsMatchingDir_IsHidden_False() {
        var settings = getSettings();
        var finder = new Finder(settings);
        assertFalse(finder.isMatchingDir(Paths.get(".git")));
    }

    @Test
    public final void testIsMatchingDir_IsHiddenIncludeHidden_True() {
        var settings = getSettings();
        settings.setIncludeHidden(true);
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(Paths.get(".git")));
    }

    @Test
    public final void testIsMatchingDir_NoPatterns_True() {
        var settings = getSettings();
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(Paths.get("/Users")));
    }

    @Test
    public final void testIsMatchingDir_MatchesInPattern_True() {
        var settings = getSettings();
        settings.addInDirPattern("Find");
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDir(Paths.get("CsFind")));
    }

    @Test
    public final void testIsMatchingDir_MatchesOutPattern_False() {
        var settings = getSettings();
        settings.addOutDirPattern("Find");
        var finder = new Finder(settings);
        assertFalse(finder.isMatchingDir(Paths.get("CsFind")));
    }

    @Test
    public final void testIsMatchingDir_DoesNotMatchInPattern_False() {
        var settings = getSettings();
        settings.addInDirPattern("FindFiles");
        var finder = new Finder(settings);
        assertFalse(finder.isMatchingDir(Paths.get("CsFind")));
    }

    @Test
    public final void testIsMatchingDir_DoesNotMatchOutPattern_True() {
        var settings = getSettings();
        settings.addOutDirPattern("FindFiles");
        var finder = new Finder(settings);
        var dir = Paths.get("CsFind");
        assertTrue(finder.isMatchingDir(dir));
    }

    @Test
    public final void testIsMatchingDir_DoesNotMatchOutPattern2_True() {
        var settings = getSettings();
        settings.addOutDirPattern("FindFiles");
        var finder = new Finder(settings);
        var dir = Paths.get("/Users/cary/src/xfind/java/javafind/ssrc/main/java/javafind");
        assertTrue(finder.isMatchingDir(dir));
    }

    /*************************************************************
     * isMatchingFileResult tests
     *************************************************************/
    @Test
    public final void testIsMatchingFileResult_NoExtensionsNoPatterns_True() {
        var settings = getSettings();
        var finder = new Finder(settings);
        var path = Paths.get("./FileUtil.cs");
        var fileResult = new FileResult(path, FileType.CODE);
        assertTrue(finder.isMatchingFileResult(fileResult));
    }

    @Test
    public final void testIsMatchingFileResult_MatchesInExtension_True() {
        var settings = getSettings();
        settings.addInExtension("cs");
        var finder = new Finder(settings);
        var path = Paths.get("./FileUtil.cs");
        var fileResult = new FileResult(path, FileType.CODE);
        assertTrue(finder.isMatchingFileResult(fileResult));
    }

    @Test
    public final void testIsMatchingFileResult_DoesNotMatchInExtension_False() {
        var settings = getSettings();
        settings.addInExtension("java");
        var finder = new Finder(settings);
        var path = Paths.get("./FileUtil.cs");
        var fileResult = new FileResult(path, FileType.CODE);
        assertFalse(finder.isMatchingFileResult(fileResult));
    }


    @Test
    public final void testIsMatchingFileResult_MatchesOutExtension_False() {
        var settings = getSettings();
        settings.addOutExtension("cs");
        var finder = new Finder(settings);
        var path = Paths.get("./FileUtil.cs");
        var fileResult = new FileResult(path, FileType.CODE);
        assertFalse(finder.isMatchingFileResult(fileResult));
    }

    @Test
    public final void testIsMatchingFileResult_DoesNotMatchOutExtension_True() {
        var settings = getSettings();
        settings.addOutExtension("java");
        var finder = new Finder(settings);
        var path = Paths.get("./FileUtil.cs");
        var fileResult = new FileResult(path, FileType.CODE);
        assertTrue(finder.isMatchingFileResult(fileResult));
    }

    @Test
    public final void testIsMatchingFileResult_MatchesInPattern_True() {
        var settings = getSettings();
        settings.addInFilePattern("Find");
        var finder = new Finder(settings);
        var path = Paths.get("./Finder.cs");
        var fileResult = new FileResult(path, FileType.CODE);
        assertTrue(finder.isMatchingFileResult(fileResult));
    }

    @Test
    public final void testIsMatchingFileResult_DoesNotMatchInPattern_False() {
        var settings = getSettings();
        settings.addInFilePattern("Find");
        var finder = new Finder(settings);
        var path = Paths.get("./FileUtil.cs");
        var fileResult = new FileResult(path, FileType.CODE);
        assertFalse(finder.isMatchingFileResult(fileResult));
    }

    @Test
    public final void testIsMatchingFileResult_MatchesOutPattern_False() {
        var settings = getSettings();
        settings.addOutFilePattern("Find");
        var finder = new Finder(settings);
        var path = Paths.get("./Finder.cs");
        var fileResult = new FileResult(path, FileType.CODE);
        assertFalse(finder.isMatchingFileResult(fileResult));
    }

    @Test
    public final void testIsMatchingFileResult_DoesNotMatchOutPattern_True() {
        var settings = getSettings();
        settings.addOutFilePattern("Find");
        var finder = new Finder(settings);
        var path = Paths.get("./FileUtil.cs");
        var fileResult = new FileResult(path, FileType.CODE);
        assertTrue(finder.isMatchingFileResult(fileResult));
    }

    /*************************************************************
     * isMatchingArchiveFile tests
     *************************************************************/
    @Test
    public final void testIsMatchingArchiveFile_NoExtensionsNoPatterns_True() {
        var settings = getSettings();
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(path));
    }

    @Test
    public final void testIsMatchingArchiveFile_MatchesInExtension_True() {
        var settings = getSettings();
        settings.addInArchiveExtension("zip");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(path));
    }

    @Test
    public final void testIsMatchingArchiveFile_DoesNotMatchInExtension_False() {
        var settings = getSettings();
        settings.addInArchiveExtension("gz");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertFalse(finder.isMatchingArchiveFile(path));
    }


    @Test
    public final void testIsMatchingArchiveFile_MatchesOutExtension_False() {
        var settings = getSettings();
        settings.addOutArchiveExtension("zip");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertFalse(finder.isMatchingArchiveFile(path));
    }

    @Test
    public final void testIsMatchingArchiveFile_DoesNotMatchOutExtension_True() {
        var settings = getSettings();
        settings.addOutArchiveExtension("gz");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(path));
    }

    @Test
    public final void testIsMatchingArchiveFile_MatchesInPattern_True() {
        var settings = getSettings();
        settings.addInArchiveFilePattern("arch");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(path));
    }

    @Test
    public final void testIsMatchingArchiveFile_DoesNotMatchInPattern_False() {
        var settings = getSettings();
        settings.addInArchiveFilePattern("archives");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertFalse(finder.isMatchingArchiveFile(path));
    }

    @Test
    public final void testIsMatchingArchiveFile_MatchesOutPattern_False() {
        var settings = getSettings();
        settings.addOutArchiveFilePattern("arch");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertFalse(finder.isMatchingArchiveFile(path));
    }

    @Test
    public final void testIsMatchingArchiveFile_DoesNotMatchOutPattern_True() {
        var settings = getSettings();
        settings.addOutArchiveFilePattern("archives");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertTrue(finder.isMatchingArchiveFile(path));
    }

    /*************************************************************
     * filterToFileResult tests
     *************************************************************/
    @Test
    public final void testFilterToFileResult_IsHidden_Null() {
        var settings = getSettings();
        var finder = new Finder(settings);
        var path = Paths.get(".gitignore");
        assertFalse(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_IsHiddenIncludeHidden_NotNull() {
        var settings = getSettings();
        settings.setIncludeHidden(true);
        var finder = new Finder(settings);
        var path = Paths.get(".gitignore");
        assertTrue(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_ArchiveNoFindArchives_Null() {
        var settings = getSettings();
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertFalse(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_ArchiveFindArchives_NotNull() {
        var settings = getSettings();
        settings.setIncludeArchives(true);
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertTrue(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_IsMatchingArchiveFile_NotNull() {
        var settings = getSettings();
        settings.setIncludeArchives(true);
        settings.addInArchiveExtension("zip");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertTrue(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_NotIsMatchingArchiveFile_Null() {
        var settings = getSettings();
        settings.setIncludeArchives(true);
        settings.addOutArchiveExtension("zip");
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertFalse(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_ArchiveFileArchivesOnly_NotNull() {
        var settings = getSettings();
        settings.setArchivesOnly(true);
        var finder = new Finder(settings);
        var path = Paths.get("archive.zip");
        assertTrue(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_NoExtensionsNoPatterns_NotNull() {
        var settings = getSettings();
        var finder = new Finder(settings);
        var path = Paths.get("FileUtil.cs");
        assertTrue(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_IsMatchingFile_NotNull() {
        var settings = getSettings();
        settings.addInExtension("cs");
        var finder = new Finder(settings);
        var path = Paths.get("FileUtil.cs");
        assertTrue(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_NotIsMatchingFile_Null() {
        var settings = getSettings();
        settings.addOutExtension("cs");
        var finder = new Finder(settings);
        var path = Paths.get("FileUtil.cs");
        assertFalse(finder.filterToFileResult(path).isPresent());
    }

    @Test
    public final void testFilterToFileResult_NonArchiveFileArchivesOnly_Null() {
        var settings = getSettings();
        settings.setArchivesOnly(true);
        var finder = new Finder(settings);
        var path = Paths.get("FileUtil.cs");
        assertFalse(finder.filterToFileResult(path).isPresent());
    }
}
