package javafind;

import org.junit.jupiter.api.Test;

import java.nio.file.Paths;
import java.time.Instant;
import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.*;

public class FinderTest {

    public FinderTest() {}

    private static FindSettings getSettings() {
        var settings = new FindSettings();
        settings.addPath(".");
        return settings;
    }

    private static String getBinPath() {
        var xfindPath = System.getenv("XFIND_PATH");
        if (xfindPath == null) {
            xfindPath = System.getenv("HOME") + "src/xfind";
        }
        return xfindPath + "/bin";
    }

    /*************************************************************
     * isMatchingDirPath tests
     *************************************************************/
    @Test
    public final void testIsMatchingDirPath_SingleDot_True() {
        var settings = getSettings();
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDirPath(Paths.get(".")));
    }

    @Test
    public final void testIsMatchingDirPath_DoubleDot_True() {
        var settings = getSettings();
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDirPath(Paths.get("..")));
    }

    @Test
    public final void testIsMatchingDirPath_IsHidden_False() {
        var settings = getSettings();
        var finder = new Finder(settings);
        assertFalse(finder.isMatchingDirPath(Paths.get(".git")));
    }

    @Test
    public final void testIsMatchingDirPath_IsHiddenIncludeHidden_True() {
        var settings = getSettings();
        settings.setIncludeHidden(true);
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDirPath(Paths.get(".git")));
    }

    @Test
    public final void testIsMatchingDirPath_NoPatterns_True() {
        var settings = getSettings();
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDirPath(Paths.get("/Users")));
    }

    @Test
    public final void testIsMatchingDirPath_MatchesInPattern_True() {
        var settings = getSettings();
        settings.addInDirPattern("Find");
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingDirPath(Paths.get("CsFind")));
    }

    @Test
    public final void testIsMatchingDirPath_MatchesOutPattern_False() {
        var settings = getSettings();
        settings.addOutDirPattern("Find");
        var finder = new Finder(settings);
        assertFalse(finder.isMatchingDirPath(Paths.get("CsFind")));
    }

    @Test
    public final void testIsMatchingDirPath_DoesNotMatchInPattern_False() {
        var settings = getSettings();
        settings.addInDirPattern("FindFiles");
        var finder = new Finder(settings);
        assertFalse(finder.isMatchingDirPath(Paths.get("CsFind")));
    }

    @Test
    public final void testIsMatchingDirPath_DoesNotMatchOutPattern_True() {
        var settings = getSettings();
        settings.addOutDirPattern("FindFiles");
        var finder = new Finder(settings);
        var dir = Paths.get("CsFind");
        assertTrue(finder.isMatchingDirPath(dir));
    }

    @Test
    public final void testIsMatchingDirPath_DoesNotMatchOutPattern2_True() {
        var settings = getSettings();
        settings.addOutDirPattern("FindFiles");
        var finder = new Finder(settings);
        var dir = Paths.get("/Users/cary/src/xfind/java/javafind/ssrc/main/java/javafind");
        assertTrue(finder.isMatchingDirPath(dir));
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

    /*************************************************************
     * followSymlinks tests
     *************************************************************/
    @Test
    public final void testFollowSymlinks_Default_Excluded() {
        var settings = new FindSettings();
        settings.addPath(getBinPath());
        var finder = new Finder(settings);
        try {
            var fileResults = finder.find();
            assertTrue(fileResults.size() < 4);
        } catch (FindException e) {
            fail();
        }
    }

    @Test
    public final void testFollowSymlinks_FollowSymlinks_Included() {
        var settings = new FindSettings();
        settings.addPath(getBinPath());
        settings.setFollowSymlinks(true);
        var finder = new Finder(settings);
        try {
            var fileResults = finder.find();
            assertTrue(fileResults.isEmpty() || fileResults.size() > 2);
        } catch (FindException e) {
            fail();
        }
    }

    @Test
    public final void testFollowSymlinks_NoFollowSymlinks_Excluded() {
        var settings = new FindSettings();
        settings.addPath(getBinPath());
        settings.setFollowSymlinks(false);
        var finder = new Finder(settings);
        try {
            var fileResults = finder.find();
            assertTrue(fileResults.size() < 4);
        } catch (FindException e) {
            fail();
        }
    }

    /*************************************************************
     * isMatchingLastMod tests
     *************************************************************/
    @Test
    public final void testIsMatchingLastMod_MinOnly_BeforeMin_False() {
        var settings = getSettings();
        settings.setMinLastMod(LocalDateTime.parse("2024-01-01T00:00:00"));
        var finder = new Finder(settings);
        assertFalse(finder.isMatchingLastMod(Instant.parse("2023-12-31T23:59:59Z")));
    }

    @Test
    public final void testIsMatchingLastMod_MinOnly_AtOrAfterMin_True() {
        var settings = getSettings();
        settings.setMinLastMod(LocalDateTime.parse("2024-01-01T00:00:00"));
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingLastMod(Instant.parse("2024-01-01T00:00:00Z")));
    }

    @Test
    public final void testIsMatchingLastMod_MaxOnly_AfterMax_False() {
        var settings = getSettings();
        settings.setMaxLastMod(LocalDateTime.parse("2024-01-01T00:00:00"));
        var finder = new Finder(settings);
        assertFalse(finder.isMatchingLastMod(Instant.parse("2024-01-01T00:00:01Z")));
    }

    @Test
    public final void testIsMatchingLastMod_MaxOnly_AtOrBeforeMax_True() {
        var settings = getSettings();
        settings.setMaxLastMod(LocalDateTime.parse("2024-01-01T00:00:00"));
        var finder = new Finder(settings);
        assertTrue(finder.isMatchingLastMod(Instant.parse("2024-01-01T00:00:00Z")));
    }
}
