package javafind;

import org.junit.Ignore;
import org.junit.Test;

import java.io.File;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import static org.junit.Assert.*;

public class FinderTest {

    public FinderTest() {}

    private static FindSettings getSettings() {
        FindSettings settings = new FindSettings();
        settings.setStartPath(".");
        settings.addFindPattern("Finder");
        return settings;
    }

    private static final String testFilePath = "/testFile2.txt";

    /*************************************************************
     * isFindDir tests
     *************************************************************/
    @Test
    public final void testisFindDir_SingleDot_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        assertTrue(finder.isFindDir(new File(".")));
    }

    @Test
    public final void testisFindDir_DoubleDot_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        assertTrue(finder.isFindDir(new File("..")));
    }

    @Test
    public final void testisFindDir_IsHidden_False() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        assertFalse(finder.isFindDir(new File(".git")));
    }

    @Test
    public final void testisFindDir_IsHiddenIncludeHidden_True() {
        FindSettings settings = getSettings();
        settings.setExcludeHidden(false);
        Finder finder = new Finder(settings);
        assertTrue(finder.isFindDir(new File(".git")));
    }

    @Test
    public final void testisFindDir_NoPatterns_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        assertTrue(finder.isFindDir(new File("/Users")));
    }

    @Test
    public final void testisFindDir_MatchesInPattern_True() {
        FindSettings settings = getSettings();
        settings.addInDirPattern("Find");
        Finder finder = new Finder(settings);
        assertTrue(finder.isFindDir(new File("CsFind")));
    }

    @Test
    public final void testisFindDir_MatchesOutPattern_False() {
        FindSettings settings = getSettings();
        settings.addOutDirPattern("Find");
        Finder finder = new Finder(settings);
        assertFalse(finder.isFindDir(new File("CsFind")));
    }

    @Test
    public final void testisFindDir_DoesNotMatchInPattern_False() {
        FindSettings settings = getSettings();
        settings.addInDirPattern("FindFiles");
        Finder finder = new Finder(settings);
        assertFalse(finder.isFindDir(new File("CsFind")));
    }

    @Test
    public final void testisFindDir_DoesNotMatchOutPattern_True() {
        FindSettings settings = getSettings();
        settings.addOutDirPattern("FindFiles");
        Finder finder = new Finder(settings);
        File dir = new File("CsFind");
        assertTrue(finder.isFindDir(dir));
    }


    /*************************************************************
     * isFindFile tests
     *************************************************************/

    @Test
    public final void testIsFindFile_NoExtensionsNoPatterns_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.isFindFile(file));
    }

    @Test
    public final void testIsFindFile_MatchesInExtension_True() {
        FindSettings settings = getSettings();
        settings.addInExtension("cs");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.isFindFile(file));
    }

    @Test
    public final void testIsFindFile_DoesNotMatchInExtension_False() {
        FindSettings settings = getSettings();
        settings.addInExtension("java");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertFalse(finder.isFindFile(file));
    }


    @Test
    public final void testIsFindFile_MatchesOutExtension_False() {
        FindSettings settings = getSettings();
        settings.addOutExtension("cs");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertFalse(finder.isFindFile(file));
    }

    @Test
    public final void testIsFindFile_DoesNotMatchOutExtension_True() {
        FindSettings settings = getSettings();
        settings.addOutExtension("java");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.isFindFile(file));
    }

    @Test
    public final void testIsFindFile_MatchesInPattern_True() {
        FindSettings settings = getSettings();
        settings.addInFilePattern("Find");
        Finder finder = new Finder(settings);
        File file = new File("Finder.cs");
        assertTrue(finder.isFindFile(file));
    }

    @Test
    public final void testIsFindFile_DoesNotMatchInPattern_False() {
        FindSettings settings = getSettings();
        settings.addInFilePattern("Find");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertFalse(finder.isFindFile(file));
    }

    @Test
    public final void testIsFindFile_MatchesOutPattern_False() {
        FindSettings settings = getSettings();
        settings.addOutFilePattern("Find");
        Finder finder = new Finder(settings);
        File file = new File("Finder.cs");
        assertFalse(finder.isFindFile(file));
    }

    @Test
    public final void testIsFindFile_DoesNotMatchOutPattern_True() {
        FindSettings settings = getSettings();
        settings.addOutFilePattern("Find");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.isFindFile(file));
    }


    /*************************************************************
     * IsArchiveFindFile tests
     *************************************************************/

    @Test
    public final void testIsArchiveFindFile_NoExtensionsNoPatterns_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isArchiveFindFile(file));
    }

    @Test
    public final void testIsArchiveFindFile_MatchesInExtension_True() {
        FindSettings settings = getSettings();
        settings.addInArchiveExtension("zip");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isArchiveFindFile(file));
    }

    @Test
    public final void testIsArchiveFindFile_DoesNotMatchInExtension_False() {
        FindSettings settings = getSettings();
        settings.addInArchiveExtension("gz");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.isArchiveFindFile(file));
    }


    @Test
    public final void testIsArchiveFindFile_MatchesOutExtension_False() {
        FindSettings settings = getSettings();
        settings.addOutArchiveExtension("zip");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.isArchiveFindFile(file));
    }

    @Test
    public final void testIsArchiveFindFile_DoesNotMatchOutExtension_True() {
        FindSettings settings = getSettings();
        settings.addOutArchiveExtension("gz");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isArchiveFindFile(file));
    }

    @Test
    public final void testIsArchiveFindFile_MatchesInPattern_True() {
        FindSettings settings = getSettings();
        settings.addInArchiveFilePattern("arch");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isArchiveFindFile(file));
    }

    @Test
    public final void testIsArchiveFindFile_DoesNotMatchInPattern_False() {
        FindSettings settings = getSettings();
        settings.addInArchiveFilePattern("archives");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.isArchiveFindFile(file));
    }

    @Test
    public final void testIsArchiveFindFile_MatchesOutPattern_False() {
        FindSettings settings = getSettings();
        settings.addOutArchiveFilePattern("arch");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.isArchiveFindFile(file));
    }

    @Test
    public final void testIsArchiveFindFile_DoesNotMatchOutPattern_True() {
        FindSettings settings = getSettings();
        settings.addOutArchiveFilePattern("archives");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.isArchiveFindFile(file));
    }

    /*************************************************************
     * FilterFile tests
     *************************************************************/

    @Test
    public final void testFilterFile_IsHidden_False() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File(".gitignore");
        assertFalse(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_IsHiddenIncludeHidden_True() {
        FindSettings settings = getSettings();
        settings.setExcludeHidden(false);
        Finder finder = new Finder(settings);
        File file = new File(".gitignore");
        assertTrue(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_ArchiveNoFindArchives_False() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_ArchiveFindArchives_True() {
        FindSettings settings = getSettings();
        settings.setFindArchives(true);
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_IsArchiveFindFile_True() {
        FindSettings settings = getSettings();
        settings.setFindArchives(true);
        settings.addInArchiveExtension("zip");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_NotIsArchiveFindFile_False() {
        FindSettings settings = getSettings();
        settings.setFindArchives(true);
        settings.addOutArchiveExtension("zip");
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertFalse(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_ArchiveFileArchivesOnly_True() {
        FindSettings settings = getSettings();
        settings.setArchivesOnly(true);
        Finder finder = new Finder(settings);
        File file = new File("archive.zip");
        assertTrue(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_NoExtensionsNoPatterns_True() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_IsFindFile_True() {
        FindSettings settings = getSettings();
        settings.addInExtension("cs");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertTrue(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_NotIsFindFile_False() {
        FindSettings settings = getSettings();
        settings.addOutExtension("cs");
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertFalse(finder.filterFile(file));
    }

    @Test
    public final void testFilterFile_NonArchiveFileArchivesOnly_False() {
        FindSettings settings = getSettings();
        settings.setArchivesOnly(true);
        Finder finder = new Finder(settings);
        File file = new File("FileUtil.cs");
        assertFalse(finder.filterFile(file));
    }

    /*************************************************************
     * findStringIterator test
     *************************************************************/
    @Test
    public final void testFindStringIterator() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        Iterator<String> lineIterator;
        try {
            InputStream is = getClass().getResourceAsStream(testFilePath);
            List<String> lines = FileUtil.getStreamLines(is);
            lineIterator = lines.iterator();
            List<FindResult> results = finder.findStringIterator(lineIterator);

            assertEquals(results.size(), 2);

            FindResult firstResult = results.get(0);
            int expectedFirstLineNum = 29;
            assertEquals(firstResult.getLineNum(), expectedFirstLineNum);
            int expectedFirstMatchStartIndex = 3;
            assertEquals(firstResult.getMatchStartIndex(), expectedFirstMatchStartIndex);
            int expectedFirstMatchEndIndex = 11;
            assertEquals(firstResult.getMatchEndIndex(), expectedFirstMatchEndIndex);

            FindResult secondResult = results.get(1);
            int expectedSecondLineNum = 35;
            assertEquals(secondResult.getLineNum(), expectedSecondLineNum);
            int expectedSecondMatchStartIndex = 24;
            assertEquals(secondResult.getMatchStartIndex(), expectedSecondMatchStartIndex);
            int expectedSecondMatchEndIndex = 32;
            assertEquals(secondResult.getMatchEndIndex(), expectedSecondMatchEndIndex);

        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    /*************************************************************
     * findMultiLineString tests
     *************************************************************/
    @Test
    public final void testFindMultiLineString() {
        FindSettings settings = getSettings();
        Finder finder = new Finder(settings);
        String contents;
        try {
            InputStream is = getClass().getResourceAsStream(testFilePath);
            contents = FileUtil.getStreamContents(is);
            //System.out.println("contents: " + contents);
            List<FindResult> results = finder.findMultiLineString(contents);

            assert(results.size() == 2);

            FindResult firstResult = results.get(0);
            int expectedFirstLineNum = 29;
            assertEquals(firstResult.getLineNum(), expectedFirstLineNum);
            int expectedFirstMatchStartIndex = 3;
            assertEquals(firstResult.getMatchStartIndex(), expectedFirstMatchStartIndex);
            int expectedFirstMatchEndIndex = 11;
            assertEquals(firstResult.getMatchEndIndex(), expectedFirstMatchEndIndex);

            FindResult secondResult = results.get(1);
            int expectedSecondLineNum = 35;
            assertEquals(secondResult.getLineNum(), expectedSecondLineNum);
            int expectedSecondMatchStartIndex = 24;
            assertEquals(secondResult.getMatchStartIndex(), expectedSecondMatchStartIndex);
            int expectedSecondMatchEndIndex = 32;
            assertEquals(secondResult.getMatchEndIndex(), expectedSecondMatchEndIndex);

        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public final void testFindMultiLineStringWithLinesBefore() {
        FindSettings settings = getSettings();
        settings.setLinesBefore(2);
        Finder finder = new Finder(settings);
        String contents;
        try {
            InputStream is = getClass().getResourceAsStream(testFilePath);
            contents = FileUtil.getStreamContents(is);
            //System.out.println("contents: " + contents);
            List<FindResult> results = finder.findMultiLineString(contents);

            assertEquals(results.size(), 2);

            FindResult firstResult = results.get(0);
            System.out.println("firstResult:\n" + firstResult);
            int expectedFirstLineNum = 29;
            assertEquals(firstResult.getLineNum(), expectedFirstLineNum);
            int expectedFirstMatchStartIndex = 3;
            assertEquals(firstResult.getMatchStartIndex(), expectedFirstMatchStartIndex);
            int expectedFirstMatchEndIndex = 11;
            assertEquals(firstResult.getMatchEndIndex(), expectedFirstMatchEndIndex);

            FindResult secondResult = results.get(1);
            System.out.println("secondResult:\n" + secondResult);
            int expectedSecondLineNum = 35;
            assertEquals(secondResult.getLineNum(), expectedSecondLineNum);
            int expectedSecondMatchStartIndex = 24;
            assertEquals(secondResult.getMatchStartIndex(), expectedSecondMatchStartIndex);
            int expectedSecondMatchEndIndex = 32;
            assertEquals(secondResult.getMatchEndIndex(), expectedSecondMatchEndIndex);

        } catch (IllegalArgumentException e) {
            fail();
        }
    }
}
