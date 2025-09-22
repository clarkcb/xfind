/*******************************************************************************
FileUtil

Utility class to determine file types, etc.

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public final class FileUtil {

    private FileUtil() {
        // inaccessible constructor for utility class
    }

    private static final String DOT = ".";
    private static final String DOT_DOT = "..";
    private static final Set<String> DOT_DIRS = new HashSet<>(Arrays.asList(DOT, DOT_DOT));
    private static final String DEFAULT_ENCODING = "UTF-8";
    private static final String TILDE = "~";
    private static final String Z_EXT = "Z";

    public static Path expandPath(Path path) {
        if (path == null) {
            throw new IllegalArgumentException("Path cannot be null");
        }

        var pathString = path.toString();
        if (pathString.startsWith(TILDE)) {
            var userPath = Paths.get(System.getProperty("user.home"));
            if (pathString.equals(TILDE) || pathString.equals(TILDE + File.separator)) {
                return userPath;
            } else if (pathString.startsWith(TILDE + File.separator)) {
                return Paths.get(userPath.toString(), pathString.substring(2));
            }
            // Another user's home directory
            var homePath = userPath.getParent();
            return Paths.get(homePath.toString(), pathString.substring(1));
        }

        // Return the path as is if it doesn't contain a tilde
        return path;
    }

    public static String getExtension(final Path path) {
        return getExtension(path.getFileName().toString());
    }

    public static String getExtension(final String fileName) {
        var ext = "";
        var lastIndex = fileName.lastIndexOf(DOT);
        if (lastIndex > 0 && lastIndex < fileName.length() - 1) {
            ext = fileName.substring(lastIndex + 1);
            if (!ext.equals(Z_EXT)) { // the only always-uppercase ext
                ext = ext.toLowerCase();
            }
        }
        return ext;
    }

    public static boolean hasExtension(final String fileName, String ext) {
        if (!ext.equals(Z_EXT)) { // the only always-uppercase ext
            ext = ext.toLowerCase();
        }
        return getExtension(fileName).equals(ext);
    }

    public static boolean isDotDir(final String name) {
        return DOT_DIRS.contains(name);
    }

    // NOTE: if the first item in the returned list is not a dotDir, it should be
    // considered an absolute path
    public static List<String> splitPath(final Path path) {
        if (path == null || path.toString().isEmpty()) {
            return Collections.emptyList();
        }
        List<String> elemList = new ArrayList<>();
        for (var p : path) {
            elemList.add(p.toString());
        }
        return elemList;
    }

    public static List<String> splitPath(final String path) {
        if (path == null || path.isEmpty()) {
            return Collections.emptyList();
        }
        return splitPath(Paths.get(path));
    }

    public static boolean isHiddenName(final String name) {
        return name.length() > 1 && name.startsWith(DOT) && !isDotDir(name);
    }

    public static boolean isHiddenPath(final Path path) {
        var elems = splitPath(path);
        for (var elem : elems) {
            if (isHiddenName(elem)) {
                return true;
            }
        }
        return false;
    }

    public static String getFileContents(final Path path) throws IOException {
        return getFileContents(path, DEFAULT_ENCODING);
    }

    public static String getFileContents(final Path path, final String enc) throws IOException {
        return getFileContents(path, Charset.forName(enc));
    }

    public static String getFileContents(final Path path, final Charset charset) throws IOException {
        return Files.readString(path, charset);
    }

    public static String getScannerContents(final Scanner scanner) throws IllegalArgumentException {
        String content;
        if (scanner.hasNext()) { // will be false if file size == 0
            content = scanner.next();
        } else {
            content = "";
        }
        return content;
    }

    public static String getStreamContents(final InputStream is) throws IllegalArgumentException {
        return getStreamContents(is, DEFAULT_ENCODING);
    }

    public static String getStreamContents(final InputStream is, final String enc) throws IllegalArgumentException {
        String content;
        try (Scanner scanner = new Scanner(is, enc).useDelimiter("\\Z")) {
            content = getScannerContents(scanner);
        } catch (NoSuchElementException | IllegalStateException e) {
            throw e;
        }
        return content;
    }

    // NOTE: user takes responsibility for closing the LineIterator once done
    public static LineIterator getFileLineIterator(final Path path) throws IOException {
        return getFileLineIterator(path, DEFAULT_ENCODING);
    }

    // NOTE: user takes responsibility for closing the LineIterator once done
    public static LineIterator getFileLineIterator(final Path path, final String enc) throws IOException {
        return FileUtils.lineIterator(path.toFile(), enc);
    }

    public static List<String> getStreamLines(final InputStream is) throws IllegalArgumentException {
        return getStreamLines(is, DEFAULT_ENCODING);
    }

    public static List<String> getStreamLines(final InputStream is, final String enc) throws IllegalArgumentException {
        var lines = new ArrayList<String>();
        var scanner = new Scanner(is, enc).useDelimiter("\r?\n");
        while (scanner.hasNext()) {
            try {
                lines.add(scanner.next());
            } catch (NoSuchElementException e) {
                break;
            }
        }
        scanner.close();
        return lines;
    }
}
