package groovyfind

import groovy.transform.CompileStatic

import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

@CompileStatic
final class FileUtil {

    private FileUtil() {
        // inaccessible constructor for utility class
    }

    private static final String DOT = '.'
    private static final String DOT_DOT = '..'
    private static final Set<String> DOT_DIRS = new HashSet<>(Arrays.asList(DOT, DOT_DOT))
    private static final String DEFAULT_ENCODING = 'UTF-8'
    private static final String Z_EXT = 'Z'

    static String getExtension(final Path path) {
        return getExtension(path.fileName.toString())
    }

    static String getExtension(final String fileName) {
        def ext = ''
        def lastIndex = fileName.lastIndexOf(DOT)
        if (lastIndex > 0 && lastIndex < fileName.length() - 1) {
            ext = fileName.substring(lastIndex + 1)
            if (ext != Z_EXT) { // the only always-uppercase ext
                ext = ext.toLowerCase()
            }
        }
        return ext
    }

    static boolean hasExtension(final String fileName, String ext) {
        if (ext != Z_EXT) { // the only always-uppercase ext
            ext = ext.toLowerCase()
        }
        return getExtension(fileName) == ext
    }

    static boolean isDotDir(final String f) {
        return DOT_DIRS.contains(f)
    }

    static boolean isHidden(final Path path) {
        return isHidden(path.fileName.toString())
    }

    static boolean isHidden(final String f) {
        return f.length() > 1 && f.take(1) == DOT && !isDotDir(f)
    }

    // NOTE: if the first item in the returned list is not a dotDir, it should be
    // considered an absolute path
    static List<String> splitPath(final Path path) {
        if (path == null || path.toString().empty) {
            return Collections.emptyList()
        }
        List<String> elemList = []
        path.each { p ->
            elemList.add(p.toString())
        }
        return elemList
    }

    static List<String> splitPath(final String path) {
        if (path == null || path.empty) {
            return Collections.emptyList()
        }
        return splitPath(Paths.get(path))
    }

    static String getFileContents(final Path path) throws IOException {
        return getFileContents(path, DEFAULT_ENCODING)
    }

    static String getFileContents(final Path path, final String enc) throws IOException {
        return getFileContents(path, Charset.forName(enc))
    }

    static String getFileContents(final Path path, final Charset charset) throws IOException {
        String content
        try (def scanner = new Scanner(new InputStreamReader(Files.newInputStream(path), charset))
                .useDelimiter('\\Z')) {
            content = getScannerContents(scanner)
        } catch (NoSuchElementException | IllegalStateException e) {
            throw e
        }
        return content
    }

    static String getScannerContents(final Scanner scanner) throws IllegalArgumentException {
        String content
        if (scanner.hasNext()) { // will be false if file size == 0
            content = scanner.next()
        } else {
            content = ''
        }
        return content
    }

    static long getSepCount(final Path path) {
        return path.toString().chars().filter(c -> c == File.separatorChar).count()
    }
}
