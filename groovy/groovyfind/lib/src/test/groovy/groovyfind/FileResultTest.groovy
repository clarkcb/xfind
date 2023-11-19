package groovyfind

import java.nio.file.Paths

import org.junit.jupiter.api.Test

import static org.junit.jupiter.api.Assertions.*

class FileResultTest {

    FileResultTest() {}

    @Test
    final void testFileResultAbsPath() {
        def dir = '~/src/xfind/groovy/groovyfind/lib/src/main/groovy/groovyfind'
        def fileName = 'FileResult.groovy'
        def path = Paths.get(dir + '/' + fileName)
        def fileResult = new FileResult(path, FileType.CODE)
        def expectedFilePath = dir + '/' + fileName
        assertEquals(expectedFilePath, fileResult.toString())
    }

    @Test
    final void testFileResultRelPath1() {
        def dir = '.'
        def fileName = 'FileResult.groovy'
        def path = Paths.get(dir + '/' + fileName)
        def fileResult = new FileResult(path, FileType.CODE)
        def expectedFilePath = dir + '/' + fileName
        assertEquals(expectedFilePath, fileResult.toString())
    }

    @Test
    final void testFileResultRelPath2() {
        def dir = '..'
        def fileName = 'FileResult.groovy'
        def path = Paths.get(dir + '/' + fileName)
        def fileResult = new FileResult(path, FileType.CODE)
        def expectedFilePath = dir + '/' + fileName
        assertEquals(expectedFilePath, fileResult.toString())
    }

}
