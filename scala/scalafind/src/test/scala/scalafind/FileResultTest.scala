package scalafind

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File

class FileResultTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  test("test_fileresult_abs_path") {
    val filepath = "~/src/xfind/scala/scalafind/src/main/scala/scalafind/FileResult.scala"
    val fileResult = new FileResult(new File(filepath), FileType.Code)
    assertEquals(filepath, fileResult.toString())
  }

  test("test_fileresult_rel_path1") {
    val filepath = "./FileResult.scala"
    val fileResult = new FileResult(new File(filepath), FileType.Code)
    assertEquals(filepath, fileResult.toString())
  }

  test("test_fileresult_rel_path2") {
    val filepath = "../FileResult.scala"
    val fileResult = new FileResult(new File(filepath), FileType.Code)
    assertEquals(filepath, fileResult.toString())
  }
}
