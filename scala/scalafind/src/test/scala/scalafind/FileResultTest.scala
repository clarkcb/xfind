package scalafind

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.nio.file.Paths


class FileResultTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  test("test_fileresult_abs_path") {
    val filePath = Paths.get("~/src/xfind/scala/scalafind/src/main/scala/scalafind/FileResult.scala")
    val fileResult = new FileResult(filePath, FileType.Code)
    assertEquals(filePath.toString, fileResult.toString)
  }

  test("test_fileresult_rel_path1") {
    val filePath = Paths.get("./FileResult.scala")
    val fileResult = new FileResult(filePath, FileType.Code)
    assertEquals(filePath.toString, fileResult.toString)
  }

  test("test_fileresult_rel_path2") {
    val filePath = Paths.get("../FileResult.scala")
    val fileResult = new FileResult(filePath, FileType.Code)
    assertEquals(filePath.toString, fileResult.toString)
  }
}
