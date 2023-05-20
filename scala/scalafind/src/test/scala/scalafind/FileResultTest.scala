package scalafind

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File
import java.nio.file.{Files, Path, Paths}


class FileResultTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  test("test_fileresult_abs_path") {
    val filePath = "~/src/xfind/scala/scalafind/src/main/scala/scalafind/FileResult.scala"
    val fileResult = new FileResult(Paths.get(filePath), FileType.Code)
    assertEquals(filePath, fileResult.toString())
  }

  test("test_fileresult_rel_path1") {
    val filePath = "./FileResult.scala"
    val fileResult = new FileResult(Paths.get(filePath), FileType.Code)
    assertEquals(filePath, fileResult.toString())
  }

  test("test_fileresult_rel_path2") {
    val filePath = "../FileResult.scala"
    val fileResult = new FileResult(Paths.get(filePath), FileType.Code)
    assertEquals(filePath, fileResult.toString())
  }
}
