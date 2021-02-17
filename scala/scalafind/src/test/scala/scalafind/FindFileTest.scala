package scalafind

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File

class FindFileTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  test("test_findfile_abs_path") {
    val filepath = "~/src/xfind/scala/scalafind/src/main/scala/scalafind/FindFile.scala"
    val findFile = new FindFile(new File(filepath), FileType.Code)
    assertEquals(filepath, findFile.toString())
  }

  test("test_findfile_rel_path1") {
    val filepath = "./FindFile.scala"
    val findFile = new FindFile(new File(filepath), FileType.Code)
    assertEquals(filepath, findFile.toString())
  }

  test("test_findfile_rel_path2") {
    val filepath = "../FindFile.scala"
    val findFile = new FindFile(new File(filepath), FileType.Code)
    assertEquals(filepath, findFile.toString())
  }
}
