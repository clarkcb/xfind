package scalafind

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

class FileUtilTest extends AnyFunSuite {
  /***************************************************************************
    * expandPath tests
    **************************************************************************/
  test("test expandPath tilde") {
    val tildePath = Paths.get("~")
    val userPath = Paths.get(System.getProperty("user.home"))
    assert(FileUtil.expandPath(tildePath).equals(userPath))
  }

  test("test expandPath tilde path") {
    val tildePath = Paths.get("~/src/xfind")
    val expandedPath = Paths.get(System.getProperty("user.home"), "src/xfind")
    assert(FileUtil.expandPath(tildePath).equals(expandedPath))
  }

  test("test expandPath tilde name path") {
    val tildePath = Paths.get("~cary/src/xfind")
    val userPath = Paths.get(System.getProperty("user.home"), "src/xfind")
    assert(FileUtil.expandPath(tildePath).equals(userPath))
  }

  /***************************************************************************
    * getExtension tests
    **************************************************************************/
  test("test get txt extension") {
    assert(FileUtil.getExtension(".filename.txt").equalsIgnoreCase("txt"))
  }

  test("test get TXT extension") {
    assert(FileUtil.getExtension(".filename.TXT").equalsIgnoreCase("txt"))
  }

  test("test get missing extension") {
    assert(FileUtil.getExtension("filename.") == "")
  }

  test("test get no extension") {
    assert(FileUtil.getExtension("filename") == "")
  }

  test("test hidden get txt extension") {
    assert(FileUtil.getExtension(".filename.txt").equalsIgnoreCase("txt"))
  }

  test("test hidden get missing extension") {
    assert(FileUtil.getExtension(".filename.") == "")
  }

  test("test hidden get no extension") {
    assert(FileUtil.getExtension(".filename") == "")
  }

  /***************************************************************************
    * isDotDir tests
    **************************************************************************/
  test("test isDotDir single dot") {
    assert(FileUtil.isDotDir("."))
  }

  test("test isDotDir double dot") {
    assert(FileUtil.isDotDir(".."))
  }

  test("test isDotDir hidden file") {
    assert(!FileUtil.isDotDir(".gitignore"))
  }

  /***************************************************************************
    * isHiddenName tests
    **************************************************************************/
  test("test isHiddenName single dot") {
    assert(!FileUtil.isHiddenName("."))
  }

  test("test isHiddenName double dot") {
    assert(!FileUtil.isHiddenName(".."))
  }

  test("test isHiddenName hidden file name") {
    assert(FileUtil.isHiddenName(".gitignore"))
  }

  test("test isHiddenName non-hidden file name") {
    assert(!FileUtil.isHiddenName("filename.txt"))
  }

  test("test isHiddenName non-hidden file path with dot") {
    assert(!FileUtil.isHiddenName("./filename.txt"))
  }

  /***************************************************************************
    * isHiddenPath tests
    **************************************************************************/
  test("test isHiddenPath single dot") {
    assert(!FileUtil.isHiddenPath(Paths.get(".")))
  }

  test("test isHiddenPath double dot") {
    assert(!FileUtil.isHiddenPath(Paths.get("..")))
  }

  test("test isHiddenPath hidden file name") {
    assert(FileUtil.isHiddenPath(Paths.get(".gitignore")))
  }

  test("test isHiddenPath non-hidden file name") {
    assert(!FileUtil.isHiddenPath(Paths.get("filename.txt")))
  }

  test("test isHiddenPath non-hidden file path with dot") {
    assert(!FileUtil.isHiddenPath(Paths.get("./filename.txt")))
  }

  /***************************************************************************
    * splitPath tests
    **************************************************************************/
  test("test splitPath path with dot") {
    val path = "./path/to/somewhere/"
    val elems = FileUtil.splitPath(path)
    assert(elems.size == 3)
    assert(elems.head == "path")
  }

  test("test splitPath path with double dot") {
    val path = "../path/to/somewhere/"
    val elems = FileUtil.splitPath(path)
    assert(elems.size == 3)
    assert(elems.head == "path")
  }

  test("test splitPath path without dot") {
    val path = "/path/to/somewhere/"
    val elems = FileUtil.splitPath(path)
    println("elems:" + elems)
    assert(elems.size == 3)
    assert(elems.head == "path")
  }
}
