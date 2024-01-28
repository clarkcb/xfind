package scalafind

import org.scalatest.funsuite.AnyFunSuite

class SortByTest extends AnyFunSuite {
  /***************************************************************************
    * fromName tests
    **************************************************************************/
  test("test fromName FILENAME") {
    assert(SortBy.forName("FILENAME").equals(SortBy.FileName))
  }

  test("test fromName filename") {
    assert(SortBy.forName("filename").equals(SortBy.FileName))
  }

  test("test fromName name") {
    assert(SortBy.forName("name").equals(SortBy.FileName))
  }

  test("test fromName FILESIZE") {
    assert(SortBy.forName("FILESIZE").equals(SortBy.FileSize))
  }

  test("test fromName filesize") {
    assert(SortBy.forName("filesize").equals(SortBy.FileSize))
  }

  test("test fromName size") {
    assert(SortBy.forName("size").equals(SortBy.FileSize))
  }

  test("test fromName FILETYPE") {
    assert(SortBy.forName("FILETYPE").equals(SortBy.FileType))
  }

  test("test fromName filetype") {
    assert(SortBy.forName("filetype").equals(SortBy.FileType))
  }

  test("test fromName type") {
    assert(SortBy.forName("type").equals(SortBy.FileType))
  }

  test("test fromName LASTMOD") {
    assert(SortBy.forName("LASTMOD").equals(SortBy.LastMod))
  }

  test("test fromName lastmod") {
    assert(SortBy.forName("lastmod").equals(SortBy.LastMod))
  }

  test("test fromName FILEPATH") {
    assert(SortBy.forName("FILEPATH").equals(SortBy.FilePath))
  }

  test("test fromName filepath") {
    assert(SortBy.forName("filepath").equals(SortBy.FilePath))
  }

  test("test fromName anything") {
    assert(SortBy.forName("anything").equals(SortBy.FilePath))
  }
}
