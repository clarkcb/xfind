package scalafind

import java.nio.file.{Path, Paths}

object FindConfig {
//  val HOME: String = System.getProperty("user.home")
  val XFIND_PATH: Path =  Option(System.getenv("XFIND_PATH")) match {
      case Some(xfindPath) => Paths.get(xfindPath)
      case _ => Paths.get(System.getProperty("user.home"), "src", "xfind")
  }
  val XFIND_DB: Path = Paths.get(XFIND_PATH.toString, "shared", "xfind.db")
}
