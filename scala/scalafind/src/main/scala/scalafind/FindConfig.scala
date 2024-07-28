package scalafind

object FindConfig {
  val HOME: String = System.getProperty("user.home")
  val XFINDPATH: String =  Option(System.getenv("XFIND_PATH")) match {
      case Some(xfindPath) => xfindPath
      case _ => HOME + "/src/xfind"
  }
  val XFINDDB: String = XFINDPATH + "/shared/xfind.db"
}
