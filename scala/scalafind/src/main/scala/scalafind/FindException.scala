package scalafind

class FindException(val message: String) extends Exception {
  override def getMessage : String = message
}

object FindError extends Enumeration {
  type FindError = Value
  val STARTPATH_NOT_DEFINED: FindError = Value("Startpath not defined")
  val STARTPATH_NOT_FOUND: FindError = Value("Startpath not found")
  val STARTPATH_NOT_READABLE: FindError = Value("Startpath not readable")
  val INVALID_RANGE_MINDEPTH_MAXDEPTH: FindError = Value("Invalid range for mindepth and maxdepth")
  val INVALID_RANGE_MINLASTMOD_MAXLASTMOD: FindError = Value("Invalid range for minlastmod and maxlastmod")
  val INVALID_RANGE_MINSIZE_MAXSIZE: FindError = Value("Invalid range for minsize and maxsize")
  val STARTPATH_DOES_NOT_MATCH: FindError = Value("Startpath does not match find settings")
}
