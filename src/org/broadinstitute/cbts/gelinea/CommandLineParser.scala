package org.broadinstitute.cbts.gelinea

trait CommandLineParser {

  protected def LOG: java.util.logging.Logger


  /**
   *  Parse the command line into a map of options
   */
  protected def parseCommandLine(commandLine: List[String]) = new CommandLine(parseCommandLineRec(commandLine))


  /**
   *  Recurrence for parsing the command line into a map of options
   */
  protected def parseCommandLineRec(commandLine: List[String]): Map[Symbol, String] = commandLine match {
    case Nil => Map()
    case "-n" :: str :: tail => parseCommandLineRec(tail) + ('network -> str)
    case "-l" :: str :: tail => parseCommandLineRec(tail) + ('geneList -> str)
    case "-s" :: str :: tail => parseCommandLineRec(tail) + ('geneSets -> str)
    case "-o" :: str :: tail => parseCommandLineRec(tail) + ('output -> str) + ('overwrite -> "false")
    case "-O" :: str :: tail => parseCommandLineRec(tail) + ('output -> str) + ('overwrite -> "true")
    case "-b" :: str :: tail => parseCommandLineRec(tail) + ('binSize -> str)
    case str :: tail => LOG warning "Unrecognized command line option: " + str; parseCommandLineRec(tail)
  }

}


/** 
 *  Wrapper class for the command line map 
 */
class CommandLine(private val commandLine: Map[Symbol, String]) {

  private val LOG = java.util.logging.Logger.getLogger(this.getClass().getName())


  /**
   * return a required command line option
   */
  def getRequired(option: Symbol): String = commandLine.get(option) match {
    case Some(value) => value
    case None => LOG severe "Missing required command line option " + option; sys.exit(1);
  }


  /**
   * return a optional integer command line option
   */
  def getOptionalInt(option: Symbol, defaultValue: Int): Int = commandLine.get(option) match {
    case Some(value) =>
      try {
        value.toInt
      }
      catch {
        case e: NumberFormatException => LOG warning "Wrong number format for " + option + ": " + value; defaultValue
      }
    case None => defaultValue
  }


  /**
   * return a optional boolean command line option
   */
  def getOptionalBoolean(option: Symbol, defaultValue: Boolean): Boolean = commandLine.get(option) match {
    case Some(value) => value.toLowerCase == "true"
    case None => defaultValue
  }

}
