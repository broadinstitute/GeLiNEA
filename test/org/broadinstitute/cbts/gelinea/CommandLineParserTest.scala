package org.broadinstitute.cbts.gelinea

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class CommandLineParserTest extends FlatSpec with Matchers with CommandLineParser {

  protected val LOG = java.util.logging.Logger.getLogger(this.getClass().getName())

  "Command line parser" should "parse command line" in {
    val cmdline = "-n network -b 50"
    val commandLine = parseCommandLine(cmdline.split(" ").toList)
    commandLine.getRequired('network) should be("network")
    commandLine.getOptionalInt('binSize, 50) should be(50)
    info("OK")
  }

  it should "not parse non-numerical values" in {
    val cmdline = "-n network -b badNumber"
    val commandLine = parseCommandLine(cmdline.split(" ").toList)
    commandLine.getRequired('network) should be("network")
    commandLine.getOptionalInt('binSize, 50) should be(50)
    info("OK")
  }


  "option -O" should "overwrite output file" in {
    val cmdline = "-n network -O output"
    val commandLine = parseCommandLine(cmdline.split(" ").toList)
    commandLine.getRequired('output) should be("output")
    commandLine.getRequired('overwrite) should be("true")
    info("OK")
  }


  "option -o" should "not overwrite output file" in {
    val cmdline = "-n network -o output"
    val commandLine = parseCommandLine(cmdline.split(" ").toList)
    commandLine.getRequired('output) should be("output")
    commandLine.getRequired('overwrite) should be("false")
    info("OK")
  }

}