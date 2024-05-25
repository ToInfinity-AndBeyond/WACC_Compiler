
package integrationTest

object TestConfig {
  // default setting is true for all options
  final val OUTPUT_TO_FILE = true          // whether result should be written to file (with .result extension)
  final val OUTPUT_TO_TERMINAL = true      // whether result should be printed to terminal
  final val OUTPUT_ONLY_FAILURE = false   // whether only failure should be printed to terminal
  final val COPY_SRC_FILES_TO_DEST = true  // whether source files should be copied to destination
}