//> using scala 2.13.12
//> using platform jvm

// dependencies
//> using dep com.github.j-mie6::parsley::5.0.0-M5
//> using test.dep org.scalatest::scalatest::3.2.18

// these are all sensible defaults to catch annoying issues
//> using options -deprecation -unchecked -feature
//> using options -Xlint:nullary-unit -Xlint:infer-any -Xlint:unused -Xlint:nonlocal-return
//> using options -Wdead-code -Wextra-implicit -Wnumeric-widen

// these will help ensure you have access to the latest parsley releases
// even before they land on maven proper, or snapshot versions, if necessary.
// just in case they cause problems, however, keep them turned off unless you
// specifically need them.
// using repositories sonatype-s01:releases
// using repositories sonatype-s01:snapshots

// these are flags used by Scala native: if you aren't using scala-native, then they do nothing
// lto-thin has decent linking times, and release-fast does not too much optimisation.
//> using nativeLto thin
//> using nativeGc commix
//> using nativeMode release-fast
