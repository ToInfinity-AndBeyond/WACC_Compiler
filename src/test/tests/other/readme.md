## template file for tests

In order to run test cases in this directory, make sure the 
"src/test/waccc/IntegrationTest/integrationTest.scala" file does not ignore the 
it should "compile custom other tests".

For examples, check folder "sample"

The following Optional settings can be configured for each .wacc file to run integration tests
- Input 
    (provided input to the emulator, defaulted to empty)

- Output (expected output, defaulted to empty) 
    (Either #syntax_error#, #semantic_error#, #runtime_err#, #undefined_behaviour# or string of expected output)
    Use placeholder #addrs# when printing address of pair/array

- Exit (exit code, defaulted to 0)