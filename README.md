CS4212 Project 2 Mini-Go
===

Team Members
---
 - Tam Wai Lun (A0033729A)
 - Qiu Guangju (A0148544Y)
 - Zhang Xiaoyun (A0152851H)

How to build compiler
---
To build our mini-go compiler, run the following script in command line

    ./build

How to compile Mini-Go programs
---
After building the compiler, to compile mini-go programs:

    ./compile [filename.go]

To compile *and* run the resulting VM code to see the program output:

    ./compile_and_run [filename.go]

For example:

    ./compile_and_run test/fac.go

`./compile [filename.go]` compiles the mini-go program stored in `filename.go`. If no filename is provided, input will be read from stdin (hint: use Ctrl-D to terminate if entering via keyboard this way).

The compiled VM code is stored in variable `vmc` and a pretty-printed version will be printed to stdout. If type check fails, `error` will be printed instead.

`./compile_and_run` additionally runs the VM code (using `VM.run`) after compilation. Any output from the mini-go program (since print command is supported) will be appended to stdout as well.

Sample Mini-Go programs
---
All sample programs are in the `test/` directory.

 - `fac.go` - Calculates and print a factorial number recursively
 - `fib.go` - Calculates and print a fibonacci number recursively
 - `sample.go` - Similar to the example on the project description, except that we print the result out. it shows the communication between two threads. 
 - `sample2.go` - Another sample program
 - `funcfail1.go`, `funcfail2.go`, `hellofail.go` - Example of type checking fail
 - `funcpass.go`, `hellopass.go` - Example of type checking pass

Additional Notes
---
For generating final VM code, we made some slight changes to the VM machine, which includes:

 1. Added some intermediate instructions like JI and ZEROI. These instructions are used for calculating the jump address.
 2. Jr. This is very similar to the jr instruction in MIPS.
 3. Shift int. This is used for adjusting the SP's position, which is used in calling functions.

This compiler should meet all the requirements in the project description. In addition, it contains partial support for concurrency. The reason why only 'partial' is that we fail to find a way to ensure that the communication between threads is safe -- hence the result is unpredictable.
But we implemented the remaining required functions and it passed all our test set so far (see sample programs included in test directory).

Tip: As the compiler only supports the '>', '+', '-', '*', '/', '==', '&&' and '!', make sure there is no "<", '<=' or ">=" in the mini-go code. Otherwise, it won't pass the parser.
