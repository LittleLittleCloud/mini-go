//pls put the content of this file to the end of README.md
To the convenience of generating final VM code, we make a slight change on VM machine, which includes:
1: add some intermediate instructions like JI and ZEROI, these instructions are used for the convenience of calculating the jump address
2: Jr. it's very similar to the jr instruction in MIPS
3: Shift int. It's used for adjusting the SP's position, which is of gread use in calling function.

the changed VM has been put into the file, 
// write the way of using it here

This compiler meets all the requirement you mentioned in the class. It even supports half of the concurrancy. The reason why only 'half' is that we fail to find a way to ensure that communication between threads is safe --- the result is unforeseeable.
But we implement the remaining required functions and it passed all our test set so far. and the test files are described in the follow:

fib.go
it calculates the 9th of Fibonacci sequence using recursive function

sample.go
it is very similar to the example on the project description, except that we print the result out. it shows the communication between two threads. 

Tips:
the compiler only support the '>','+','-','*','/','==','&&'and '!'. so pls make sure that there is no "<",'<='or ">=" in the test code. it won't pass the parser.


