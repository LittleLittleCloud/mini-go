type instructions =
                  Halt
    
                  (* Stack operations 
 
                      PopS simply drops top-most value
                      Add  pops two top-most values and
                           pushes result onto stack
                          
                           The order matters for Div, ...
                           We first pop the left then the right operand

                      Output Prints top-most value onto console

                  *)
                  | PushS of int
                  | PopS
                  | Add 
                  | Sub
                  | Div
                  | Mult
                  | Lt
                  | Gt
                  | Eq
                  | And 
                  | Shift of int 
                  | Not
                  | Output

                  (* (Conditional) jumps 

                     Set PC to position indicated where
                     the position refers to the position in the list of instructions.
                     First position = 0 !

                  *)
                  | NonZero of int
                  | NonZeroI of string
                  | JumpI of string
                  | Zero of int
                  | Jump of int
                  | Jr

                  (* Memory operations 

                    Assign (memLoc, valInteger)

                    PushToStack memLoc

                           push value stored in memory cell memLoc onto stack

                    AssignFromStack (relStackPos, memLoc)
                        stack remains unchanged
                        relStackPos = 1 to access top-most element

                  *)

                  | Assign of int*int
                  | PushToStack of int
                  | AssignFromStack of int*int

                  (* run-time (environment) stack 

                      PushE i
                      PopE
 
                          as for computation stack

                      PushToEnv memLoc                 
 
                           push value stored in memory cell memLoc onto env stack

                      AssignFromEnv (relEnvStackPos, memLoc)

                        same as AssignFromStack but for env stack


                      UpdateToEnv (relEnvStackPos, memLoc)

                         Update relEnvStackPos with memLoc

                         This is more of a convenience function as we could
 
                         1. pop the top-most relEnvStack positions and store
                            them into some temporary memory locations

                         2. pop the top-most (the position we are interested in)

                         3. push memLoc (its value)

                         4. restore the other elements by pushing their values
                            stored into some temporary memory locations
                            back onto the stack

                  *)

                  | PushE of int
                  | PopE
                  | PushToEnv of int
                  | AssignFromEnv of int*int
                  | UpdateToEnv of int*int                                           

                  (* shared memory and concurrency *)
                  | Lock of int
                  | Unlock of int
                  | Thread of instructions list