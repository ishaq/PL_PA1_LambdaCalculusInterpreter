# Lambda Calculus Interpreter
This is [Programming Assignment 1](http://www.cs.rpi.edu/academics/courses/fall17/proglang/pa1/pa1.html) for CSCI.4430/6430 Programming Languages Fall 2017 at [RPI](http://cs.rpi.edu).

**NOTE:** The following code does what's needed of PA1, However it is terribly written, primarily because I am still trying to get over the "imperitive thinking".

### TODO:
1. get rid of all the nested parens, use $ and . as appropriately
1. reduce should not return IO Lexp. Change it back to the way PA1Helper expects (Lexp -> Lexp)
1. Simplify alpha_rename to use functional thinking, currently it is essentially an impertive routine (specially for the case  of an Apply expression).
1. Currently idx variable to alpha_rename assumes no more than 10 rename operations in a sub expression of an Apply. This should  be fixed. One way to do it would be to have the alpha_rename function return the value of next idx. This should fix the issue but it is still imperitive. Need to figure out a functional way
