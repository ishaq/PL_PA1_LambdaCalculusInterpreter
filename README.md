# Lambda Calculus Interpreter
This is [Programming Assignment 1](http://www.cs.rpi.edu/academics/courses/fall17/proglang/pa1/pa1.html) for CSCI.4430/6430 Programming Languages Fall 2017 at [RPI](http://cs.rpi.edu).

## Authors
* Muhammad Ishaq (ishaqm@rpi.edu)
* Daniel Park (parkd5@rpi.edu)


## Build and Run

```bash
$ cd <the code directory>
$ ghc --make main.hs
$ ./main [input_file_name]
``` 

If no input file is specified, it uses `input.lambda` as default.

## Features
* Beta Reduction
* Eta Conversion
* Alpha Renaming when required: alpha renaming is done by appending digits to the end of variable name e.g. `y` becomes `y#` where `# = 1,2,3, ...`

## Known Issues
* The code needs to be improved, right now it is written with an "imperitive thinking" shoehorned into "functional thinking". Having said that, it does what it is supposed to do and we are confident that it can work with lambda expressions of any length.

