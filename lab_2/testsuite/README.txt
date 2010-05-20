The Javalette test programs are in (subdirectories of) directory examples.

This directory contains a test driver (Grade.hs, RunCommand.hs and KompTest.hs) that
can be used to run the tests for your project.

Prerequisites
-------------
You will have to do 

make

in this directory to compile the test program, giving the executable program Grade
in this same directory.

Running the tests
-----------------

Assume that your submission directory is dir and that your
compiler is called jlc. Assume also that dir/lib 
contains the runtime support file (Runtime.class for submission A,
runtime.bc and/or runtime.o for submission B). For submission A, 
also jasmin.jar should be in dir/lib.

The test driver takes a number of options and two directories as
command line arguments. The possible options are

-s <name>                   The name of your compiler (in directory dir) is <name> (default is "jlc")
-b JVM                              Target files are JVM .class files
-b LLVM                           Target files are LLVM .bc files
-b x86                              Target files are x86 .o files
-x <extension>     Implemented extensions (only for submission B).

The first of the two path arguments specifies where to find the
directory examples which contains the testsuite (it is in this
directory). The second specifies your submission directory. 
Thus, if your compiler is called "javalettec", you may place
yourself in this directory and run

./Grade -s javalettec . dir

to compile all the basic javalette programs. The test driver will not 
attempt to run the good programs, so you may do the above
already when you have the parser working, and then when you 
have the typechecker working. 

To also run the good programs, you must specify the backend as
indicated above, i.e. for submission A

./Grade -s javalettec .b JVM . dir

The option -s javalettec and the last argument dir tells the test
driver to look for an executable of that name in directory dir.

The first file argument . specifies where to find directory examples.

The test driver will report its activities in compiling the test
programs and running the good ones. If your compiler is correct, 
output will end as follows:

Summary:
 0 compiling basic javalette (45/45)
 0 running basic (LLVM) (21/21)

Credits total: 0

All 45 test programs were compiled and gave correct indication OK or
ERROR to stderr. The 21 correct programs were run and gave correct output.


Preparing a submission
-------------------

Your submission must be structured as specified in section 8 of the
project description. We suggest that, after having prepared your tar
ball, you place it in an empty directory dir1 and run

./Grade -b JVM . dir1 

from this directory. The grading program, when it finds a tar ball in
the submission directory starts by extracting and building your
compiler, before running the test suite. This is how we test your
submission, so you can check whether building succeeds before
you submit.

