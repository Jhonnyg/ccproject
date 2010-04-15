runhaskell main.hs testsuite/examples/good/$1.jl > testsuite/examples/good/$1.j
java -jar jasmin.jar -d testsuite/examples/good/ testsuite/examples/good/$1.j
java -cp testsuite/examples/good/,./ $1

