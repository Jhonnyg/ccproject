#runhaskell main.hs $1.jl > $1.j
ghc --make main.hs
./main $1 > $1.j
java -jar jasmin.jar $1.j
