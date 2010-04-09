runhaskell main.hs $1.jl > $1.j
java -jar jasmin.jar $1.j
