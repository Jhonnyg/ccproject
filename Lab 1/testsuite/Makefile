all: Grade

tests.tgz:
	tar -czvf $@ `find examples -name "*.jl" -or -name "*.output"`

%: %.hs *.hs
	ghc -main-is $* -threaded --make $<
clean:
	rm *.{hi,o} Grade
