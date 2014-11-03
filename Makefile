go: Main
	@./Main

%: %.hs
	ghc $@

clean:
	rm Main
	rm *.o
	rm *.hi
