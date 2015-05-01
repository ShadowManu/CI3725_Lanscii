all:
	alex alex.x
	ghc --make alex.hs -o lanscii

clean:
	rm *.o *.hi alex.hs lanscii
