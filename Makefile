all:
	alex Alex.x
	ghc --make Main.hs -o lanscii

clean:
	rm *.o *.hi alex.hs lanscii
