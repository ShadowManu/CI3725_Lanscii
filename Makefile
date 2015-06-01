all: alex
	ghc --make Main.hs -o lanscii

alex:	
	alex Alex.x

clean:
	rm *.o *.hi alex.hs lanscii
