all: alex happy
	ghc -XFlexibleInstances --make Main.hs -o lanscii

alex:
	alex Alex.x

happy: alex
	happy Happy.y -o Happy.hs

clean:
	rm *.o *.hi Alex.hs Happy.hs lanscii
