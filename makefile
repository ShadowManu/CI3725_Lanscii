all:
	alex alex.x
	ghc --make alex.hs -o lanscii
