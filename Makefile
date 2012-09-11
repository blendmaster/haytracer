all:
	ghc Main.hs -o raytrace

clean:
	rm -f *.hi *.o raytrace
