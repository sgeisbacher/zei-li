.PHONY: clean
clean:
	rm -v *.o *.hi

.PHONY: build
build:
	ghc -o zei main.hs