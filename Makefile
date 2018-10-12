VERSION=0.0.1

.PHONY: clean
clean:
	rm -v *.o *.hi

.PHONY: lint
lint:
	docker run -ti -v `pwd`:/project -w /project jamesmstone/hlint src

.PHONY: build
build:
	ghc -o zei src/Main.hs
