VERSION=0.0.1

.PHONY: clean
clean:
	rm -v *.o *.hi

.PHONY: build
build:
	ghc -o zei main.hs

.PHONY: dist
dist:
	echo "ATTENTIONE! this is indented for running on a macos-amd64-machine"
	ghc -o dist/zei-$(VERSION).darwin-amd64 main.hs
	docker run -v `pwd`/dist:/dist sgeisbacher/zeili-build ghc -o /dist/zei-$(VERSION).linux-amd64 main.hs