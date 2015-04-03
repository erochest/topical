
FLAGS=--enable-tests

all: init test docs package

init:
	cabal sandbox init
	make deps

test: build
	cabal test --test-option=--color

specs: build
	./dist/build/topical-specs/topical-specs

run: build
	cabal run -- stoplists/en.txt creativity.txt > tiles.csv

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

clean:
	cabal clean

distclean: clean
	cabal sandbox delete

configure: clean
	cabal configure ${FLAGS}

deps: clean
	cabal install --only-dependencies --allow-newer ${FLAGS}
	make configure

build:
	cabal build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild
