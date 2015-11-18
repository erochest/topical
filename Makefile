FLAGS=--enable-tests
SRC=$(shell find src -name '*.hs')
TOPICAL=stack build --pedantic --exec topical

all: init test docs package

init: stack.yaml

stack.yaml:
	stack init --prefer-nightly

test: build
	stack test --pedantic

run:
	$(TOPICAL) stoplists/en.txt creativity.txt > tiles.csv

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

package: test configure
	cabal check
	cabal sdist

upload: package
	cabal upload --check `ls dist/*.tar.gz | sort | tail -1`
	cabal upload `ls dist/*.tar.gz | sort | tail -1`

configure:
	cabal configure --package-db=clear --package-db=global --package-db=`stack path --snapshot-pkg-db` --package-db=`stack path --local-pkg-db`

watch:
	ghcid "--command=stack ghci"

clean:
	stack clean
	codex cache clean

distclean: clean

build:
	stack build --pedantic

restart: distclean init build

rebuild: clean build

install:
	stack install

tags: ${SRC}
	codex update

argon:
        find src -name \*.hs | xargs argon

hlint:
	hlint *.hs src specs

.PHONY: all init test run clean distclean configure deps build restart rebuild package upload install argon hlint watch
