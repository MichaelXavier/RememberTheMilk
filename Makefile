CABAL   = cabal
GHC_PKG = ghc-pkg
RUN     = runhaskell

all: build

doc: configure
	$(CABAL) haddock

install: install_deps
	$(CABAL) install

uninstall:
	 $(GHC_PKG) unregister rememberthemilk

build: configure install_deps
	$(CABAL) build

install_deps: rememberthemilk.cabal
	$(CABAL) install --only-dependencies

configure: rememberthemilk.cabal **/*.hs
	$(CABAL) configure

clean:
	$(CABAL) clean

quick_spec: Web/RememberTheMilk/**/*.hs
	$(RUN) Web/RememberTheMilk/Testing/Main.hs

spec: configure_tests
	$(CABAL) build
	$(CABAL) test

configure_tests:
	$(CABAL) configure --enable-tests --user
