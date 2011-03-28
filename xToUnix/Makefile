
GHC := ghc --make -Wall -O99 -fno-warn-type-defaults -fforce-recomp -rtsopts

all: xToUnix

xToUnix: Makefile xToUnix.hs
	$(GHC) xToUnix.hs
