
% : %.hs
	ghc --make $@

PROGS := \
  copy \
  find \
  fixeols \
  jsonMain \
  NumExprCheck \
  pgm \
  readDouble \
  Setup \
  sumInternetUsage \
  tempFileFun \
  toUpperBanner \
  toupper \
  UnitsCheck \

.PHONY: pgm-test
pgm-test: pgm
	./pgm-test

all: ${PROGS}

test: pgm-test

.PHONY: clean
clean:
	rm -f ${PROGS}
