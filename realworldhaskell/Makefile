
% : %.hs
	ghc --make $@

PROGS := \
  copy \
  csv \
  DesugarMonads \
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

all: ${PROGS}

.PHONY: pgm-test
pgm-test: pgm
	./pgm-test

test: pgm-test

.PHONY: clean
clean:
	rm -f ${PROGS}
