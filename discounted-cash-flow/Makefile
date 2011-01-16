PROGRAM := DiscountedCashFlow

.PHONY:
all: $(PROGRAM)

$(PROGRAM): $(PROGRAM).hs
	ghc --make $<

.PHONY:
clean:
	rm -f *.hi *.o $(PROGRAM)
