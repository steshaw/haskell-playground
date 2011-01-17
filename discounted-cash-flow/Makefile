PROGRAM := DiscountedCashFlow

.PHONY:
all: $(PROGRAM)

.PHONY:
run: all
	./gen-reports

$(PROGRAM): $(PROGRAM).hs
	ghc --make $<

.PHONY:
clean:
	rm -f *.hi *.o $(PROGRAM)
