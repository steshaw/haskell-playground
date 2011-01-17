PROGRAM := DiscountedCashFlow

.PHONY:
all: $(PROGRAM)

.PHONY:
run: all
	./DiscountedCashFlow 500 0.0 9.9 | tee dcf-house-price-value-using-500-per-week-rent.txt

$(PROGRAM): $(PROGRAM).hs
	ghc --make $<

.PHONY:
clean:
	rm -f *.hi *.o $(PROGRAM)
