PROGRAM := DiscountedCashFlow

.PHONY:
all: $(PROGRAM)

.PHONY:
run: all
	./DiscountedCashFlow 0.1 9.9 | tee dcf-house-price-value-using-400-per-week-rent.txt

$(PROGRAM): $(PROGRAM).hs
	ghc --make $<

.PHONY:
clean:
	rm -f *.hi *.o $(PROGRAM)
