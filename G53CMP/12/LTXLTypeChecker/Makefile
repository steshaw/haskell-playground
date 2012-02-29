all:
	happy Parser.y
	ghc --make Main

clean:
	rm Parser.hs *.hi *.o Main

test:
	./Main test1.ltxl | diff - test1.result
	./Main test2.ltxl | diff - test2.result
	./Main test3.ltxl | diff - test3.result
