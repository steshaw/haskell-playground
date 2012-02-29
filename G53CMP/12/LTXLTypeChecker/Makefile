all:
	happy Parser.y
	ghc --make Main

clean:
	rm Parser.hs *.hi *.o Main

test:
	./Main test1.ltxl | diff - test1.result
	./Main test2.ltxl | diff - test2.result
	./Main test3.ltxl | diff - test3.result
	./Main test4.ltxl | diff - test4.result
	./Main test5.ltxl | diff - test5.result
	./Main test6.ltxl | diff - test6.result
	./Main test7.ltxl | diff - test7.result
	./Main test8.ltxl | diff - test8.result
	./Main test9.ltxl | diff - test9.result
