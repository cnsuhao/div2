ptest: ptest.o product.o
	g++ -g ptest.o product.o -o ptest

ptest.o: ptest.cpp product.h
	g++ -c -g ptest.cpp 

product.o: product.cpp product.h
	g++ -c -g product.cpp 

clean:
	rm -f *.o ptest
