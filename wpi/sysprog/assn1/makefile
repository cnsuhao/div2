prodtest: prodtest.o product.o
	g++ prodtest.o product.o -o prodtest
prodtest.o: prodtest.cpp product.h
	g++ -c prodtest.cpp
product.o: product.cpp product.h
	g++ -c product.cpp


clean: 
	rm -f *.o prodtest
