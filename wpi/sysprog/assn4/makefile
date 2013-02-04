ctest: ctest.o mystring.o product.o
	g++ -g ctest.o mystring.o product.o -o ctest

ctest.o: ctest.cpp mystring.h product.h
	g++ -g -c ctest.cpp

mystring.o: mystring.cpp mystring.h product.h
	g++ -g -c mystring.cpp

product.o: product.cpp product.h
	g++ -g -c product.cpp

clean:
	rm -f *.o ctest

docs:
	doxygen
	chmod go+r html/*
	cp html/* ~/public_html/assn4/
