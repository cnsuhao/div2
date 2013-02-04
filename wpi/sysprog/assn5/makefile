all: stest stest2 stest3

stest: stest.o employee.o
	g++ stest.o employee.o -o stest

stest2: stest2.o employee.o
	g++ -g stest2.o employee.o -o stest2

stest3: stest3.o employee.o
	g++ -g stest3.o employee.o -o stest3

stest.o: stest.cpp employee.h
	g++ -g -c stest.cpp

stest2.o: stest2.cpp employee.h
	g++ -g -c stest2.cpp

stest3.o: stest3.cpp employee.h
	g++ -g -c stest3.cpp

employee.o: employee.cpp employee.h
	g++ -g -c employee.cpp

clean:
	rm -f *.o stest stest_debug stest_debugplus stest2 stest3 stest.txt
	rm -rf html latex 

docs:
	doxygen
	chmod go+r html/*

debug: stest.cpp employee.cpp employee.h
	g++ -c -g stest.cpp
	g++ -c -g employee.cpp
	g++ -g stest.o employee.o -o stest_debug

debugplus: stest.cpp employee.cpp employee.h
	g++ -c -g stest.cpp -DDEBUG
	g++ -c -g employee.cpp
	g++ -g stest.o -o stest_debugplus -DDEBUG
