all: stacktest queuetest tnodetest tnodetest2

stacktest: stacktest.o stack.o
	g++ -g stacktest.o stack.o -o stacktest

stack.o: stack.cpp stack.h
	g++ -g -c stack.cpp

stacktest.o: stacktest.cpp stack.h
	g++ -g -c stacktest.cpp

queuetest: queuetest.o queue.o
	g++ -g queuetest.o queue.o -o queuetest

queue.o: queue.cpp queue.h
	g++ -g -c queue.cpp

queuetest.o: queuetest.cpp queue.h
	g++ -g -c queuetest.cpp
	
tnodetest: tnodetest.o tnode.o
	g++ -g tnodetest.o tnode.o -o tnodetest

tnodetest2: tnodetest2.o tnode.o
	g++ -g tnodetest2.o tnode.o -o tnodetest2

tnode.o: tnode.cpp tnode.h
	g++ -g -c tnode.cpp
	
tnodetest.o: tnodetest.cpp tnode.h
	g++ -g -c tnodetest.cpp
	
tnodetest2.o: tnodetest2.cpp tnode.h
	g++ -g -c tnodetest2.cpp
	
clean:
	rm -f *.o
	rm -f stacktest queuetest tnodetest tnodetest2 
	rm -rf html latex 

docs:
	doxygen
	chmod go+r html/*
