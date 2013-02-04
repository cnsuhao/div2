employee: ccc_empl.o employee.o manager.o highlevelmanager.o
	g++ -g ccc_empl.o employee.o manager.o highlevelmanager.o -o employee

ccc_empl.o: ccc_empl.h ccc_empl.cpp
	g++ -c -g ccc_empl.cpp

employee.o: ccc_empl.h employee.cpp
	g++ -c -g employee.cpp

manager.o: manager.h manager.cpp
	g++ -c -g manager.cpp

highlevelmanager.o: highlevelmanager.h highlevelmanager.cpp
	g++ -c -g highlevelmanager.cpp

clean:
	rm -f *.o
	rm -f employee

docs:
	doxygen
	chmod go+r html/*
	cp html/* ~/public_html/assn2/
