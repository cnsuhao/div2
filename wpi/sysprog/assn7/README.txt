ALEC GOEBEL
CS 2303
2/29/07

A program to test the copying of files through various methods.  Method 1 runs approx. twice as fast as two.

copyfile1:
real    0m1.169s
user    0m0.209s
sys     0m0.042s

copyfile2:
real    0m2.515s
user    0m0.066s
sys     0m0.861s


INSTALL
-------
untar, then run
make

RUN
---
run the programs previously created
./cptest infile outfile [copymethod] [buffersize]

where infile is the file to be copied and outfile is the output

copymethod uses functions copyfile 1-3
1 uses FILE poitners
2 uses integer descripters
3 is the same as two but accepts buffersize as an option

CLEAN
---
run "make clean"
