#include <iostream>
#include <fstream>

#include <cstdlib>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h> 

using namespace std;

// function prototype
int copyfile1(char* infilename, char* outfilename);
int copyfile2(char* infilename, char* outfilename);
int copyfile3(char* infilename, char* outfilename, int b_size);

/** cptest.cpp
 * A file copying program.
 * Derived partially from caesar.cpp by Horstmann and Budd from big C++
 */

/**
   Prints usage instructions.
   @param program_name the name of this program
*/
void usage(char* program_name)
{  
   cout << "\nUsage: " << program_name
	<< " infile outfile [method] [buffersize]\n\n"
	<< "infile: file to copy\n"
	<< "outfile: output of copy\n"
	<< "method: copy method to use (1-3)\n"
	<< "buffersize: for method 3, size of buffer\n\n";
   exit(1);
}

/**
   Prints file opening error message
   @param filename the name of the file that could not be opened
*/
void open_file_error(char* filename)
{  
   cout << "Error opening file " << filename << "\n";
   exit(1);
}

/** Main program: copies a file.
    @param argc Number of command-line arguments (including program name).
    @param argv Array of pointers to character arays holding arguments.
    @return 0 if successful, 1 if fail.
*/
int main(int argc, char* argv[])
{  
  char* infilename; // Names of files.
  char* outfilename;

  if (argc<3 || argc>5) usage(argv[0]); // Must have exactly 2 arguments.

  infilename = argv[1];
  outfilename = argv[2];
  int returnstatus = -1;
  int method = 0;

  // Perform the copying
  switch(argc) {
  case 3:
    returnstatus = copyfile1(infilename, outfilename);
    break;
  case 4:
    method = atoi(argv[3]);
    switch(method) {
    case 1:
      returnstatus = copyfile1(infilename, outfilename);
      break;
    case 2:
      returnstatus = copyfile2(infilename, outfilename);
      break;
    default:
      usage(argv[0]);
    }
  case 5:
    method = atoi(argv[3]);
    switch(method) {
    case 1:
      returnstatus = copyfile1(infilename, outfilename);
      break;
    case 2:
      returnstatus = copyfile2(infilename, outfilename);
      break;
    case 3:
      returnstatus = copyfile3(infilename, outfilename, atoi(argv[4]));
      break;
    default:
      usage(argv[0]);
    }
  }
  
  printf("file copied successful!\n");

  return returnstatus;
}

/** Copies one file to another using formatted I/O, one character at a time.
 @param infilename Name of input file
 @param outfilename Name of output file
 @return 0 if successful, 1 if error.
*/
int copyfile1(char* infilename, char* outfilename) {
  FILE* infile; //File handles for source and destination.
  FILE* outfile;

  infile = fopen(infilename, "r"); // Open the input and output files.
  if (infile == NULL) {
    open_file_error(infilename);
    return 1;
  }

  outfile = fopen(outfilename, "w");
  if (outfile == NULL) {
    open_file_error(outfilename);
    return 1;
  }

  int intch;  // Character read from input file. must be an int to catch EOF.
  unsigned char ch; // Characted stripped down to a byte.

  // Read each character from the file, checking for EOF.
  while ((intch = fgetc(infile)) != EOF) {
    ch = static_cast<unsigned char> (intch); // Convert to one-byte char.
    fputc(ch, outfile); // Write out.
  }

  // All done--close the files and return success code.
  fclose(infile);
  fclose(outfile);

  return 0; // Success!
}

/** Copies one file to another using formatted I/O, one character at a time.
 @param infilename Name of input file
 @param outfilename Name of output file
 @return 0 if successful, 1 if error.
*/
int copyfile2(char* infilename, char* outfilename) {
  int infile; //File handles for source and destination.
  int outfile;

  infile = open(infilename, O_RDONLY); // Open the input and output files.
  
  if (infile == -1) {
    open_file_error(infilename);
    return 1;
  }

  outfile = open(outfilename, O_WRONLY | O_CREAT | O_TRUNC, 
                S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if (outfile == -1) {
    open_file_error(outfilename);
    return 1;
  }

  char buffer[32];
  int num_read;

  // Read each character from the file, checking for EOF
  while ((num_read = read(infile, buffer, 32))!=0) {
    write(outfile, buffer, num_read); // Write out.
  }

  // All done--close the files and return success code.
  close(infile);
  close(outfile);

  return 0; // Success!
}


/** Copies one file to another using formatted I/O, one character at a time.
 @param infilename Name of input file
 @param outfilename Name of output file
 @param b_size size of the buffer copied
 @return 0 if successful, 1 if error.
*/
int copyfile3(char* infilename, char* outfilename, int b_size) {
  int infile; //File handles for source and destination.
  int outfile;

  infile = open(infilename, O_RDONLY); // Open the input and output files.
  
  if (infile == -1) {
    open_file_error(infilename);
    return 1;
  }

  outfile = open(outfilename, O_WRONLY | O_CREAT | O_TRUNC, 
                S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if (outfile == -1) {
    open_file_error(outfilename);
    return 1;
  }

  char buffer[b_size];
  int num_read;

  // Read each character from the file, checking for EOF
  while ((num_read = read(infile, buffer, b_size))!=0) {
    write(outfile, buffer, num_read); // Write out.
  }

  // All done--close the files and return success code.
  close(infile);
  close(outfile);

  return 0; // Success!
}
