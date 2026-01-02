// Frontend of the 'Minimal UART 3' Assembler
// by Carsten Herting (slu4) 2025, last update 3.8.2025

// Win/MSYS2: Build with g++ asm.cpp -O2 -oasm.exe -s -static
// Linux:     Build with g++ asm.cpp -O2 -oasm     -s

#include "mnemonics.h"
#include "asm.h"

int main(int argc, char *argv[])
{
	bool dosym = false;																// by default don't output a symbol table
	std::string symtag = "";													// by default don't use any symbol tag
	int filenamepos = 0;															// extract possible -s parameter and filename
	for (int i=1; i<argc; i++)												// index zero contains "asm" itself
	{
		if (argv[i][0] == '-' && argv[i][1] == 's')	{ dosym = true; symtag = std::string(&argv[i][2]); }
		else filenamepos = i;														// nope, plain filename => remember it's index inside argv[]
	}

	if (filenamepos > 0)															// does a valid argument position of a filename exist?
	{
		std::ifstream file(argv[filenamepos]);
		if (file.is_open())
		{
      std::stringstream hexout, errors;
      std::string source;
      std::getline(file, source, '\0');
      file.close();
      Assembler(source, MNEMONICS, hexout, errors, dosym, symtag);
      if (errors.str().size() == 0) std::cout << hexout.str(); else std::cout << errors.str();
		}
		else std::cout << ("ERROR: Can't open \"" + std::string(argv[filenamepos]) + "\".\n");
	}
	else
	{
		std::cout << "Minimal UART 3 Assembler by C. Herting (slu4) 2026\n";
		std::cout << "  Usage: asm <sourcefile> [-s[<tag>]]\n";
		std::cout << "assembles a <sourcefile> to machine code and outputs\n";
		std::cout << "the result in Intel HEX format to the console.\n";
		std::cout << "  -s[<tag>]  outputs a list of symbolic constants\n";
		std::cout << "             [starting with <tag>] and their values.\n";
	}
	return 0;
}
