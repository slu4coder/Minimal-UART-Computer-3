// Minimal UART Tiny Mnemonics Jul 19th 2025

#include <vector>
#include <string>

const std::vector<std::string> MNEMONICS		// Index = OpCode
{
  "NOT","NOB","NOW","LSL","LLB","LLW","LSR","LRB","ROL","RLB","RLW","ROR","RRB","AND","OOR","XOR",
  "BNE","BEQ","BPL","BMI","BCC","BCS","BLE","BGT","JPA","JPR","JPS","RTS","PHS","PLS","LDI","LDA",
  "LDR","LDS","STB","STR","STS","INC","INB","INW","DEC","DEB","DEW","ADI","ADA","ADR","ADB","ADW",
  "SUI","SUA","SUR","SUB","SUW","CPI","CPA","CPR","ACB","ACW","SCB","SCW","WIN","INP","OUT","___",
};
