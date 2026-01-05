/*
  ------------------------------------------------------------------
  Minimal UART 3 Assembler by Carsten Herting (slu4) -- Jan 5th 2026
  ------------------------------------------------------------------
  See licensing information at the end of this file.
*/

#include <cstdint>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <cstring>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>

const std::vector<std::string> MNEMONICS // Minimal UART 3 instruction set -- Jul 19th 2025 (index = opCode)
{
  "NOT","NOB","NOW","LSL","LLB","LLW","LSR","LRB","ROL","RLB","RLW","ROR","RRB","AND","OOR","XOR",
  "BNE","BEQ","BPL","BMI","BCC","BCS","BLE","BGT","JPA","JPR","JPS","RTS","PHS","PLS","LDI","LDA",
  "LDR","LDS","STB","STR","STS","INC","INB","INW","DEC","DEB","DEW","ADI","ADA","ADR","ADB","ADW",
  "SUI","SUA","SUR","SUB","SUW","CPI","CPA","CPR","ACB","ACW","SCB","SCW","WIN","INP","OUT","___",
};

// check for opcode
int opCode(const std::string& src, size_t p, size_t len)
{
  if (len != 3) return -1; // not an opcode
  std::string str = src.substr(p, len);
  for (size_t i=0; i<MNEMONICS.size(); i++) if (str == MNEMONICS[i]) return static_cast<int>(i); // return opcode
  return -1; // not an opcode
}

// find line number of given position ep
std::string linenr(const std::string& src, size_t ep)
{
  size_t n = 1;
  for (size_t i = 0; i < std::min(ep, src.size()); ++i) if (src[i] == '\n') ++n;
  return std::to_string(n);
}

// advance 'ep' over whitespace and separators until a start of an element and return element's length
size_t findelem(const std::string& src, size_t& ep)
{
  while (true)
  {
    if (ep >= src.size()) return 0; // EOF reached
    else if (static_cast<unsigned char>(src[ep]) <= 32 || src[ep] == ',') ep++;	// consume element separators
    else if (src[ep] == ';') // consume ; comments
    {
      while (true)
      {
        ep++;
        if (ep >= src.size()) return 0;	// comment closes with EOF => no more elements
        if (src[ep] == '\n') { ep++; break; } // comment closes with LF
      }
    }
    else if (src[ep] == '\'') // enter special case '.. string
    {
      size_t np = ep + 1; // parse beyond string start marker
      while (true)
      {
        if (np >= src.size()) return np - ep; // 
        if (src[np] == '\'' || src[np] == '\n') break; // may still not be a valid '..' string
        np++;
      }
      return np + 1 - ep; // return length including ' marker(s)
    }
    else // handle other elements
    {
      size_t np = ep + 1; // parse beyond elements first char
      while (np < src.size() && static_cast<unsigned char>(src[np]) > 32 && src[np] != ',' && src[np] != ';') np++; // consume printable characters except , and ;
      return np - ep; // return element length
    }
  }
}

// emit code in proper Intel HEX format
class HexPrinter
{
  public:
    HexPrinter(std::stringstream& out) : mOut(out) {}
    ~HexPrinter() { if (used > 0) emitBuffer(); mOut << ":00000001FF\n"; } // write end of hex file
    void SetAddress(uint16_t laddr) { if (used > 0) emitBuffer(); linaddr = laddr; } // begin new line at new address
    uint16_t GetAddress() { return linaddr + used; } // returns the current emission address
    void Emit(uint8_t b) { buffer[used++] = b; if (used == 16) emitBuffer(); } // emit a byte
  protected:
    void emitBuffer() // emits current buffer as a line (only call if buffer is non-empty!)
    {
      mOut << ":" << std::hex << std::uppercase << std::setfill('0');
      uint8_t pch = (linaddr & 0xff00)>>8;
      uint8_t pcl = linaddr & 0x00ff;
      mOut << std::setw(2) << used << std::setw(2) << int(pch) << std::setw(2) << int(pcl) << "00";
      uint8_t checksum = used + pch + pcl;
      for(size_t i=0; i<used; i++) { mOut << std::setw(2) << int(buffer[i]); checksum += buffer[i]; }
      mOut << std::setw(2) << int((~checksum + 1) & 0xff) << "\n";
      linaddr += used; used = 0;
    }
    uint8_t buffer[16]{}; // emission line buffer
    size_t used{ 0 }; // number of emitted bytes pending in buffer
    uint16_t linaddr{ 0 }; // start address of the current data in buffer
    std::stringstream& mOut; // emission into this string stream
};

bool parse_org(const std::string& src, size_t& ep, size_t& elen, uint16_t& pc, std::stringstream& errors)
{
  ep += elen; elen = findelem(src, ep); // move to next element
  if (elen > 2 && src[ep] == '0' && src[ep+1] == 'x') // check for 0x. (hex format)
  {
    size_t k = src.find_first_not_of("0123456789abcdefABCDEF", ep+2);
    if (k == std::string::npos) k = ep + elen; // hex address until EOF?
    if (k != ep + elen || elen < 3 || elen > 6) // make sure element is a hex address 0x0..0xffff
      { errors << "ERROR in line " << linenr(src, ep) << ": Invalid hex address.\n"; return false; }
    pc = std::stoi(src.substr(ep+2, elen-2), nullptr, 16); // extract hex value // 16-bit hex value
  }
  else { errors << "ERROR in line " << linenr(src, ep) << ": Expecting hex format.\n"; return false; }
  return true; // pc, ep, elen have been updated successfully
}

static inline bool is_digit(char c) { return (c >= '0' && c <= '9'); }
static inline bool is_alpha(char c) { return ( (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ); }
static inline bool is_labelstart(char c) { return (c == '_' || is_alpha(c)); }
static inline bool is_labelbody(char c) { return (c == '_' || is_alpha(c) || is_digit(c)); }
static bool is_label(const std::string& def)
{
  if (def.size() < 1 || !is_labelstart(def[0])) return false;
  for (size_t i=1; i<def.size(); i++) if (!is_labelbody(def[i])) return false;
  return true;
}

void Assembler(const std::string& src, std::stringstream& hexout, std::stringstream& errors, bool dosym, std::string symtag)
{
  // PASS 1: #org sets pc, every element increases pc by its bytesize, calculates addresses of label definition
  std::vector<std::string> labels; // Liste aller Label-Definitionen mit ":"
  std::vector<uint16_t> labelpc; // Adresse aller Label-Definitionen
  uint16_t pc = 0; // program counter kepping track of target location
  size_t ep = 0; // elememt pointer (source string index)
  size_t elen; // element length (0 = EOF)
  while ((elen = findelem(src, ep)) != 0)
  {
    if (src[ep+elen-1] == ':') // label: definition?
    {
      std::string def = src.substr(ep, elen-1);
      if (is_label(def) == false) { errors << "ERROR in line " << linenr(src, ep) << ": Invalid label definition.\n"; return; }
      for(size_t i=0; i<labels.size(); i++) // ersetzte label referenece durch value
        if (def == labels[i]) { errors << "ERROR in line " << linenr(src, ep) << ": \'" << def << "\' already exists.\n"; return; }
      labels.emplace_back(src.substr(ep, elen-1)); labelpc.emplace_back(pc);
    }
    else if (elen == 4 && src.substr(ep, 4) == "#org") // ignore other preprocessor commands here
    {
      if (parse_org(src, ep, elen, pc, errors) == false) return;
    }
    else if (src[ep] == '\'')
    {
      if (elen > 2 && src[ep+elen-1] == '\'') pc += elen-2; // regular '..' string?
      else { errors << "ERROR in line " << linenr(src, ep) << ": Invalid string.\n"; return; }
    }
    else if (src[ep] == '<' || src[ep] == '>') pc += 1; // an LSB/MSB operator defines the expression size immediately
    else // derive the expression size from element start
    {
      size_t xp = ep; size_t xlen = elen;
      if (src[xp] == '+' || src[xp] == '-') { xp++; if (--xlen == 0) { errors << "ERROR in line " << linenr(src, ep) << ": Invalid expression.\n"; return; } }      
      if (xlen > 1 && src.substr(xp, 2) == "0x") // starts with hex value (byte or word?)
      {
        xlen -= 2; xp += 2; // consume '0x'
        size_t k = src.find_first_not_of("0123456789abcdefABCDEF", xp);
        size_t end = xp + xlen;
        if (k == std::string::npos || k > end) k = end;
        if (k == xp+1 || k == xp+2) pc += 1; // case 8-bit hex (0x0 or 0x00)
        else if (k == xp+3 || k == xp+4) pc += 2; // case 16-bit hex value (0x000 or 0x0000)
        else { errors << "ERROR in line " << linenr(src, ep) << ": Invalid hex value.\n"; return; }
      }
      else if (src[xp] >= '0' && src[xp] <= '9') pc += 1; // starts with plain number => always treat as 8-bit
      else if (opCode(src, xp, xlen) != -1) pc += 1; // 8-bit mnemonic
      else pc += 2; // * and labels count 16-bit
    }
    ep += elen; // hop over remains of current element/expression
  }

  // Ausgabe der Liste aller symbolic constants and their address values [starting with 'tag'].
  if (dosym)
  {
    for(int k=0; k<labels.size(); k++)            					    // Prüfe: Ist das Element ein label?
    {
      int adr = labelpc[k];
      if (symtag == labels[k].substr(0, symtag.length()))
      {	
        hexout << "#org 0x" << std::hex << std::setfill('0') << std::setw(2) << int((adr&0xff00)>>8) << std::setw(2) << int(adr&0x00ff) << " " << labels[k] << ":\n";
      }
    }
  }
  else
  {
    // PASS 2: Code is emitted to the emission counter mc. mc is stored inside the HEX class.
    // mc is only incremented during emission. pc is always incremented according to element size.
    pc = 0; ep = 0; // go back to start of source, reset program and data counter
    bool isemit = true; // default: code emission is on
    HexPrinter HEX(hexout); // holds mc (use HEX.GetAddress())

    while ((elen = findelem(src, ep)) != 0)
    {
      if (elen == 4 && src.substr(ep, 4) == "#org")
      {
        if (parse_org(src, ep, elen, pc, errors) == false) return; // #org always changes pc
        if (isemit) { HEX.SetAddress(pc); }	// #org only changes mc if emission is on
      }
      else if (elen == 5 && src.substr(ep, 5) == "#mute") isemit = false;
      else if (elen == 5 && src.substr(ep, 5) == "#emit") isemit = true;
      else if (src[ep+elen-1] == ':'); // skip label definitions - they don't have a size
      else if (src[ep] == '\'')	// string '..' (format was checked in pass 1)
      {
        for (size_t i=ep+1; i<ep+1+elen-2; i++) { pc++; if (isemit) HEX.Emit(src[i]); }
      }
      else if (opCode(src, ep, elen) != -1) { pc++; if (isemit) HEX.Emit(opCode(src, ep, elen)); } // mnemonic
      else // SIMPLE EXPRESSION PARSING (first element determins size)
      {
        char lsbmsb = 0; // undefined expression size (later '<', '>' or 'w')
        int term = 0, expr = 0, sign = 1; // init expression value
        size_t x = ep; // use a dedicated expression parsing pointer
        if (src[x] == '<' || src[x] == '>') lsbmsb = src[x++]; // read LSB and MSB operators (elen > 0 ist guaranteed here)
        while (x < ep+elen) // parse +/- separated terms of expression [ep, ep+elen[
        {
          if (src[x] == '+') { sign = 1; term=0; x++; } else if (src[x] == '-') { sign = -1; term=0; x++; } // consume sign
          if (x < ep+elen)
          {
            if (x < ep+elen - 1 && src.substr(x, 2) == "0x") // hex word or byte
            {
              size_t end = ep + elen;
              size_t k = src.find_first_not_of("0123456789abcdefABCDEF", x+2);
              if (k == std::string::npos || k > end) k = end;
              size_t digits = k - (x+2);
              if (digits < 1 || digits > 4) { errors << "ERROR in line " << linenr(src, ep) << ": Invalid hex value.\n"; return; }
              term = std::stoi(src.substr(x+2, digits), nullptr, 16); // extract hex value
              x = k; // consume this hex term
              if (lsbmsb == 0) { if (digits > 2) lsbmsb = 'w'; else lsbmsb = '<'; } // only set size if still undefined
            }
            else if (src[x] == '*') { term = HEX.GetAddress(); x++; if (lsbmsb == 0) lsbmsb = 'w'; } // * = emission pointer
            else if (src[x] >= '0' && src[x] <= '9') // decimal number
            {
              term = 0; // Hosenträger
              while (x < ep+elen && src[x] >= '0' && src[x] <= '9') { term *= 10; term += src[x++]-'0'; }
              if (lsbmsb == 0) lsbmsb = '<'; // only set size if still undefined
            }
            else // must be a label
            {
              if (lsbmsb == 0) lsbmsb = 'w';
              std::string ref; // cut out this reference
              size_t k = src.find_first_of(" ,;+-\n\r\t", x);	// finde label-Ende
              size_t end = ep + elen; if (k == std::string::npos || k > end) k = end;
              ref = src.substr(x, k - x); x = k;
              if (is_label(ref) == false) { errors << "ERROR in line " << linenr(src, ep) << ": Invalid label reference.\n"; return; }
              bool isknown = false; // is it a known label?
              for(int i=0; i<labels.size(); i++) // ersetzte label referenece durch value
                if (ref == labels[i]) { term = labelpc[i]; isknown = true; break; }
              if (!isknown) { errors << "ERROR in line " << linenr(src, ep) << ": Unknown label reference \'" << ref << "\'\n"; return; }
            }
            expr += sign * term; // add/subtract this term to the expression
          }
          else { errors << "ERROR in line " << linenr(src, ep) << ": Invalid expression.\n"; return; }
        }
        
        // WE HAVE THE EXPRESSION VALUE - NOW EMIT IT
        if (lsbmsb == 'w') { pc += 2; if (isemit) { HEX.Emit(expr & 0x00ff); HEX.Emit((expr & 0xff00)>>8); } } // emit LSB, MSB
        else if (lsbmsb == '>') { pc++; if (isemit) HEX.Emit((expr & 0xff00)>>8); } // emit MSB only
        else { pc++; if (isemit) HEX.Emit(expr & 0x00ff); } // emit LSB only
      }
      ep += elen; // hop over processed element // getchar();
    }
  }
}

/*
  FUNCTIONAL DESCRIPTION
  ----------------------
  The code reads an assembly source file, translates the mnemonics into machine opcodes,
  and emits the code in Intel Hex format. The program is structured into two main passes: 1. Parse
  the source code and store label definitions with their program counter (PC) values. 2. Emit the
  machine code based on the parsed instructions and label values. Here's an overview of the key components
  and how they work together:
    1) File handling: The program reads the input assembly file specified as a command-line argument 
  and stores the content in a string called src.
    2) findelem() function: This function iterates over the source code, skipping whitespaces and comments.
  It returns the length of the current element in the source code (0 for EOF).
    3) Pass 1: The program iterates through the source code using findelem() to identify label definitions,
  mnemonics, preprocessor commands, and expressions. It stores the label definitions and their corresponding
  PC values in a dictionary called labels. It also increments the PC based on the type of element encountered.
    4) Pass 2: The program resets the PC and iterates through the source code again, emitting machine code 
  based on the parsed instructions and label values. During this pass, it handles preprocessor commands
  (e.g., #org, #mute, and #emit), label definitions, mnemonics, strings, operators, and expressions.
  #org always sets the program counter (pc) but only sets memory counter (mc) when #emit is set.
  This construct allows assembling a program at a memory address differing from it's later target (handle with care!).
    5) Output: The generated machine code is converted into a string in Intel Hex format.

  Note that for simplicity the expression size is always deduced from the expressions's first term ignoring any sign, i.e.
  <... byte, >... byte, 0x0000 = word, 0x00 = byte, 15 = byte, label = word.

  EBNF of Assembly Language by Carsten Herting (slu4) 2023
  --------------------------------------------------------
  char       = ? any character ?
  letter     = 'a'..'z' | 'A'..'Z'
  digit      = '0'..'9'
  hexdigit   = digit | 'a'..'f' | 'A'..'F'
  hexnum     = '0x', hexdigit, [hexdigit], [hexdigit], [hexdigit]
  dec_lsb    = digit+ (* only the lsb part of a dec number will be used *)
  label      = ('_' | letter), {'_' | letter | digit}+
  comment    = ';', {char - ('\n' | EOF)}
  gap        = (' ' | ',' | '\t' | '\r' | '\n' | comment)+
  add-op     = '+' | '-'
  lsbmsb-op  = '<' | '>'
  mnemonic   = 'NOT' | ... | 'OUT'
  pre-proc   = '#emit' | '#mute' | ('#org', gap, hexnum)

  definition = label, ':'
  string     = "'", {char - ('\n' | EOF | "'")}, "'"

  program    = [gap], [element, {gap, element}], [gap], EOF
  element    = pre-proc | string | mnemonic | definition | expression
  expression = [lsbmsb-op], [add-op], term, {add-op, term} (* 1st term determines size *)
  term       = hexnum | dec_lsb | label | '*'
*/

/*
  LICENSING INFORMATION
  ---------------------
  This file is free software: you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation, either
  version 3 of the License, or (at your option) any later version.

  This file is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
  License for more details. You should have received a copy of the GNU General Public License along
  with this program. If not, see https://www.gnu.org/licenses/.
*/
