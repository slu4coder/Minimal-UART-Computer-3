; **************************************************************
; *****                                                    *****
; ***** MinOS2 Operating System for the Minimal UART CPU 3 *****
; *****                                                    *****
; ***** by Carsten Herting (slu4) - updated: Jul 26th 2025 *****
; *****                                                    *****
; **************************************************************

; LICENSING INFORMATION
; This is free software: you can redistribute it and/or modify it under the terms of the
; GNU General Public License as published by the Free Software Foundation, either
; version 3 of the License, or (at your option) any later version.
; This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
; the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
; License for more details. You should have received a copy of the GNU General Public License along
; with this program. If not, see https://www.gnu.org/licenses/.

#org 0x0000

  _Start:       JPA OS_Start                                   ; JUMP TABLE
  _Prompt:      JPA OS_Prompt
  _FlashErase:  JPA OS_FlashErase
  _FlashWrite:  JPA OS_FlashWrite
  _FindFile:    JPA OS_FindFile
  _LoadFile:    JPA OS_LoadFile
  _SaveFile:    JPA OS_SaveFile
  _MemMove:     JPA OS_MemMove
  _Random:      JPA OS_Random
  _ReadLine:    JPA OS_ReadLine
  _ReadHex:     JPA OS_ReadHex
  _SkipSpace:   JPA OS_SkipSpace
  _Print:       JPA OS_Print
  _PrintHex:    JPA OS_PrintHex
  _CursorX:     JPA OS_CursorX
  _CursorY:     JPA OS_CursorY

OS_Start:       LDI <flashcode STB PtrA+0                       ; copy FLASH byte write/check routine to RAM
                LDI >flashcode STB PtrA+1
                LDI <OS_FlashByte STB PtrB+0
                LDI >OS_FlashByte STB PtrB+1
  copyloop:     LDR PtrA STR PtrB INW PtrA INB PtrB
                CPI <FlashByteEnd BCC copyloop

                LDI <logotxt STB PtrP+0                        ; print splash screen w/o stack init
                LDI >logotxt STB PtrP+1
  logoloop:     LDR PtrP CPI 0 BEQ OS_Prompt
                  OUT INW PtrP JPA logoloop

OS_Prompt:      LDI 0xfe STB 0xffff                            ; init stack
                LDI <readytxt PHS LDI >readytxt PHS            ; READY.
                JPS OS_Print PLS PLS  
  newline:      LDI <_ReadBuffer STB _ReadPtr+0                ; parse a line of user input
                LDI >_ReadBuffer STB _ReadPtr+1
                JPS OS_ReadLine                                ; MAIN LOOP: read in a line of user input
                JPS OS_SkipSpace                               ; omit leading spaces
                LDR _ReadPtr CPI 10 BEQ newline                ; empty line?
                  JPS OS_LoadFile CPI 0 BEQ OS_Error           ; load the file (=instruction)
                    JPR PtrD                                   ; run a loaded program with command line ptr on stack

OS_Error:       LDI <errortxt PHS LDI >errortxt PHS JPS OS_Print
                JPA OS_Prompt

  logotxt:      27, '[H', 27, '[H', 27, '[J', 27, '[?25h'
                27, '[12G', 'Minimal UART CPU 3.3 - MinOS', 10, 10
                27, '[12G', '32KB RAM - 32KB SSD - 8.0MHz', 10, 10, 0
  readytxt:     'READY.', 10, 0
  errortxt:     'Unknown file.', 10, 0
  backsptext:   27, '[D ', 27, '[D', 0
  
; -------------------------------------------------------------------------------------
; Reads a line of input into _ReadBuffer starting from _ReadPtr
; set _ReadPtr to the desired position within _ReadBuffer buffer (1-50 chars)
; modifies: _ReadPtr
; -------------------------------------------------------------------------------------
OS_ReadLine:    LDA _ReadPtr+0 PHS LDA _ReadPtr+1 PHS       ; push desired buffer start
  waitchar:     WIN CPI 10 BEQ isenter                      ; handle enter
                  CPI 8 BEQ isbackspace                     ; handle BACKSPACE
                  CPI 32 BCC waitchar                       ; ignore TAB, ESC
                  CPI 128 BCS waitchar                      ; ignore UP, DN, PgUp, etc.
                    STR _ReadPtr                            ; handle REGULAR CHAR
                      LDA _ReadPtr+0 CPI <ReadLast BEQ waitchar
                        LDR _ReadPtr OUT INB _ReadPtr+0     ; print it only if it fits into input buffer
                        JPA waitchar
  isbackspace:    LDS 2 CPA _ReadPtr+0 BEQ waitchar         ; handle BACKSPACE
                    LDI <backsptext PHS LDI >backsptext PHS
                    JPS _Print PLS PLS
                    DEB _ReadPtr+0 JPA waitchar
  isenter:        STR _ReadPtr OUT                          ; perform 'ENTER'
                  PLS STB _ReadPtr+1 PLS STB _ReadPtr+0     ; move pointer back to start of input buffer
                  RTS

; ---------------------------------------------------------------------------------------
; Advances the input line pointer _ReadPtr over SPACES
; modifies: _ReadPtr
; ---------------------------------------------------------------------------------------
OS_SkipSpace:   LDR _ReadPtr CPI 32 BNE ps_useit            ; skip SPACE
                  INB _ReadPtr+0 JPA OS_SkipSpace           ; there is always a LF at the end
  ps_useit:     RTS

; --------------------------------------------------
; Schreibt einen nullterminierten String at <stradr>
; push: stradr_lsb, stradr_msb
; pull: #, #
; --------------------------------------------------
OS_Print:       LDS 4 STB PtrP+0                ; get string pointer LSB
                LDS 3 STB PtrP+1                ; get string pointer MSB
  printloop:    LDR PtrP CPI 0 BEQ printend     ; self-modifying code
                  OUT INW PtrP JPA printloop    ; will wait a bit longer than needed
  printend:     RTS

; --------------------------------------------------
; Prints out a byte value <val> in HEX format
; push: <val>
; pull: #
; --------------------------------------------------
OS_PrintHex:    LDS 3
                LSL ROL ROL ROL ROL                   ; rotate upper nibble to lower nibble
                LSL LSL LSL LSL LSL ROL ROL ROL ROL   ; clear upper nibble
                ADI '0' CPI 58 BCC th_msn
                  ADI 39
  th_msn:       OUT LDS 3
                LSL LSL LSL LSL LSL ROL ROL ROL ROL   ; clear upper nibble
                ADI '0' CPI 58 BCC th_lsn
                  ADI 39
  th_lsn:       OUT RTS

; --------------------------------------------------
; Parses hex number 0000..ffff from _ReadPtr into _ReadNum
; breaks at any char != [0..9, a..f]
; modifies: _ReadPtr, _ReadNum
; --------------------------------------------------
OS_ReadHex:     LDI 0 STB _ReadNum+0 STB _ReadNum+1
                LDI 0xf0 STB _ReadNum+2
  hxgetchar:    LDR _ReadPtr                    ; input string lesen
                CPI 'g' BCS hxreturn            ; above f? -> melde Fehler!
                CPI 'a' BCS hxletter            ; a..f?
                CPI ':' BCS hxreturn            ; above 9? -> Separator: Zurück, wenn was da ist, sonst übergehen.
                CPI '0' BCS hxzahl              ; 0..9?
                  JPA hxreturn                  ; unter 0? -> Separator: Zurück, wenn was da ist, sonst übergehen.
  hxletter:     SUI 39
  hxzahl:       SUI 48 PHS
                LLW _ReadNum RLB _ReadNum+2     ; shift existing hex data 4 steps to the left
                LLW _ReadNum RLB _ReadNum+2
                LLW _ReadNum RLB _ReadNum+2
                LLW _ReadNum RLB _ReadNum+2
                PLS ADB _ReadNum+0              ; add new hex nibble (carry cannot happen)
                INW _ReadPtr JPA hxgetchar
  hxreturn:     RTS

; ******************************
; Generates a pseudo-random byte
; A: pseudo-random byte
; ******************************
OS_Random:      INB _RandomState+0 STB 0xff00   ; ++x
                LDA _RandomState+3 STB 0xff01   ; c
                XOR XOR XOR XOR XOR XOR XOR XOR ; result c^x in F1
                LDA _RandomState+1 STB 0xff00   ; a in F0
                XOR XOR XOR XOR XOR XOR XOR XOR ; A/F1 = a^c^x
                STB _RandomState+1              ; a = a^c^x
                ADB _RandomState+2              ; A/b = b+a
                LSR                             ; logical shift right b result
                ADA _RandomState+3 STB 0xff00   ; add c and move to F0
                XOR XOR XOR XOR XOR XOR XOR XOR ; result a^c^x in A/F1
                STB _RandomState+3              ; A/c = (c+(b>>1))^a
                RTS

; --------------------------------------------------
; positions/moves (or sets) the cursor
; push: cursor position/displacement (or x, y position)
; pull: # (or #, #)
; modifies: PtrP
; --------------------------------------------------
OS_CursorX:     LDS 3 INC STB PtrP+0 JPS cu_print LDI 'G' OUT RTS     ; set x
OS_CursorY:     LDS 3 INC STB PtrP+0 JPS cu_print LDI 'd' OUT RTS     ; set y
  cu_print:     LDI 27 OUT LDI '[' OUT
  cu_number:    LDI '0' STB PtrP+1               ; reset x10 decimal digit
  cu_loop:      LDI 10 SUB PtrP+0 BCC cu_store10
                  INB PtrP+1 JPA cu_loop         ; increase x10 digit
  cu_store10:   LDA PtrP+1 CPI '0' BEQ cu_store1 ; skip a zero x10 digit
                  OUT                            ; output a non-zero x10 digit
  cu_store1:    LDA PtrP+0 ADI 58 OUT            ; add '0' + 10, which was already subtracted above
                RTS

; --------------------------------------------------
; Searches SSD for file <filename> given at _ReadPtr (any char <= 39 terminates <filename>)
; returns A=1: _ReadPtr points *beyond* <filename>, PtrA points at start of file in FLASH
; returns A=0: _ReadPtr points to *start* of <filename>, PtrA points *beyond last file* in FLASH
; modifies: _ReadPtr, PtrA, PtrB, PtrC
; --------------------------------------------------
OS_FindFile:      ; browse through all stored files and see if <filename> matches name, any char <=39 stops
                  LDI <FileSystem STB PtrA+0                       ; goto start of SSD file system
                  LDI >FileSystem STB PtrA+1
  ff_search:        LDR PtrA CPI 0xff BEQ ff_returnfalse           ; end of data reached -> no match
                    ; check if name matches (across banks)
                    LDA PtrA+0 STB PtrC+0                          ; PtrA -> PtrC
                    LDA PtrA+1 STB PtrC+1
                    LDA _ReadPtr+0 STB PtrB+0                      ; _ReadPtr -> PtrB
                    LDA _ReadPtr+1 STB PtrB+1
  match_loop:       LDR PtrB CPI 40 BCS ff_isnoend                 ; cast code <= 39 (') (SPACE, ENTER) to 0
                      LDI 0
  ff_isnoend:       CPR PtrC BNE files_dontmatch                   ; stimmen Buchstaben überein?
                      CPI 0 BEQ ff_returntrue                      ; wurde gemeinsame 0 erreicht => match!
                        INW PtrB INW PtrC JPA match_loop
                    ; this filename does not match => jump over (across banks)
  files_dontmatch:  LDI 22 ADW PtrA                                ; advance over header to bytesize LSB
                    LDR PtrA STB PtrB+0 INW PtrA                   ; extract bytesize -> PtrB
                    LDR PtrA STB PtrB+1 INW PtrA
                    LDA PtrB+0 ADW PtrA LDA PtrB+1 ADB PtrA+1      ; PtrA points beyond this file
                    JPA ff_search
  ff_returntrue:    LDA PtrB+0 STB _ReadPtr+0                      ; parse over good filename
                    LDA PtrB+1 STB _ReadPtr+1
                    LDI 1 RTS
  ff_returnfalse:   LDI 0 RTS                                      ; not found, don't change _ReadPtr

; -------------------------------------------------------
; Loads <filename> pointed to by _ReadPtr from SSD
; <filename> must be terminated by <= 39 "'"
; success: returns A=1, _ReadPtr points beyond <filename>
; failure: returns A=0, _ReadPtr points to <filename>
; modifies: _ReadPtr, PtrA, PtrB, PtrC, PtrD
; -------------------------------------------------------
OS_LoadFile:        JPS OS_FindFile CPI 1 BNE lf_failure            ; check result in A
                      ; PtrA now points to file in FLASH
                      LDI 20 ADW PtrA                               ; search for target addr
                      LDR PtrA STB PtrC+0 STB PtrD+0                ; destination addr -> PtrC, PtrD
                      INW PtrA
                      LDR PtrA STB PtrC+1 STB PtrD+1
                      INW PtrA
                      LDR PtrA STB PtrB+0 INW PtrA                  ; bytesize -> PtrB (PtrA now points to data)
                      LDR PtrA STB PtrB+1 INW PtrA
  lf_loadloop:        DEW PtrB BCC lf_success                      ; alles kopiert?
                        LDR PtrA STR PtrC                          ; copy block from A -> to C
                        INW PtrC INW PtrA
                        JPA lf_loadloop
  lf_success:         LDI 1 RTS                                    ; switch off FLASH
  lf_failure:         LDI 0 RTS

; --------------------------------------------------
; Saves a RAM area as file <name> to SSD drive, checks if there is enough space, asks before overwriting
; expects: _ReadPtr points to filename starting with char >= 40, terminated by char <= 39
; push: first_lsb, first_msb, last_lsb, last_msb
; pull: #, #, #, result (1: success, 0: failure, 2: user abortion) same as in A
; modifies: X, PtrA, PtrB, PtrC, PtrD, PtrE, PtrF, _ReadPtr
; --------------------------------------------------
OS_SaveFile:      LDS 3 STB PtrF+1 LDS 4 STB PtrF+0
                  LDS 5 STB PtrE+1 LDS 6 STB PtrE+0
                  ; assemble a zero-filled 20-byte filename starting at _ReadBuffer for the header
                  LDI 19 STB 0xff00                                ; copy up to 19 chars of filename
                  LDI <_ReadBuffer STB PtrD+0                      ; _ReadBuffer -> temp PtrD
                  LDI >_ReadBuffer STB PtrD+1
  sf_namecopy:    LDR _ReadPtr CPI 40 BCC sf_nameend               ; read a name char, anything <= 39 ends name
                    STR PtrD INW _ReadPtr INW PtrD                 ; copy name char
                    DEB 0xff00 BNE sf_namecopy
  sf_nameend:     LDI 0 STR PtrD                                   ; overwrite rest including 20th byte with zero
                  INW PtrD DEB 0xff00 BCS sf_nameend               ; PtrD points beyond 20-byte area

                  ; invalidate exisiting files with that name, look for enough free space on the SSD
  sf_existfile:   LDI <_ReadBuffer STB _ReadPtr+0                  ; _ReadPtr points back to filename
                  LDI >_ReadBuffer STB _ReadPtr+1
                  JPS OS_FindFile CPI 1 BNE sf_foundfree
                    LDA PtrA+1 CPI 0x10 BCC sf_returnfalse         ; file is in write-protected area
  sf_notprotect:    LDI <sf_asktext PHS LDI >sf_asktext PHS
                    JPS OS_Print PLS PLS
                    WIN CPI 'y' BEQ sf_overwrite
                      LDI 10 OUT JPA sf_returnbrk        ; used break => no error
  sf_overwrite:     LDI 10 OUT
                    ; invalidate existing filename by setting it's first byte to 0 (this always works in NOR FLASH!)
                    LDI 0xaa STB 0x5555                            ; INIT FLASH BYTE WRITE PROGRAM
                    LDI 0x55 STB 0x2aaa
                    LDI 0xa0 STB 0x5555
                    LDI 0 PHS PHS JPS OS_FlashByte PLS PLS          ; START INVALIDATE WRITE PROCESS (overwrite to 0)
                    JPA sf_existfile

  sf_foundfree:   ; PtrA now point to free SSD space
                  LDA PtrE+1 SUB PtrF+1                            ; calculate data bytesize in PtrF
                  LDA PtrE+0 SUW PtrF
                  INW PtrF                                         ; PtrF = last - first + 1 = bytesize

                  LDA PtrA+1 STB PtrB+1
                  LDA PtrA+0 STB PtrB+0
                  LDA PtrF+0 ADW PtrB LDI 24 ADW PtrB              ; add data bytesize + 24 bytes for header
                  LDA PtrF+1 ADB PtrB+1 CPI 0x80 BCS sf_returnfalse  ; overflow > 0x8000?

                  ; write header start address and bytesize
  sf_fitsin:      LDA PtrE+0 STB _ReadBuffer+20                    ; write start addr to header
                  LDA PtrE+1 STB _ReadBuffer+21
                  LDA PtrF+0 STB _ReadBuffer+22                    ; write data bytesize to header
                  LDA PtrF+1 STB _ReadBuffer+23

                  ; write header to FLASH memory
                  LDI <_ReadBuffer STB PtrC+0                      ; start addr of header -> PtrC
                  LDI >_ReadBuffer STB PtrC+1                      ; free addr is already in PtrA
                  LDI 0 STB PtrB+1 LDI 24 STB PtrB+0               ; bytesize of header -> PtrB
                  JPS OS_FlashWrite                                ; write the header (incrementing PtrA)
                  LDA PtrB+1 CPI 0xff BNE sf_returnfalse           ; check if all bytes have been written successfully

                  ; write body to FLASH memory
                  LDA _ReadBuffer+20 STB PtrC+0                    ; start -> PtrC
                  LDA _ReadBuffer+21 STB PtrC+1
                  LDA _ReadBuffer+22 STB PtrB+0                    ; bytesize -> PtrB
                  LDA _ReadBuffer+23 STB PtrB+1                    ; PtrA already positioned behind header
                  JPS OS_FlashWrite                                ; write the data body
                  LDA PtrB+1 CPI 0xff BNE sf_returnfalse           ; check if all bytes have been written successfully
                    LDI 1 STS 6 RTS                                ; return success, FLASH off

  sf_returnfalse: LDI 0 STS 6 RTS                                  ; return failure, FLASH off
  sf_returnbrk:   LDI 2 STS 6 RTS                                  ; signal user abortion

  sf_asktext:     'Overwrite (y/n)?', 0

; --------------------------------------------------
; Writes data to free FLASH memory starting at PtrA
; The caller has to ensure that this is indeed FREE
; AND FORMATED FLASH, containing only 0xff.
; PtrC: RAM source, PtrB: bytesize
; modifies: PtrB (0xffff: success, else failure)
;           PtrA (points beyond written FLASH data, if successful)
;           PtrC (points to beyond written RAM source data, if successful)
;           0xff00
; --------------------------------------------------
OS_FlashWrite:    DEW PtrB BCC fw_return                      ; count down bytesize, underflow => done => success
                    LDI 0xaa STB 0x5555                       ; INIT FLASH BYTE WRITE PROGRAM
                    LDI 0x55 STB 0x2aaa
                    LDI 0xa0 STB 0x5555
                    LDR PtrC PHS PHS JPS OS_FlashByte PLS PLS ; INITIATE BYTE WRITE PROCESS IN RAM
                    INW PtrC INW PtrA JPA OS_FlashWrite       ; increase both pointers to next byte
  fw_return:      RTS

; ----------------------------------------------------------------------
; Eraseses a 4KB FLASH sector without any protection (handle with care!)
; the sector is only defined by the bits 12..15 of PtrA
; ----------------------------------------------------------------------
OS_FlashErase:  LDI 0xaa STB 0x5555              ; issue FLASH ERASE COMMAND
                LDI 0x55 STB 0x2aaa
                LDI 0x80 STB 0x5555
                LDI 0xaa STB 0x5555
                LDI 0x55 STB 0x2aaa
                LDI 0x30 PHS LDI 0xff PHS
                JPS OS_FlashByte PLS PLS         ; initiate the BLOCK ERASE command in RAM
                RTS

; --------------------------------------------------
; Moves N bytes from S.. to D.. taking overlap into account
; push: D_lsb, D_msb, S_lsb, S_msb, N_lsb, N_msb
; Pull: #, #, #, #, #, #
; --------------------------------------------------
OS_MemMove:   LDS 3 STB PtrB+1 LDS 4 STB PtrB+0            ; B = number of bytes
              DEW PtrB BCC mc_done
              LDS 5 STB PtrA+1 LDS 6 STB PtrA+0            ; A = source
              LDS 7 STB PtrC+1 LDS 8 STB PtrC+0            ; C = destination
              LDA PtrA+1 CPA PtrC+1 BCC a_less_c BNE c_less_a
                LDA PtrA+0 CPA PtrC+0 BCC a_less_c BEQ mc_done
  c_less_a:   LDR PtrA STR PtrC
              INW PtrA INW PtrC
              DEW PtrB BCS c_less_a
                RTS
  a_less_c:   LDA PtrB+1 ADB PtrA+1 LDA PtrB+1 ADB PtrC+1
              LDA PtrB+0 ADW PtrA LDA PtrB+0 ADW PtrC
    alc_loop: LDR PtrA STR PtrC
              DEW PtrA DEW PtrC
              DEW PtrB BCS alc_loop
  mc_done:      RTS

; **********************************************************************************************************

FileSystem:                                  ; start of the file system

; **********************************************************************************************************

'run', 0, '               ', 0, RunStart, RunEnd-RunStart ; file header

  ; --------------------------------------------------
  ; Displays the directory of the SSD drive
  ; usage: "jump <address> <ENTER>"
  ; --------------------------------------------------
  RunStart:       JPS _SkipSpace JPS _ReadHex             ; skip spaces and parse first address
                  LDA _ReadNum+2 CPI 0xf0 BEQ 0x8000      ; default ist 0x8000
                    JPR _ReadNum

  RunEnd:

; **********************************************************************************************************

'save', 0, '              ', 0, SaveStart, SaveEnd-SaveStart    ; file header

  ; --------------------------------------------------
  ; usage: "save <first_hex_addr> <last_hex_addr> <filename> <ENTER>"
  ; receives access to command line on stack
  ; --------------------------------------------------
  SaveStart:      LDI 1 STB 0xff00                              ; read in first and last hex woard address
    sv_loop:      JPS _SkipSpace JPS _ReadHex                   ; skip spaces and parse first address
                  LDA _ReadNum+2 CPI 0xf0 BEQ sv_syntax         ; wurde eine Zahl eingelesen?
                    LDA _ReadNum+0 PHS LDA _ReadNum+1 PHS       ; push onto stack
                  DEB 0xff00 BCS sv_loop
                    JPS _SkipSpace
                    LDR _ReadPtr CPI 40 BCC sv_syntax           ; look for a valid filename, <= 39 ends name
                      JPS _SaveFile CPI 0 BNE _Prompt
                        LDI <sv_errortxt PHS LDI >sv_errortxt PHS
                        JPA sv_print
    sv_syntax:      LDI <sv_syntaxtxt PHS LDI >sv_syntaxtxt PHS
    sv_print:       JPS _Print JPA _Prompt                      ; stack cleanup intentionally left out

    sv_syntaxtxt: 'save <fst> <lst> <name>', 10, 0
    sv_errortxt:  'ERROR', 10, 0

  SaveEnd:

; **********************************************************************************************************

'dir', 0, '               ', 0, DirStart, DirEnd-DirStart       ; file header

  ; ---------------------------------------------------------------------------
  ; Displays the directory of the SSD drive
  ; usage: "dir <ENTER>"
  ; Note: Do not make this routine any longer - it barely fits into command RAM
  ; ---------------------------------------------------------------------------
  DirStart:       LDI <dirtext PHS LDI >dirtext PHS                    ; print the directory headline
                  JPS _Print PLS PLS
                  LDI 0x80 STB PtrC+1 LDI 0x00 STB PtrC+0              ; PtrC = total SSD bytesize
                  LDI >FileSystem STB PtrA+1 SUB PtrC+1                ; PtrA = start of SSD area
                  LDI <FileSystem STB PtrA+0 SUW PtrC	         

  dc_lookfiles:   LDR PtrA CPI 0xff BEQ dc_endreached   ; end of used area reached?
                    ; first extract all data, later decide on printing
                    LDA PtrA+0 STB _ReadNum+0                          ; copy PtrA for printing the name
                    LDA PtrA+1 STB _ReadNum+1
                    LDI 20 ADW PtrA               										 ; jump over filename
                    LDR PtrA STB PtrE+0 INW PtrA                       ; read start address -> PtrE
                    LDR PtrA STB PtrE+1 INW PtrA
                    LDR PtrA STB PtrB+0 INW PtrA                       ; read bytesize -> PtrB
                    LDR PtrA STB PtrB+1 INW PtrA                       ; PtrA now points to data section
                    LDA PtrB+0 ADW PtrA                                ; add data byte size to reach beyond file
                    LDA PtrB+1 ADB PtrA+1
                    LDA PtrB+0 SUW PtrC                                ; subtract data bytesize in PtrB from PtrC
                    LDA PtrB+1 SUW PtrC+1
                    LDI 24 SUW PtrC                                    ; subtract headersize from PtrC

                    INP CPI 33 BCC _Prompt                             ; SPACE, ENTER, DEL or ESC = user break

										LDR _ReadNum CPI 0 BEQ dc_lookfiles  							 ; check if it is a deleted file
  dc_nextchar:        OUT INW _ReadNum                                 ; print filename char            
  dc_noover:          LDR _ReadNum CPI 0 BNE dc_nextchar  						 ; print stuff until end marker
  dc_nameend:         LDI 20 PHS JPS _CursorX PLS
                      LDA PtrE+1 PHS JPS _PrintHex PLS                 ; start
                      LDA PtrE+0 PHS JPS _PrintHex PLS
                      LDI 25 PHS JPS _CursorX PLS
                      LDA PtrB+1 PHS JPS _PrintHex PLS                 ; bytesize
                      LDA PtrB+0 PHS JPS _PrintHex PLS
                      LDI 10 OUT JPA dc_lookfiles

  dc_endreached:  LDI 25 PHS JPS _CursorX PLS
                  LDA PtrC+1 PHS JPS _PrintHex PLS
                  LDA PtrC+0 PHS JPS _PrintHex PLS
                  LDI 20 PHS JPS _CursorX PLS
                  LDI <freetext PHS LDI >freetext PHS JPS _Print
                  JPA _Prompt

  dirtext:      10, 'FILENAME........... DEST SIZE', 10, 0
  freetext:     'FREE ', 10, 0

  DirEnd:

; **********************************************************************************************************

'del', 0, '               ', 0, DelStart, DelEnd-DelStart ; file header

  ; --------------------------------------------------
  ; Deletes a file from the SSD
  ; usage: "del <filename> <ENTER>"
  ; modifies: X
  ; --------------------------------------------------
  DelStart:       JPS _SkipSpace
                  LDR _ReadPtr CPI 40 BCC de_syntax       ; look for a valid filename, code <= 39 ends
                    ; invalidate exisiting file with that name
                    JPS _FindFile CPI 1 BNE de_notferror
                      LDA PtrA+1 CPI 0x10 BCC de_canterror  ; protect OS command files (sector 1)
                    ; file exists and may be deleted, invalidate it's name to 0
    de_candel:      LDI 0xaa STB 0x5555                   ; INIT FLASH WRITE PROGRAM
                    LDI 0x55 STB 0x2aaa
                    LDI 0xa0 STB 0x5555
                    LDI 0 PHS PHS JPS OS_FlashByte PLS PLS ; START WRITE PROCESS
                    JPA _Prompt

  de_syntax:      LDI <de_errortxt PHS LDI >de_errortxt PHS JPS _Print JPA _Prompt
  de_canterror:   LDI <de_canttxt PHS LDI >de_canttxt PHS JPS _Print JPA _Prompt
  de_notferror:   LDI <de_notftxt PHS LDI >de_notftxt PHS JPS _Print JPA _Prompt

  de_errortxt:    'del <name>', 10, 0
  de_canttxt:     'PROTECTED', 10, 0
  de_notftxt:     '?', 10, 0

  DelEnd:

; **********************************************************************************************************

'format', 0, '            ', 0, FormatStart, FormatEnd-FormatStart      ; file header

  ; --------------------------------------------------
  ; usage: "format <ENTER>"
  ; --------------------------------------------------
  FormatStart:    LDI <fm_asktext PHS LDI >fm_asktext PHS JPS _Print PLS PLS
                    WIN CPI 'y' BNE _Prompt
                    LDI <fm_formtext PHS LDI >fm_formtext
                    PHS JPS _Print PLS PLS  
                    LDI 0x10 STB PtrA+1 LDI 0x00 STB PtrA+0         ; start of SSD user sector
    format_loop:    JPS OS_FlashErase
                    LDI 0x10 ADB PtrA+1
                    CPI 0x80 BCC format_loop
                      JPA _Prompt

    fm_asktext:   'SURE? (y/n)', 10, 0
    fm_formtext:  'Formating...', 10, 0

  FormatEnd:

; **********************************************************************************************************

'clear', 0, '             ', 0, ClearStart, ClearEnd-ClearStart    ; file header

  ; --------------------------------------------------
  ; Clears the VGA screen and positions the cursor at the top
  ; usage: "jump <address> <ENTER>"
  ; --------------------------------------------------
  ClearStart:     LDI <clrtxt PHS LDI >clrtxt PHS JPS _Print PLS PLS
                  JPA _Prompt

    clrtxt:       27, '[H', 27, '[J', 0                      ; home & clear screen

  ClearEnd:

; **********************************************************************************************************

'memset', 0, '            ', 0, MemsetStart, MemsetEnd-MemsetStart          ; file header

  ; --------------------------------------------------
  ; usage: "memset <adr_first> <adr_last> <byte> <ENTER>"
  ; --------------------------------------------------
  MemsetStart:    JPS _SkipSpace JPS _ReadHex             ; skip spaces and parse first address
                  LDA _ReadNum+2 CPI 0xf0 BEQ mf_syntax   ; wurde eine Zahl eingelesen?
                    LDA _ReadNum+0 STB PtrA+0             ; first address
                    LDA _ReadNum+1 STB PtrA+1
                  JPS _SkipSpace JPS _ReadHex             ; skip spaces and parse last address
                  LDA _ReadNum+2 CPI 0xf0 BEQ mf_syntax
                    LDA _ReadNum+0 STB PtrB+0             ; last address
                    LDA _ReadNum+1 STB PtrB+1
                  JPS _SkipSpace JPS _ReadHex             ; skip spaces and parse byte value
                  LDA _ReadNum+2 CPI 0xf0 BEQ mf_syntax

  mfnext:           LDA _ReadNum+0 STR PtrA               ; BESCHREIBE DEN SPEICHER
                    LDA PtrA+0 CPA PtrB+0 BNE mfweiter
                      LDA PtrA+1 CPA PtrB+1 BEQ _Prompt
  mfweiter:             INW PtrA
                        JPA mfnext

  mf_syntax:      LDI <mf_errortxt PHS LDI >mf_errortxt PHS JPS _Print
                  JPA _Prompt

  mf_errortxt:    'memset <fst> <lst> <val>', 10, 0

  MemsetEnd:

; **********************************************************************************************************

'memmove', 0, '           ', 0, MemmoveStart, MemmoveEnd-MemmoveStart   ; file header

  ; --------------------------------------------------
  ; usage: "memmove <adr_first> <adr_last> <adr_dest> <ENTER>"
  ; --------------------------------------------------
  MemmoveStart:   JPS _SkipSpace JPS _ReadHex                    ; skip spaces and parse first address
                  LDA _ReadNum+2 CPI 0xf0 BEQ sc_syntax          ; wurde eine Zahl eingelesen?
                    LDA _ReadNum+0 STB PtrA+0                    ; first address
                    LDA _ReadNum+1 STB PtrA+1
                  JPS _SkipSpace JPS _ReadHex                    ; skip spaces and parse last address
                  LDA _ReadNum+2 CPI 0xf0 BEQ sc_syntax
                    LDA _ReadNum+0 STB PtrB+0                    ; last address
                    LDA _ReadNum+1 STB PtrB+1
                  JPS _SkipSpace JPS _ReadHex                    ; skip spaces and parse byte value
                  LDA _ReadNum+2 CPI 0xf0 BEQ sc_syntax
                    LDA _ReadNum+0 PHS
                    LDA _ReadNum+1 PHS                           ; push destination
                    LDA PtrA+0 PHS SUW PtrB                      ; push source
                    LDA PtrA+1 PHS SUB PtrB+1 INW PtrB           ; B = B - A + 1
                    LDA PtrB+0 PHS LDA PtrB+1 PHS                ; push number of bytes
                    JPS OS_MemMove                               ; do not clean up the stack
                    JPA _Prompt

  sc_syntax:      LDI <sc_errortxt PHS LDI >sc_errortxt PHS JPS _Print
                  JPA _Prompt

  sc_errortxt:    'memmove <fst> <lst> <dst>', 10, 0

  MemmoveEnd:

; **********************************************************************************************************

'mon', 0, '               ', 0, MonStart, MonEnd-MonStart ; file header

  ; --------------------------------------------------
  ; Memory Monitor
  ; usage: "mon [hexaddr.hexaddr] <ENTER>"
  ; modifies: 0xff00, PtrA, PtrC, PtrD, PtrE = mode
  ; --------------------------------------------------
  MonStart:   LDI <montxt PHS LDI >montxt PHS JPS _Print PLS PLS   ; print start line
              LDI 0 STB PtrA+0 STB PtrA+1                 ; default memory address and bank

              LDR _ReadPtr CPI 10 BNE initline            ; use command line of mon

              JPS _SkipSpace JPS _ReadHex                 ; skip an optional start address
              LDA _ReadNum+2 CPI 0xf0 BEQ monline         ; wurde eine Zahl eingelesen?
                JPS mon_numtoa                            ; first address

  monline:    JPS mon_addr
              LDI ' ' OUT

              LDI <_ReadBuffer+5 STB _ReadPtr+0           ; parse fewer bytes due to "0000 _"
              LDI >_ReadBuffer+5 STB _ReadPtr+1
              JPS _ReadLine                               ; get a line of input until ENTER or end of input buffer

  initline:   LDI 0xf0 STB _ReadNum+2                     ; invalidate parsed number
              LDI 0 STB _ReadNum+0 STB _ReadNum+1         ; reset monitor
                    STB PtrE                              ; mode = 0

              LDR _ReadPtr CPI 10 BNE parsing
                JPA _Prompt                               ; FLASH off and back to OS

  parsing:    LDR _ReadPtr                                ; BYTE-BY-BYTE PARSING OF THE LINE INPUT BUFFER
              CPI ':' BEQ doppel                          ; : switch to 'deposit' mode
              CPI '.' BEQ punkt                           ; . switch to 'list' mode
              CPI 'a' BCS sletter                         ; a..f for hex numbers
              CPI '0' BCS szahl                           ; 0..9 for numbers
                LDA _ReadNum+2                            ; ALLES ANDERE IST "ENTER"
                CPI 0xf0 BNE doaction                     ; prüfe, ob valide parse-Daten vorliegen
  clrparsed:      LDI 0xf0 STB _ReadNum+2                 ; ***** ENDE DES PARSINGS (AUCH MEHRERER BYTES) *****
                  LDI 0 STB _ReadNum+0 STB _ReadNum+1
  parsed:         LDR _ReadPtr                            ; ENDE DES PARSINGS EINES BYTES
                  CPI 10 BEQ monline                      ; prüfe hier NOCHMAL auf ENTER wg. Zeilenende
                    INB _ReadPtr JPA parsing              ; gehe zum nächsten Zeichen des Puffers

  doppel:     LDI 1 JPA setmode                           ; : => umschalten auf DEPOSIT mode
  punkt:      LDI 2                                       ; . => umschalten auf LIST mode
  setmode:    STB PtrE                                    ; change mode
              LDA _ReadNum+2                              ; validen input vorhergehend . oder : als 'PtrA' übernehmen
              CPI 0xf0 BEQ clrparsed                      ; liegt kein valider input vor?
    setmemadr:  JPS mon_numtoa
                JPA clrparsed                             ; . : kam ohne valide Addresse davor

    sletter:  SUI 39                                      ; parse one byte normal hex input
    szahl:    SUI 48 PHS
              LLW _ReadNum RLB _ReadNum+2                 ; this automatically validates a parsed number
              LLW _ReadNum RLB _ReadNum+2                 ; shift existing hex data to the left
              LLW _ReadNum RLB _ReadNum+2
              LLW _ReadNum RLB _ReadNum+2
              PLS ADB _ReadNum                            ; add new hex nibble to the right
              JPA parsed

  doaction:     LDA PtrE                                  ; ***** ES LIEGT EIN VALIDES PARSE-DATUM VOR *****
                CPI 0 BEQ setmemadr                       ; mode=0 -> übernimm Daten als einfache neue PtrA
                CPI 1 BEQ mode_deposit                    ; mode=1 -> übernimm Daten als 'deposit'

  ; mode=2 -> Daten sind 'list until', print list
  startlistpage:  LDI 24 STB PtrC                         ; reuse as line counter
  startlistline:  LDI 16 STB 0xff00                       ; init 16-bytes counter
                  JPS mon_addr
                  LDI ' ' OUT
  nextlist:       LDR PtrA
                  PHS JPS _PrintHex PLS                   ; Speicherinhalt drucken
                  LDA PtrA+0
                  CPA _ReadNum+0
                  BNE listweiter
                    LDA PtrA+1
                    CPA _ReadNum+1
                    BNE listweiter
                      JPS mon_enter
                      JPA clrparsed
  listweiter:     INW PtrA
                  DEB 0xff00 BEQ lineend
                    LSL LSL LSL LSL LSL BNE nextlist
                      LDI ' ' OUT
                      JPA nextlist                        ; bug-fix by paulscottrobson Thank you!

  lineend:        JPS mon_enter
                  DEB PtrC BNE startlistline              ; reuse as line counter
                    WIN CPI 27 BNE startlistpage          ; warte auf Tastendruck
                      JPA clrparsed

  mode_deposit: LDA _ReadNum STR PtrA                     ; validen Daten -> deposit in RAM only
                INW PtrA JPA clrparsed

  mon_enter:    LDI 10 OUT                      ; ENTER
                RTS
  mon_addr:     LDA PtrA+1 PHS JPS _PrintHex PLS          ; Drucke aktuelle list-Adresse
                LDA PtrA+0 PHS JPS _PrintHex PLS
                RTS
  mon_numtoa:   LDA _ReadNum+0 STB PtrA+0                 ; valide Daten -> PtrA
                LDA _ReadNum+1 STB PtrA+1
                RTS

  montxt:       10, 'MONITOR (',39,':',39,' write, ',39,'.',39,' until)', 10, 0

  MonEnd:

; **********************************************************************************************************

'defrag', 0, '            ', 0, DefragStart, DefragEnd-DefragStart     ; file header

  ; ---------------------------------------------------------------
  ; Defragments the SSD by removing/formating/freeing deleted parts
  ; usage: "defrag <ENTER>"
  ; bis: pointer beyond processed FLASH area
  ; next: pointer beyond current file in FLASH
  ; nextsec: next 4KB sector to write data into
  ; ---------------------------------------------------------------
  DefragStart:    LDI 0 STB dg_next+0 STB dg_bis+0
                  LDI 0x10 STB dg_next+1 STB dg_bis+1
                  LDI 0x01 STB dg_nextsec                              ; set to start of user SSD

    dg_nextchunk: LDI 0x00 STB dg_ram+0 LDI 0xe0 STB dg_ram+1          ; reset RAM buffer pointer to buffer start
                  
    dg_biseqnext: LDA dg_bis+0 CPA dg_next+0 BNE dg_copyabyte          ; copyloop (bis = next?)
                    LDA dg_bis+1 CPA dg_next+1 BNE dg_copyabyte

                        ; current file was processed (read into RAM) completely => try fetching next file
                        LDA dg_next+0 STB PtrA+0                       ; bis now points beyond current file
                        LDA dg_next+1 STB PtrA+1
      dg_checknext:     LDR PtrA                                       ; READ BYTE AT "NEXT" LOCATION
                        CPI 0xff BEQ dg_endofused                      ; END OF USED SSD AREA REACHED?
                          PHS                                          ; NO! -> keep first byte for "0x00" check
                          LDI 22 ADW PtrA                              ; find file data bytesize
                          LDR PtrA STB PtrB+0 INW PtrA                 ; read bytesize -> PtrB
                          LDR PtrA STB PtrB+1 INW PtrA                 ; PtrA now point to data section
                          LDA PtrB+0 ADW PtrA+0                        ; add bytesize to get beyond file for new "next"
                          LDA PtrB+1 ADB PtrA+1                        ; PtrA points beyond current file!
                          LDA PtrA+0 STB dg_next+0                     ; WE HAVE AN (UNTESTED) NEXT FILE LOCATION
                          LDA PtrA+1 STB dg_next+1
                          PLS CPI 0x00 BNE dg_copythisfile             ; *bis != 0? Visible file that needs copying?
                            LDI '.' OUT                                ; invisible fragment!
                            LDA dg_next+0 STB dg_bis+0                 ; mark it as processed..
                            LDA dg_next+1 STB dg_bis+1                 ; .. without copying it to RAM
                            JPA dg_checknext                           ; go look for a non-deleted file
    dg_copythisfile:      LDI 'f' OUT                                  ; 'f' signals a visible file
                          JPA dg_biseqnext                             ; reenter copying loop

    dg_copyabyte: LDA dg_ram+1 CPI >0xf000 BCC dg_ramokay              ; is there enough space in RAM 0x8000..0xefff?
                    ; 4KB RAM buffer is full => byte cannot be read and written
                    JPS writeRAM LDA PtrB+1 CPI 0xff BEQ dg_nextchunk  ; formats and writes (unfinished) chunk

      dg_error:       LDI <dg_errtxt PHS LDI >dg_errtxt PHS JPS _Print ; error message
                      JPA _Prompt

    dg_ramokay:   ; read a byte from 'dg_bis' into dg_ram buffer
                  LDR dg_bis STR dg_ram                                ; read FLASH address and result store in RAM
                  INW dg_ram INW dg_bis JPA dg_biseqnext

; writes a max. 4KB sector of copied RAM data to start of FLASH sector 'dg_nextsec'
; PtrB+0..1: RAM bytesize (PtrB+1 = 0xff signals SUCCESS)
; PtrC+0..1: RAM source addr
  writeRAM:     LDA dg_nextsec LSL LSL LSL LSL                         ; dg_nextsec<<12 = 
                STB PtrA+1 LDI 0 STB PtrA+0                            ; start address of next sector
                LDI <0xe000 STB PtrC+0 LDI >0xe000 STB PtrC+1          ; PtrC = RAM source
                LDA dg_ram+0 STB PtrB+0                                ; dg_ram = PtrB = bytesize
                LDI 0xe0 SUB dg_ram+1 STB PtrB+1                       ; = RAM "beyond" pointer - RAM "start"
                DEW dg_ram BCS dg_arebytes                             ; check for bytes in the buffer
                  STB PtrB+1 RTS                                       ; return "success" since nothing was written
  dg_arebytes:  LDI '#' OUT                                            ; indicate FLASH sector write
                JPS OS_FlashErase                                      ; erase this FLASH bank
                JPS OS_FlashWrite                                      ; write RAM chunk to FLASH (sets PtrB+1=0xff for SUCCESS)
                INB dg_nextsec                                         ; advance to next free sector
                RTS

; end of used SSD area => write the rest of RAM buffer to FLASH, format the freed banks
  dg_endofused: JPS writeRAM LDA PtrB+1 CPI 0xff BNE dg_error          ; formats and writes (unfinished) chunk
                  DEW dg_bis
                  LSL LSR LSR LSR LSR LSR STB dg_bis                   ; find final used sector
    dg_laloop:    LDA dg_nextsec CPA dg_bis BCC dg_format BEQ dg_format ; branch on less or equal
                    LDI 10 OUT JPA _Prompt                   ; END OF DEFRAG
    dg_format:    LSL LSL LSL LSL STB PtrA+1 JPS OS_FlashErase         ; format freed sector
                  LDI '-' OUT
                  INB dg_nextsec JPA dg_laloop

  dg_errtxt:    10, 'ERROR', 10, 0

  DefragEnd:

; **********************************************************************************************************

'receive', 0, '           ', 0, ReceiveStart, ReceiveEnd-ReceiveStart     ; file header

  ; -----------------------------------------------
  ; usage: "receive <ENTER>", then paste a HEX file
  ; modifies: PtrA, PtrB, PtrC
  ; -----------------------------------------------
  ReceiveStart:   LDI 0xfe STB 0xffff
                  LDI <hl_starttext PHS LDI >hl_starttext PHS
                  JPS _Print PLS PLS
                  LDI >_ReadBuffer STB PtrA+1 STB _ReadPtr+1
                  LDI 0xff STB hl_first+0 STB hl_first+1
                  LDI 0 STB hl_errors+0 STB hl_errors+1          ; clear number of errors
                        STB hl_last+0 STB hl_last+0

  hl_readline:    LDI <_ReadBuffer STB PtrA+0 STB _ReadPtr+0
  hl_readchar:    WIN CPI 27 BEQ _Prompt
                    STR PtrA                                     ; store char ggfs. out
                    CPI 10 BEQ hl_scanforhex                     ; end of the line?
                      INB PtrA JPA hl_readchar                   ; look for more (just write CR=13 here)

  hl_scanforhex:  LDR _ReadPtr
                  CPI ':' BEQ hl_validline
                    CPI 10 BEQ hl_readline                       ; ignore CR=13
                      INB _ReadPtr JPA hl_scanforhex

  hl_validline:   INB _ReadPtr                                   ; move over ':'
                  JPS hl_ReadHexByte                             ; parse number of data bytes
                  STB hl_numbytes STB hl_checksum                ; store number, init line checksum
                  JPS hl_ReadHexByte STB PtrB+1 ADB hl_checksum  ; parse 16-bit address
                  JPS hl_ReadHexByte STB PtrB+0 ADB hl_checksum
                  JPS hl_ReadHexByte                             ; parse record type
                  CPI 0x01 BEQ hl_endoffile
                    CPI 0x00 BNE hl_countaserr                   ; only allow DATA type 0x00 here
                      DEB hl_numbytes BCC hl_alllineread         ; > 0 bytes to process?
                        LDA hl_first+1 CPI 0xff BNE hl_dataloop  ; store target address as "first" if still invalid
                          LDA PtrB+0 STB hl_first+0
                          LDA PtrB+1 STB hl_first+1
  hl_dataloop:          JPS hl_ReadHexByte STR PtrB ADB hl_checksum
                        INW PtrB DEB hl_numbytes BCS hl_dataloop
                          LDA PtrB+1 STB hl_last+1
                          LDA PtrB+0 STB hl_last+0

  hl_alllineread:         JPS hl_ReadHexByte ADB hl_checksum     ; read the checksum at the end
                          CPI 0x00 BEQ hl_readline               ; no errors? -> goto next line
  hl_countaserr:            INW hl_errors JPA hl_readline        ; go read the next line even with errors

  hl_endoffile:   ADB hl_checksum                                ; add record type that was already read
                  JPS hl_ReadHexByte ADB hl_checksum BNE hl_haveerrors   ; errors in last checksum?
                    LDA hl_errors+0 CPI 0 BNE hl_haveerrors      ; .. or in any line of the file?
                    LDA hl_errors+1 CPI 0 BEQ hl_alldone         ; .. or in any line of the file?
  hl_haveerrors:  LDI <hl_errortext PHS LDI >hl_errortext PHS    ; output the number of errors that occured
                  JPS _Print PLS PLS
                  LDA hl_errors+1 PHS JPS _PrintHex PLS          ; output number of errors
                  LDA hl_errors+0 PHS JPS _PrintHex PLS
                  JPA hl_exit

  hl_alldone:     LDA hl_first+1 CPI 0x80 BCS hl_noflash       ; test for valid RAM address
                    LDI <hl_errorflash PHS
                    LDI >hl_errorflash PHS
                    JPS _Print PLS PLS
                    JPA _Prompt  
  hl_noflash:     LDI <hl_ramarea PHS LDI >hl_ramarea PHS
                  JPS _Print PLS PLS
                  LDA hl_first+1 PHS JPS _PrintHex PLS
                  LDA hl_first+0 PHS JPS _PrintHex PLS
                  LDI 32 OUT
                  DEW hl_last
                  LDA hl_last+1 PHS JPS _PrintHex PLS
                  LDA hl_last+0 PHS JPS _PrintHex PLS
  hl_exit:        LDI 10 OUT                                     ; ENTER
                  JPA _Prompt

  ; *****************************************************************
  ; Parse a two digit HEX number
  ; *****************************************************************
  hl_ReadHexByte:   LDR _ReadPtr SUI 48
                    CPI 17 BCC hl_gotfirst
                      SUI 7
    hl_gotfirst:    LSL LSL LSL LSL STB hl_hexresult                         ; store upper nibble
                    INB _ReadPtr
                    LDR _ReadPtr SUI 48
                    CPI 17 BCC hl_gotsecond
                      SUI 7
    hl_gotsecond:   ADB hl_hexresult                             ; add lower nibble
                    INB _ReadPtr
                    LDA hl_hexresult                             ; return full byte value in A
                    RTS

  hl_starttext:     'Waiting for HEX (ESC)', 10, 0
  hl_errortext:     'Checksum errors ', 0
  hl_errorflash:    'Expecting RAM data', 10, 0
  hl_ramarea:       'Data written to ', 0

  ReceiveEnd:

; **********************************************************************************************************

0, '                  ', 0, 0x0000, 0x1000-*-2                   ; dummy file filling up rest of the bank

; ----------------------------------------------------------------------
; Writes a byte to FLASH after the appropriate sequence has already been initiated.
; This routine will be copied and executed from RAM.
; push: byte to write, byte to check
; pull: #, #
; ----------------------------------------------------------------------
flashcode:
  #mute #org 0xfea0 #emit
  OS_FlashByte: LDS 4 STR PtrA                   ; write a byte
  check_loop:   LDS 3 CPR PtrA BNE check_loop    ; check result
                  RTS
  FlashByteEnd:

; **********************************************************************************************************

#mute #org 0xfea0                      ; GLOBAL OS LABELS AND CONSTANTS

                0,0,0,0,0,0,0,0        ; FLASH write routine will be copied here (14 bytes)
                0,0,0,0,0,0,0,0        
PtrA:           0xffff, 0xff           ; lokaler pointer (3 bytes) used for FLASH addr and bank
PtrB:           0xffff, 0xff           ; lokaler pointer (3 bytes)
PtrC:           0xffff, 0xff           ; lokaler pointer (3 bytes)
PtrD:                                  ; lokaler pointer (2 bytes)
hl_hexresult:   0xff
hl_checksum:    0xff
PtrE:                                  ; lokaler pointer (2 bytes)
dg_ram:                                ; pointer to next free pos in RAM buffer
hl_numbytes:    0xffff
PtrF:                                  ; lokaler pointer (2 bytes)
dg_bis:                                ; pointer to last read location of FLASH
hl_errors:      0xffff
PtrG:                                  ; lokaler pointer (2 bytes)
hl_first:
dg_next:        0xffff                 ; pointer beyond current file's FLASH area
PtrH:                                  ; lokaler pointer (2 bytes)
hl_last:
dg_nextsec:     0xffff                 ; next 4KB FLASH sector
PtrP:           0xffff                 ; lokaler pointer (2 bytes) used by all OS print routines

_RandomState:   0xff, 0xff, 0xff, 0xff ; 4-byte storage (x, a, b, c) state of the pseudo-random generator
_ReadPtr:       0xffff                 ; Zeiger (2 bytes) auf das letzte eingelesene Zeichen (to be reset at startup)
_ReadNum:       0xffff, 0xff           ; 3-byte storage for parsed 16-bit number, MSB: 0xf0=invalid, 0x00=valid
_ReadBuffer:    0,0,0,0,0,0,0,0,0,0    ; OS read buffer (50 bytes = input line)
                0,0,0,0,0,0,0,0,0,0
                0,0,0,0,0,0,0,0,0,0
                0,0,0,0,0,0,0,0,0,0
                0,0,0,0,0,0,0,0,0
ReadLast:       0                      ; last byte of read buffer
