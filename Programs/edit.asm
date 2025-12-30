; ---------------------------------------------------
;
;      Text Editor for the 'Minimal Tiny Computer'
;
; written by Carsten Herting - last update 03.01.2024
;
; ---------------------------------------------------

; LICENSING INFORMATION
; This file is free software: you can redistribute it and/or modify it under the terms of the
; GNU General Public License as published by the Free Software Foundation, either
; version 3 of the License, or (at your option) any later version.
; This file is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
; General Public License for more details. You should have received a copy of the GNU General
; Public License along with this program. If not, see https://www.gnu.org/licenses/.

; Note: The maximum text filesize is 12KB. The maximum receivable (Ctrl+R) filesize, however, is only
; 6KB, since the data is read into the copy/paste buffer and only then copied into the existing file.

#org 0x8000

Editor:       LDI 0xfe STB 0xffff                   ; init the stack pointer

              LDA iscoldstart CPI 1 BNE warmstart   ; is it a warmstart?
                LDI 0 STB iscoldstart STB data      ; invalidate text data only at first (cold) start
                LDI <copybuf STB copyptr+0          ; invalidate copied data
                LDI >copybuf STB copyptr+1
                JPS clrnameptr
                LDI 0 STB namebuf                   ; invalidate filename

  warmstart:  JPS _SkipSpace                        ; parse command line, skip spaces after 'edit/run   filename'
              LDR _ReadPtr CPI 33 BCC mainload      ; FILENAME following 'edit' in command line?
                LDA _ReadPtr+0 STB strcpy_s+0       ; prepare copy of filename into buffer
                LDA _ReadPtr+1 STB strcpy_s+1
                LDI <namebuf STB strcpy_d+0
                LDI >namebuf STB strcpy_d+1
                JPS _LoadFile                       ; load it with filename in _ReadPtr
                CPI 1 BEQ loaddone                  ; everything okay?
                  LDI '?' OUT
                  LDI 10 OUT                        ; ENTER
                  JPA _Prompt
    loaddone:   LDI 0 STR _ReadPtr                  ; LOADED! => truncate rest of command line
                JPS strcpy                          ; copy filename into name buffer
                LDA strcpy_d+0 STB nameptr+0        ; copy nameptr (pointing to end of file)
                LDA strcpy_d+1 STB nameptr+1

; ------------------------------------------------------------

  mainload:   LDI >data STB cptr+1 STB tptr+1
              LDI 0 STB markptr+0 STB markptr+1     ; jump here after LOAD
                    STB cptr+0 STB tptr+0           ; init cursor to top of file
                    STB yorg+1 STB changed
                    STB xcur STB ycur STB xorg
              LDI 1 STB yorg+0
              JPS pullline
              LDI 2 STB redraw                      ; redraw 2: all

  mainclear:  LDI 0 STB state

  mainloop:   LDA state CPI 0 BEQ StateChar         ; handles normal character input
              CPI 'N' BEQ StateNew                  ; process according to state
              CPI 'L' BEQ StateLoad
              CPI 'S' BEQ StateSave
                JPA StateReceive                    ; must be 'R'

; ------------------------------------------------------------
; SCREEN REFRESH
; ------------------------------------------------------------

Update:       LDA xcur SUI <width-5 BCC ud_usexorg        ; check whether cursor has moved outside of viewport
                CPA xorg BCS ud_usexmin
  ud_usexorg: LDA xorg
  ud_usexmin: CPA xcur BCC ud_useit
                LDA xcur
  ud_useit:   CPA xorg BEQ ud_notnew
                STB xorg
                JPS pushline
                LDI 2 STB redraw

  ud_notnew:  LDA redraw                            ; switch "redraw"
              CPI 1 BEQ ud_lindraw
                CPI 2 BEQ ud_alldraw
  ud_return:      LDI 0 STB redraw
                  LDA state CPI 0 BNE ud_exit       ; state != 0 ? exit!
                    LDA ycur PHS JPS _CursorY PLS   ; state = 0 => place cursor
                    LDA xcur SUA xorg ADI 4
                    PHS JPS _CursorX PLS
  ud_exit:          RTS

; ------------------------------------------------------------

  ud_alldraw:   JPS Clear                               ; REDRAW = 2: CLEAR AND DRAW WHOLE SCREEN
                LDA yorg+0 STB pu_len+0
                LDA yorg+1 STB pu_len+1
                JPS PrintLine                           ; print first line number
                LDI 0 STB pc_n                          ; count drawn rows
                LDA tptr+0 STB pc_ptr+0 STB pc_sptr+0   ; init ptr and start ptr to beginning of top line
                LDA tptr+1 STB pc_ptr+1 STB pc_sptr+1
  ud_while:     LDA pc_n CPI <height BCS ud_return
                LDR pc_ptr CPI 0 BEQ ud_return
                  CPI 10 BNE ud_notret
                    LDA pc_n CPI <height-1 BCS ud_dontprint    ; it was ENTER
                      LDI 10 OUT              ; print ENTER
                      LDA yorg+0 STB pu_len+0
                      LDA yorg+1 STB pu_len+1
                      LDA pc_n INC ADW pu_len
                      JPS PrintNext                     ; print consecutive line number
  ud_dontprint:       INW pc_ptr
                      LDA pc_ptr+0 STB pc_sptr+0
                      LDA pc_ptr+1 STB pc_sptr+1
                      INB pc_n JPA ud_while
  ud_notret:      LDA pc_ptr+0 SUA pc_sptr+0            ; sptr: line start, ptr: current pos in line
                  SUA xorg BCC ud_notprint              ; skip chars left of visible area
                    SUI <width-4 BCS ud_notprint         ; only print pos [xorg..xorg + width-4-1]
                      LDR pc_ptr OUT                    ; print visible char
  ud_notprint:    INW pc_ptr JPA ud_while               ; advance position and continue
                  
; ------------------------------------------------------------

  ud_lindraw:   LDA ycur PHS JPS _CursorY PLS           ; REDRAW = 1: CLEAR AND DRAW CURRENT LINE ONLY
                JPS ClearRow
                LDA yorg+0 STB pu_len+0
                LDA yorg+1 STB pu_len+1
                LDA ycur ADW pu_len
                JPS PrintLine                           ; reprint current line number
                LDI <line STB pc_ptr+0 LDI >line STB pc_ptr+1 ; ptr = current line position
  ud_while2:    LDR pc_ptr CPI 0 BEQ ud_return
                  CPI 10 BEQ ud_return
                    LDA pc_ptr+0
                    SUA xorg BCC ud_notprnt
                      SUI <width-4 BCS ud_notprnt
                        LDR pc_ptr OUT                  ; output the line haracter
  ud_notprnt:   INB pc_ptr+0 JPA ud_while2

; ------------------------------------------------------------
; INPUT HANDLER FOR CHARACTER INPUT (INCLUDING ESC SEQUENCES)
; ------------------------------------------------------------

StateChar:  JPS Update                                 ; check if redraw is needed but always draw cursor
            WIN CPI 27 BEQ sc_IsESC                    ; handle multi-char ESC sequences efficiently
              CPI 127 BEQ pc_Delete BCS mainloop       ; discard chars >127
                CPI 32 BCS pc_default                  ; printable chars (including LF) are handled by "pc_default"
                CPI 10 BEQ pc_default                  ; ENTER (LF)
                CPI 9 BNE sc_nonprint                  ; TAB
                  LDI 32 JPA pc_default                ; convert to SPACE
  sc_nonprint:  CPI 8 BEQ pc_BackSp                    ; CHECK FOR NON-PRINTABLES <32
                CPI 0x01 BEQ pc_CtrlA
                CPI 0x18 BEQ pc_CtrlX
                CPI 0x03 BEQ pc_CtrlC
                CPI 0x16 BEQ pc_CtrlV
                CPI 0x0c BEQ pc_CtrlL
                CPI 0x13 BEQ pc_CtrlS
                CPI 0x0e BEQ pc_CtrlN
                CPI 0x12 BEQ pc_CtrlR
                CPI 0x14 BEQ pc_CtrlT
                CPI 0x11 BEQ pc_CtrlQ
                  JPA mainloop                         ; discard other characters, e.g. CR

  sc_IsESC: WIN CPI '[' BNE mainloop WIN               ; ESC handler: assert "[" or ignore and go back to state = 0
              CPI 'A' BEQ pc_Up
              CPI 'B' BEQ pc_Down
              CPI 'D' BEQ pc_Left
              CPI 'C' BEQ pc_Right
              CPI 'H' BEQ pc_Pos1                      ; is used on Linux (see below for Windows)
              CPI 'F' BEQ pc_End
              CPI '5' BEQ pc_PgUp
              CPI '6' BEQ pc_PgDown
                STB 0xff00 WIN                         ; possible ~ sequence: remember current char
                CPI '~' BNE mainloop                   ; test for ~
                  LDA 0xff00
                  CPI '1' BEQ pc_Pos1                  ; 1~ is used on Windows (see above for Linux)
                  CPI '4' BEQ pc_End                   ; 4~
                  CPI '5' BEQ pc_PgUp                  ; 5~
                  CPI '6' BEQ pc_PgDown                ; 6~
                    JPA mainloop

; ------------------------------------------------------------

  pc_CtrlA:     LDA tptr+0 STB marktptr+0          ; save tptr
                LDA tptr+1 STB marktptr+1
                LDA yorg+0 STB markyorg+0          ; save yorg
                LDA yorg+1 STB markyorg+1
                LDA cptr+0 STB markptr+0           ; save "cptr + xcur" as cursor pos in text
                LDA cptr+1 STB markptr+1
                LDA xcur STB markx ADW markptr     ; save cursor x/y
                LDA ycur STB marky
                JPA mainloop

; ------------------------------------------------------------

  pc_CtrlX:     JPS pushline                             ; update data with current line
                JPS CopyMarked
                LDI <copybuf CPA copyptr+0 BNE pc_n42ok
                  LDI >copybuf CPA copyptr+1 BEQ mainloop ; has something been saved?
    pc_n42ok:       LDA cptr+0 STB pc_sptr+0
                    LDA cptr+1 STB pc_sptr+1
                    LDA xcur ADW pc_sptr                 ; pc_sptr = current cursor position
                    LDI 0 PHS LDA pc_sptr+0 PHS LDA pc_sptr+1 PHS
                    JPS length PLS                       ; get rest length of text to shift
                    PLS STB pc_ptr+1 PLS STB pc_ptr+0 INW pc_ptr  ; pc_ptr = number of bytes to shift (incl. 0)
                    LDA markptr+0 PHS LDA markptr+1 PHS  ; push destination
                    LDA pc_sptr+0 PHS LDA pc_sptr+1 PHS  ; push sources
                    LDA pc_ptr+0 PHS LDA pc_ptr+1 PHS    ; push number of bytes to move
                    JPS _MemMove LDI 6 ADB 0xffff        ; move and clean up stack
                LDA marktptr+0 STB tptr+0 LDA marktptr+1 STB tptr+1
                LDA markyorg+0 STB yorg+0 LDA markyorg+1 STB yorg+1
                LDA markptr+0 STB cptr+0 LDA markptr+1 STB cptr+1
                LDA markx STB xcur SUW cptr LDA marky STB ycur
  pc_reuse2:    JPS pullline
                LDI 2 STB redraw
                JPA mainloop

; ------------------------------------------------------------

  pc_CtrlC:     JPS pushline
                JPS CopyMarked
                JPA mainloop

; ------------------------------------------------------------

  pc_CtrlV:     JPS pushline

                LDA cptr+0 STB pc_ptr+0 LDA cptr+1 STB pc_ptr+1
                LDA xcur ADW pc_ptr                   ; pc_ptr = address of current cursor position in the line
                LDI 0 PHS                             ; find remaining data length until (and excluding here) 0
                LDA pc_ptr+0 STB pc_dptr+0 PHS        ; pc_dptr = source for move, destination for insert
                LDA pc_ptr+1 STB pc_dptr+1 PHS
                JPS length PLS
                PLS STB pc_sptr+1 PLS STB pc_sptr+0   ; pc_sptr = bytesize of remaining text that needs shifting
                INW pc_sptr                           ;           increase for zero-terminator!

                LDI <copybuf STB pu_len+0              ; pu_len = bytesize of clipboard data
                LDI >copybuf STB pu_len+1
                LDA copyptr+1 SUB pu_len+1
                LDA copyptr+0 SUW pu_len

                LDA pu_len+1 ADB pc_ptr+1 LDA pu_len+0 ADW pc_ptr+0    ; pc_ptr = move destination address
                LDA pc_ptr+0 PHS LDA pc_ptr+1 PHS     ; push move destination
                LDA pc_dptr+0 PHS LDA pc_dptr+1 PHS   ; push move source
                LDA pc_sptr+0 PHS LDA pc_sptr+1 PHS   ; push move anzahl
                JPS _MemMove LDI 6 ADB 0xffff         ; move exisiting text to pc_ptr

                ; move the reverse stuff into text, update cptr, xcur, ycur, yorg and tptr char by char

                LDI <copybuf STB pc_ptr+0              ; pc_ptr = source at top of clipboard
                LDI >copybuf STB pc_ptr+1
  ctrlv_loop:   DEW pu_len BCC pc_reuse2              ; insert the backwards clipboard and *** exit *** here
                  LDR pc_ptr STR pc_dptr              ; copy a char starting at cursor pos
                  CPI 10 BEQ pc22enter                ; was it an ENTER?
                    INB xcur JPA pc22done             ; no ENTER -> only move cursor left
    pc22enter:    LDI 0 STB xcur                      ; ENTER-Event!
                  LDA cptr+0 PHS LDA cptr+1 PHS
                  JPS getnext                         ; modifies pc_sptr!!!
                  PLS STB cptr+1 PLS STB cptr+0
                  LDA ycur CPI <height-1 BCS pc22doorg       ; was cursor at the bottom?
                    INB ycur JPA pc22done             ; no bottom -> only move cursor down
    pc22doorg:    INW yorg                            ; move origin down
                  LDA tptr+0 PHS LDA tptr+1 PHS
                  JPS getnext                         ; modifies pc_sptr!!!
                  PLS STB tptr+1 PLS STB tptr+0
    pc22done:     INW pc_dptr DEW pc_ptr              ; advance to next char, got down clipboard data
                  JPA ctrlv_loop

; ------------------------------------------------------------

  pc_CtrlQ:     JPS pushline
                JPS Clear
                JPA _Prompt

; ------------------------------------------------------------

  pc_CtrlR:     LDI 'R' STB state               ; RECEIVE
                LDI <copybuf STB copyptr+0      ; reset copyptr
                LDI >copybuf STB copyptr+1
                LDI 0 PHS JPS _CursorY PLS
                JPS ClearRow
                LDI <receivestr PHS LDI >receivestr PHS
                JPS _Print PLS PLS
                JPA mainloop

; ------------------------------------------------------------

  pc_CtrlT:     JPS Clear                       ; TRANSMIT FILE
                LDI <data PHS LDI >data PHS     ; print out the whole file via UART
                JPS _Print PLS PLS
                WIN JPA rd2mainclear

; ------------------------------------------------------------

  pc_CtrlL:     LDI 'L' STB state               ; change state to "L"
                LDI <loadstr PHS LDI >loadstr PHS
  pc_reuseL1:   JPS pushline                    ; from here on same for "L and "S"
                LDI 0 PHS JPS _CursorY PLS
                JPS ClearRow                    ; clears both X and Y position
                JPS _Print PLS PLS              ; print pushed command descriptor
                LDI <namebuf PHS LDI >namebuf PHS
  pc_reuseL2:   JPS _Print PLS PLS              ; print filename
                JPA mainloop

; ------------------------------------------------------------

  pc_CtrlS:     LDI 'S' STB state
    pc_AbortS:  LDI <savestr PHS LDI >savestr PHS     ; print "SAVE"
                JPA pc_reuseL1

; ------------------------------------------------------------

  pc_CtrlN:     LDI 'N' STB state                     ; new file
                LDI <newstr PHS LDI >newstr PHS
                JPS pushline
                LDI 0 PHS JPS _CursorY PLS
                JPS ClearRow
                JPA pc_reuseL2

; ------------------------------------------------------------

  pc_BackSp:    LDI 0 STB markptr+0 STB markptr+1
                LDI 1 STB redraw
                LDA xcur CPI 0 BEQ pc_8else
                  DEB xcur PHS
                  LDI >line PHS                             ; case "xcur > 0"
                  JPS cutchar PLS PLS
                  LDI 1 STB changed
                  JPA mainloop
    pc_8else:   LDA cptr+0 PHS LDA cptr+1 PHS JPS getprev   ; case "xcur = 0"
                PLS STB pc_ptr+1 PLS STB pc_ptr+0           ; pc_ptr = prev
                CPA cptr+0 BNE pc_8if
                  LDA pc_ptr+1 CPA cptr+1 BEQ mainloop
    pc_8if:         JPS pushline                            ; prev != cptr -> alles okay
                    LDI 10 PHS LDA pc_ptr+0 PHS LDA pc_ptr+1 PHS
                    JPS length PLS PLS PLS STB pc_n         ; pc_n = length of prev exclusive return
                    LDI 10 PHS LDA cptr+0 PHS LDA cptr+1 PHS
                    JPS length PLS PLS PLS                  ; length of cptr exclusive return
                    ADA pc_n BCS mainloop CPI 254 BCS mainloop ; test l0 + l1 < 254
                      DEW cptr
                      LDA cptr+0 PHS LDA cptr+1 PHS
                      JPS cutchar PLS PLS
                      DEB ycur
                      LDA pc_n STB xcur
                      LDA pc_ptr+0 STB cptr+0 LDA pc_ptr+1 STB cptr+1   ; cptr = prev
                      JPA pc_reuse2

; ------------------------------------------------------------

  pc_Delete:    LDI 0 STB markptr+0 STB markptr+1
                LDA xcur STB pc_dptr+0
                LDI >line STB pc_dptr+1 
                LDR pc_dptr CPI 10 BNE pc_127else
                  JPS pushline                        ; case delete an '\n'
                  LDA cptr+0 PHS LDA cptr+1 PHS JPS getnext
                  PLS STB pc_ptr+1 PLS STB pc_ptr+0   ; pc_ptr = next
                  LDI 10 PHS LDA pc_ptr+0 PHS LDA pc_ptr+1 PHS
                  JPS length PLS PLS PLS STB pc_n     ; length of next line
                  LDI 10 PHS LDA cptr+0 PHS LDA cptr+1 PHS
                  JPS length PLS PLS PLS              ; length of this line
                  ADA pc_n BCS mainloop CPI 254 BCS mainloop
                    DEW pc_ptr                        ; next - 1
                    LDA pc_ptr+0 PHS LDA pc_ptr+1 PHS
                    JPS cutchar PLS PLS
                    JPA pc_reuse2
    pc_127else: CPI 0 BEQ mainloop                    ;  do not cut the very last zero in the file
                  LDA xcur PHS LDI >line PHS
                  JPS cutchar PLS PLS
                  LDI 1 STB changed
                  LDI 1 STB redraw
                  JPA mainloop

; ------------------------------------------------------------

  pc_default: STB 0xff00                              ; store new character in X
              LDI 0 PHS JPS linelength
              PLS CPI 254 BCS mainloop
                STB pc_sptr+0 INC STB pc_dptr+0        ; pc_dptr = destination = pc_sptr + 1 (one beyond)
                LDI >line STB pc_sptr+1 STB pc_dptr+1

    pc_forlp: LDA pc_sptr+0 CPA xcur BCC pc_endf       ; shift the line content to the right including xcur index
                LDR pc_sptr STR pc_dptr
                DEB pc_dptr+0 DEB pc_sptr+0 BCS pc_forlp
    pc_endf:  LDI 1 STB changed                        ; mark es changed
              LDA 0xff00 STR pc_dptr                   ; now put in the new character
              CPI 10 BNE pc_not10
                JPS pushline
                LDA cptr+0 PHS LDA cptr+1 PHS JPS getnext   ; cptr = getnext(cptr)
                PLS STB cptr+1 PLS STB cptr+0
                LDA ycur CPI <height-1 BCS pc_bottom
                  INB ycur
                  JPA pc_daswars
    pc_bottom:  LDA tptr+0 PHS LDA tptr+1 PHS JPS getnext   ; tptr = getnext(tptr)
                PLS STB tptr+1 PLS STB tptr+0
                INW yorg
    pc_daswars: JPS pullline
                LDI 0 STB xcur LDI 2 ; draw all
                JPA pc_dend
    pc_not10: INB xcur LDI 1      ; draw line
    pc_dend:  STB redraw
              LDI 0 STB markptr+0 STB markptr+1
              JPA mainloop

; ------------------------------------------------------------

  pc_Up:        JPS pushline
                LDA cptr+0 PHS LDA cptr+1 PHS JPS getprev
                PLS STB pc_ptr+1 PLS STB pc_ptr+0
                CPA cptr+0 BNE csi_ain
                  LDA pc_ptr+1 CPA cptr+1 BEQ mainclear               ; leave if
    csi_ain:        LDA pc_ptr+0 STB cptr+0 LDA pc_ptr+1 STB cptr+1
                      LDA ycur CPI 0 BEQ csi_aelse
                        DEB ycur
                        JPA csi_bweiter
    csi_aelse:        LDA tptr+0 PHS LDA tptr+1 PHS JPS getprev
                      PLS STB tptr+1 PLS STB tptr+0
                      JPS ScrollDn
                      DEW yorg
                      LDI 1 STB redraw
                      JPA csi_bweiter

; ------------------------------------------------------------

  pc_Down:      JPS pushline
                LDA cptr+0 PHS LDA cptr+1 PHS JPS getnext
                PLS STB pc_ptr+1 PLS STB pc_ptr+0
                CPA cptr+0 BNE csi_bin
                  LDA pc_ptr+1 CPA cptr+1 BEQ mainclear               ; leave if
    csi_bin:        DEW pc_ptr LDR pc_ptr CPI 10 BNE mainclear
                      INW pc_ptr
                      LDA pc_ptr+0 STB cptr+0 LDA pc_ptr+1 STB cptr+1
                      LDA ycur CPI <height-1 BCS csi_belse
                        INB ycur
                        JPA csi_bweiter
    csi_belse:        LDA tptr+0 PHS LDA tptr+1 PHS JPS getnext
                      PLS STB tptr+1 PLS STB tptr+0
                      JPS ScrollUp
                      INW yorg
                      LDI 1 STB redraw
    csi_bweiter:      JPS pullline
                      LDI 10 PHS JPS linelength
                      PLS CPA xcur BCS mainclear
                        STB xcur JPA mainclear

; ------------------------------------------------------------

  pc_Left:      LDA xcur CPI 0 BEQ csi_delse
                  DEB xcur JPA mainclear
    csi_delse:  LDA cptr+0 CPI <data BNE csi_din
                  LDA cptr+1 CPI >data BEQ mainclear
      csi_din:      JPS pushline
                    LDA cptr+0 PHS LDA cptr+1 PHS JPS getprev
                    PLS STB pc_ptr+1 PLS STB pc_ptr+0
                    CPA cptr+0 BNE csi_din2
                      LDA pc_ptr+1 CPA cptr+1 BEQ mainclear           ; leave if
        csi_din2:       LDA pc_ptr+0 STB cptr+0 LDA pc_ptr+1 STB cptr+1
                        LDA ycur CPI 0 BEQ csi_delse2
                          DEB ycur
                          JPA csi_dweiter
        csi_delse2:     LDA tptr+0 PHS LDA tptr+1 PHS JPS getnext
                        PLS STB tptr+1 PLS STB tptr+0
                        JPS ScrollDn
                        DEW yorg
                        LDI 1 STB redraw
        csi_dweiter:    JPS pullline
                        LDI 10 PHS JPS linelength
                        PLS STB xcur JPA mainclear

; ------------------------------------------------------------

  pc_Right:     LDI 10 PHS JPS linelength
                PLS CPA xcur BCC csi_celse BEQ csi_celse
                  INB xcur JPA mainclear
    csi_celse:  STB pc_ptr+0
                LDI >line STB pc_ptr+1
                LDR pc_ptr CPI 0 BEQ mainclear
                  JPS pushline
                  LDA cptr+0 PHS LDA cptr+1 PHS JPS getnext
                  PLS STB pc_ptr+1 PLS STB pc_ptr+0
                  CPA cptr+0 BNE csi_cin
                    LDA pc_ptr+1 CPA cptr+1 BEQ mainclear
      csi_cin:        DEW pc_ptr LDR pc_ptr CPI 10 BNE mainclear
                        INW pc_ptr
                        LDA pc_ptr+0 STB cptr+0 LDA pc_ptr+1 STB cptr+1
                        LDA ycur CPI <height-1 BCS csi_celse2
                          INB ycur
                          JPA csi_cweiter
      csi_celse2:       LDA tptr+0 PHS LDA tptr+1 PHS JPS getnext
                        PLS STB tptr+1 PLS STB tptr+0
                        JPS ScrollUp
                        INW yorg
                        LDI 1 STB redraw
      csi_cweiter:      JPS pullline
        csi_reuse:      LDI 0 STB xcur
                        JPA mainclear

; ------------------------------------------------------------

  pc_Pos1:        JPA csi_reuse

; ------------------------------------------------------------

  pc_End:         LDI 10 PHS JPS linelength
                  PLS STB xcur JPA mainclear

; ------------------------------------------------------------

  pc_PgUp:        LDA cptr+1 CPI >data BNE pp5_noteq            ; if (cptr == data) break;
                    LDA cptr+0 CPI <data BEQ mainclear          ; quick exit when already up
    pp5_noteq:        JPS pushline
                  LDA tptr+1 CPI >data BNE pp5_else             ; if (tptr == data) ...
                    LDA tptr+0 CPI <data BNE pp5_else
                      STB cptr+0 LDA tptr+1 STB cptr+1          ; ... { cptr = tptr; ycur = 0; }
                      LDI 0 STB ycur JPA pp5_pullout
    pp5_else:     LDI 0 STB pc_n
    pp5_loop:     LDA tptr+0 PHS LDA tptr+1 PHS JPS getprev
                  LDS 2 CPA tptr+0 BNE pp5_lpnoteq
                    LDS 1 CPA tptr+1 BEQ pp5_lpout
    pp5_lpnoteq:      PLS STB tptr+1 PLS STB tptr+0
                      DEW yorg
                      INB pc_n CPI <height BCC pp5_loop
    pp5_lpout:    LDA ycur ADA pc_n SUI <height BCS pp5_ispos
                    LDI 0
    pp5_ispos:    STB ycur
                  LDA tptr+1 STB cptr+1 LDA tptr+0 STB cptr+0   ; cptr = tptr
                  LDI 0 STB pc_n                                      ; for (int i=0; i<ycur; i++) cptr = getnext(cptr);
    pp5_for:      LDA pc_n CPA ycur BCS pp5_pullout
                    LDA cptr+0 PHS LDA cptr+1 PHS JPS getnext
                    PLS STB cptr+1 PLS STB cptr+0
                    INB pc_n JPA pp5_for
    pp5_pullout:        JPS pullline
                        LDI 10 PHS JPS linelength
                        PLS CPA xcur BCC pp5_useit
                          LDA xcur
          pp5_useit:    STB xcur
          rd2mainclear: LDI 2 STB redraw JPA mainclear

; ------------------------------------------------------------

  pc_PgDown:      LDA cptr+0 PHS LDA cptr+1 PHS JPS getnext     ; bptr = getnext(cptr);
                  PLS STB bptr+1 PLS STB bptr+0
                  LDR bptr CPI 0 BEQ mainclear                  ; if (*bptr == 0) break;
                    JPS pushline
                  LDI 0 STB pc_n
    pp3_for1:     INB pc_n ADA ycur SUI <height-1 BCS pp3_for1end      ; start for with i=1
                    LDA bptr+0 PHS LDA bptr+1 PHS JPS getnext   ; char* n = getnext(bptr);
                    PLS STB pc_ptr+1 PLS STB pc_ptr+0
                    LDR pc_ptr CPI 0 BEQ pp3_for1brk            ; if (*n == 0) break;
                      LDA pc_ptr+0 STB bptr+0                   ; else bptr = n;
                      LDA pc_ptr+1 STB bptr+1
                      JPA pp3_for1
    pp3_for1brk:  LDA bptr+0 STB cptr+0 LDA bptr+1 STB cptr+1   ; if (i < height-1-ycur) { cptr = bptr; ycur += i; }
                  LDA pc_n ADB ycur JPA pp5_pullout             ; reuse code from pc_PgUp
    pp3_for1end:  LDI 0 STB pc_n
    pp3_for2:       LDA bptr+0 PHS LDA bptr+1 PHS JPS getnext   ; char* n = getnext(bptr);
                    PLS STB pc_ptr+1 PLS STB pc_ptr+0
                    LDR pc_ptr CPI 0 BEQ pp3_for2brk            ; if (*n == 0) break;
                      LDA pc_ptr+0 STB bptr+0                   ; else bptr = n;
                      LDA pc_ptr+1 STB bptr+1
                      LDA tptr+0 PHS LDA tptr+1 PHS JPS getnext ; tptr = getnext(tptr);
                      PLS STB tptr+1 PLS STB tptr+0
                      LDA cptr+0 PHS LDA cptr+1 PHS JPS getnext ; tptr = getnext(tptr);
                      PLS STB cptr+1 PLS STB cptr+0
                      INW yorg INB pc_n CPI <height BCC pp3_for2
                        JPA pp5_pullout                         ; normal end reached
    pp3_for2brk:  LDA pc_n CPI 0 BNE pp5_pullout
                    LDA bptr+0 STB cptr+0 LDA bptr+1 STB cptr+1 ; if (i == 0) { cptr = bptr; ycur = height-1; }
                    LDI <height-1 STB ycur JPA pp5_pullout

; ------------------------------------------------------------
; INPUT HANDLER FOR MENU STATE "New", "Load", "Save"
; ------------------------------------------------------------

StateReceive:   WIN CPI 27 BEQ rec_end            ; ESC ends this process
                CPI 13 BEQ StateReceive           ; ignore CR
                CPI 9 BNE rec_normal
                  LDI ' ' STR copyptr DEW copyptr ; convert TAB to double SPACE
                  LDI ' '
  rec_normal:     STR copyptr DEW copyptr
                  JPA StateReceive
  rec_end:      LDI 0 STB state
                JPA pc_CtrlV

; ------------------------------------------------------------

StateNew:   WIN CPI 'y' BNE rd2mainclear
              LDI 0 STB data
              LDI 0 STB namebuf
              JPS clrnameptr
              JPA mainload

; ------------------------------------------------------------

StateLoad:  WIN CPI 10 BNE pl_next1                       ; ENTER
              LDA nameptr+0 CPI <namebuf BEQ rd2mainclear ; is there a non-zero filename?
                LDI <namebuf STB strcpy_s+0               ; copy filename into _ReadBuffer (out of bank #00!!!)
                LDI >namebuf STB strcpy_s+1
                LDI <_ReadBuffer STB strcpy_d+0 STB _ReadPtr+0  ; point _ReadPtr to start of filename
                LDI >_ReadBuffer STB strcpy_d+1 STB _ReadPtr+1
                JPS strcpy                                ; copy the filename away from FLASH interferance
                JPS _LoadFile CPI 1 BEQ mainload          ; success?
                  JPA mainloop

  pl_next1:   CPI 8 BNE pl_default                        ; handle backspace
                LDA nameptr+0 CPI <namebuf BEQ mainloop   ; left border reached?
                  JPS Delete
                  DEW nameptr LDI 0 STR nameptr           ; write zero terminator
                  JPA mainloop

  pl_default: CPI 0xe0 BEQ _Start                         ; Ctrl+Q also works here
              CPI 27 BEQ rd2mainclear                     ; DEFAULT: ESC -> leave input
              CPI 33 BCC mainloop                         ;          ignore SPACE and below
              CPI 0x80 BCS mainloop                       ;          ignore chars >= 128
              STB 0xff00
              LDA nameptr+0 SUI <namebuf CPI 19 BCS mainloop  ; filename too long?
                LDA 0xff00 STR nameptr OUT
                INW nameptr LDI 0 STR nameptr             ; always write a zero at the end of the name
                JPA mainloop

; ------------------------------------------------------------

StateSave:  WIN CPI 10 BNE pl_next1                       ; ATTENTION: SaveFile modifies _ReadBuffer!!!
              LDA nameptr+0 CPI <namebuf BEQ rd2mainclear ; ENTER: Any name there?
                LDI 0 STB pc_ptr+0 LDI >data STB pc_ptr+1 ; find the end of the file
  ps_findz:     LDR pc_ptr CPI 0 BEQ ps_saveit            ; found last (0) byte?
                  INW pc_ptr JPA ps_findz
  ps_saveit:    LDI <namebuf STB _ReadPtr+0               ; set parse pointer to start of the name
                LDI >namebuf STB _ReadPtr+1
                LDI <data PHS LDI >data PHS               ; start address of the data
                LDA pc_ptr+0 PHS LDA pc_ptr+1 PHS         ; address of last byte to save (0)
                LDI 32 OUT                                ; behind the name: move right for potential "OVERWRITING (y/n)?"
                JPS _SaveFile PLS PLS PLS                 ; call save (will copy filename to _ReadBuffer start)
                PLS CPI 1 BEQ rd2mainclear                ; get result: success?
                  JPA pc_AbortS                           ; 0: error or 2: user abortion => back to name input

; ------------------------------------------------------------
; Copies a marked area in reversed order downwards from copybuf
; modifies: pc_ptr, pu_len
; ------------------------------------------------------------
CopyMarked:   LDI <copybuf STB copyptr+0 ; set copyptr to upper end of usable RAM
              LDI >copybuf STB copyptr+1
              LDA cptr+0 STB pu_len+0   ; pc_ptr = starts at markptr and goes up reight before cptr
              LDA cptr+1 STB pu_len+1   ; pu_len = current text pos - markptr = marked bytesize
              LDA xcur ADW pu_len
              LDA markptr+1 STB pc_ptr+1 SUB pu_len+1 BCC cm_return  ; subtract markptr to get the required bytesize
              LDA markptr+0 STB pc_ptr+0 SUW pu_len BCC cm_return    ; don't do anything if A is later than cursor
  cm_loop:      DEW pu_len BCC cm_return                             ; copy the stuff
                  LDR pc_ptr STR copyptr
                  INW pc_ptr DEW copyptr                             ; reverse order
                  JPA cm_loop
  cm_return:  RTS

; ------------------------------------------------------------
; HELPER ROUTINES
; ------------------------------------------------------------

; init the filename pointer to start of filename buffer
clrnameptr:     LDI <namebuf STB nameptr+0       ; point to start of filename
                LDI >namebuf STB nameptr+1
                RTS

; calculate the length of a string (excluding the terminator)
; push: terminator (active lower or equal), string_LSB, string_MSB
; pull: #, len_MSB, len_LSB
length:         LDS 4 STB lenptr+0 LDS 3 STB lenptr+1
                LDS 5 STB lenloop+1              ; get terminator
  lenloop:      LDI 0xcc CPA
  lenptr:       0xcccc BCS lenende           ; stops when reaching char <= terminator
                  INW lenptr JPA lenloop
  lenende:      LDS 3 SUB lenptr+1
                LDS 4 SUW lenptr STS 4
                LDA lenptr+0 STS 5
                RTS

; ------------------------------------------------------------

; returns next line address after \n or returns address of EOF
; push: address LSB, MSB
; pull: address MSB, LSB
getnext:        LDS 4 STB pu_sptr+0
                LDS 3 STB pu_sptr+1
  gn_loop:      LDR pu_sptr
                CPI 0 BEQ gn_return
                CPI 10 BEQ gn_addret
                  INW pu_sptr
                  JPA gn_loop
  gn_addret:    INW pu_sptr
  gn_return:    LDA pu_sptr+1 STS 3
                LDA pu_sptr+0 STS 4
                RTS

; ------------------------------------------------------------

; returns previous line's address or returns the same address
; push: address LSB, MSB
; pull: address MSB, LSB
getprev:        LDS 4 STB pu_sptr+0
                LDS 3 STB pu_sptr+1
                LDI >data CPA pu_sptr+1 BNE gp_loop1
                LDI <data CPA pu_sptr+0 BEQ gp_return
  gp_loop1:       DEW pu_sptr
  gp_loop2:       LDI >data CPA pu_sptr+1 BNE gp_loopon
                  LDI <data CPA pu_sptr+0 BEQ gp_return
  gp_loopon:        DEW pu_sptr
                    LDR pu_sptr CPI 10 BNE gp_loop2
                      INW pu_sptr
  gp_return:    LDA pu_sptr+1 STS 3
                LDA pu_sptr+0 STS 4
                RTS

; ------------------------------------------------------------

; pulls current line incuding \n into 'line buffer' and terminates with zero
pullline:       LDA cptr+0 PHS LDA cptr+1 PHS
                JPS getnext
                PLS PLS SUA cptr+0 STB pu_n
                  LDI <line STB pu_dptr+0 LDI >line STB pu_dptr+1
                  LDA cptr+0 STB pu_sptr+0 LDA cptr+1 STB pu_sptr+1
  pl_loop:      DEB pu_n BCC pl_return
                  LDR pu_sptr STR pu_dptr
                  INW pu_sptr INW pu_dptr
                  JPA pl_loop
  pl_return:    LDI 0 STR pu_dptr
                LDI 0 STB changed
                RTS

; ------------------------------------------------------------

; push 'line buffer' into current line postion, replacing the old line
pushline:       LDA changed CPI 0 BEQ pl_nochange
                  LDI 0 STB changed
                  LDA cptr+0 STB pu_dptr+0 PHS
                  LDA cptr+1 STB pu_dptr+1 PHS
                  JPS getnext
                  PLS STB pu_sptr+1 PLS STB pu_sptr+0           ; get next pointer
                  LDI 0 PHS JPS linelength
                  PLS STB pu_n                                  ; get newsize of "line"
                  ADW pu_dptr                                   ; pu_dptr = cptr + newsize
                  LDI 0 PHS LDA pu_sptr+0 PHS LDA pu_sptr+1 PHS
                  JPS length PLS
                  PLS STB pu_len+1 PLS STB pu_len+0 INW pu_len  ; pu_len = rest (incl. zero)
                  LDA pu_dptr+0 PHS LDA pu_dptr+1 PHS           ; push dest
                  LDA pu_sptr+0 PHS LDA pu_sptr+1 PHS           ; push source
                  LDA pu_len+0 PHS LDA pu_len+1 PHS             ; push size
                  JPS _MemMove LDI 6 ADB 0xffff
                  LDA cptr+0 PHS LDA cptr+1 PHS                 ; push dest
                  LDI <line PHS LDI >line PHS                   ; push source
                  LDA pu_n PHS LDI 0 PHS                        ; push size
                  JPS _MemMove LDI 6 ADB 0xffff
  pl_nochange:  RTS

; ------------------------------------------------------------

; cuts out a character from a zero-terminated string, moving the tail end and shortening the string
; push: str_lsb, str_msb
; pull: #, #
cutchar:        LDS 3 STB cut_dptr+1 STB cut_sptr+1             ; retrieve address of the char to cut
                LDS 4 STB cut_dptr+0 STB cut_sptr+0
                INW cut_sptr
  cut_loop:     LDA
  cut_sptr:     0xffff STB
  cut_dptr:     0xffff CPI 0 BEQ cut_done
                  INW cut_sptr INW cut_dptr JPA cut_loop
  cut_done:     RTS

; ----------------------------------------------------------
; Prints 3-digit dec line number <pu_len>
; modifies: pu_len (2 bytes)
; ----------------------------------------------------------
PrintLine:      LDI '0' STB pstring+0 STB pstring+1 STB pstring+2
  p100loop:     LDI 100 SUW pu_len BCC p99end100
                  INB pstring+0 JPA p100loop
  p99end100:    LDI 100 ADW pu_len
  p10loop:      LDI 10 SUB pu_len+0 BCC p99end10
                  INB pstring+1 JPA p10loop
  p99end10:     LDI 58 ADA pu_len+0 STB pstring+2
PrintNext:      LDA pstring+0 OUT               ; output current number
                LDA pstring+1 OUT
                LDA pstring+2 OUT
                LDI '|' OUT
                INB pstring+2 CPI ':' BCC pexit ; advance to next number
                  LDI '0' STB pstring+2
                INB pstring+1 CPI ':' BCC pexit
                  LDI '0' STB pstring+1
                INB pstring+0
  pexit:        RTS

  pstring:      '000'

; ------------------------------------------------------------
; Scrolling in terminal mode is easy
; ------------------------------------------------------------
ScrollUp:       LDI <uptxt PHS LDI >uptxt JPA printit
ScrollDn:       LDI <dntxt PHS LDI >dntxt JPA printit
ClearRow:       LDI <rowtxt PHS LDI >rowtxt JPA printit
Clear:          LDI <clrtxt PHS LDI >clrtxt JPA printit
Delete:         LDI <deltxt PHS LDI >deltxt
  printit:      PHS JPS _Print PLS PLS RTS

  uptxt:        27, '[S', 0
  dntxt:        27, '[T', 0
  clrtxt:       27, '[H', 27, '[J', 0
  deltxt:       ' ', 27, '[2D ', 27, '[D', 0
  rowtxt:       27, '[1G', 27, '[K', 0                      ; move cursor to the left and clear the row

; ------------------------------------------------------------
; copies the content of a source string at 'strcpy_s' to a
; destination at 'strcpy_d' (no overlap allowed! no safety!)
; ------------------------------------------------------------
strcpy:         LDA
  strcpy_s:     0xcccc                                     ; self-modifying code
                STB
  strcpy_d:     0xcccc CPI 0 BEQ strcpyend
                  INW strcpy_s INW strcpy_d JPA strcpy
  strcpyend:    RTS

; --------------------------------------------------------------------
; Determins the offset of first appearance of the a char <= terminator
; --------------------------------------------------------------------
linelength:     LDI 0 STB llptr+0               ; clear the LSB
  llloop:       LDS 3
                CPA
  llptr:        0xcc >line BCS llfound
                  INB llptr+0 JPA llloop        ; endless loop if terminator is not found
  llfound:      LDA llptr+0 STS 3 RTS

; ------------------------------------------------------------
; DATA AREA OF THE EDITOR
; ------------------------------------------------------------

newstr:         'NEW (y/n)?', 0
loadstr:        'LOAD:', 0
savestr:        'SAVE:', 0
receivestr:     'REC (ESC)', 0

iscoldstart:    1             ; indicating first (cold) start of this editor
namebuf:        '...................', 0  ; buffer string for filename
nameptr:        0xffff
copyptr:        0xffff        ; points to the next free byte below copied data, growing downwards
markptr:        0xffff        ; pointer to linestart of a marked area, invalid: MSB = 0x00
marktptr:       0xffff        ; remembers the top ptr while marking
markyorg:       0xffff        ; remember the yorg that fits to the top
markx:          0xff          ; remembers xcur while marking
marky:          0xff          ; remembers ycur while marking
tptr:           0xffff        ; top pointer
cptr:           0xffff        ; current line pointer
bptr:           0xffff        ; bottom pointer
state:          0xff          ; 0: edit mode, N: New, S: Save, L: Load
xcur:           0xff          ; abs x position of the cursor (starting from 0)
ycur:           0xff          ; abs y position of the cursor (starting from 0)
xorg:           0xff          ; horizontal position of viewport
yorg:           0xffff        ; global line number of the top column on the screen (starting from 1)
redraw:         0xff          ; 2: all, 1: line, 0: nix
changed:        0xff          ; 1: line was changed, 0: line is unchnaged (no need to pushline)

pc_sptr:        0xffff        ; shared by functions that only use these basic procedures:
pc_dptr:        0xffff        ; pushline / pullline / getprev / getnext / length
pc_ptr:         0xffff
pc_n:           0xff

pu_sptr:        0xffff        ; used inside these often used funtions:
pu_dptr:        0xffff        ; pushline / pullline / getprev / getnext / length
pu_len:         0xffff
pu_n:           0xff

#mute                         ; CONSTANTS

#org 0x0032     width:        ; screen xdim in characters: 50
#org 0x001e     height:       ; screen ydim in characters: 30

#org 0x8f00     line:         ; alligned line buffer
#org 0x9000     data:         ; beginning of the data area
#org 0xdfff     copybuf:      ; top end of the copy buffer (growing downwards)

#mute                         ; MinOS label definitions generated by 'asm os.asm -s_'

#org 0x0000 _Start:
#org 0x0003 _Prompt:
#org 0x0006 _FlashErase:
#org 0x0009 _FlashWrite:
#org 0x000c _FindFile:
#org 0x000f _LoadFile:
#org 0x0012 _SaveFile:
#org 0x0015 _MemMove:
#org 0x0018 _Random:
#org 0x001b _ReadLine:
#org 0x001e _ReadHex:
#org 0x0021 _SkipSpace:
#org 0x0024 _Print:
#org 0x0027 _PrintHex:
#org 0x002a _CursorX:
#org 0x002d _CursorY:
#org 0xfec5 _RandomState:
#org 0xfec9 _ReadPtr:
#org 0xfecb _ReadNum:
#org 0xfece _ReadBuffer:
