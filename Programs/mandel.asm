; ------------------------------------------------------------------------
; Displays the Mandelbrot set by projecting the area (-2.5...1) * (-1...1)
; onto 32 x 22 pixels using a maximum of 15 iterations and 16/32-bit math
; operations with fixed-point int values written by C. Herting (slu4) 2024
; ------------------------------------------------------------------------
#org 0xe000   LDI 1 STB anz

loop:         DEB anz BCC _Prompt
              
              LDI 0 STB ypos
              LDI 0xfe STB cb+1 LDI 0x06 STB cb+0   ; set cb
nextline:     LDI 0 STB xpos
              LDI 0xfa STB ca+1 LDI 0xf8 STB ca+0   ; set ca

nextpixel:    LDA ca+0 STB za+0 LDA ca+1 STB za+1   ; init with za=ca and zb=cb
              LDA cb+0 STB zb+0 LDA cb+1 STB zb+1
              LDI 14 STB iter                       ; set max iteration steps to (n+1)
iterate:      LDA za+0 STB inta+0 STB intb+0        ; calculate za^2
              LDA za+1 STB inta+1 STB intb+1
              JPS Multiply
              LDA intc+3 LSR                        ; store (result>>9) in zaq
              LDA intc+2 ROR STB zaq+1
              LDA intc+1 ROR STB zaq+0

              LDA zb+0 STB inta+0 STB intb+0        ; calculate zb^2
              LDA zb+1 STB inta+1 STB intb+1
              JPS Multiply
              LDA intc+3 LSR                        ; store (result>>9) in zbq
              LDA intc+2 ROR STB zbq+1
              LDA intc+1 ROR STB zbq+0

              LDA zaq+0 ADA zbq+0                   ; calculate za^2 + zb^2
              LDA zaq+1 BCC nocarry
                INC                                 ; add carry
  nocarry:    ADA zbq+1
              CPI 0x08 BCS plotpixel                ; quit iteration if result >= 4
                LDA za+0 STB inta+0 LDA za+1 STB inta+1 ; zb = (za * zb)>>8 + cb (eff x2)
                LDA zb+0 STB intb+0 LDA zb+1 STB intb+1
                JPS Multiply                        ; intc_32 = inta_16 * intb_16
                LDA intc+1 STB zb+0
                LDA intc+2 ADA cb+1 STB zb+1
                LDA cb+0 ADW zb
                LDA zaq+0 STB za+0 LDA zaq+1 STB za+1  ; za = za^2 - zb^2 + ca
                LDA zbq+1 SUB za+1 LDA zbq+0 SUW za
                LDA ca+0 ADW za LDA ca+1 ADB za+1
                DEB iter BCS iterate                ; 15 iterations from 14..0

plotpixel:    LDI 33 ADB iter OUT         ; plot current pixel in ASCII style
              LDI 56 ADW ca
              INB xpos CPI 32 BCC nextpixel         ; advance to next position
                LDI 10 OUT                ; output ENTER
                LDI 46 ADW cb
                INB ypos CPI 22 BCC nextline        ; advance to next line
                  JPA loop

; ----------------------------------------------------------------------
; Fast signed multiplication 32-bit intc = (16-bit inta) * (16-bit intb)
; ----------------------------------------------------------------------
Multiply:     LDI 0 STB sign
              LDI 0x80 CPA inta+1 BGT aposi ADB sign NOW inta INW inta  ; make A positive -A=~A+1
  aposi:      LDI 0x80 CPA intb+1 BGT bposi ADB sign NOW intb INW intb  ; make B positive -B=~B+1
  bposi:      LDI 0 STB intc+0 STB intc+1 STB intc+2 STB intc+3      ; set result C=0
              LDI 16 STB cnt JPA entry                    ; C=0 does not need shifting
next:           LLW intc+0 RLW intc+2                     ; shift the current result up one step
  entry:      LLW inta BCC bitisoff                       ; shift out the highest bit of A
                LDA intb+0 ADW intc+0 LDI 0 ACW intc+2    ; add 16-bit B to 32-bit C result
                LDA intb+1 ADB intc+1 LDI 0 ACW intc+2
  bitisoff:   DEB cnt BNE next
                LDA sign CPI 0 BEQ exit
                  NOW intc+0 NOW intc+2                   ; negate 32-bit result
                  INW intc+0 LDI 0 ACW intc+2
  exit:         RTS

#mute

inta:         0x0000                                      ; math registers
intb:         0x0000
intc:         0x0000, 0x0000, 0
sign:         0
cnt:          0

xpos:         0
ypos:         0
iter:         0
ca:           0, 0                                        ; fixed-point Mandelbrot variables
cb:           0, 0
za:           0, 0
zb:           0, 0
zaq:          0, 0
zbq:          0, 0

anz:          0

#org 0x0003 _Prompt:
#org 0x0024 _Print:
