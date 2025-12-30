#org 0xe000

start:      LDI ' '
next:       OUT INC CPI 0x80 BCC next
              JPA start
