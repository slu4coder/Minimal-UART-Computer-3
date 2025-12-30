# NEW: Version 3 of the Minimal UART Computer

Hi there, it's been a while. I am back with a big update of my DIY 'Minimal UART Computer', a machine built around a serial interface just from a handful of logic ICs, RAM and FLASH memory. For it's size, simplicity and self-imposed limitations, it's got more processing power than these two classic 1980's machines combined. It's mission is to be a fun learning platform for understanding computers on a fundamental level.

<img width="779" height="625" alt="Minimal_UART_3" src="https://github.com/user-attachments/assets/df3e3bac-6c39-49c3-8b9c-45a26bc524d2" />

● Manual: https://docs.google.com/document/d/1nIM-WRrVe7SzYY-DS1jcVHC4TCuIhlQ6LCHnZnEml7k/edit?usp=sharing

● The Minimal UART Computer has a dedicated hardware serial 'Minimal Terminal': https://github.com/slu4coder/Minimal-Terminal
but can also be operated via a USB-to-serial breakout board and a terminal emulation (e. g. Tera Term) of cause.
  
OVERVIEW:

● Textbook 'Von-Neumann' architecture, 8-bit data bus, 16-bit address bus

● 64 instructions (conditional branching, subroutines, stack and word operations)

● 32KB RAM, 32KB SSD with file system (format, load, save, dir, delete, defrag, ...)

● Adjustable UART I/O (7 data bits, 2 stop bits, 9600bps – 500000bps)

● Adjustable CPU clock speed (single step – 8MHz)

● Processing power ~1.1Mips @ 8MHz comparable to 2x Commodore 64 or 2x Apple II

● Clear(est?) and simple(st? – you judge!) design

● ALU with 2 data registers A & B

● 3 flags (zero, negative, carry)

● 24 control signals

● Only 30(!) 74HCxx logic ICs, RAM and FLASH

● NEW: Blinkenlights ;-)

● Operating system wiht native assembler and editor

● Games and demos (Tetris, Mandelbrot)

● Cycle-exact emulator, cross-assembler (Win64 and Linux)

I hope you find this information useful, educational or otherwise interesting. I'd love to hear about your build, so meet me on YouTube:

https://www.youtube.com/channel/UCXYQcMpUBT3aaQKfmAVJNow

This is a free and non-commercial project. I am in no way associated with any activies selling this as a product.
Any such activity represents a license violation. Individual licenses apply for hardware and software parts. Please refer to the
appropriate documentations for detailed licensing information.

See https://github.com/slu4coder/Minimal-64x4-Home-Computer for a more feature-rich alternative.

Have fun!

Do you like this project and want to give something back? Thank you so much for your support!
https://www.paypal.me/carstenherting
