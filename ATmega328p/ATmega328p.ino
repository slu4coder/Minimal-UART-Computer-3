// ATmega328P (16MHz) adjustable clock generator for the Minimal UART Computer 3 written by Carsten Herting (slu4) 2025
// This software allows to control the CPU frequency (single step - 8MHz) and the UART rate via two pushbuttons (SELECT, SINGLE)
// See more information on the PCB layout of the Minimal UART Computer 3

// CPU rate      8MHz       1MHz       100kHz     10kHz      1kHz       100Hz      10Hz                 1Hz
int ocr[8] = {   0,         7,         79,        799,       7999,      9999,      12499,               31249     };
int pre[8] = {  _BV(CS10), _BV(CS10), _BV(CS10), _BV(CS10), _BV(CS10), _BV(CS11), _BV(CS11)|_BV(CS10), _BV(CS12) }; // prescale 1, 1, 1, 1, 1, 8, 64, 256

// UART rate     0: 500kbps, 1: 250kbps, 3: 125kbps, 4: 100kbps, 7: 62.5kbps, 12: 38.4kbps, 25: 19.2kbps, 51: 9.6kbps
int uart_ocr[8] = { 0, 1, 3, 4, 7, 12, 25, 51 };

int clkstate = 0; // initial clock state: 8MHz
int uartstate = 2; // initial UART bitrate: 125kbps
bool single = false; // single-step mode on/off
bool showuart = false; // show UART state (blinking LEDs) on/off
bool showexit = false; // indicates leaving 'showuart' state

void setup()
{  
  DDRB  = 0b00001010; // pins 8-13 (9: CPU_OSC via OC1A, 11: UART_OSC via OC2A)
  PORTB = 0b00000101; // 20k pull-up on pins 8 ( and 10 for buttons

  // UART clock (bit rate = UART_CLK / 16)
  TCCR2A = _BV(COM2A0) | _BV(WGM21);  // Toggle OC2A for UART (pin 11)
  TCCR2B = _BV(CS20);                 // CTC, Prescaler 1
  OCR2A = uart_ocr[uartstate];

  // CPU clock (CLK)
  TCCR1A = _BV(COM1A0);               // Toggle OC1A for CLK (pin 9)333
  TCCR1B = _BV(WGM12) | pre[clkstate];
  OCR1A  = ocr[clkstate];

  // use pins A0-A2 to indicate clock clkstate
  DDRC  = 0b00000111;
  PORTC = clkstate;
}

#define BUTTON_IS_NONE    0
#define BUTTON_IS_MODE    1
#define BUTTON_IS_UART    2

int SelectButton()
{
  static bool selectstate = false;

  int m = 0; int s = 0;
  for (int i=0; i<50; i++)
  {
    m += bool(PINB & 0b00000100); // sample MODE SELECT button
    s += bool(PINB & 0b00000001); // sample SINGLE STEP button
  }

  if (m > 45) // MODE SELECT isn't pressed or was released
  {
    if (selectstate == false) return BUTTON_IS_NONE;
    else { selectstate = false; delay(5); return BUTTON_IS_NONE; }
  }
  
  if (m < 5)
  {
    if (selectstate == true) return BUTTON_IS_NONE; // key was already pressed => no new keypress
    else
    {
      selectstate = true; // key wasn't pressed before => report new keypress
      delay(5);
      if (s < 5) return BUTTON_IS_UART; else return BUTTON_IS_MODE;
    }
  }
  return BUTTON_IS_NONE;
}

void loop()
{  
  if (single == true) // case "single step mode"
  {
    if (showuart == false)
    {
      int b = SelectButton();
      if (b == BUTTON_IS_MODE) { TCCR1A |= _BV(COM1A0); single = false; } // turn clock on
      else if (b == BUTTON_IS_UART) { showuart = true; delay (5); } // enter uart mode
      else // BUTTON_IS_NONE
      {
        int z = 0; for (int i=0; i<50; i++) z += bool(PINB & 0b00000001);
        if (z > 45) { bitWrite(PORTB, 1, HIGH); delay (5); }
        else if (z < 5) { bitWrite(PORTB, 1, LOW); delay(5); }
      }
    }
    else // showuart == true
    {
      if (SelectButton() == BUTTON_IS_UART)
      {
        delay(5);
        uartstate = (uartstate + 1) & 7;
        PORTC = uartstate;
        OCR2A = uart_ocr[uartstate];
      }
      if ((millis() & 127) > 64) PORTC = uartstate; else PORTC = 0;
      int z = 0; for (int i=0; i<50; i++) z += bool(PINB & 0b00000001); // read SINGLE button
      if (z > 45) { showuart = false; TCCR1A |= _BV(COM1A0); single = false; PORTC = clkstate; delay(5); } // go back and restart clock
    }
  }
  else // single == false
  {
    if ((PINB & 0b00000001) == 0) { TCCR1A &= ~_BV(COM1A0); delay(5); single = true; } // clock off
    else if (SelectButton() == BUTTON_IS_MODE)
    {
      clkstate = (clkstate + 1) & 7; // rotate clkstate
      PORTC = clkstate;
      TCCR1B = _BV(WGM12) | pre[clkstate]; // set CTC and Prescaler
      OCR1A  = ocr[clkstate]; // set timer
    }      
  }
}

