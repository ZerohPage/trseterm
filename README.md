# TRSEterm
RS232 implementation for the c64 in TRSE

“Turbo Rascal Syntax error, “;” expected but “BEGIN” (Turbo Rascal SE, TRSE) is a complete suite (IDE, compiler, programming language, image sprite level resource editor) intended for developing games/demos for the 8 / 16-bit line of computers, with a focus on the MOS 6502, the Motorola 68000, the (GB)Z80 and the X86. TRSE currently supports application development for the C64, C128, VIC-20, PLUS4, NES, Gameboy, PET, ZX Spectrum, TIKI 100, Amstrad CPC 464, Atari 2600, 8086AT, Amiga 500 and the Atari ST 520 (complete list here). With the benefits of a modern IDE (error messages, code completion, syntax highlighting etc) and a bunch of fast built-in tools, it has never been easier to program for your favorite obsolete system!

https://lemonspawn.com/turbo-rascal-syntax-error-expected-but-begin/


RS232 implementation unit is RS232.tru

Demo program is main.ras

**Usage**

**Include the RS232 unit :**

@use "RS232"

**Set the baud rate :**

// -- Set the baud rate (BAUD_300,BAUD_1200,BAUD_2400) -- //

RS232_set_baudrate(BAUD_2400);

**Init the RS232 Channel :**

// -- initialize rs232 -- //

RS232_Init();
  
**Read A byte :**

// -- call read_byte (result is stored in RS232_RS232_BYTE_IN) -- //

RS232_read_byte();



