# TRSEterm
RS232 implementation for the c64 in TRSE

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



