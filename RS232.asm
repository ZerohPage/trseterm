 processor 6502
	org $800
	; Starting new memory block at $800
	.byte    $0, $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $32,$30,$36,$34
	.byte    $29, $00, $00, $00
	; Ending memory block
EndBlock113
	org $810
	; Starting new memory block at $810
RS232
	jmp block1
RS232_RS232_BYTE_IN	dc.b	
RS232_KEYBOARD_BYTE_IN	dc.b	
RS232_RS232_input_buffer	dc.b	 
	org RS232_RS232_input_buffer+256
RS232_RS232_output_buffer	dc.b	 
	org RS232_RS232_output_buffer+256
	; ***********  Defining procedure : RS232_Init
	;    Procedure type : User-defined procedure
RS232_Init
	; Assigning memory location
	; Assigning single variable : $f7
	lda #<RS232_RS232_input_buffer
	; Calling storevariable
	sta $f7
	; Assigning memory location
	; Assigning single variable : $f8
	lda #>RS232_RS232_input_buffer
	; Calling storevariable
	sta $f8
	; Assigning memory location
	; Assigning single variable : $f9
	lda #<RS232_RS232_output_buffer
	; Calling storevariable
	sta $f9
	; Assigning memory location
	; Assigning single variable : $fa
	lda #>RS232_RS232_output_buffer
	; Calling storevariable
	sta $fa
	; ****** Inline assembler section
		
    	lda #3                      ; logical file number
    	ldx #2                      ; 2 = rs-232 device
    	ldy #0                      ; no extra command
    	jsr SETLFS
    	
	
; // -- setup a logical file descriptor, pointing to device 2(user port) -- 
; //
; // -- open the logical file -- 
; //
	jsr $ffc0
	rts
	
; // -----------------------------------------------------------------------------
; //
; // set the baud rate
; // -----------------------------------------------------------------------------
; //
	; ***********  Defining procedure : RS232_set_baudrate
	;    Procedure type : User-defined procedure
RS232_b	dc.b	
RS232_set_baudrate_block3
RS232_set_baudrate
	; Assigning memory location
	; Assigning single variable : $293
	lda RS232_b
	; Calling storevariable
	sta $293
	rts
	
; // -----------------------------------------------------------------------------
; //
; // attempt to read a byte                                          
; // -----------------------------------------------------------------------------
; //
	; ***********  Defining procedure : RS232_read_byte
	;    Procedure type : User-defined procedure
RS232_read_byte
	; ****** Inline assembler section
	ldx #$03
	
; // -- logical file 3 -- 
; //
	jsr $ffc6
	
; // -- CHKIN. Define file as default input.(Must call OPEN beforehands.) -- 
; //
	jsr $ffe4
	; ****** Inline assembler section
	sta RS232_RS232_BYTE_IN
	
; // -- GETIN. Read byte from default input.(If not keyboard, must call OPEN and CHKIN beforehands.) -- 
; //
; // -- transfer the result of GETIN to RS232_RS232_BYTE_IN as CLRCHN kills the A register -- 
; //
	jsr $ffcc
	rts
	
; // -- CLRCHN. Close default input/output files(for serial bus, send UNTALK and/or UNLISTEN); restore default input/output to keyboard/screen. -- 
; //
; // -----------------------------------------------------------------------------
; //
; // attempt to write a byte                                          
; // -----------------------------------------------------------------------------
; //
	; ***********  Defining procedure : RS232_write_byte
	;    Procedure type : User-defined procedure
RS232_write_byte
	; ****** Inline assembler section
	ldx #$03
	; ****** Inline assembler section
	tay
	
; // -- logical file 3 -- 
; //
; // -- preserve the accumulator as it is changed by CHKOUT -- 
; //
	jsr $ffc9
	; ****** Inline assembler section
	tya
	
; // -- CHKOUT. Define file as default output.(Must call OPEN beforehands.) -- 
; // 
; // -- restore accumulator from Y -- 
; //
	jsr $ffd2
	
; // -- CHROUT. Write byte to default output.(If not screen, must call OPEN and CHKOUT beforehands.) -- 
; //
	jsr $ffcc
	rts
	
; // -- CLRCHN. Close default input/output files(for serial bus, send UNTALK and/or UNLISTEN); restore default input/output to keyboard/screen. -- 
; //
; // -----------------------------------------------------------------------------
; //
; // read a byte from the keyboard                                          
; // -----------------------------------------------------------------------------
; //
	; ***********  Defining procedure : RS232_read_keyboard
	;    Procedure type : User-defined procedure
RS232_read_keyboard
	jsr $ffe4
	; ****** Inline assembler section
	sta RS232_KEYBOARD_BYTE_IN
	; Binary clause Simplified: NOTEQUALS
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq RS232_read_keyboard_elsedoneblock10
RS232_read_keyboard_ConditionalTrueBlock8: ;Main true block ;keep 
	jsr RS232_write_byte
RS232_read_keyboard_elsedoneblock10
	rts
block1
EndSymbol
	; End of program
	; Ending memory block
EndBlock115
