 processor 6502
	org $800
	; Starting new memory block at $800
	.byte    $0, $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $32,$30,$36,$34
	.byte    $29, $00, $00, $00
	; Ending memory block
EndBlock9
	org $810
	; Starting new memory block at $810
C64Project
	jmp block1
RS232_RS232_BYTE_IN	dc.b	
RS232_KEYBOARD_BYTE_IN	dc.b	
RS232_RS232_input_buffer	dc.b	 
	org RS232_RS232_input_buffer+256
RS232_RS232_output_buffer	dc.b	 
	org RS232_RS232_output_buffer+256
title		dc.b	"TRSETERM VERSION 0.1"
	dc.b	0
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto2
screenmemory =  $fe
screen_x = $4C
screen_y = $4E
SetScreenPosition
	sta screenmemory+1
	lda #0
	sta screenmemory
	ldy screen_y
	beq sydone
syloop
	clc
	adc #40
	bcc sskip
	inc screenmemory+1
sskip
	dey
	bne syloop
sydone
	ldx screen_x
	beq sxdone
	clc
	adc screen_x
	bcc sxdone
	inc screenmemory+1
sxdone
	sta screenmemory
	rts
initmoveto_moveto2
	rts
	; ***********  Defining procedure : initprintstring
	;    Procedure type : Built-in function
	;    Requires initialization : no
print_text = $4C
print_number_text .dc "    ",0
printstring
	ldy #0
printstringloop
	lda (print_text),y
	cmp #0 ;keep
	beq printstring_done
	cmp #64
	bcc printstring_skip
	sec
	sbc #64
printstring_skip
	sta (screenmemory),y
	iny
	dex
	cpx #0
	beq printstring_done
	jmp printstringloop
printstring_done
	rts
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
RS232_set_baudrate_block4
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
	beq RS232_read_keyboard_elsedoneblock11
RS232_read_keyboard_ConditionalTrueBlock9: ;Main true block ;keep 
	jsr RS232_write_byte
RS232_read_keyboard_elsedoneblock11
	rts
block1
	
; // -- clear the screen -- 
; //
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop14
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne MainProgram_clearloop14
	
; // -- move to position x = 1 y = y in 1st bank $0400(screen memory)
	; MoveTo optimization
	lda #$29
	sta screenmemory
	lda #$04
	sta screenmemory+1
	
; // -- print out the title string -- 
; //
	clc
	lda #<title
	adc #$0
	ldy #>title
	sta print_text+0
	sty print_text+1
	ldx #$14 ; optimized, look out for bugs
	jsr printstring
	
; // -- Set the baud rate(BAUD_300,BAUD_1200,BAUD_2400) -- 
; //
	; Assigning single variable : RS232_b
	lda #$a
	; Calling storevariable
	sta RS232_b
	jsr RS232_set_baudrate
	
; // -- initialize rs232 -- 
; //
	jsr RS232_Init
MainProgram_while17
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock20
MainProgram_ConditionalTrueBlock18: ;Main true block ;keep 
	
; // -- call read_byte(result is stored in RS232_RS232_BYTE_IN) -- 
; //
	jsr RS232_read_byte
	; Binary clause Simplified: NOTEQUALS
	lda RS232_RS232_BYTE_IN
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elseblock33
MainProgram_ConditionalTrueBlock32: ;Main true block ;keep 
	
; // -- if a byte is read, output it to the screen -- 
; //
; // -- otherwise read the keyboard,send the byte to the rs232 channel and display on screen -- 
; //
	jsr $ffd2
	jmp MainProgram_elsedoneblock34
MainProgram_elseblock33
	
; //RS232::read_keyboard();
	jsr RS232_read_keyboard
	jsr $ffd2
MainProgram_elsedoneblock34
	jmp MainProgram_while17
MainProgram_elsedoneblock20
	jmp * ; loop like (?/%
EndSymbol
	; End of program
	; Ending memory block
EndBlock11
