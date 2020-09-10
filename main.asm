 processor 6502
	org $800
	; Starting new memory block at $800
	.byte    $0, $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $32,$30,$36,$34
	.byte    $29, $00, $00, $00
	; Ending memory block
EndBlock25
	org $810
	; Starting new memory block at $810
C64Project
	jmp block1
RS232_RS232_BYTE_IN	dc.b	0
RS232_KEYBOARD_BYTE_IN	dc.b	0
RS232_input_buffer	dc.b	 
	org RS232_input_buffer+256
RS232_output_buffer	dc.b	 
	org RS232_output_buffer+256
title		dc.b	"TRSETERM VERSION 0.1(MENU=F1)"
	dc.b	0
inkey	dc.b	$00
	
; //procedure CallRoutine(args : TCallArguments);
; //begin
; //	asm("	lda RS232_args_RS232_TCallArguments_RS232_TCallArguments_a
; //			ldx RS232_args_RS232_TCallArguments_RS232_TCallArguments_x
; //			ldy RS232_args_RS232_TCallArguments_RS232_TCallArguments_y
; //			jsr RS232_args_RS232_TCallArguments_RS232_TCallArguments_Addr
; //	");
; //end;
; //
; // -----------------------------------------------------------------------------
; //
; // initialize the rs232 port and set up the buffers(for use later)
; // -----------------------------------------------------------------------------
; //	
	; ***********  Defining procedure : RS232_CallRoutine
	;    Procedure type : User-defined procedure
	rts
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto2
screenmemory =  $fe
colormemory =  $fc
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
	; ***********  Defining procedure : initprintdecimal
	;    Procedure type : Built-in function
	;    Requires initialization : no
ipd_div_hi dc.b 0
ipd_div_lo dc.b 0
init_printdecimal_div10
	ldx #$11
	lda #$00
	clc
init_printdecimal_loop
	rol
	cmp #$0A
	bcc init_printdecimal_skip
	sbc #$0A
init_printdecimal_skip
	rol ipd_div_lo
	rol ipd_div_hi
	dex
	bne init_printdecimal_loop
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
	
; //procedure CallRoutine(args : TCallArguments);
; //begin
; //	asm("	lda RS232_args_RS232_TCallArguments_RS232_TCallArguments_a
; //			ldx RS232_args_RS232_TCallArguments_RS232_TCallArguments_x
; //			ldy RS232_args_RS232_TCallArguments_RS232_TCallArguments_y
; //			jsr RS232_args_RS232_TCallArguments_RS232_TCallArguments_Addr
; //	");
; //end;
; //
; // -----------------------------------------------------------------------------
; //
; // initialize the rs232 port and set up the buffers(for use later)
; // -----------------------------------------------------------------------------
; //	
	; ***********  Defining procedure : RS232_CallRoutine
	;    Procedure type : User-defined procedure
	rts
	; ***********  Defining procedure : RS232_Init
	;    Procedure type : User-defined procedure
RS232_Init
	; Assigning memory location
	; Assigning single variable : $f7
	lda #<RS232_input_buffer
	; Calling storevariable
	sta $f7
	; Assigning memory location
	; Assigning single variable : $f8
	lda #>RS232_input_buffer
	; Calling storevariable
	sta $f8
	; Assigning memory location
	; Assigning single variable : $f9
	lda #<RS232_output_buffer
	; Calling storevariable
	sta $f9
	; Assigning memory location
	; Assigning single variable : $fa
	lda #>RS232_output_buffer
	; Calling storevariable
	sta $fa
	; ****** Inline assembler section
		
    	lda #3                      ; logical file number
    	ldx #2                      ; 2 = rs-232 device
    	ldy #0                      ; no extra command
    	jsr RS232_SETLFS
    	
	
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
RS232_baud	dc.b	0
RS232_set_baudrate_block4
RS232_set_baudrate
	; Assigning memory location
	; Assigning single variable : $293
	lda RS232_baud
	; Calling storevariable
	sta $293
	; ****** Inline assembler section
		
    	lda #3                      ; logical file number
    	ldx #2                      ; 2 = rs-232 device
    	ldy #0                      ; no extra command
    	jsr RS232_SETLFS
    	
	
; // -- setup a logical file descriptor, pointing to device 2(user port) -- 
; //
; // -- close the logical file(in case its already open) -- 
; //
	jsr $ffc3
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
RS232_OUTBYTE	dc.b	0
RS232_write_byte_block6
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
	; ****** Inline assembler section
   lda RS232_OUTBYTE
	
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
	rts
	; ***********  Defining procedure : ShowMenu
	;    Procedure type : User-defined procedure
menu_line0		dc.b	"PRESS 1 TO 3 TO SELECT BAUD RATE"
	dc.b	0
menu_line1		dc.b	"1. 2400 BAUD"
	dc.b	0
menu_line2		dc.b	"2. 1200 BAUD"
	dc.b	0
menu_line3		dc.b	"3. 300  BAUD"
	dc.b	0
ShowMenu_block8
ShowMenu
	; Clear screen with offset
	lda #$20
	ldx #$fa
ShowMenu_clearloop9
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne ShowMenu_clearloop9
	; Assigning memory location
	; Assigning single variable : $d020
	lda #$6
	; Calling storevariable
	sta $d020
	; MoveTo optimization
	lda #$01
	sta screenmemory
	lda #$04
	sta screenmemory+1
	clc
	lda #<title
	adc #$0
	ldy #>title
	sta print_text+0
	sty print_text+1
	ldx #$1e ; optimized, look out for bugs
	jsr printstring
	; MoveTo optimization
	lda #$79
	sta screenmemory
	lda #$04
	sta screenmemory+1
	clc
	lda #<menu_line0
	adc #$0
	ldy #>menu_line0
	sta print_text+0
	sty print_text+1
	ldx #$20 ; optimized, look out for bugs
	jsr printstring
	; MoveTo optimization
	lda #$c9
	sta screenmemory
	lda #$04
	sta screenmemory+1
	clc
	lda #<menu_line1
	adc #$0
	ldy #>menu_line1
	sta print_text+0
	sty print_text+1
	ldx #$c ; optimized, look out for bugs
	jsr printstring
	; MoveTo optimization
	lda #$19
	sta screenmemory
	lda #$05
	sta screenmemory+1
	clc
	lda #<menu_line2
	adc #$0
	ldy #>menu_line2
	sta print_text+0
	sty print_text+1
	ldx #$c ; optimized, look out for bugs
	jsr printstring
	; MoveTo optimization
	lda #$69
	sta screenmemory
	lda #$05
	sta screenmemory+1
	clc
	lda #<menu_line3
	adc #$0
	ldy #>menu_line3
	sta print_text+0
	sty print_text+1
	ldx #$c ; optimized, look out for bugs
	jsr printstring
	jsr term_read_keyboard
ShowMenu_while20
	; Binary clause Simplified: EQUALS
	lda inkey
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne ShowMenu_elsedoneblock23
ShowMenu_ConditionalTrueBlock21: ;Main true block ;keep 
	jsr term_read_keyboard
	jmp ShowMenu_while20
ShowMenu_elsedoneblock23
	; Binary clause Simplified: EQUALS
	lda inkey
	; Compare with pure num / var optimization
	cmp #$31;keep
	bne ShowMenu_elseblock28
ShowMenu_ConditionalTrueBlock27: ;Main true block ;keep 
	; Assigning single variable : RS232_baud
	lda #$a
	; Calling storevariable
	sta RS232_baud
	jsr RS232_set_baudrate
	jmp ShowMenu_elsedoneblock29
ShowMenu_elseblock28
	; Binary clause Simplified: EQUALS
	lda inkey
	; Compare with pure num / var optimization
	cmp #$32;keep
	bne ShowMenu_elseblock56
ShowMenu_ConditionalTrueBlock55: ;Main true block ;keep 
	; Assigning single variable : RS232_baud
	lda #$8
	; Calling storevariable
	sta RS232_baud
	jsr RS232_set_baudrate
	jmp ShowMenu_elsedoneblock57
ShowMenu_elseblock56
	; Binary clause Simplified: EQUALS
	lda inkey
	; Compare with pure num / var optimization
	cmp #$33;keep
	bne ShowMenu_elsedoneblock71
ShowMenu_ConditionalTrueBlock69: ;Main true block ;keep 
	; Assigning single variable : RS232_baud
	lda #$6
	; Calling storevariable
	sta RS232_baud
	jsr RS232_set_baudrate
ShowMenu_elsedoneblock71
ShowMenu_elsedoneblock57
ShowMenu_elsedoneblock29
	jsr SetupTerminal
	; Clear screen with offset
	lda #$20
	ldx #$fa
ShowMenu_clearloop74
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne ShowMenu_clearloop74
	; Assigning memory location
	; Assigning single variable : $d020
	lda #$0
	; Calling storevariable
	sta $d020
	rts
	
; // -----------------------------------------------------------------------------
; //
; // Setup the terminal
; // -----------------------------------------------------------------------------
; //	
	; ***********  Defining procedure : SetupTerminal
	;    Procedure type : User-defined procedure
SetupTerminal
	
; // -- initialize rs232 -- 
; //
	jsr RS232_Init
	rts
	
; // -----------------------------------------------------------------------------
; //
; // non-rs232 keyboard read
; // -----------------------------------------------------------------------------
; //
	; ***********  Defining procedure : term_read_keyboard
	;    Procedure type : User-defined procedure
term_read_keyboard
	jsr $ffe4
	; ****** Inline assembler section
	sta inkey
	rts
block1
	
; // -- clear the screen -- 
; //
	; Assigning memory location
	; Assigning single variable : $d020
	lda #$0
	; Calling storevariable
	sta $d020
	; Assigning memory location
	; Assigning single variable : $d021
	; Calling storevariable
	sta $d021
	; Assigning memory location
	; Assigning single variable : $d018
	lda #$17
	; Calling storevariable
	sta $d018
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop77
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne MainProgram_clearloop77
	
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
	
; // -- show the menu -- 
; //
	jsr ShowMenu
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop80
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne MainProgram_clearloop80
MainProgram_while81
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock84
MainProgram_ConditionalTrueBlock82: ;Main true block ;keep 
	
; //moveto(0,0,$04);
; //printdecimal(inkey,1);          
; // -- debug stuff -- 
; //
; // -- call read_byte(result is stored in RS232_RS232_BYTE_IN) -- 
; //
	jsr RS232_read_byte
	; Binary clause Simplified: NOTEQUALS
	lda RS232_RS232_BYTE_IN
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock120
MainProgram_ConditionalTrueBlock118: ;Main true block ;keep 
	
; // -- if a byte is read, output it to the screen -- 
; //
; // -- otherwise read the keyboard,send the byte to the rs232 channel and display on screen -- 
; //
	jsr $ffd2
MainProgram_elsedoneblock120
	jsr RS232_read_keyboard
	; Binary clause Simplified: NOTEQUALS
	lda RS232_KEYBOARD_BYTE_IN
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock126
MainProgram_ConditionalTrueBlock124: ;Main true block ;keep 
	; MoveTo optimization
	lda #$1e
	sta screenmemory
	lda #$04
	sta screenmemory+1
	ldy #0
	lda RS232_KEYBOARD_BYTE_IN
	sta ipd_div_lo
	sty ipd_div_hi
	ldy #$2 ; optimized, look out for bugs
MainProgram_printdecimal138
	jsr init_printdecimal_div10 
	ora #$30
	sta (screenmemory),y
	dey
	bpl MainProgram_printdecimal138
	; Binary clause Simplified: EQUALS
	lda RS232_KEYBOARD_BYTE_IN
	; Compare with pure num / var optimization
	cmp #$85;keep
	bne MainProgram_elseblock141
MainProgram_ConditionalTrueBlock140: ;Main true block ;keep 
	
; // -- using the constant KEY_F1 doesnt seem to work here other key constants seem ok  -- 
; //
	jsr ShowMenu
	jmp MainProgram_elsedoneblock142
MainProgram_elseblock141
	; Assigning single variable : RS232_OUTBYTE
	lda RS232_KEYBOARD_BYTE_IN
	; Calling storevariable
	sta RS232_OUTBYTE
	jsr RS232_write_byte
MainProgram_elsedoneblock142
	; Assigning single variable : RS232_KEYBOARD_BYTE_IN
	lda #$0
	; Calling storevariable
	sta RS232_KEYBOARD_BYTE_IN
MainProgram_elsedoneblock126
	jmp MainProgram_while81
MainProgram_elsedoneblock84
	jmp * ; loop like (?/%
EndSymbol
	; End of program
	; Ending memory block
EndBlock27
