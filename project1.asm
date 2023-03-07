$NOLIST
$MODLP51RC2
$LIST

; setup buttons
PWM_OUTPUT   equ P2.7
TEMP_SOAK_PB equ P0.0
TIME_SOAK_PB equ P0.1
TEMP_REFL_PB equ P0.3
TIME_REFL_PB equ P0.5
STOP_PB equ P2.4
START_PB equ P4.5
RETURN_PB equ P4.4
FLASH_CE  EQU  P2.5
SPEAKER  EQU P2.6 ; Used with a MOSFET to turn off speaker when not in use

CLK           EQU 22118400  ; Microcontroller system crystal frequency in Hz
SYSCLK        EQU 22118400  ; Microcontroller system clock frequency in Hz
TIMER1_RATE   EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
TIMER1_RELOAD EQU 0x10000-(SYSCLK/TIMER1_RATE)
TIMER2_RATE   EQU 1000      ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
BAUD 		  EQU 115200
BRG_VAL		  EQU (0x100-(CLK/(16*BAUD)))

MY_SCLK equ P1.3
MY_MISO equ P1.2
MY_MOSI equ P1.1
CE_ADC equ P1.0

; Commands supported by the SPI flash memory according to the datasheet
WRITE_ENABLE     EQU 0x06  ; Address:0 Dummy:0 Num:0
WRITE_DISABLE    EQU 0x04  ; Address:0 Dummy:0 Num:0
READ_STATUS      EQU 0x05  ; Address:0 Dummy:0 Num:1 to infinite
READ_BYTES       EQU 0x03  ; Address:3 Dummy:0 Num:1 to infinite
READ_SILICON_ID  EQU 0xab  ; Address:0 Dummy:3 Num:1 to infinite
FAST_READ        EQU 0x0b  ; Address:3 Dummy:1 Num:1 to infinite
WRITE_STATUS     EQU 0x01  ; Address:0 Dummy:0 Num:1
WRITE_BYTES      EQU 0x02  ; Address:3 Dummy:0 Num:1 to 256
ERASE_ALL        EQU 0xc7  ; Address:0 Dummy:0 Num:n0
ERASE_BLOCK      EQU 0xd8  ; Address:3 Dummy:0 Num:0
READ_DEVICE_ID   EQU 0x9f  ; Address:0 Dummy:2 Num:1 to infinite

;Reset vector
org 0x00000
   ljmp main
   
org 0x001B ; Timer/Counter 1 overflow interrupt vector. Used in this code to replay the wave file.
	ljmp Timer1_ISR
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR
	
$NOLIST
$include(LCD_4bit.inc)
$include(math32.inc)
$LIST
SHIFT_PB EQU P2.2
DSEG at 0x30; Before the state machine!
state:      ds 1
temp_soak:  ds 1
time_soak:  ds 1
temp_refl:  ds 1
time_refl:  ds 1
Count1ms:   ds 2 ; Used to determine when half second has passed
sec:	ds 1
mins:	ds 1
temp:       ds 1
pwm_ratio:        ds 2
result:     ds  2
bcd:        ds  5
x:          ds  4
y:          ds  4
lmc:        ds  4
avg:         ds  4
Result_Cold:     ds 2
Result_Hot:     ds 2
w:   ds 3 ; 24-bit play counter.  Decremented in Timer 1 ISR.

bseg
second_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
abort_flag:  dbit 1
start_flag:        dbit 1
mf: dbit 1



cseg
;LCD PINS
LCD_RS equ P3.2
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7


Initial_Message:  db 'TS  tS  TR  tR  ', 0
time_soak_msg:    db 'SOAK TEMP:     <', 0
temp_soak_msg:    db 'SOAK TIME:     <', 0
time_reflow_msg:  db 'REFLOW TEMP:   <', 0
temp_reflow_msg:  db 'REFLOW TIME:   <', 0
current_temp:     db 'Temp:          <', 0
current_time:     db 'Time:          <', 0 
error:     		  db 'Failed:        <', 0 
error2:     	  db 'Aborted        <', 0 
state1messageT: 	  db 'RamptoSoak      ', 0
state2messageT: 	  db 'Soaking 2 T     ', 0
state3messageT: 	  db 'Ramp->P 3 T     ', 0
state4messageT: 	  db 'Reflow 4 T      ', 0
state5messageT: 	  db 'Cooling 5 T     ', 0
state1messageC:   db 'RamptoSoak C    ', 0
state2messageC:   db 'Soaking 2 C     ', 0
state3messageC:   db 'Ramp->P 3 C     ', 0
state4messageC:   db 'Reflow 4 C      ', 0
state5messageC:   db 'Cooling 5 C     ', 0
blank:            db '                ', 0
stopmessage:	  db 'Reflow stopped  ', 0
;-------------------------------------;
; ISR for Timer 1.  Used to playback  ;
; the WAV file stored in the SPI      ;
; flash memory.                       ;
;-------------------------------------;
Timer1_ISR:
	; The registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Check if the play counter is zero.  If so, stop playing sound.
	mov a, w+0
	orl a, w+1
	orl a, w+2
	jz stop_playing
	
	; Decrement play counter 'w'.  In this implementation 'w' is a 24-bit counter.
	mov a, #0xff
	dec w+0
	cjne a, w+0, keep_playing
	dec w+1
	cjne a, w+1, keep_playing
	dec w+2
	
keep_playing:
	setb SPEAKER
	lcall Send_SPI ; Read the next byte from the SPI Flash...
	mov P0, a ; WARNING: Remove this if not using an external DAC to use the pins of P0 as GPIO
	add a, #0x80
	mov DADH, a ; Output to DAC. DAC output is pin P2.3
	orl DADC, #0b_0100_0000 ; Start DAC by setting GO/BSY=1
	sjmp Timer1_ISR_Done

stop_playing:
	clr TR1 ; Stop timer 1
	setb FLASH_CE  ; Disable SPI Flash
	clr SPEAKER ; Turn off speaker.  Removes hissing noise when not playing sound.
	mov DADH, #0x80 ; middle of range
	orl DADC, #0b_0100_0000 ; Start DAC by setting GO/BSY=1

Timer1_ISR_Done:	
	pop psw
	pop acc
	reti

;---------------------------------;
; Sends AND receives a byte via   ;
; SPI.                            ;
;---------------------------------;
Send_SPI:
	SPIBIT MAC
	    ; Send/Receive bit %0
		rlc a
		mov MY_MOSI, c
		setb MY_SCLK
		mov c, MY_MISO
		clr MY_SCLK
		mov acc.0, c
	ENDMAC
	
	SPIBIT(7)
	SPIBIT(6)
	SPIBIT(5)
	SPIBIT(4)
	SPIBIT(3)
	SPIBIT(2)
	SPIBIT(1)
	SPIBIT(0)

	ret

init:
	; Configure P2.0, P2.4, P2.5 as open drain outputs
	orl P2M0, #0b_0011_0001
	orl P2M1, #0b_0011_0001
	setb MY_MISO  ; Configured as input
	setb FLASH_CE ; CS=1 for SPI flash memory
	clr MY_SCLK   ; Rest state of SCLK=0
	clr SPEAKER   ; Turn off speaker.
	
	; Configure timer 1
	anl	TMOD, #0x0F ; Clear the bits of timer 1 in TMOD
	orl	TMOD, #0x10 ; Set timer 1 in 16-bit timer mode.  Don't change the bits of timer 0
	mov TH1, #high(TIMER1_RELOAD)
	mov TL1, #low(TIMER1_RELOAD)
	; Set autoreload value
	mov RH1, #high(TIMER1_RELOAD)
	mov RL1, #low(TIMER1_RELOAD)

	; Enable the timer and interrupts
    setb ET1  ; Enable timer 1 interrupt
	; setb TR1 ; Timer 1 is only enabled to play stored sound

	; Configure the DAC.  The DAC output we are using is P2.3, but P2.2 is also reserved.
	mov DADI, #0b_1010_0000 ; ACON=1
	mov DADC, #0b_0011_1010 ; Enabled, DAC mode, Left adjusted, CLK/4
	mov DADH, #0x80 ; Middle of scale
	mov DADL, #0
	orl DADC, #0b_0100_0000 ; Start DAC by GO/BSY=1
check_DAC_init:
	mov a, DADC
	jb acc.6, check_DAC_init ; Wait for DAC to finish
	
	setb EA ; Enable interrupts
	
	ret
	

;---------------------------------;
; Timer 2 Stuff                   ;
;---------------------------------;

Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret
;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	;cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	clr c
    mov a, pwm_ratio+0
    subb a, Count1ms+0
    mov a, pwm_ratio+1
    subb a, Count1ms+1
    ; if Count1ms > pwm_ratio  the carry is set.  Just copy the carry to the pwm output pin:
    mov PWM_OUTPUT, c
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ;nstruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb second_flag ; Let the main program know half second had passed
	
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	inc sec

Timer2_ISR_done:
	pop psw
	pop acc
	reti

;SPI ADC SHITANDPISS

INIT_SPI: 
     setb MY_MISO    ; Make MISO an input pin 
     clr MY_SCLK     ; For mode (0,0) SCLK is zero 
     ret 
  
DO_SPI_G: 
    push acc 
    mov R1, #0      ; Received byte stored in R1 
    mov R2, #8      ; Loop counter (8-bits) 
DO_SPI_G_LOOP: 
    mov a, R0       ; Byte to write is in R0 
    rlc a           ; Carry flag has bit to write 
    mov R0, a 
    mov MY_MOSI, c 
    setb MY_SCLK    ; Transmit 
    mov c, MY_MISO  ; Read received bit 
    mov a, R1       ; Save received bit in R1 
    rlc a 
    mov R1, a 
    clr MY_SCLK 
    djnz R2, DO_SPI_G_LOOP 
    pop acc 
    ret 

Left_blank mac
	mov a, %0
	anl a, #0xf0
	swap a
	jz Left_blank_%M_a
	ljmp %1
Left_blank_%M_a:
	Display_char(#' ')
	mov a, %0
	anl a, #0x0f
	jz Left_blank_%M_b
	ljmp %1
Left_blank_%M_b:
	Display_char(#' ')
endmac

Send_BCD mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD
	pop ar0
endmac

?Send_BCD:
	push acc
	;send most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	;send least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret
	
Display_10_digit_BCD:
	Set_Cursor(2, 7)
	Display_BCD(bcd+4)
	Display_BCD(bcd+3)
	Display_BCD(bcd+2)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	; Replace all the zeros to the left with blanks
	Set_Cursor(2, 7)
	Left_blank(bcd+4, skip_blank)
	Left_blank(bcd+3, skip_blank)
	Left_blank(bcd+2, skip_blank)
	Left_blank(bcd+1, skip_blank)
	mov a, bcd+0
	anl a, #0f0h
	swap a
	jnz skip_blank
	Display_char(#' ')
skip_blank:
	ret

; Eight bit number to display passed in ?a?.
; Sends result to LCD
SendToLCD:
	mov b, #100
	div ab
	orl a, #0x30 ; Convert hundreds to ASCII
	lcall ?WriteData ; Send to LCD
	mov a, b ; Remainder is in register b
	mov b, #10
	div ab
	orl a, #0x30 ; Convert tens to ASCII
	lcall ?WriteData; Send to LCD
	mov a, b
	orl a, #0x30 ; Convert units to ASCII
	lcall ?WriteData; Send to LCD
	ret

loadbyte mac
	mov a, %0
	movx @dptr, a
	inc dptr
endmac

putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port (for the temp/putty stuff)
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret
	
Read_ADC:
	;thermocpuple
    clr CE_ADC
    mov R0, #00000001B
    lcall DO_SPI_G
    
    mov R0, #10000000B
    lcall DO_SPI_G
    mov a, R1
    anl a, #00000011B
    mov Result_Hot+1, a
    
    mov R0, #55H
    lcall DO_SPI_G
    mov Result_Hot+0, R1
    setb CE_ADC
    
    ;LM335
    clr CE_ADC
    mov R0, #00000001B
    lcall DO_SPI_G
    
    mov R0, #10010000B
    lcall DO_SPI_G
    mov a, R1
    anl a, #00000011B
    mov Result_Cold+1, a
    
    mov R0, #55H
    lcall DO_SPI_G
    mov Result_Cold+0, R1
    setb CE_ADC
    

	mov x+0, Result_Hot+0
	mov x+1, Result_Hot+1
	mov x+2, #0
	mov x+3, #0
	

	Load_y(282)
	lcall mul32
	
	Load_y(1000)
	lcall div32
	
	mov temp+0, x+0
	mov temp+1, x+1
	mov temp+2, #0
	mov temp+3, #0
 
	mov x+0, Result_Cold+0
	mov x+1, Result_Cold+1
	mov x+2, #0
	mov x+3, #0
	
	load_Y(410)
	lcall mul32
	
	load_Y(1023)
	lcall div32
	
	load_Y(273)
	lcall sub32
	
	mov y+0, temp+0
	mov y+1, temp+1
	mov y+2, #0
	mov y+3, #0
	
	lcall add32
	lcall hex2bcd
	
	lcall Display_10_digit_BCD
	;lcall wait_for_P4_5
	
	mov a, BCD+1
	anl a, #0xf0
	swap a
	orl a, #'0'
	lcall putchar
	
	mov a, BCD+1
	anl a, #0x0f
	orl a, #'0'
	lcall putchar

	mov a, BCD+0
	anl a, #0xf0
	swap a
	orl a, #'0'
	lcall putchar
	
	mov a, BCD+0
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
		
	mov a, #'\r'
	lcall putchar
	   
	mov a, #'\n'
	lcall putchar

	ret
	

InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;

    SendToSerialPort:
	mov b, #100
	div ab
	orl a, #0x30 ; Convert hundreds to ASCII
	lcall putchar ; Send to PuTTY/Python/Matlab
	mov a, b    ; Remainder is in register b
	mov b, #10
	div ab
	orl a, #0x30 ; Convert tens to ASCII
	lcall putchar ; Send to PuTTY/Python/Matlab
	mov a, b
	orl a, #0x30 ; Convert units to ASCII
	lcall putchar ; Send to PuTTY/Python/Matlab
	ret

Save_Configuration:
	push IE
	clr EA
	mov FCON, #0x08 ; Page Buffer Mapping Enabled (FPS = 1)
	mov dptr, #0x7f80 ; Last page of flash memory
	loadbyte(temp_soak) ; @0x7f80
	loadbyte(time_soak) ; @0x7f81
	loadbyte(temp_refl) ; @0x7f82
	loadbyte(time_refl) ; @0x7f83
	loadbyte(#0x55) ; First key value @0x7f84
	loadbyte(#0xAA) ; Second key value @0x7f85
	mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0)
	orl EECON, #0b01000000 ; Enable auto-erase on next write sequence
	mov FCON, #0x50 ; Write trigger first byte
	mov FCON, #0xA0 ; Write trigger second byte
	; CPU idles until writing of flash completes.
	mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0)
	anl EECON, #0b10111111 ; Disable auto-erase
	ret
	
Change_8bit_Variable MAC
	jb %0, %2
	Wait_Milli_Seconds(#50) ; de-bounce
	jb %0, %2
	jnb %0, $
	jb SHIFT_PB, skip%Mb
	dec %1
	sjmp skip%Ma
	skip%Mb:
	inc %1
	skip%Ma:
ENDMAC

get_temp:
	mov x+0, result+0
    mov x+1, result+1
    mov x+2, #0
    mov x+3, #0
    
    Load_Y(282)
    lcall mul32
    
    Load_Y(1000)
    lcall div32
    
    Load_Y(22)
    lcall add32
    
    lcall hex2bcd
    Send_BCD(bcd+1)
    Send_BCD(bcd)
    
    ret
    
getbyte mac
	clr a
	movc a, @a+dptr
	mov %0, a
	inc dptr
Endmac

Load_Configuration:
	mov dptr, #0x7f84 ; First key value location.
	getbyte(R0) ; 0x7f84 should contain 0x55
	cjne R0, #0x55, Load_Defaults
	getbyte(R0) ; 0x7f85 should contain 0xAA
	cjne R0, #0xAA, Load_Defaults
	; Keys are good. Get stored values.
	mov dptr, #0x7f80
	getbyte(temp_soak) ; 0x7f80
	getbyte(time_soak) ; 0x7f81
	getbyte(temp_refl) ; 0x7f82
	getbyte(time_refl) ; 0x7f83
	ret

; Load defaults if 'keys' are incorrect
Load_Defaults:
	
	mov temp_soak, #30
	mov time_soak, #25
	mov temp_refl, #30
	mov time_refl, #25
	ret
	
display_temp mac
	mov a, %0
	mov b, #100
    div	ab	
    add	a, #0x30
    Set_Cursor(2,13)
    mov	acc, a
    setb LCD_RS
    lcall LCD_byte
    mov	a, b
    mov	b, #10
    div	ab
    add	a, #0x30
    Set_Cursor(2,14)
    mov	acc, a
    setb LCD_RS
    lcall LCD_byte
    mov	a, b
    add	a, #0x30
    Set_Cursor(2,15)
    mov	acc, a
    setb LCD_RS
    lcall LCD_byte
endmac

main:
	;Initialization
	mov SP, #0x7F

	mov P0M0, #0
	mov P0M1, #0
    mov sec, #0x00
    mov state, #0

	lcall Load_Configuration
	lcall Init
	lcall Timer2_Init
	setb EA ; Enable interrupts
	lcall LCD_4BIT
	lcall INIT_SPI
    lcall InitSerialPort
    clr second_flag
    clr abort_flag
	mov pwm_ratio+0, #low(0)
	mov pwm_ratio+1, #high(0)
    Set_Cursor(1,1)
	Send_Constant_String(#Initial_Message)
	Set_Cursor(2,1)
	mov a, temp_soak
	lcall SendToLCD
	Set_Cursor(2,5)
	mov a, time_soak
	lcall SendToLCD
	Set_Cursor(2,9)
	mov a, temp_refl
	lcall SendToLCD
	Set_Cursor(2,13)
	mov a, time_refl
	lcall SendToLCD
    	; After initialization the program stays in this 'forever' loop
loop:
	Change_8bit_Variable(TEMP_SOAK_PB, temp_soak, loop_a)
	Set_Cursor(2, 1)
	mov a, temp_soak
	lcall SendToLCD
	lcall Save_Configuration
loop_a:
	Change_8bit_Variable(TIME_SOAK_PB, time_soak, loop_b)
	Set_Cursor(2, 5)
	mov a, time_soak
	lcall SendToLCD
	lcall Save_Configuration
loop_b:
	Change_8bit_Variable(TEMP_REFL_PB, temp_refl, loop_c)
	Set_Cursor(2, 9)
	mov a, temp_refl
	lcall SendToLCD
	lcall Save_Configuration
loop_c:
	Change_8bit_Variable(TIME_REFL_PB, time_refl, forever_loop)
	Set_Cursor(2, 13)
	mov a, time_refl
	lcall SendToLCD
	lcall Save_Configuration
	ljmp FSM1
	
forever_loop:
	jnb second_flag, loop_f
	clr second_flag
	
	lcall Read_ADC
	
	;lcall get_temp
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
    ljmp FSM1
	
loop_f:
	;ljmp forever_loop
	ljmp loop_a
    
FSM1:
    mov a, state 
    

state0:
	
    cjne a, #0, state1
    mov pwm_ratio+0, #low(0);0
	mov pwm_ratio+1, #high(0);0
    jb START_PB, state0_done
    Wait_Milli_Seconds(#50) ; debouncing
	jb START_PB, state0_done
    jnb START_PB, $
    mov state, #1
    ljmp state1
    mov sec, #0x00
    
state0_done:
	ljmp loop
	
state2a:
	ljmp state2

state1:
	cjne a, #1, state2a
	Set_Cursor(1, 1)
	Send_Constant_String(#state1messageT)
	Set_Cursor(1, 13)
	mov a, sec
	lcall SendToLCD
	Set_Cursor(2, 1)
	Send_Constant_String(#temp_soak_msg)
	display_temp(x)
	mov pwm_ratio+0, #low(1000)
	mov pwm_ratio+1, #high(1000)
	jb abort_flag, Safety_Passed
    jb STOP_PB, check_temp
    Wait_Milli_Seconds(#50) ; debouncing
	jb STOP_PB, check_temp
    jnb STOP_PB, $
    ljmp stop
check_temp:
	mov a, x
	cjne a, #50, check_time
	ljmp set_flag
	
	check_time:
	mov a, sec
	cjne a, #60, Safety_Passed
	ljmp ERROR_alt
	
	set_flag:
	SETB abort_flag
	
	Safety_Passed:
	mov a, temp_soak
	clr c
	subb a, x
	jnc state1_done
	mov state, #2
	mov sec, #0

state1_done:
	ljmp forever_loop

ERROR_alt:
	ljmp state_ERROR
state3_alt:
	ljmp state3	

state2:
	cjne a, #2, state3_alt
	Set_Cursor(1, 1)
	Send_Constant_String(#state2messageT)
	Set_Cursor(1, 13)
	mov a, sec
	lcall SendToLCD
	Set_Cursor(2, 1)
	Send_Constant_String(#state2messageC)
	display_temp(x)
	mov pwm_ratio+0, #low(200)
	mov pwm_ratio+1, #high(200)
	mov a, time_soak
	clr c
	subb a, sec
	jb STOP_PB, Break
    Wait_Milli_Seconds(#50) ; debouncing
	jb STOP_PB, Break
    jnb STOP_PB, $
    ljmp stop
Break:
	jnc state2_done
	mov state, #3
	mov sec, #0

state2_done:
	ljmp forever_loop

state0_alt:
	ljmp state0
state4_alt:
	ljmp state4
state3:
	cjne a, #3, state4_alt
	Set_Cursor(1, 1)
	Send_Constant_String(#state3messageT)
	Set_Cursor(1, 13)
	mov a, sec
	lcall SendToLCD
	Set_Cursor(2, 1)
	Send_Constant_String(#state3messageC)
	display_temp(x)
    mov pwm_ratio+0, #low(1000)
    mov pwm_ratio+1, #high(1000)
    mov a, temp_refl
    subb a, x
	jb STOP_PB, Break2
    Wait_Milli_Seconds(#50) ; debouncing
	jb STOP_PB, Break2
    jnb STOP_PB, $
    ljmp stop
Break2:
    jnc state3_done
    mov state, #4
    mov sec, #0x00
state3_done:
	ljmp forever_loop
stop:
	Set_Cursor(1,1)
	Send_Constant_String(#stopmessage)
	Set_Cursor(2,1)
	Send_Constant_String(#blank)
	mov pwm_ratio+0, #low(0)
    mov pwm_ratio+1, #high(0)
    ljmp stop
state5_alt:
	ljmp state5
state4:
	cjne a, #4, state5_alt
	Set_Cursor(1, 1)
	Send_Constant_String(#state4messageT)
	Set_Cursor(1, 13)
	mov a, sec
	lcall SendToLCD
	Set_Cursor(2, 1)
	Send_Constant_String(#state4messageC)
	display_temp(x)
    mov pwm_ratio+0, #low(200)
    mov pwm_ratio+1, #high(200)
    mov a, time_refl
    clr c
    subb a , sec
	jb STOP_PB, Break3
    Wait_Milli_Seconds(#50) ; debouncing
	jb STOP_PB, Break3
    jnb STOP_PB, $
    ljmp stop
Break3:
    jnc state4_done
    mov state, #5
state4_done:
	ljmp forever_loop
state0_C:
	ljmp state0_alt
state5:
	cjne a, #5, state0_C
	Set_Cursor(1, 1)
	Send_Constant_String(#state5messageT)
	Set_Cursor(1, 13)
	mov a, sec
	lcall SendToLCD
	Set_Cursor(2, 1)
	Send_Constant_String(#state5messageC)
	display_temp(x)
    mov pwm_ratio+0, #low(0)
    mov pwm_ratio+1, #high(0)
    Load_Y(30)
	lcall x_lt_y
	jnb mf, loop_start


loop_start:
	mov state, #0

state5_done:
	ljmp forever_loop
	
state_ERROR:
    Set_Cursor(1,1)
  	Send_Constant_String(#error)
  	Set_Cursor(2,1)
  	Send_Constant_String(#error2)
  	sjmp state_ERROR

END