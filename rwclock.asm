; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.1 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P1.1
UPDOWN        equ P2.4
INCSECONDS    equ P0.2
INCMINUTES    equ P0.4
INCHOURS      equ P0.6
TOGGLEAMPM    equ P0.0
TOGGLEALARM   equ P2.0

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_CounterSeconds:  ds 2 ; The BCD counter incrememted in the ISR and displayed in the main loop
BCD_CounterMinutes:  ds 2
BCD_CounterHours:    ds 2
Clock_CounterSeconds:ds 2 ; The BCD counter incrememted in the ISR and displayed in the main loop
Clock_CounterMinutes:ds 2
Clock_CounterHours:  ds 2
CounterVariable:     ds 1
ClockCounter:        ds 1
AmOrPmCounter:       ds 1
ToggleAlarmOnOff:    ds 1
SimpleCounter:       ds 1

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Time HH:MM:SS ', 0
Initial_Message2: db 'OFF  HH:MM:SS AM ', 0
Alarm_Message1 :  db 'OFF  00:00:00 ', 0
Alarm_Message2 :  db 'ON   00:00:00 ', 0
PM             :  db 'PM'            , 0
AM             :  db 'AM'            , 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	
	
	cpl SOUND_OUT ; Connect speaker to P1.1!
	

	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
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
	cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done_4 ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done_4
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
    ;counter
	mov a, SimpleCounter
    ;if the counter doesnt equal to 1 jump to branch "jump1"
	cjne a, #0x01, jump1

    ;clear a to clear and mov bcd_counters into a
	clr a
	add a, BCD_CounterHours
	subb a, Clock_CounterHours ;here we subtract so that timer knows when to trigger sound
	cjne a, #0x00, jump1 ;if a is equal to 0 then we know that the alarm is not on so we jump to the next branch
	mov a, #0x00
	add a, BCD_CounterMinutes
	subb a, Clock_CounterMinutes
	cjne a, #0x00, jump1
 	mov a, #0x00
	add a, BCD_CounterSeconds
	subb a, Clock_CounterSeconds
	cjne a, #0x00, jump1
	setb TR0 ;trigger alarm
	sjmp CHANGEALARM

jump1:
;here we clear timer 0
	clr TR0
    ;proceed with code
CHANGEALARM:
    ;now to check if button P2.0 is pressed to trigger alarm change
	jb TOGGLEALARM, jump2
	Wait_Milli_Seconds(#50)
    jb TOGGLEALARM, jump2
	mov a, ToggleAlarmOnOff

    ;if button is pressed then it would connect to ground(0)
    ;if button isnt pressed then alarm is off
	cjne a, #0x01, ALARMON
	dec ToggleAlarmOnOff
	dec SimpleCounter
	Set_Cursor(2, 1)
    Send_Constant_String(#Alarm_Message1)
    
	cjne a, #0x99, jump2
Timer2_ISR_done_4:
	ljmp Timer2_ISR_done_3
jump2:
	sjmp jump3

ALARMON:
	Set_Cursor(2, 1)
	Send_Constant_String(#Alarm_Message2)
	inc ToggleAlarmOnOff
	inc SimpleCounter

jump3:
	clr a
    ;this function basically forces to go to the next branch because
    ;since we clear a, a will never equal 99
	cjne a, #0x99, CHANGETIME
Timer2_ISR_done_3:
	ljmp Timer2_ISR_done_2

CHANGETIME:
 ;if the button that increments hours is not pressed go to the branch that
 ;will increment minutes
	jb INCHOURS, notIncHours
	Wait_Milli_Seconds(#50)
	jb INCHOURS, notIncHours
	mov a, ToggleAlarmOnOff
	cjne a, #0x01, TIMEHOURS
	mov a, Clock_CounterHours
	add a, #0x01
	da a
	mov Clock_CounterHours, a 
	clr a    
	sjmp notIncHours
TIMEHOURS:
    ;add one to the BCDCOUNTER FOR TIME
	mov a, BCD_CounterHours
	add a, #0x01
	da a
	mov BCD_CounterHours, a  
	clr a    

notIncHours:
    ;if the button that increments minutes is not pressed go to the branch that
    ;will increment minutes
    jb INCMINUTES, notIncMinutes
	Wait_Milli_Seconds(#50)
	jb INCMINUTES, notIncMinutes
	mov a, ToggleAlarmOnOff
	cjne a, #0x01, TIMEMINUTES
	mov a, Clock_CounterMinutes
	add a, #0x01
	da a
	mov Clock_CounterMinutes, a 
	clr a    
	sjmp notIncMinutes
TIMEMINUTES:
	mov a, BCD_CounterMinutes
	add a, #0x01
	da a
	mov BCD_CounterMinutes, a   
	clr a               
notIncMinutes:
    ;if the button that increments seconds is not pressed go to the branch that
    ;will increment seconds
	jb INCSECONDS, notIncSeconds
	Wait_Milli_Seconds(#50)
	jb INCSECONDS, notIncSeconds
	mov a, ToggleAlarmOnOff
	cjne a, #0x01, TIMESECONDS
	mov a, Clock_CounterSeconds
	add a, #0x01
	da a
	mov Clock_CounterSeconds, a   
	clr a  
	sjmp notIncSeconds
TIMESECONDS:
	mov a, BCD_CounterSeconds
	add a, #0x01
	da a
	mov BCD_CounterSeconds, a  
	clr a  
notIncSeconds:
 ;this condition will never equal 61 because we cleared above
 ;which means it will go straight to the next branch
	cjne a, #0x61, TIMEAMPM

Timer2_ISR_done_1:
	ljmp Timer2_ISR_done

TIMEAMPM:
 ; this function checks if it is am or pm and branches to make changes
	jb TOGGLEAMPM, ContinueOn
	Wait_Milli_Seconds(#50)
	mov a, CounterVariable
	Set_Cursor(1,15)
	cjne a, #0x00, ChangeToAM
	mov a, ToggleAlarmOnOff
	cjne a, #0x01, ChangeToPM
	Set_Cursor(2,15)
ChangeToPM:
	Send_Constant_String(#PM)
	inc CounterVariable
	sjmp ContinueOn
ChangeToAM:
	mov a, ToggleAlarmOnOff
	cjne a, #0x01, GoNext6
	Set_Cursor(2,15)
GoNext6:
	Send_Constant_String(#AM)
	dec CounterVariable
ContinueOn:
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_CounterSeconds
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	cjne a, #0x99, ResumeTimeIncrement
Timer2_ISR_done_2:
	ljmp Timer2_ISR_done_1

ResumeTimeIncrement:
	cjne a, #0x60, not60s
	clr a		
	inc BCD_CounterMinutes
	mov a, BCD_CounterMinutes
	da a
	cjne a, #0x60, not60m
	clr a
	inc BCD_CounterHours
	mov a, BCD_CounterHours
	da a
	cjne a, #0x12, twelveHOUR
	mov a, #0x00
	mov a, CounterVariable
	Set_Cursor(1,15)
	cjne a, #0x00, PMtoAM
	Send_Constant_String(#PM)
	inc CounterVariable
	mov a, #0x12
	sjmp normalcase
twelveHOUR:
	cjne a, #0x13, normalcase
	sjmp next1
PMtoAM:
	Send_Constant_String(#AM)
	dec CounterVariable
	sjmp normalcase
next1:
	da a
	clr a
	mov BCD_CounterSeconds, a
	mov BCD_CounterMinutes, a
	mov a, #0x01
	mov BCD_CounterHours, a
normalcase:
	mov BCD_CounterHours, a
	clr a
not60m:
	mov BCD_CounterMinutes, a
	clr a
not60s:
	mov BCD_CounterSeconds, a
	clr a
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_Cursor(2, 1)
    Send_Constant_String(#Initial_Message2)
    Set_Cursor(1,15)
    Send_Constant_String(#AM)
    setb half_seconds_flag
	mov BCD_CounterSeconds, #0x00
	mov BCD_CounterMinutes, #0x00
	mov BCD_CounterHours, #0x00
	mov Clock_CounterSeconds, #0x00
	mov Clock_CounterMinutes, #0x00
	mov Clock_CounterHours, #0x00
	mov CounterVariable, #0x00
	mov ClockCounter, #0x00
	mov AmOrPmCounter, #0x00
	mov ToggleAlarmOnOff, #0x00
	mov SimpleCounter, #0x00

	
	; After initialization the program stays in this 'forever' loop
loop:	
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a

	; Now clear the BCD counter
	mov BCD_CounterSeconds, a
	setb TR2                ; Start timer 2
	sjmp loop_b             ; Display the new value
	
loop_a:
	jnb half_seconds_flag, loop
loop_b:
    clr half_seconds_flag 
    
	
    ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    
	Set_Cursor(1, 12)    ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_CounterSeconds) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 9)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_CounterMinutes) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_CounterHours) ; This macro is also in 'LCD_4bit.inc'
	
	Set_Cursor(2, 12)    ; the place in the LCD where we want the BCD counter value
	Display_BCD(Clock_CounterSeconds) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 9)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(Clock_CounterMinutes) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(Clock_CounterHours) ; This macro is also in 'LCD_4bit.inc'
    ljmp loop
END