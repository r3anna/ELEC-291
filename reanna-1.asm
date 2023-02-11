



; LCD_test_4bit.asm: Initializes and uses an LCD in 4-bit mode
; using the most common procedure found on the internet.
$NOLIST
$MODLP51RC2
$LIST


org 0000H
    ljmp myprogram


; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7


; When using a 22.1184MHz crystal in fast mode
; one cycle takes 1.0/22.1184MHz = 45.21123 ns


;---------------------------------;
; Wait 40 microseconds            ;
;---------------------------------;
Wait40uSec:
    push AR0
    mov R0, #177
L0:
    nop
    nop
    djnz R0, L0 ; 1+1+3 cycles->5*45.21123ns*177=40us
    pop AR0
    ret


;---------------------------------;
; Wait 'R2' milliseconds          ;
;---------------------------------;
WaitmilliSec:
    push AR0
    push AR1
L3: mov R1, #45
L2: mov R0, #250
L1: djnz R0, L1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, L2 ; 22.51519us*45=1.013ms
    djnz R2, L3 ; number of millisecons to wait passed in R2
    pop AR1
    pop AR0
    ret


;---------------------------------;
; Toggles the LCD's 'E' pin       ;
;---------------------------------;
LCD_pulse:
    setb LCD_E
    lcall Wait40uSec
    clr LCD_E
    ret


;---------------------------------;
; Writes data to LCD              ;
;---------------------------------;
WriteData:
    setb LCD_RS
    ljmp LCD_byte


;---------------------------------;
; Writes command to LCD           ;
;---------------------------------;
WriteCommand:
    clr LCD_RS
    ljmp LCD_byte


;---------------------------------;
; Writes acc to LCD in 4-bit mode ;
;---------------------------------;
LCD_byte:
    ; Write high 4 bits first
    mov c, ACC.7
    mov LCD_D7, c
    mov c, ACC.6
    mov LCD_D6, c
    mov c, ACC.5
    mov LCD_D5, c
    mov c, ACC.4
    mov LCD_D4, c
    lcall LCD_pulse


    ; Write low 4 bits next
    mov c, ACC.3
    mov LCD_D7, c
    mov c, ACC.2
    mov LCD_D6, c
    mov c, ACC.1
    mov LCD_D5, c
    mov c, ACC.0
    mov LCD_D4, c
    lcall LCD_pulse
    ret


;---------------------------------;
; Configure LCD in 4-bit mode     ;
;---------------------------------;
LCD_4BIT:
    clr LCD_E   ; Resting state of LCD's enable is zero
    ; clr LCD_RW  ; Not used, pin tied to GND


    ; After power on, wait for the LCD start up time before initializing
    ; NOTE: the preprogrammed power-on delay of 16 ms on the AT89LP51RC2
    ; seems to be enough.  That is why these two lines are commented out.
    ; Also, commenting these two lines improves simulation time in Multisim.
    ; mov R2, #40
    ; lcall WaitmilliSec


    ; First make sure the LCD is in 8-bit mode and then change to 4-bit mode
    mov a, #0x33
    lcall WriteCommand
    mov a, #0x33
    lcall WriteCommand
    mov a, #0x32 ; change to 4-bit mode
    lcall WriteCommand


    ; Configure the LCD
    mov a, #0x28
    lcall WriteCommand
    mov a, #0x0c
    lcall WriteCommand
    mov a, #0x01 ;  Clear screen command (takes some time)
    lcall WriteCommand


    ;Wait for clear screen command to finish. Usually takes 1.52ms.
    mov R2, #2
    lcall WaitmilliSec
    ret
   
  animationcat:
   
    mov a, #0xC8
    lcall WriteCommand
    mov a, #0H
    lcall WriteData
    lcall WaitmilliSec
    mov a, #0xC8
    lcall WriteCommand
    mov a, #1H
    lcall WriteData
    lcall WaitmilliSec
    mov a, #0x1c
    lcall WriteCommand

    lcall animationcat


;---------------------------------;
; Main loop.  Initialize stack,   ;
; ports, LCD, and displays        ;
; letters on the LCD              ;
;---------------------------------;
myprogram:
    mov SP, #7FH
    lcall LCD_4BIT


    ;NAME
    mov a, #0x80 ; Move cursor to line 1 column 1
    lcall WriteCommand
    mov a, #'R'
    lcall WriteData
   
    mov a, #0x81
    lcall WriteCommand
    mov a, #'E'
    lcall WriteData


    mov a, #0x82
    lcall WriteCommand
    mov a, #'A'
    lcall WriteData
   
    mov a, #0x83
    lcall WriteCommand
    mov a, #'N'
    lcall WriteData
   
    mov a, #0x84
    lcall WriteCommand
    mov a, #'N'
    lcall WriteData
   
    mov a, #0x85
    lcall WriteCommand
    mov a, #'A'
    lcall WriteData
   
    mov a, #0x86
    lcall WriteCommand
    mov a, #' '
    lcall WriteData
   
    mov a, #0x87
    lcall WriteCommand
    mov a, #'W'
    lcall WriteData
   
    mov a, #0x88
    lcall WriteCommand
    mov a, #'O'
    lcall WriteData
   
    mov a, #0x89
    lcall WriteCommand
    mov a, #'N'
    lcall WriteData
   
    mov a, #0x8A
    lcall WriteCommand
    mov a, #'G'
    lcall WriteData
   

   mov   a,#0x50H         ;Load the location where we want to store
    lcall WriteCommand    ;Send the command
    mov   a,#0x1H         ;Load row 1 data
    lcall WriteData   ;Send the data
    mov   a,#0x2H         ;Load row 2 data
    lcall WriteData   ;Send the data
    mov   a,#0x4H         ;Load row 3 data
    lcall WriteData   ;Send the data
    mov   a,#0x8H         ;Load row 4 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 5 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 6 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 7 data
    lcall WriteData   ;Send the data
    mov   a,#0x10H         ;Load row 8 data
    lcall WriteData   ;Send the data
   
    mov a, #0x8c
    lcall WriteCommand
    mov a,#2H ; For pattern @40H
    lcall WriteData   
       

    mov   a,#0x58H         ;Load the location where we want to store
    lcall WriteCommand    ;Send the command
    mov   a,#0x1fH         ;Load row 1 data
    lcall WriteData   ;Send the data
    mov   a,#0x0H         ;Load row 2 data
    lcall WriteData   ;Send the data
    mov   a,#0x0H         ;Load row 3 data
    lcall WriteData   ;Send the data
    mov   a,#0x0H         ;Load row 4 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 5 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 6 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 7 data
    lcall WriteData   ;Send the data
    mov   a,#0x0H         ;Load row 8 data
    lcall WriteData   ;Send the data
   
    mov a, #0x8d
    lcall WriteCommand
    mov a,#3H ; For pattern @40H
    lcall WriteData          




mov   a,#0x60H         ;Load the location where we want to store
    lcall WriteCommand    ;Send the command
    mov   a,#0x10H         ;Load row 1 data
    lcall WriteData   ;Send the data
    mov   a,#0x08H         ;Load row 2 data
    lcall WriteData   ;Send the data
    mov   a,#0x4H         ;Load row 3 data
    lcall WriteData   ;Send the data
    mov   a,#0x2H         ;Load row 4 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 5 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 6 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 7 data
    lcall WriteData   ;Send the data
    mov   a,#0x1H         ;Load row 8 data
    lcall WriteData   ;Send the data
   
    mov a, #0x8e
    lcall WriteCommand
    mov a,#4H ; For pattern @40H
    lcall WriteData  


    ;STUDENT NUMBER
    mov a, #0xC0
    lcall WriteCommand
    mov a, #'9'
    lcall WriteData
   
    mov a, #0xC1
    lcall WriteCommand
    mov a, #'8'
    lcall WriteData
   
    mov a, #0xC2
    lcall WriteCommand
    mov a, #'7'
    lcall WriteData
   
    mov a, #0xC3
    lcall WriteCommand
    mov a, #'1'
    lcall WriteData
   
    mov a, #0xC4
    lcall WriteCommand
    mov a, #'7'
    lcall WriteData
   
    mov a, #0xC5
    lcall WriteCommand
    mov a, #'8'
    lcall WriteData
   
    mov a, #0xC6
    lcall WriteCommand
    mov a, #'4'
    lcall WriteData
   
    mov a, #0xC7
    lcall WriteCommand
    mov a, #'6'
    lcall WriteData


mov   a,#0x68H         ;Load the location where we want to store
    lcall WriteCommand    ;Send the command
    mov   a,#0x14H         ;Load row 1 data
    lcall WriteData   ;Send the data
    mov   a,#0x14H         ;Load row 2 data
    lcall WriteData   ;Send the data
    mov   a,#0x12H         ;Load row 3 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 4 data
    lcall WriteData   ;Send the data
    mov   a,#0x8H         ;Load row 5 data
    lcall WriteData   ;Send the data
    mov   a,#0x4H         ;Load row 6 data
    lcall WriteData   ;Send the data
    mov   a,#0x2H         ;Load row 7 data
    lcall WriteData   ;Send the data
    mov   a,#0x1H         ;Load row 8 data
    lcall WriteData   ;Send the data
   
    mov a, #0xcc
    lcall WriteCommand
    mov a,#5H ; For pattern @40H
    lcall WriteData      


mov   a,#0x70H         ;Load the location where we want to store
    lcall WriteCommand    ;Send the command
    mov   a,#0x0H         ;Load row 1 data
    lcall WriteData   ;Send the data
    mov   a,#0x0H         ;Load row 2 data
    lcall WriteData   ;Send the data
    mov   a,#0x0H         ;Load row 3 data
    lcall WriteData   ;Send the data
    mov   a,#0x0H         ;Load row 4 data
    lcall WriteData   ;Send the data
    mov   a,#0x1fH         ;Load row 5 data
    lcall WriteData   ;Send the data
    mov   a,#0x0H         ;Load row 6 data
    lcall WriteData   ;Send the data
    mov   a,#0x0H         ;Load row 7 data
    lcall WriteData   ;Send the data
    mov   a,#0x1fH         ;Load row 8 data
    lcall WriteData   ;Send the data
   
    mov a, #0xcd
    lcall WriteCommand
    mov a,#6H ; For pattern @40H
    lcall WriteData         


mov   a,#0x78H         ;Load the location where we want to store
    lcall WriteCommand    ;Send the command
    mov   a,#0x5H         ;Load row 1 data
    lcall WriteData   ;Send the data
    mov   a,#0x5H         ;Load row 2 data
    lcall WriteData   ;Send the data
    mov   a,#0x9H         ;Load row 3 data
    lcall WriteData   ;Send the data
    mov   a,#0x11H         ;Load row 4 data
    lcall WriteData   ;Send the data
    mov   a,#0x2H         ;Load row 5 data
    lcall WriteData   ;Send the data
    mov   a,#0x4H         ;Load row 6 data
    lcall WriteData   ;Send the data
    mov   a,#0x8H         ;Load row 7 data
    lcall WriteData   ;Send the data
    mov   a,#0x10H         ;Load row 8 data
    lcall WriteData   ;Send the data
   
    mov a, #0xce
    lcall WriteCommand
    mov a,#7H ; For pattern @40H
    lcall WriteData    




   ;CAT 1
    mov a, #0xC8
   
    mov   a,#40H         ;Load the location where we want to store
    lcall WriteCommand    ;Send the command
    mov   a,#00H         ;Load row 1 data
    lcall WriteData   ;Send the data
    mov   a,#14H         ;Load row 2 data
    lcall WriteData   ;Send the data
    mov   a,#1dH         ;Load row 3 data
    lcall WriteData   ;Send the data
    mov   a,#1dH         ;Load row 4 data
    lcall WriteData   ;Send the data
    mov   a,#09H         ;Load row 5 data
    lcall WriteData   ;Send the data
    mov   a,#1dH         ;Load row 6 data
    lcall WriteData   ;Send the data
    mov   a,#1EH         ;Load row 7 data
    lcall WriteData   ;Send the data
    mov   a,#00H         ;Load row 8 data
    lcall WriteData   ;Send the data
   
    lcall WaitmilliSec
   
    mov a, #0xC8
    lcall WriteCommand
    mov a,#1H ; For pattern @40H
    lcall WriteData          
   
   
    mov   a,#48H         ;Load the location where we want to store
    lcall WriteCommand    ;Send the command
    mov   a,#00H         ;Load row 1 data
    lcall WriteData   ;Send the data
    mov   a,#05H         ;Load row 2 data
    lcall WriteData   ;Send the data
    mov   a,#17H         ;Load row 3 data
    lcall WriteData   ;Send the data
    mov   a,#17H         ;Load row 4 data
    lcall WriteData   ;Send the data
    mov   a,#12H         ;Load row 5 data
    lcall WriteData   ;Send the data
    mov   a,#17H         ;Load row 6 data
    lcall WriteData   ;Send the data
    mov   a,#1FH         ;Load row 7 data
    lcall WriteData   ;Send the data
    mov   a,#00H         ;Load row 8 data
    lcall WriteData   ;Send the data
   
    mov a, #0xC8
    lcall WriteCommand
    mov a,#0H ; For pattern @40H
    lcall WriteData          
   
    lcall animationcat
; smiley face time
    




    


   
forever:
    sjmp forever
END



