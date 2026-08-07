;=============================================================================
; @(#)floppyti.asm  0.2  2014/04/23
;   ________        _________________.________
;  /  _____/  ____ /   _____/   __   \   ____/
; /   \  ___ /  _ \\_____  \\____    /____  \
; \    \_\  (  <_> )        \  /    //       \
;  \______  /\____/_______  / /____//______  /
;         \/              \/               \/
; Copyright (c) 2014 by Alessandro Fraschetti.
;
; Description: floppyti. Floppy, fottiti!
; Target.....: Microchip PIC 16F628A Microcontroller
; Compiler...: Microchip Assembler (MPASM)
; Note.......:
;
; MIT License
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
;=============================================================================

        processor   16f628a
        #include    <p16f628a.inc>
      ;  __config   _CP_OFF & _CPD_OFF & _BODEN_OFF & _LVP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC
      ;  __CONFIG   _CP_OFF & _DATA_CP_OFF & _LVP_OFF & _BOREN_OFF & _MCLRE_OFF & _WDT_OFF & _PWRTE_ON & _INTOSC_OSC_NOCLKOUT
      ;  __CONFIG    _CP_OFF & _DATA_CP_OFF & _LVP_OFF & _BOREN_OFF & _WDT_OFF & _MCLRE_ON & _PWRTE_ON & _HS_OSC
        __CONFIG    _CP_OFF & _LVP_OFF & _BOREN_OFF & _WDT_OFF & _MCLRE_OFF & _PWRTE_ON & _HS_OSC
                    ; _CP_[ON/OFF]    : code protect program memory enable/disable
                    ; _CPD_[ON/OFF]   : code protect data memory enable/disable
                    ; _LVP_[ON/OFF]   : Low Voltage ICSP enable/disable
                    ; _BODEN_[ON/OFF] : Brown-Out Reset enable/disable
                    ; _WDT_[ON/OFF]   : watchdog timer enable/disable
                    ; _MCLRE_[ON/OFF] : MCLR pin function  digital IO/MCLR
                    ; _PWRTE_[ON/OFF] : power-up timer enable/disable
;=============================================================================

;=============================================================================
;  Manifest constants
;=============================================================================
; FDD Consts
FDD         equ     PORTA
DIR         equ     RA2
STEP        equ     RA3
DRVSEL      equ     RA4
FDDRANGE    equ	    .1
HOMESTEPS   equ     .90
CENTERSTEPS equ     .40


; Config Consts
DIPSW	    equ	    PORTB

; Midi Consts
COMM        equ     PORTB
IACTIVITY   equ     RB0
IN          equ     RB1
OACTIVITY   equ     RB3

; midiIOStatus Flags
INDATA      equ     0x00
OUTDATA     equ     0x01

; messageStatusByte Flags
STATUSBYTE  equ     0x00
DATABYTE1   equ     0x01
DATABYTE2   equ     0x02
NOTEON      equ     0x04

; fddStatus Flags
DIRFLAG     equ     0x04
;=============================================================================

;=============================================================================
;  File register use
;=============================================================================
        cblock      h'20'
            w_temp                      ; variable used for context saving
            status_temp                 ; variable used for context saving
            pclath_temp                 ; variable used for context saving

            d1, d2, d3                  ; delay routine vars
	    d4

            midiInByte                  ; midi-in Byte Register
            midiIOStatus                ; midi-in/out Status Register
            midiInListenChannel         ; midi-in listen channel

            msgStatusByte               ; midi-in message: Status Byte Register
            msgDataByte1                ; midi-in message: Data Byte 1 Register
            msgDataByte2                ; midi-in message: Data Byte 2 Register
            msgStatus                   ; midi-in message Status Register

            noteOnCounter               ; midi notes-on counter
            noteToPlay                  ; midi note to play out

            fddStatus                   ; fdd status register
	    fddPos                      ; fdd head pos within the sweep range
            tickCounter                 ; counter for note task execution
            outputStatus                ; output (on/off) status register
        endc
;=============================================================================

;=============================================================================
;  Start of code
;=============================================================================
;start
        org         h'0000'             ; processor reset vector
        goto        main                ; jump to the main routine

        org         h'0004'             ; interrupt vector location
        movwf       w_temp              ; save off current W register contents
        movf        STATUS, W           ; move status register into W register
        movwf       status_temp         ; save off contents of STATUS register
        movf        PCLATH, W           ; move pclath register into W register
        movwf       pclath_temp         ; save off contents of PCLATH register

        ; isr code can go here or be located as a call subroutine elsewhere

        movf        pclath_temp, W      ; retrieve copy of PCLATH register
        movwf       PCLATH              ; restore pre-isr PCLATH register contents
        movf        status_temp, W      ; retrieve copy of STATUS register
        movwf       STATUS              ; restore pre-isr STATUS register contents
        swapf       w_temp, F
        swapf       w_temp, W           ; restore pre-isr W register contents
        retfie                          ; return from interrupt
;=============================================================================

;=============================================================================
;  Init I/O ports
;=============================================================================
init_ports
        errorlevel  -302

        ; set PORTA (RA0-RA7) as:
        ;   RA2 Out=DIR, RA3 Out=STEP, RA4 Out=DRVSEL
        bcf         STATUS, RP0             ; select Bank0
        clrf        PORTA                   ; initialize PORTA by clearing output data latches
        movlw       h'07'                   ; turn comparators off
        movwf       CMCON                   ; and set port A mode I/O digital
        bsf         STATUS, RP0             ; select Bank1
        movlw       b'11100000'             ; PORTA input/output
        movwf       TRISA

        ; set PORTB (RB0-RB3) as:
        ;   Out=IN Activity, In=MIDI-IN, Out= -, Out=OUT Activity
        ; set PORTB (RB4-RB7) as:
        ;   All-Input=MIDI-IN CHANNEL
        bcf         STATUS, RP0             ; select Bank0
        clrf        PORTB                   ; initialize PORTB by clearing output data latches
        bsf         STATUS, RP0             ; select Bank1
        movlw       b'11110010'             ; PORTB input/output
        movwf       TRISB

        bcf         STATUS, RP0             ; select Bank0

        errorlevel  +302

        return
;=============================================================================

;=============================================================================
;  Init USART
;=============================================================================
init_usart
        errorlevel  -302

        bsf         STATUS, RP0             ; select Bank1
        movlw       h'09'                   ; 31250 bauds on 20MHz osc.
        movwf       SPBRG
        movlw       b'00000000'             ; async tx 8 bit
        movwf       TXSTA
        bcf         STATUS, RP0             ; return to page 0
        movlw       b'10010000'             ; async rx 8 bit
        movwf       RCSTA
        bcf         STATUS, RP0             ; select Bank0

        errorlevel  +302

        return
;=============================================================================

;=============================================================================
;  Init Timer0 (internal clock source)
;=============================================================================
init_timer
        errorlevel  -302

        ; Clear the Timer0 registers
        bcf         STATUS, RP0             ; select Bank0
        clrf        TMR0                    ; clear module register

        ; Disable interrupts
        bcf         INTCON, T0IE            ; mask timer interrupt

        ; Set the Timer0 control register
        bsf         STATUS, RP0             ; select Bank1
        movlw       b'10001000'             ; setup prescaler and timer
        movwf       OPTION_REG
        bcf         STATUS, RP0             ; select Bank0

        errorlevel  +302

        return
;=============================================================================

;=============================================================================
;  Midi Note -> Ticks Counter Decode Look-up Table
;  timer=8bit, prescaler=1, Fosc=20MHz -> 256*1/(20000000/4) = 51.2us (19531.25Hz)
;=============================================================================
note_to_ticks_decode
        addwf       PCL, F
octave0
        retlw       d'255'  ; C0 (16.35Hz)
        retlw       d'255'  ; C#0 (17.32Hz)
        retlw       d'255'  ; D0 (18.35Hz)
        retlw       d'255'  ; D#0 (19.45Hz)
        retlw       d'255'  ; E0 (20.60Hz)
        retlw       d'255'  ; F0 (21.83Hz)
        retlw       d'255'  ; F#0 (23.12Hz)
        retlw       d'255'  ; G0 (24.50Hz)
        retlw       d'255'  ; G#0 (25.96Hz)
        retlw       d'255'  ; A0 (27.50Hz)
        retlw       d'255'  ; A#0 (29.14Hz)
        retlw       d'255'  ; B0 (30.87Hz)
octave1
        retlw       d'255'  ; C1 (32.70Hz)
        retlw       d'255'  ; C#1 (34.65Hz)
        retlw       d'255'  ; D1 (36.71Hz)
        retlw       d'255'  ; D#1 (38.89Hz)
        retlw       d'255'  ; E1 (41.20Hz)
        retlw       d'255'  ; F1 (43.65Hz)
        retlw       d'255'  ; F#1 (46.25Hz)
        retlw       d'255'  ; G1 (49.00Hz)
        retlw       d'255'  ; G#1 (51.91Hz)
        retlw       d'255'  ; A1 (55.00Hz)
        retlw       d'255'  ; A#1 (58.27Hz)
        retlw       d'255'  ; B1 (61.74Hz)
octave2
        retlw       d'255'  ; C2 (65.41Hz)
        retlw       d'255'  ; C#2 (69.30Hz)
        retlw       d'255'  ; D2 (73.42Hz)
        retlw       d'251'  ; D#2 (77.78Hz)
        retlw       d'237'  ; E2 (82.41Hz)
        retlw       d'224'  ; F2 (87.31Hz)
        retlw       d'211'  ; F#2 (92.50Hz)
        retlw       d'199'  ; G2 (98.00Hz)
        retlw       d'188'  ; G#2 (103.83Hz)
        retlw       d'178'  ; A2 (110.00Hz)
        retlw       d'168'  ; A#2 (116.54Hz)
        retlw       d'158'  ; B2 (123.47Hz)
octave3
        retlw       d'149'  ; C3 (130.81Hz)
        retlw       d'141'  ; C#3 (138.59Hz)
        retlw       d'133'  ; D3 (146.83Hz)
        retlw       d'126'  ; D#3 (155.56Hz)
        retlw       d'119'  ; E3 (164.81Hz)
        retlw       d'112'  ; F3 (174.61Hz)
        retlw       d'106'  ; F#3 (185.00Hz)
        retlw       d'100'  ; G3 (196.00Hz)
        retlw       d'94'   ; G#3 (207.65Hz)
        retlw       d'89'   ; A3 (220.00Hz)
        retlw       d'84'   ; A#3 (233.08Hz)
        retlw       d'79'   ; B3 (246.94Hz)
octave4
        retlw       d'75'   ; C4 (261.63Hz)
        retlw       d'70'   ; C#4 (277.18Hz)
        retlw       d'67'   ; D4 (293.66Hz)
        retlw       d'63'   ; D#4 (311.13Hz)
        retlw       d'59'   ; E4 (329.63Hz)
        retlw       d'56'   ; F4 (349.23Hz)
        retlw       d'53'   ; F#4 (369.99Hz)
        retlw       d'50'   ; G4 (392.00Hz)
        retlw       d'47'   ; G#4 (415.30Hz)
        retlw       d'44'   ; A4 (440.00Hz)
        retlw       d'42'   ; A#4 (466.16Hz)
        retlw       d'40'   ; B4 (493.88Hz)
octave5
        retlw       d'37'   ; C5 (523.25Hz)
        retlw       d'35'   ; C#5 (554.37Hz)
        retlw       d'33'   ; D5 (587.33Hz)
        retlw       d'31'   ; D#5 (622.25Hz)
        retlw       d'30'   ; E5 (659.26Hz)
        retlw       d'28'   ; F5 (698.46Hz)
        retlw       d'26'   ; F#5 (739.99Hz)
        retlw       d'25'   ; G5 (783.99Hz)
        retlw       d'24'   ; G#5 (830.61Hz)
        retlw       d'22'   ; A5 (880.00Hz)
        retlw       d'21'   ; A#5 (932.33Hz)
        retlw       d'20'   ; B5 (987.77Hz)
octave6
        retlw       d'19'   ; C6 (1046.50Hz)
        retlw       d'18'   ; C#6 (1108.73Hz)
        retlw       d'17'   ; D6 (1174.66Hz)
        retlw       d'16'   ; D#6 (1244.51Hz)
        retlw       d'15'   ; E6 (1318.51Hz)
        retlw       d'14'   ; F6 (1396.91Hz)
        retlw       d'13'   ; F#6 (1479.98Hz)
        retlw       d'12'   ; G6 (1567.98Hz)
        retlw       d'12'   ; G#6 (1661.22Hz)
        retlw       d'11'   ; A6 (1760.00Hz)
        retlw       d'10'   ; A#6 (1864.66Hz)
        retlw       d'10'   ; B6 (1975.53Hz)
octave7                     ; same as octave6
        retlw       d'19'
        retlw       d'18'
        retlw       d'17'
        retlw       d'16'
        retlw       d'15'
        retlw       d'14'
        retlw       d'13'
        retlw       d'12'
        retlw       d'12'
        retlw       d'11'
        retlw       d'10'
        retlw       d'10'
octave8                     ; same as octave6
        retlw       d'19'
        retlw       d'18'
        retlw       d'17'
        retlw       d'16'
        retlw       d'15'
        retlw       d'14'
        retlw       d'13'
        retlw       d'12'
        retlw       d'12'
        retlw       d'11'
        retlw       d'10'
        retlw       d'10'
octave9                     ; same as octave6
        retlw       d'19'
        retlw       d'18'
        retlw       d'17'
        retlw       d'16'
        retlw       d'15'
        retlw       d'14'
        retlw       d'13'
        retlw       d'12'
        retlw       d'12'
        retlw       d'11'
        retlw       d'10'
        retlw       d'10'
octave10                    ; same as octave6
        retlw       d'19'
        retlw       d'18'
        retlw       d'17'
        retlw       d'16'
        retlw       d'15'
        retlw       d'14'
        retlw       d'13'
        retlw       d'12'
;=============================================================================

;=============================================================================
;  Delay routines
;=============================================================================
delay1ms
        movlw       0xC6                    ; 993 cycles
        movwf       d1
        movlw       0x01
        movwf       d2
delay1ms_0
        decfsz      d1, f
        goto        $+2
        decfsz      d2, f
        goto        delay1ms_0
        goto        $+1                     ; 3 cycles
        nop
        return                              ; 4 cycles (including call)
;=============================================================================
delay500ms                                  ;2499992 cycles
        movlw       0x15
        movwf       d1
        movlw       0x74
        movwf       d2
        movlw       0x06
        movwf       d3
delay500ms_0
        decfsz      d1, f
        goto        $+2
        decfsz      d2, f
        goto        $+2
        decfsz      d3, f
        goto        delay500ms_0
        goto        $+1                    ;4 cycles
        goto        $+1
        nop
        return                             ;4 cycles (including call)
;=============================================================================

;=============================================================================
;  FDD Routines
;=============================================================================
center_fdd
        movlw       HOMESTEPS
        movwf       d4
home_loop
        call        step_out
        call        delay1ms
	call        delay1ms
        decfsz      d4, F
        goto        home_loop
        call        delay1ms
        movlw       CENTERSTEPS
        movwf       d4
center_loop
        call        step_in
        call        delay1ms
	call        delay1ms
        decfsz      d4, F
        goto        center_loop
	clrf        fddPos
        bcf         fddStatus, DIRFLAG
        return

;=============================================================================
step_out
        bcf         FDD, DIR		    ; DIR=0 -> to track 0
	nop
	nop
        bsf         FDD, STEP
 	nop
	nop
	nop
	nop
	nop
	nop
        bcf         FDD, STEP
        return
;=============================================================================
step_in
        bsf         FDD, DIR		    ; DIR=1 -> to track 79
	nop
	nop
        bsf         FDD, STEP
 	nop
	nop
	nop
	nop
	nop
	nop
        bcf         FDD, STEP
        return
;=============================================================================

;=============================================================================
;  Tasks Routines
;=============================================================================
scan_midi_in_listen_channel
	swapf       DIPSW, W
        andlw       b'00001111'
        movwf       midiInListenChannel
        return
;=============================================================================
scan_midi_in_data
        bcf         midiIOStatus, INDATA    ; clear midi-in status flag
        btfss       RCSTA, OERR
        goto        $+3
        bcf         RCSTA, CREN
        bsf         RCSTA, CREN
        btfss       PIR1, RCIF              ; test for incoming data
        return
        bsf         COMM, IACTIVITY         ; turn on activity led
        movf        RCREG, W                ; set midi-in Byte Register
        movwf       midiInByte              ;
        bsf         midiIOStatus, INDATA    ; set midi-in status flag
        bcf         COMM, IACTIVITY         ; turn off activity led
        return
;=============================================================================
create_midi_in_message
        movf        midiInByte, W           ; test for statusbyte
        andlw       b'10000000'
        sublw       b'10000000'
        btfsc       STATUS, Z
        goto        check_statusbyte        ;  is statusbyte, check data
        btfss       msgStatus, STATUSBYTE   ; test for statusbyte
        return
        btfss       msgStatus, DATABYTE1    ; test for databyte
        goto        check_databyte1         ;  is databyte1, check data
        goto        check_databyte2         ;  is databyte2, check data
check_statusbyte
	movf        midiInByte, W	    ; test midi listen channel
        andlw       b'00001111'
        subwf       midiInListenChannel, W
        btfss       STATUS, Z
	nop	    ;	goto	    check_others
	clrf        msgStatusByte
        bcf         msgStatus, DATABYTE1
        bcf         msgStatus, DATABYTE2
        movf        midiInByte, W           ; save midi byte on data register
        andlw       b'11110000'
        movwf       msgStatusByte
check_note_on
        movf        msgStatusByte, W        ; test for note on
        sublw       b'10010000'
        btfss       STATUS, Z
        goto        check_note_off
        bsf         msgStatus, STATUSBYTE
        bsf         msgStatus, NOTEON
        return
check_note_off
        movf        msgStatusByte, W        ; test for note off
        sublw       b'10000000'
        btfss       STATUS, Z
        goto        check_others
        bsf         msgStatus, STATUSBYTE
        bcf         msgStatus, NOTEON
        return
check_others
	clrf        msgStatus
        return
check_databyte1
        movf        midiInByte, W           ; save midi byte on data
        movwf       msgDataByte1            ; and status registers
        bsf         msgStatus, DATABYTE1
        return
check_databyte2
        movf        midiInByte, W           ; save midi byte on data
        movwf       msgDataByte2            ; and status registers
        bsf         msgStatus, DATABYTE2
        return
;=============================================================================
parse_midi_in_message
        btfss       msgStatus, DATABYTE1    ; check if message is complete
        return
        btfss       msgStatus, DATABYTE2
        return
        btfss       msgStatus, NOTEON       ; check message type
        goto        parse_note_off
        movf        msgDataByte2, F
        btfsc       STATUS, Z
        goto        parse_note_off
parse_note_on
        bcf         msgStatus, DATABYTE1
	bcf         msgStatus, DATABYTE2
        incf        noteOnCounter, F        ; do note on
        movf        msgDataByte1, W
        movwf       noteToPlay
	call        note_to_ticks_decode
        movwf       tickCounter
        return
parse_note_off
        bcf         msgStatus, DATABYTE1
	bcf         msgStatus, DATABYTE2
        movf        noteOnCounter, F
        btfss       STATUS, Z
        decfsz      noteOnCounter, F        ; do note off
        return
        clrf        noteToPlay
        return
;=============================================================================
output_fdd_note
;        btfsc       fddStatus, DIRFLAG
;        goto        $+4
;        call        step_in
;        bsf         fddStatus, DIRFLAG
;        goto        $+3
;        call        step_out
;        bcf         fddStatus, DIRFLAG
;        return
        btfsc       fddStatus, DIRFLAG
        goto        moving_in
moving_out
        call        step_out                ; one step further out
        incf        fddPos, F
        movlw       FDDRANGE
        subwf       fddPos, W
        btfss       STATUS, Z               ; reached the outer limit?
        return
        bsf         fddStatus, DIRFLAG      ; reverse dir for next call
        return
moving_in
        call        step_in                 ; one step back in
        movf        fddPos, F
        btfsc       STATUS, Z               ; already at 0? (safety)
        goto        at_zero
        decf        fddPos, F
        movf        fddPos, F
        btfss       STATUS, Z               ; reached the inner limit (0)?
        return
at_zero
        bcf         fddStatus, DIRFLAG      ; reverse dir for next call
        return
;=============================================================================

;=============================================================================
;  main routine
;=============================================================================
main
        call        init_ports              ; init devices

        clrf        msgStatus               ; clean data and status registers
        clrf        msgStatusByte
        clrf        msgDataByte1
        clrf        msgDataByte2
        clrf        midiIOStatus
        clrf        midiInByte
        clrf        noteToPlay
        clrf        noteOnCounter
        bsf         FDD, DRVSEL

post
        call        delay500ms
        bsf         COMM, IACTIVITY
        bsf         COMM, OACTIVITY
        bcf         FDD, DRVSEL
        call        delay500ms
	call	    center_fdd
        call        delay500ms
        bcf         COMM, IACTIVITY
        bcf         COMM, OACTIVITY
;        bsf         FDD, DRVSEL

        call        init_usart
        call        init_timer

;==== tasks scheduler ========================================================
schedulerloop
        btfss       INTCON, T0IF            ; timer overflow (51.2us)?
        goto        schedulerloop           ; no, loop!
        bcf         INTCON, T0IF            ; reset overflow flag

; --  scan input -------------------------------------------------------------
task1
        call        scan_midi_in_listen_channel
        call        scan_midi_in_data

; --  parse midi-in data -----------------------------------------------------
task2
        btfsc       midiIOStatus, INDATA
        call        create_midi_in_message
        btfsc       msgStatus, STATUSBYTE
        call        parse_midi_in_message

; --  fdd activity led -------------------------------------------------------
task3
        movf        noteToPlay, F
        btfsc       STATUS, Z
        goto        $+3
        bsf         COMM, OACTIVITY
        goto        task4
        bcf         COMM, OACTIVITY
        goto        endloop

; --  fdd output note --------------------------------------------------------
task4
        movf        noteToPlay, F
        btfsc       STATUS, Z
        goto        endloop
        decfsz      tickCounter, F          ; countdown and test
        goto        endloop                 ; not time yet
        movf        noteToPlay, W
        call        note_to_ticks_decode    ; decode databyte to ticks
        movwf       tickCounter             ; reset counter
        call        output_fdd_note         ; play note

endloop
        goto        schedulerloop

;==== tasks scheduler ========================================================
        end
