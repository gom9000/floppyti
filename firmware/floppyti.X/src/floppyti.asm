;=============================================================================
; @(#)floppyti.asm  0.1  2014/04/23
;   ________        _________________.________
;  /  _____/  ____ /   _____/   __   \   ____/
; /   \  ___ /  _ \\_____  \\____    /____  \
; \    \_\  (  <_> )        \  /    //       \
;  \______  /\____/_______  / /____//______  /
;         \/              \/               \/
; Copyright (c) 2014 by Alessandro Fraschetti.
; All Rights Reserved.
;
; Description: floppyti. Floppy, fottiti!
; Input......:
; Output.....:
; Note.......:
;=============================================================================

        processor   16f628a
        #include    <p16f628a.inc>
      ;  __config  	_CP_OFF & _CPD_OFF & _BODEN_OFF & _LVP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC
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
FDD         equ     PORTA
DIR         equ     RA2
STEP        equ     RA3
DRVSEL      equ     RA4

COMM        equ     PORTB
IACTIVITY   equ     RB0
IN          equ     RB1
OACTIVITY   equ     RB3

; midiIOStatus Flags
INDATA      equ		0x00
OUTDATA     equ		0x01

; messageStatusByte Flags
STATUSBYTE  equ     0x00
DATABYTE1   equ     0x01
DATABYTE2   equ     0x02
NOTEON      equ     0x04

; fddStatus Flags
DIRFLAG		equ		0x04
;=============================================================================

;=============================================================================
;  File register use
;=============================================================================
		cblock		h'20'
			w_temp						; variable used for context saving
			status_temp					; variable used for context saving
            pclath_temp                 ; variable used for context saving

            d1, d2, d3					; delay routine vars

            midiInByte                  ; midi-in Byte Register
            midiIOStatus                ; midi-in/out Status Register
            midiInListenChannel         ; midi-in listen channel

            msgStatusByte               ; midi-in message: Status Byte Register
            msgDataByte1                ; midi-in message: Data Byte 1 Register
            msgDataByte2                ; midi-in message: Data Byte 2 Register
            msgStatus                   ; midi-in message Status Register

            noteOnCounter               ; midi notes-on counter
            noteToPlay                  ; midi note to play out

			fddStatus					; fdd status register
            tickCounter                 ; counter for note task execution
            outputStatus                ; output (on/off) status register
		endc
;=============================================================================

;=============================================================================
;  Start of code
;=============================================================================
;start
		org			h'0000'				; processor reset vector
		goto		main				; jump to the main routine

		org			h'0004'				; interrupt vector location
		movwf		w_temp				; save off current W register contents
		movf		STATUS, W			; move status register into W register
		movwf		status_temp			; save off contents of STATUS register
        movf        PCLATH, W           ; move pclath register into W register
        movwf       pclath_temp         ; save off contents of PCLATH register

        ; isr code can go here or be located as a call subroutine elsewhere

        movf        pclath_temp, W      ; retrieve copy of PCLATH register
        movwf       PCLATH              ; restore pre-isr PCLATH register contents
		movf		status_temp, W		; retrieve copy of STATUS register
		movwf		STATUS				; restore pre-isr STATUS register contents
		swapf		w_temp, F
		swapf		w_temp, W			; restore pre-isr W register contents
		retfie							; return from interrupt
;=============================================================================

;=============================================================================
;  Init I/O ports
;=============================================================================
init_ports
		errorlevel	-302

        ; set PORTA (RA0-RA7) as:
        ;   RA2 Out=DIR, RA3 Out=STEP, RA4 Out=DRVSEL
  		bcf			STATUS, RP0				; select Bank0
  		clrf		PORTA					; initialize PORTA by clearing output data latches
        movlw       h'07'                   ; turn comparators off
        movwf       CMCON                   ; and set port A mode I/O digital
  		bsf			STATUS, RP0				; select Bank1
  		movlw		b'11100000'				; PORTA input/output
  		movwf		TRISA

  		; set PORTB (RB0-RB3) as:
        ;   Out=IN Activity, In=MIDI-IN, Out= -, Out=OUT Activity
        ; set PORTB (RB4-RB7) as:
        ;   All-Input=MIDI-IN CHANNEL
  		bcf			STATUS, RP0				; select Bank0
  		clrf		PORTB					; initialize PORTB by clearing output data latches
  		bsf			STATUS, RP0				; select Bank1
  		movlw		b'11110010'				; PORTB input/output
  		movwf		TRISB

        bcf			STATUS, RP0				; select Bank0

		errorlevel  +302

  		return
;=============================================================================

;=============================================================================
;  Init USART
;=============================================================================
init_usart
        errorlevel	-302

        bsf			STATUS, RP0				; select Bank1
        movlw   	h'09'             		; 31250 bauds on 20MHz osc.
        movwf   	SPBRG
        movlw   	b'00000000'     		; async tx 8 bit
        movwf   	TXSTA
        bcf     	STATUS, RP0             ; return to page 0
        movlw   	b'10010000'    			; async rx 8 bit
        movwf   	RCSTA
        bcf			STATUS, RP0				; select Bank0

        errorlevel  +302

        return
;=============================================================================

;=============================================================================
;  Init Timer0 (internal clock source)
;=============================================================================
init_timer
        errorlevel	-302

        ; Clear the Timer0 registers
        bcf			STATUS, RP0				; select Bank0
        clrf        TMR0                    ; clear module register

        ; Disable interrupts
        bcf         INTCON, T0IE            ; mask timer interrupt

        ; Set the Timer0 control register
        bsf			STATUS, RP0				; select Bank1
        movlw       b'10000000'             ; setup prescaler (0) and timer
        movwf       OPTION_REG
        bcf			STATUS, RP0				; select Bank0

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
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
octave1
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
octave2
        retlw       d'255'
        retlw       d'255'
        retlw       d'255'
        retlw       d'251'
        retlw       d'237'
        retlw       d'224'
        retlw       d'211'
        retlw       d'199'
        retlw       d'188'
        retlw       d'178'
        retlw       d'168'
        retlw       d'158'
octave3
        retlw       d'149'  ;130.81Hz
        retlw       d'141'
        retlw       d'133'
        retlw       d'126'
        retlw       d'119'
        retlw       d'112'
        retlw       d'106'
        retlw       d'100'
        retlw       d'94'
        retlw       d'89'
        retlw       d'83'
        retlw       d'79'
octave4
        retlw       d'75'
        retlw       d'70'
        retlw       d'67'
        retlw       d'63'
        retlw       d'59'
        retlw       d'56'
        retlw       d'53'
        retlw       d'50'
        retlw       d'47'
        retlw       d'44'   ; 440Hz
        retlw       d'42'
        retlw       d'40'
octave5
        retlw       d'37'
        retlw       d'35'
        retlw       d'33'
        retlw       d'32'
        retlw       d'30'
        retlw       d'28'
        retlw       d'26'
        retlw       d'25'
        retlw       d'24'
        retlw       d'22'
        retlw       d'21'
        retlw       d'20'
octave6
        retlw       d'19'
        retlw       d'18'
        retlw       d'17'
        retlw       d'16'
        retlw       d'15'
        retlw       d'14'
        retlw       d'14'
        retlw       d'12'
        retlw       d'12'
        retlw       d'11'
        retlw       d'10'
        retlw       d'10'
octave7
        retlw       d'19'
        retlw       d'18'
        retlw       d'17'
        retlw       d'16'
        retlw       d'15'
        retlw       d'14'
        retlw       d'14'
        retlw       d'12'
        retlw       d'12'
        retlw       d'11'
        retlw       d'10'
        retlw       d'10'
octave8
        retlw       d'19'
        retlw       d'18'
        retlw       d'17'
        retlw       d'16'
        retlw       d'15'
        retlw       d'14'
        retlw       d'14'
        retlw       d'12'
        retlw       d'12'
        retlw       d'11'
        retlw       d'10'
        retlw       d'10'
octave9
        retlw       d'19'
        retlw       d'18'
        retlw       d'17'
        retlw       d'16'
        retlw       d'15'
        retlw       d'14'
        retlw       d'14'
        retlw       d'12'
        retlw       d'12'
        retlw       d'11'
        retlw       d'10'
        retlw       d'10'
octave10
        retlw       d'19'
        retlw       d'18'
        retlw       d'17'
        retlw       d'16'
        retlw       d'15'
        retlw       d'14'
;=============================================================================

;=============================================================================
;  Delay routines
;=============================================================================
delay1ms
        movlw       0xC6					; 993 cycles
        movwf       d1
        movlw       0x01
        movwf       d2
delay1ms_0
        decfsz      d1, f
        goto        $+2
        decfsz      d2, f
        goto        delay1ms_0
        goto        $+1						; 3 cycles
        nop
        return								; 4 cycles (including call)
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
step_out
        bsf         FDD, DIR
        bsf         FDD, STEP
 ;       call        delay1ms
        bcf         FDD, STEP
        return
;=============================================================================
step_in
        bcf         FDD, DIR
        bsf         FDD, STEP
 ;       call        delay1ms
        bcf         FDD, STEP
        return
;=============================================================================

;=============================================================================
;  Tasks Routines
;=============================================================================
scan_midi_in_listen_channel
		movf 		COMM, W
        andlw		b'11110000'
        movwf 		midiInListenChannel
        rrf         midiInListenChannel, F
        rrf         midiInListenChannel, F
        rrf         midiInListenChannel, F
        rrf         midiInListenChannel, F
        return
;=============================================================================
scan_midi_in_data
		bcf			midiIOStatus, INDATA    ; clear midi-in status flag
        btfss       RCSTA, OERR
        goto        $+3
        bcf         RCSTA, CREN
        bsf         RCSTA, CREN
		btfss		PIR1, RCIF				; test for incoming data
		return
		bsf			COMM, IACTIVITY         ; turn on activity led
        movf		RCREG, W                ; set midi-in Byte Register
		movwf		midiInByte              ;
		bsf			midiIOStatus, INDATA    ; set midi-in status flag
        bcf			COMM, IACTIVITY         ; turn off activity led
		return
;=============================================================================
create_midi_in_message
        movf		midiInByte, W           ; test for statusbyte
        andlw       b'10001111'
        sublw       b'10000000' ;;; todo test midi listen channel...
        btfsc       STATUS, Z
        goto        check_statusbyte        ;  is statusbyte, check data
        btfss       msgStatus, STATUSBYTE   ; test for statusbyte
        return
        btfss       msgStatus, DATABYTE1    ; test for databyte
        goto        check_databyte1         ;  is databyte1, check data
        goto        check_databyte2         ;  is databyte2, check data
check_statusbyte
        clrf        msgStatus
        bcf         msgStatus, STATUSBYTE
        clrf        msgStatusByte
        clrf        msgDataByte1
        clrf        msgDataByte2
        movf        midiInByte, W           ; save midi byte on data register
        andlw       b'11110000'
        movwf		msgStatusByte
check_note_on
        movf		msgStatusByte, W        ; test for note on
        sublw       b'10010000'
        btfss       STATUS, Z
        goto        check_note_off
        bsf         msgStatus, STATUSBYTE
        bsf         msgStatus, NOTEON
        return
check_note_off
        movf		msgStatusByte, W        ; test for note off
        sublw       b'10000000'
        btfss       STATUS, Z
        goto        check_others
        bsf         msgStatus, STATUSBYTE
        bcf         msgStatus, NOTEON
        return
check_others
        return
check_databyte1
        movf        midiInByte, W           ; save midi byte on data
        movwf		msgDataByte1            ; and status registers
        bsf         msgStatus, DATABYTE1
        return
check_databyte2
        movf        midiInByte, W           ; save midi byte on data
        movwf		msgDataByte2            ; and status registers
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
        clrf        msgStatus
        incf        noteOnCounter, F        ; do note on
        movf        msgDataByte1, W
        movwf		noteToPlay
        return
parse_note_off
        clrf        msgStatus
        movf        noteOnCounter, F
        btfss       STATUS, Z
        decfsz      noteOnCounter, F        ; do note off
        return
        clrf		noteToPlay
        return
;=============================================================================
output_fdd_note
        btfsc       fddStatus, DIRFLAG
        goto        $+4
        call        step_in
        bsf         fddStatus, DIRFLAG
        goto        $+3
        call        step_out
        bcf         fddStatus, DIRFLAG
        return
;=============================================================================

;=============================================================================
;  main routine
;=============================================================================
main
		call 		init_ports              ; init devices

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
        call        delay500ms
        bcf         COMM, IACTIVITY
        bcf         COMM, OACTIVITY
;        bsf         FDD, DRVSEL

		call		init_usart
        call        init_timer

;==== tasks scheduler ========================================================
schedulerloop
        btfss       INTCON, T0IF            ; timer overflow (51.2us)?
        goto        schedulerloop			; no, loop!
        bcf         INTCON, T0IF            ; reset overflow flag

; --  scan input -------------------------------------------------------------
task1
        call        scan_midi_in_listen_channel
        call        scan_midi_in_data

; --  parse midi-in data -----------------------------------------------------
task2
        btfsc		midiIOStatus, INDATA
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
