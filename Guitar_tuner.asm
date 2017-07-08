;  **  ATmega128(L) Assembly Language File - IAR Assembler Syntax **
;
;  ** Author : Kevin Tham and Md Huzaifah
;  ** Company : Imperial College London
;  ** Last updated: 06/03/2015
;  ** Comment : Guitar tuner programme
;				Interrupt waits for ADC. Main program runs between interrupt cycles
;				Multiplication subroutines adapted from source code of Steve Berman, Mark Krangle & Rich Levy
;  				for their project <Design of a Real-Time Digital Guitar Tuner>
;  ** Features: 21-tap finite impulse response (FIR) filter, frequency counter using 'zero-crossing' of rising edges
;				
;
;******Guitar Tunings******
; Note		Freq(Hz)	Bandpass(Hz)
;	E		82.41		79.41-85.41
;	A		110.00		107-113
;	D		146.83		143.83-149.83
;	G		196.00		193-199
;	B		246.94		241.94-251.94
;	HiE		329.63		324.63-334.63


.DEVICE ATmega128
.include "m128def.inc"
;
;***** Warning!!! Duplicate Register labels!!! *****
.def ADCDL = r19		; bottom 8 bits of ADC output
.def ADCDH = r20		; top 2 bits of ADC output
;
;***** Variable Definitions *****
.def	mc8u = r6		; multiplicand
.def	mp8u = r7		; multiplier
.def	m8uL = r7		; result Low byte
.def	m8uH = r8		; result High byte

.def	addsubA0 = r2	; LSB of first operand and answer
.def	addsubA1 = r3	; these three registers act as 
.def	addsubA2 = r4   ; the accumulator for filter

.def	addsubB0 = r7	; LSB of second operand
.def	addsubB1 = r8   ; these 2 registers are also result from multiplication
.def	addsubB2 = r9

.def	filtout0 = r2	; current filtered output (LSB)
.def	filtout1 = r3
.def	filtout2 = r4	; MSB

.def	whichstring = r0;miscellaneous registers
.def	num_sample = r1
.def	oldsign = r11
.def	coeffsign = r12
.def	totalneg = r13
.def 	save  = r14
.def	crossings = r15
.def	looper = r16
.def	mem_end = r17
.def	store = r18
.def	count   = r19
.def	count_S = r20
.def	count_T = r21
.def	temp  = r22
.def	temp2 = r23
.def	threshold = r24  
.def	flag = r25

;***** Button assignments *****
;						SW76543210
.equ	Estr	    =	0b11111110
.equ	Astr		=	0b11111101
.equ	Dstr		=	0b11111011
.equ	Gstr		=	0b11110111
.equ	Bstr		=	0b11101111
.equ	HiEstr		=	0b11011111
;
;***** Required sample counts and crossings *****
.equ	HiE_samp    = 100
.equ	HiE_crossings  = 12		; equals to number of periods + 1
.equ	HiE_threshold2 = 2		; 2% threshold
.equ	HiE_threshold5 = 6		; 5% threshold
.equ	HiE_threshold10 = 12	; 10% threshold

.equ	B_samp    = 121
.equ	B_crossings  = 11	
.equ	B_threshold2 = 2
.equ	B_threshold5 = 6
.equ	B_threshold10 = 12

.equ	G_samp    = 183		
.equ	G_crossings  = 13	
.equ	G_threshold2 = 3
.equ	G_threshold5 = 9
.equ	G_threshold10 = 17

.equ	D_samp    = 143		
.equ	D_crossings  = 8	
.equ	D_threshold2 = 3		
.equ	D_threshold5 = 7
.equ	D_threshold10 = 14

.equ	A_samp    = 163		
.equ	A_crossings  = 7
.equ	A_threshold2 = 3
.equ	A_threshold5 = 8
.equ	A_threshold10 = 16

.equ	E_samp    = 182		
.equ	E_crossings  = 6
.equ	E_threshold2 = 4
.equ	E_threshold5 = 9
.equ	E_threshold10 = 18

;***** DC Offsets *****
;
.equ	hE_offset0 = 58		   ; -424
.equ	hE_offset1 = FE
.equ	hE_offset2 = FF
;
.equ	B_offset0 = B0         ; 12720
.equ	B_offset1 = 31
.equ	B_offset2 = 00
;
.equ	G_offset0 = B4         ; 5300
.equ	G_offset1 = 14
.equ	G_offset2 = 00
;
.equ	D_offset0 = E6         ; 27878
.equ	D_offset1 = 6C
.equ	D_offset2 = 00
;
.equ	A_offset0 = $FE        ; 64766
.equ	A_offset1 = $FC
.equ	A_offset2 = $00
;
.equ	E_offset0 = $F6        ; 79606
.equ	E_offset1 = $36
.equ	E_offset2 = $01
;
.DSEG

input: .BYTE 21

Eb		:	.BYTE 21
Ebsign	:	.BYTE 2
Ebsignno:   .BYTE 1
Ab		:	.BYTE 21
Absign	:	.BYTE 8
Absignno:   .BYTE 1
Db		:	.BYTE 21
Dbsign	:	.BYTE 10
Dbsignno:   .BYTE 1
Gb		:	.BYTE 21
Gbsign	:	.BYTE 14
Gbsignno:	.BYTE 1
Bb		:	.BYTE 21
Bbsign	:   .BYTE 12
Bbsignno:   .BYTE 1
HiEb	:	.BYTE 21
HiEbsign:	.BYTE 8
HiEbsignno: .BYTE 1
;
.CSEG
.ORG	$0000
        rjmp RESET
;
.ORG	$0018
	    rjmp trig_in


RESET:                
     
		; ***** Stack Pointer Setup Code *****
		ldi		r16, $0F		; Stack Pointer Setup 
		out		SPH,r16			; Stack Pointer High Byte 
		ldi		r16, $FF		; Stack Pointer Setup 
		out		SPL,r16			; Stack Pointer Low Byte 
   		
		; ***** RAMPZ Setup Code ***** 
		ldi  	r16, $00		; 1 = EPLM acts on upper 64K
		out 	RAMPZ, r16		; 0 = EPLM acts -on lower 64K
   		
		; ***** Sleep Mode And SRAM  *****
		ldi 	r16, $C0		; Idle Mode - SE bit in MCUCR not set
		out 	MCUCR, r16		; External SRAM Enable Wait State Enabled
   
		; ***** Port A Setup Code *****  
		ldi 	r16, $00		; Address AD7 to AD0
		out 	DDRA, r16		; Port A Direction Register
		ldi 	r16, $FF		; Init value 
		out 	PORTA, r16		; Port A value
   
		; ***** Port B Setup Code *****  
		ldi 	r16, $FF		; 
		out 	DDRB , r16		; Port B Direction Register
		ldi 	r16, $FF		; Init value 
		out 	PORTB, r16		; Port B value

		; ***** Port D Setup Code *****
		ldi 	r16, $00		; I/O: 
		out 	DDRD, r16		; Port D Direction Register
		ldi	 	r16, $FF		; Init value 
		out	 	PORTD, r16		; Port D value

		; ***** Port E Setup Code *****  
		ldi r16, $FF		
		out DDRE, r16		; Port E Direction Register
		ldi r16, $FF		; Init value 
		out PORTE, r16		; Port E value
   		;
		; ***** Port F Setup Code *****  
		; Analog : PF7 PF6 PF5 PF4 PF3 PF2 PF1 PF0  No Digital 
		

		; ******* ADC init *******
		;
		ldi 	temp, 0				; Set up ADC to read
		out 	ADMUX, temp			; Channel 0	
		ldi 	temp, 0b10000110	; Free run disabled, prescaler = 64
		out 	ADCSR, temp			; start off (turn on when ready)
		ldi 	temp, 0b01000000	; start A-D conversion
		in 		temp2, ADCSR
		or 		temp, temp2
		out 	ADCSR, temp
ADCwait:in		temp, ADCSR			; wait for conversion to be complete
		sbrc 	temp, 6				; by checking ADSC bit
		rjmp	ADCwait

		; ******* Match Register Setup *******
		;
		ldi		temp, 0b00010000	; enable output compare match interrupt enable
		out		TIMSK, temp			; on timer/counter1
		ldi		temp, 0x0A			; set Match A register
		out		OCR1AH, temp		; to 2667 = 0x0A6B
		ldi		temp, 0x6B			; so the ADC has a sampling rate of
		out		OCR1AL, temp		; 3kHz

		; ******* Timer Setup *******
		;
		ldi		temp, 0b00001001	; set prescaler to 1 and clear on compare match
		out		TCCR1b, temp
		set
		sei							; enable all interrupts
		clr 	temp				; clear all relevant registers during initialization
		clr 	temp2
		clr		looper
		clr 	filtout2
		clr		filtout1
		clr		filtout0
		ldi		flag, 1				; set flag register to 1. flag gets turned off after first 21 
									; ADC values (loop1 -> loop)
		; ******* Initialize Filter Coefficients *******

;HiE coefficients		
		ldi		ZL, low(HiEb)
		ldi		ZH, high(HiEb)
		ldi		temp, 2
		st		Z+, temp
		ldi		temp, 3
		st		Z+, temp
		ldi		temp, 4
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 7
		st		Z+, temp
		ldi		temp, 17
		st		Z+, temp
		ldi		temp, 21
		st		Z+, temp
		ldi		temp, 13
		st		Z+, temp
		ldi		temp, 6
		st		Z+, temp
		ldi		temp, 24
		st		Z+, temp
		ldi		temp, 32
		st		Z+, temp
		ldi		temp, 24
		st		Z+, temp
		ldi		temp, 6
		st		Z+, temp
		ldi		temp, 13
		st		Z+, temp
		ldi		temp, 21
		st		Z+, temp
		ldi		temp, 17
		st		Z+, temp
		ldi		temp, 7
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 4
		st		Z+, temp
		ldi		temp, 3
		st		Z+, temp
		ldi		temp, 2
		st		Z+, temp
;HiE coefficient negative sign position
		ldi 	YL, low(HiEbsign)
		ldi		YH, high(HiEbsign)
		ldi		temp, 5
		st		Y+, temp
		ldi		temp, 6
		st		Y+, temp
		ldi		temp, 7
		st		Y+, temp
		ldi		temp, 8
		st		Y+, temp
		ldi		temp, 14
		st		Y+, temp
		ldi		temp, 15
		st		Y+, temp
		ldi		temp, 16
		st		Y+, temp
		ldi		temp, 17
		st		Y+, temp
;HiE number of negative					; NOTE: Total number of negatives +1 of actual number
		ldi		XL, low(HiEbsignno)		; to keep stack pointer at correct position	
		ldi		XH, high(HiEbsignno)
		ldi 	temp, 9
		st		X+, temp

;*****************************************************
;B coefficients		
		ldi		ZL, low(Bb)
		ldi		ZH, high(Bb)
		ldi		temp, 6
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 15
		st		Z+, temp
		ldi		temp, 41
		st		Z+, temp
		ldi		temp, 67
		st		Z+, temp
		ldi		temp, 78
		st		Z+, temp
		ldi		temp, 55
		st		Z+, temp
		ldi		temp, 3
		st		Z+, temp
		ldi		temp, 79
		st		Z+, temp
		ldi		temp, 144
		st		Z+, temp
		ldi		temp, 170
		st		Z+, temp
		ldi		temp, 144
		st		Z+, temp
		ldi		temp, 79
		st		Z+, temp
		ldi		temp, 3
		st		Z+, temp
		ldi		temp, 55
		st		Z+, temp
		ldi		temp, 78
		st		Z+, temp
		ldi		temp, 67
		st		Z+, temp
		ldi		temp, 41
		st		Z+, temp
		ldi		temp, 15
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 6
		st		Z+, temp
;B coefficient negative sign position
		ldi 	YL, low(Bbsign)
		ldi		YH, high(Bbsign)
		ldi		temp, 2
		st		Y+, temp
		ldi		temp, 3
		st		Y+, temp
		ldi		temp, 4
		st		Y+, temp
		ldi		temp, 5
		st		Y+, temp
		ldi		temp, 6
		st		Y+, temp
		ldi		temp, 7
		st		Y+, temp
		ldi		temp, 15
		st		Y+, temp
		ldi		temp, 16
		st		Y+, temp
		ldi		temp, 17
		st		Y+, temp
		ldi		temp, 18
		st		Y+, temp
		ldi		temp, 19
		st		Y+, temp
		ldi		temp, 20
		st		Y+, temp
;B number of negative
		ldi		XL, low(Bbsignno)
		ldi		XH, high(Bbsignno)
		ldi 	temp, 13
		st		X+, temp

;*****************************************************
;G coefficients		
		ldi		ZL, low(Gb)
		ldi		ZH, high(Gb)
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 2
		st		Z+, temp
		ldi		temp, 4
		st		Z+, temp
		ldi		temp, 6
		st		Z+, temp
		ldi		temp, 7
		st		Z+, temp
		ldi		temp, 5
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 6
		st		Z+, temp
		ldi		temp, 14
		st		Z+, temp
		ldi		temp, 20
		st		Z+, temp
		ldi		temp, 22
		st		Z+, temp
		ldi		temp, 20
		st		Z+, temp
		ldi		temp, 14
		st		Z+, temp
		ldi		temp, 6
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 5
		st		Z+, temp
		ldi		temp, 7
		st		Z+, temp
		ldi		temp, 6
		st		Z+, temp
		ldi		temp, 4
		st		Z+, temp
		ldi		temp, 2
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
;G coefficient negative sign position
		ldi 	YL, low(Gbsign)
		ldi		YH, high(Gbsign)
		ldi		temp, 1
		st		Y+, temp
		ldi		temp, 2
		st		Y+, temp
		ldi		temp, 3
		st		Y+, temp
		ldi		temp, 4
		st		Y+, temp
		ldi		temp, 5
		st		Y+, temp
		ldi		temp, 6
		st		Y+, temp
		ldi		temp, 7
		st		Y+, temp
		ldi		temp, 15
		st		Y+, temp
		ldi		temp, 16
		st		Y+, temp
		ldi		temp, 17
		st		Y+, temp
		ldi		temp, 18
		st		Y+, temp
		ldi		temp, 19
		st		Y+, temp
		ldi		temp, 20
		st		Y+, temp
		ldi		temp, 21
		st		Y+, temp
;G number of negative
		ldi		XL, low(Gbsignno)
		ldi		XH, high(Gbsignno)
		ldi 	temp, 15
		st		X+, temp

;*****************************************************
;D coefficients		
		ldi		ZL, low(Db)
		ldi		ZH, high(Db)
		ldi		temp, 4
		st		Z+, temp
		ldi		temp, 5
		st		Z+, temp
		ldi		temp, 7
		st		Z+, temp
		ldi		temp, 8
		st		Z+, temp
		ldi		temp, 6
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 13
		st		Z+, temp
		ldi		temp, 27
		st		Z+, temp
		ldi		temp, 41
		st		Z+, temp
		ldi		temp, 52
		st		Z+, temp
		ldi		temp, 55
		st		Z+, temp
		ldi		temp, 52
		st		Z+, temp
		ldi		temp, 41
		st		Z+, temp
		ldi		temp, 27
		st		Z+, temp
		ldi		temp, 13
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 6
		st		Z+, temp
		ldi		temp, 8
		st		Z+, temp
		ldi		temp, 7
		st		Z+, temp
		ldi		temp, 5
		st		Z+, temp
		ldi		temp, 4
		st		Z+, temp
;D coefficient negative sign position
		ldi 	YL, low(Dbsign)
		ldi		YH, high(Dbsign)
		ldi		temp, 1
		st		Y+, temp
		ldi		temp, 2
		st		Y+, temp
		ldi		temp, 3
		st		Y+, temp
		ldi		temp, 4
		st		Y+, temp
		ldi		temp, 5
		st		Y+, temp
		ldi		temp, 17
		st		Y+, temp
		ldi		temp, 18
		st		Y+, temp
		ldi		temp, 19
		st		Y+, temp
		ldi		temp, 20
		st		Y+, temp
		ldi		temp, 21
		st		Y+, temp
;D number of negative
		ldi		XL, low(Dbsignno)
		ldi		XH, high(Dbsignno)
		ldi 	temp, 11
		st		X+, temp

;*****************************************************
;A coefficients		
		ldi		ZL, low(Ab)
		ldi		ZH, high(Ab)
		ldi		temp, 5
		st		Z+, temp
		ldi		temp, 4
		st		Z+, temp
		ldi		temp, 4
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 7
		st		Z+, temp
		ldi		temp, 19
		st		Z+, temp
		ldi		temp, 37
		st		Z+, temp
		ldi		temp, 55
		st		Z+, temp
		ldi		temp, 73
		st		Z+, temp
		ldi		temp, 84
		st		Z+, temp
		ldi		temp, 89
		st		Z+, temp
		ldi		temp, 84
		st		Z+, temp
		ldi		temp, 73
		st		Z+, temp
		ldi		temp, 55
		st		Z+, temp
		ldi		temp, 37
		st		Z+, temp
		ldi		temp, 19
		st		Z+, temp
		ldi		temp, 7
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 4
		st		Z+, temp
		ldi		temp, 4
		st		Z+, temp
		ldi		temp, 5
		st		Z+, temp		
;A coefficient negative sign position
		ldi 	YL, low(Absign)
		ldi		YH, high(Absign)
		ldi		temp, 1
		st		Y+, temp
		ldi		temp, 2
		st		Y+, temp
		ldi		temp, 3
		st		Y+, temp
		ldi		temp, 4
		st		Y+, temp
		ldi		temp, 18
		st		Y+, temp
		ldi		temp, 19
		st		Y+, temp
		ldi		temp, 20
		st		Y+, temp
		ldi		temp, 21
		st		Y+, temp
;A number of negative
		ldi		XL, low(Absignno)
		ldi		XH, high(Absignno)
		ldi 	temp, 9
		st		X+, temp

;*****************************************************
;E coefficients		
		ldi		ZL, low(Eb)
		ldi		ZH, high(Eb)
		ldi		temp, 1
		st		Z+, temp
		ldi		temp, 0
		st		Z+, temp
		ldi		temp, 3
		st		Z+, temp
		ldi		temp, 8
		st		Z+, temp
		ldi		temp, 18
		st		Z+, temp
		ldi		temp, 31
		st		Z+, temp
		ldi		temp, 47
		st		Z+, temp
		ldi		temp, 63
		st		Z+, temp
		ldi		temp, 76
		st		Z+, temp
		ldi		temp, 86
		st		Z+, temp
		ldi		temp, 89
		st		Z+, temp
		ldi		temp, 86
		st		Z+, temp
		ldi		temp, 76
		st		Z+, temp
		ldi		temp, 63
		st		Z+, temp
		ldi		temp, 47
		st		Z+, temp
		ldi		temp, 31
		st		Z+, temp
		ldi		temp, 18
		st		Z+, temp
		ldi		temp, 8
		st		Z+, temp
		ldi		temp, 3
		st		Z+, temp
		ldi		temp, 0
		st		Z+, temp
		ldi		temp, 1
		st		Z+, temp		
;E coefficient negative sign position
		ldi 	YL, low(Ebsign)
		ldi		YH, high(Ebsign)
		ldi		temp, 1
		st		Y+, temp
		ldi		temp, 21
		st		Y+, temp
;E number of negative
		ldi		XL, low(Ebsignno)
		ldi		XH, high(Ebsignno)
		ldi 	temp, 3
		st		X+, temp		
	
		; ******* Main program start *******

waiting:								; wait for button input to select string
		in 		temp, PIND
        cpi		temp, 0xFF
		breq 	waiting

select:
		in		temp, PIND
		
Bload:	cpi		temp, Bstr				; load filter coefficients according to string selected
		brne	Gload					; by comparing PIND in turn				
		mov		whichstring, temp
		ldi		ZL,	low(Bbsignno)
		ldi		ZH,	high(Bbsignno)
		ld		totalneg, Z
		ldi		YL, low(Bb)
		ldi		YH, high(Bb)
		ldi 	ZL, low(Bbsign)
		ldi		ZH, high(Bbsign)
		ldi		threshold, B_samp		; load required sample count, crossings and thresholds
		mov		num_sample, threshold	; according to string selected
		ldi		threshold, B_crossings
		mov		crossings, threshold
		ldi		threshold, B_threshold2  
		rjmp	release

Gload:	cpi		temp, Gstr
		brne	Dload
		mov		whichstring, temp
		ldi		ZL,	low(Gbsignno)
		ldi		ZH,	high(Gbsignno)
		ld		totalneg, Z
		ldi		YL, low(Gb)
		ldi		YH, high(Gb)
		ldi 	ZL, low(Gbsign)
		ldi		ZH, high(Gbsign)
		ldi		threshold, G_samp
		mov		num_sample, threshold
		ldi		threshold, G_crossings
		mov		crossings, threshold
		ldi		threshold, G_threshold2  
		rjmp	release

Dload:	cpi		temp, Dstr
		brne	Aload
		mov		whichstring, temp
		ldi		ZL,	low(Dbsignno)
		ldi		ZH,	high(Dbsignno)
		ld		totalneg, Z
		ldi		YL, low(Db)
		ldi		YH, high(Db)
		ldi 	ZL, low(Dbsign)
		ldi		ZH, high(Dbsign)
		ldi		threshold, D_samp
		mov		num_sample, threshold
		ldi		threshold, D_crossings
		mov		crossings, threshold
		ldi		threshold, D_threshold2  
		rjmp	release

Aload:	cpi		temp, Astr
		brne	Eload
		mov		whichstring, temp
		ldi		ZL,	low(Absignno)
		ldi		ZH,	high(Absignno)
		ld		totalneg, Z
		ldi		YL, low(Ab)
		ldi		YH, high(Ab)
		ldi 	ZL, low(Absign)
		ldi		ZH, high(Absign)
		ldi		threshold, A_samp
		mov		num_sample, threshold
		ldi		threshold, A_crossings
		mov		crossings, threshold
		ldi		threshold, A_threshold2  
		rjmp	release

Eload:	cpi		temp, Estr
		brne	HiEload
		mov		whichstring, temp
		ldi		ZL,	low(Ebsignno)
		ldi		ZH,	high(Ebsignno)
		ld		totalneg, Z
		ldi		YL, low(Eb)
		ldi		YH, high(Eb)
		ldi 	ZL, low(Ebsign)
		ldi		ZH, high(Ebsign)
		ldi		threshold, E_samp
		mov		num_sample, threshold
		ldi		threshold, E_crossings
		mov		crossings, threshold
		ldi		threshold, E_threshold2  
		rjmp	release

HiEload:cpi		temp, HiEstr
		brne	farwaiting
		mov		whichstring, temp
		ldi		ZL,	low(HiEbsignno)
		ldi		ZH,	high(HiEbsignno)
		ld		totalneg, Z
		ldi		YL, low(HiEb)
		ldi		YH, high(HiEb)
		ldi 	ZL, low(HiEbsign)
		ldi		ZH, high(HiEbsign)
		ldi		threshold, HiE_samp
		mov		num_sample, threshold
		ldi		threshold, HiE_crossings
		mov		crossings, threshold
		ldi		threshold, HiE_threshold2
		rjmp	release  
		
farwaiting:							; subroutines starting with 'far' used to overcome
		rjmp	waiting				; branch out of reach error
						
release:
		in		temp2, PIND			; confirm string selection and continue
		cpi		temp2, 0xFF			; if PIND released
		brne 	release

init:	
		ldi 	XL, low(input)		; address in SRAM to store inputs from ADC
		ldi		XH, high(input)
		ldi    	mem_end, 21
		ld		coeffsign, Z+

checkT:								; loop if waiting for new input
		clr 	temp				; clear out registers to restart
		clr		temp2
		sbrs	flag, 0				; skip if flag on (still on first loop)
		ldi		looper, 0	
		brts	adc_wait			; check for T flag
		rjmp 	checkT

adc_wait:							; wait for ADC to finish conversion
		in 		temp, ADCSR
		sbrc	temp, 6
		rjmp	adc_wait
		clt
		in 		temp, ADCL
		in		temp2, ADCH
		lsr		temp2				; divide by 4 to convert 10 bit to 8 bit
		ror 	temp
		lsr 	temp2
		ror 	temp
		cpi		flag, 1
		breq	loop1	
		st		X+, temp
		sbrc	flag, 7
		rjmp	clearflag
		rjmp	loop
		
loop1: 
        st 		X, temp             ; first loop to fill SRAM (21 values for 21 coeffs)
        ld      mp8u, X+			; wait for interrupts to fill up input
        ld      mc8u, Y+			; multiply input from ADC and filter coeff on the go
        rcall   mpy8u				; and store as an accumulated sum
        inc     looper
		cp		looper, coeffsign	; check if current coeff is + or -
		breq	subtractor1
		rjmp	adder1
subtractor1:						; subtract from accumulated filtered output
		rcall 	sub24				; if coeff sign is negative
		ld		coeffsign, Z+
		clr 	r8
		rjmp	cont1
adder1:	rcall   add24				; add if coeff sign is positive
		clr 	r8
cont1:  cpi     looper, 21
        brne    checkT              ; wait for SRAM to be filled 
        clr     flag                ; clear flag to go into loop subroutine next round
        dec     mem_end
		sbiw	X, 21
		sbiw	Y, 21
		sub		ZL, totalneg
		sbci	ZH, 0
		ld		coeffsign, Z+
        rjmp    dcoffset         	; before calculating filtered output

loop:	
		ld		mp8u, X+            ; subsequent loops, 21 slots in SRAM already filled 
		ld		mc8u, Y+            ; each interrupt adds a new ADC input
		rcall	mpy8u				; and  produces a new filtered output
		inc		looper
		cp		looper, coeffsign
		breq	subtractor
		rjmp	adder
subtractor:
		rcall 	sub24
		ld		coeffsign, Z+
		clr 	r8
		rjmp	cont
adder:	rcall   add24
		clr 	r8
cont:	cp		looper, mem_end
		breq	rollX
		rjmp	skip

rollX:								; to prevent input pointer from overshooting 
		sbrc	flag, 7				; and accessing other memory sectors, pointer
		rjmp	skip				; has to be brought back to the beginning
		sbiw	X, 21				; of memory sector. 
		dec		mem_end
        breq    farreset_mem_end
		rjmp	skip

farreset_mem_end:
		rjmp	reset_mem_end

skip:								; if rollX not required
		cpi		looper, 21			; continue calculations until 21 values
		brne	loop
		sbiw	Y, 21				; roll back coefficient pointer after 21 values calculated
		sub		ZL, totalneg
		sbci	ZH, 0
		ld		coeffsign, Z+
		
dcoffset:							; dcoffset required for A and E strings
		push 	addsubB0			; since filtered output is displaced 
		push	addsubB1			; too far for proper zero crossing counting
		push	addsubB2
		push 	temp
		push	temp2	
		mov		temp2, whichstring
		cpi		temp2, highEstr
		breq	hEstroffset
		cpi		temp2, Bstr
		breq	Bstroffset
		cpi		temp2, Gstr
		breq	Gstroffset
		cpi		temp2, Dstr
		breq	Dstroffset
		cpi		temp2, Astr
		breq	Astroffset
		cpi		temp2, Estr
		breq	Estroffset
		rjmp	popout	
		
hEstroffset:
		ldi		temp, hE_offset0
		mov		addsubB0, temp
		ldi		temp, hE_offset1
		mov		addsubB1, temp
		ldi		temp, hE_offset2
		mov		addsubB2, temp
		rcall	sub24
		rjmp	popout
		
Bstroffset:
		ldi		temp, B_offset0
		mov		addsubB0, temp
		ldi		temp, B_offset1
		mov		addsubB1, temp
		ldi		temp, B_offset2
		mov		addsubB2, temp
		rcall	sub24
		rjmp	popout
		
Gstroffset:
		ldi		temp, G_offset0
		mov		addsubB0, temp
		ldi		temp, G_offset1
		mov		addsubB1, temp
		ldi		temp, G_offset2
		mov		addsubB2, temp
		rcall	sub24
		rjmp	popout
		
Dstroffset:
		ldi		temp, D_offset0
		mov		addsubB0, temp
		ldi		temp, D_offset1
		mov		addsubB1, temp
		ldi		temp, D_offset2
		mov		addsubB2, temp
		rcall	sub24
		rjmp	popout

Astroffset:
		ldi		temp, A_offset0
		mov		addsubB0, temp
		ldi		temp, A_offset1
		mov		addsubB1, temp
		ldi		temp, A_offset2
		mov		addsubB2, temp
		rcall	sub24
		rjmp	popout

Estroffset:
		ldi		temp, E_offset0
		mov		addsubB0, temp
		ldi		temp, E_offset1
		mov		addsubB1, temp
		ldi		temp, E_offset2
		mov		addsubB2, temp
		rcall	sub24
		rjmp	popout		

popout:	pop		temp2
		pop 	temp
		pop		addsubB2
		pop		addsubB1
		pop		addsubB0
		
freq_count:							; 2nd half of code deals with counting the frequency
		cpi		count_T, 0
		breq	skipS				; only start counting samples after 1st zero crossing 
		inc 	count_S
skipS:	ldi		temp, 1
		sbrs 	filtout2, 7			; check MSB of filtered output
		ldi 	temp, 0				; set temp=0 if negative, temp=1 if positive
		cp		temp, oldsign		; compare with previous sign value to determine whether sign has changed
		brne	signchange			

no_rise_edge:
		mov		oldsign, temp		; hold temp value (0 or 1) in register oldsign to be compared 
		clr 	filtout1			; with next filtered value
		clr		filtout2
		clr		filtout0
		rjmp 	checkT
		
signchange:
		sbrc	filtout2, 7			; check if rising edge
		rjmp 	no_rise_edge		; branch out if not rising
		inc 	count_T
		mov		oldsign, temp		; hold temp value (0 or 1) in register oldsign to be compared 
									; with next filtered value
checkcross:		
		clr 	filtout1
		clr		filtout2
		clr		filtout0
		cp		count_T, crossings	; check if signal has reached number of crossings required
		brne	farcheckT			; if not equal continue with next ADC input
		out		PORTB, count_S
		sub 	count_S, num_sample
		ldi		temp2, 0			; check the difference in sample number
		sbrc	count_S, 7			; now count_S is the difference in sample number
		ldi 	temp2, 1			; temp2=0 means positive, temp2=1 means negative
		sbrc	temp2, 0
		neg		count_S				; two's complement to find magnitude of difference
		
		cp		count_S, threshold	; check if frequency is sharp, flat or tuned
		brlo 	tuned				; by comparing calculated sample number 
		sbrc	temp2, 0			; to 'tuned' sample number
		rjmp 	sharp	
		rjmp	flat	
back:
		out		PORTE, temp			; output result of frequency measurement on LEDs 
		clr 	count_S
		clr		count_T
		in		temp, PIND			; check button input
		cp	 	temp, whichstring	; if same button or
		breq	farcheckT			; no button
		cpi 	temp, 0b11111111	; use same filter settings
		breq	farcheckT
	 	rjmp 	waiting	

farcheckT:							; since branch instruction can't branch far enough (yet again)
		rjmp	checkT
			
reset_mem_end:						  
        ldi     mem_end, 21			; reset mem_end so next loop starts at position 1
		ldi		flag, 0b10000000	; set MSB of flag to notify X pointer to be reset on next loop
        rjmp    skip      

clearflag:
		sbiw	X, 21				; reset SRAM slots available for ADC input back to 21
		clr		flag
		rjmp 	loop

tuned:  ldi		temp, 0b11100111	; configure LED pattern for 'tuned'
		rjmp	back
		
sharp:	mov		temp2, whichstring	; check which string is being tuned
		cpi		temp2, Estr			; and branch to corresponding subroutine
		breq	Esharp2
		cpi		temp2, Astr
		breq	Asharp2
		cpi		temp2, Dstr
		breq	Dsharp2
		cpi		temp2, Gstr
		breq	Gsharp2
		cpi		temp2, Bstr
		breq	Bsharp2
		cpi		temp2, HiEstr
		breq	HiEsharp2

Esharp2:cpi		count_S, E_threshold5
		brsh	Esharp5				; check how far off frequency is from tuned value in turn
		ldi		temp, 0b11011111	; configure LED pattern for 2% 'sharp'
		rjmp	back
Esharp5:cpi		count_S, E_threshold10
		brsh	Esharp10
		ldi		temp, 0b10111111	; configure LED pattern for 5% 'sharp'
		rjmp	back
Esharp10:	
		ldi		temp, 0b01111111	; configure LED pattern for 10% 'sharp'
		rjmp	back

Asharp2:cpi		count_S, A_threshold5
		brsh	Asharp5
		ldi		temp, 0b11011111
		rjmp	back
Asharp5:cpi		count_S, A_threshold10
		brsh	Asharp10
		ldi		temp, 0b10111111
		rjmp	back
Asharp10:	
		ldi		temp, 0b01111111
		rjmp	back

Dsharp2:cpi		count_S, D_threshold5
		brsh	Dsharp5
		ldi		temp, 0b11011111
		rjmp	back
Dsharp5:cpi		count_S, D_threshold10
		brsh	Dsharp10
		ldi		temp, 0b10111111
		rjmp	back
Dsharp10:	
		ldi		temp, 0b01111111
		rjmp	back

Gsharp2:cpi		count_S, G_threshold5
		brsh	Gsharp5
		ldi		temp, 0b11011111
		rjmp	back
Gsharp5:cpi		count_S, G_threshold10
		brsh	Gsharp10
		ldi		temp, 0b10111111
		rjmp	back
Gsharp10:	
		ldi		temp, 0b01111111
		rjmp	back

Bsharp2:cpi		count_S, B_threshold5
		brsh	Bsharp5
		ldi		temp, 0b11011111
		rjmp	back
Bsharp5:cpi		count_S, B_threshold10
		brsh	Bsharp10
		ldi		temp, 0b10111111
		rjmp	back
Bsharp10:	
		ldi		temp, 0b01111111
		rjmp	back

HiEsharp2:
		cpi		count_S, HiE_threshold5
		brsh	HiEsharp5
		ldi		temp, 0b11011111
		rjmp	back
HiEsharp5:
		cpi		count_S, HiE_threshold10
		brsh	HiEsharp10
		ldi		temp, 0b10111111
		rjmp	back
HiEsharp10:	
		ldi		temp, 0b01111111
		rjmp	back

flat:	mov		temp2, whichstring	; similar procedure with flat values
		cpi		temp2, Estr
		breq	Eflat2
		cpi		temp2, Astr
		breq	Aflat2
		cpi		temp2, Dstr
		breq	Dflat2
		cpi		temp2, Gstr
		breq	Gflat2
		cpi		temp2, Bstr
		breq	Bflat2
		cpi		temp2, HiEstr
		breq	HiEflat2

Eflat2:	cpi		count_S, E_threshold5
		brsh	Eflat5
		ldi		temp, 0b11111011	; configure LED pattern for 2% 'sharp'
		rjmp 	back
Eflat5: cpi		count_S, E_threshold10
		brsh	Eflat10
		ldi		temp, 0b11111101	; configure LED pattern for 5% 'flat'
		rjmp	back
Eflat10:	
		ldi		temp, 0b11111110	; configure LED pattern for 10% 'flat'
		rjmp 	back
		
Aflat2:	cpi		count_S, A_threshold5
		brsh	Aflat5
		ldi		temp, 0b11111011
		rjmp 	back
Aflat5: cpi		count_S, A_threshold10
		brsh	Aflat10
		ldi		temp, 0b11111101
		rjmp	back
Aflat10:	
		ldi		temp, 0b11111110
		rjmp 	back

Dflat2:	cpi		count_S, D_threshold5
		brsh	Dflat5
		ldi		temp, 0b11111011
		rjmp 	back
Dflat5: cpi		count_S, D_threshold10
		brsh	Dflat10
		ldi		temp, 0b11111101
		rjmp	back
Dflat10:	
		ldi		temp, 0b11111110
		rjmp 	back

Gflat2:	cpi		count_S, G_threshold5
		brsh	Gflat5
		ldi		temp, 0b11111011
		rjmp 	back
Gflat5: cpi		count_S, G_threshold10
		brsh	Gflat10
		ldi		temp, 0b11111101
		rjmp	back
Gflat10:	
		ldi		temp, 0b11111110
		rjmp 	back

Bflat2:	cpi		count_S, B_threshold5
		brsh	Bflat5
		ldi		temp, 0b11111011
		rjmp 	back
Bflat5: cpi		count_S, B_threshold10
		brsh	Bflat10
		ldi		temp, 0b11111101
		rjmp	back
Bflat10:	
		ldi		temp, 0b11111110
		rjmp 	back

HiEflat2:	
		cpi		count_S, HiE_threshold5
		brsh	HiEflat5
		ldi		temp, 0b11111011
		rjmp 	back
HiEflat5: 
		cpi		count_S, HiE_threshold10
		brsh	HiEflat10
		ldi		temp, 0b11111101
		rjmp	back
HiEflat10:	
		ldi		temp, 0b11111110
		rjmp 	back

; ***** Interrupt Routine *****
;
trig_in: 
		in		save, SREG
		push 	temp
		push 	temp2
		ldi 	temp, 0b01000000	; start A-D conversion
		in 		temp2, ADCSR
		or 		temp, temp2
		out		ADCSR, temp
		out		SREG, save
		set
		pop 	temp2
		pop 	temp
		reti	

;***** 24-bit Add function, result in addsubA0..3*****

add24:	add	addsubA0, addsubB0		; Add low bytes
		adc	addsubA1, addsubB1		; Add high bytes with carry
		adc	addsubA2, addsubB2
		ret

;***** 24-bit Subtract function, result in addsubA0..3 (A-B)
;***** Code
sub24:	sub	addsubA0,addsubB0		; Subtract low bytes
		sbc	addsubA1,addsubB1		; Subtract high byte with carry
		sbc	addsubA2,addsubB2
		ret
		
;***************************************************************************
;*
;* "mpy8u" - 8x8 Bit Unsigned Multiplication
;* <<Adapted from source code of Steve Berman, Mark Krangle & Rich Levy>>
;*
;* This subroutine multiplies the two register variables mp8u and mc8u.
;* The result is placed in registers m8uH, m8uL
;*  
;* Number of words	:34 + return
;* Number of cycles	:34 + return
;* Low registers used	:None
;* High registers used  :3 (mc8u,mp8u/m8uL,m8uH)	
;*
;* Note: Result Low byte and the multiplier share the same register.
;* This causes the multiplier to be overwritten by the result.
;*
;***************************************************************************

mpy8u:	clr	m8uH		;clear result High byte
	lsr	mp8u		;shift multiplier
	
	brcc	noad80		;if carry set
	add	m8uH,mc8u	;    add multiplicand to result High byte
noad80:	ror	m8uH		;shift right result High byte 
	ror	m8uL		;rotate right result L byte and multiplier

	brcc	noad81		;if carry set
	add	m8uH,mc8u	;    add multiplicand to result High byte
noad81:	ror	m8uH		;shift right result High byte 
	ror	m8uL		;rotate right result L byte and multiplier

	brcc	noad82		;if carry set
	add	m8uH,mc8u	;    add multiplicand to result High byte
noad82:	ror	m8uH		;shift right result High byte 
	ror	m8uL		;rotate right result L byte and multiplier

	brcc	noad83		;if carry set
	add	m8uH,mc8u	;    add multiplicand to result High byte
noad83:	ror	m8uH		;shift right result High byte 
	ror	m8uL		;rotate right result L byte and multiplier

	brcc	noad84		;if carry set
	add	m8uH,mc8u	;    add multiplicand to result High byte
noad84:	ror	m8uH		;shift right result High byte 
	ror	m8uL		;rotate right result L byte and multiplier

	brcc	noad85		;if carry set
	add	m8uH,mc8u	;    add multiplicand to result High byte
noad85:	ror	m8uH		;shift right result High byte 
	ror	m8uL		;rotate right result L byte and multiplier

	brcc	noad86		;if carry set
	add	m8uH,mc8u	;    add multiplicand to result High byte
noad86:	ror	m8uH		;shift right result High byte 
	ror	m8uL		;rotate right result L byte and multiplier

	brcc	noad87		;if carry set
	add	m8uH,mc8u	;    add multiplicand to result High byte
noad87:	ror	m8uH		;shift right result High byte 
	ror	m8uL		;rotate right result L byte and multiplier
	
	ret	
