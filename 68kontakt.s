; SPREADPOINT
; 68KONTAKT (AMIGA, >= OCS, >= 68000, >= 512 KB, Kickstart 1.x)
; (C) 2024 DEPECHE

; The formula for the pattern comes from Martin Kleppe's post:
; https://x.com/aemkei/status/1378461643399696388

; Build with vasm:
; vasmm68k_mot -kick1hunks -Fhunkexe -o 68kontakt -nosym 68kontakt.s
; Change value of the 23rd byte in the resulting binary from $00 to $ff
; in order to clear bitmap memory and to complete copper list.
; See Losso's blog post https://heckmeck.de/demoscene/hypotrain
; We assume the binary will be loaded below address $30000.

bitplane1	equ	$3c000
bitplane2	equ	$40000+$07d0		; $07d0 will be used as value
pwidth		equ	40
numchars	equ	3
delta		equ	8			; y spacing of chars
spacing		equ	3			; x (*8) spacing of chars

	code_c					; must run in chip memory

	lea	clist(pc),a0			;
	move.l	a0,$dff080			;

	lea	bitplane1,a0			;
	lea	bitplane2-bitplane1(a0),a3	;

; draw pattern ((x-128) * 64) % (y-128)

	moveq	#-127,d1			; y
	moveq	#-128,d4			; d4.b = %1000 0000 (bitpattern)
.yloop	move.w	#-163,d0			; x (-160 = centered)
.xloop	tst.w	d1				; avoid division by zero
	beq	.zero				; (beq .zero -> filled, beq .notset -> gap)
	move.w	d0,d5				; copy of x
	asl.w	#6,d5				; *64
	ext.l	d5				;
	divs.w	d1,d5				;
	swap	d5				; reminder
	tst.w	d5				;
	bne	.notset				;
.zero	or.b	d4,(a0)				; set bit
.notset	addq.w	#1,d0				; advance x
	ror.b	#1,d4				; shift bitpattern
	bcc	.stay				;
	addq.l	#1,a0				; next byte
.stay	cmp.w	#-163+320,d0			;
	bne	.xloop				;
	addq.w	#1,d1				; advance y
	cmp.w	#-127+256,d1			;
	bne	.yloop				;

.main	move.l	a3,a2				; clear bitplane2 (partly)
;	move.w	#(120+80)*10-1,d7		; = $7cf
	move.w	a3,d7				; a3.w = $7d0
.cls	clr.l	(a2)+				;
	dbf	d7,.cls				;

	lea	(128-32-3)*pwidth+(20-(numchars>>1*spacing))(a3),a4 ; base text position
	moveq	#pwidth,d1			; const (used twice)

	lea	text(pc),a0			;
	moveq	#numchars-1,d3			;
.chars	and.w	#$003f,d4			; y pos (note: d4 was not initialized)
	move.w	d4,d0				;
	mulu.w	d1,d0				;
	lea	(a4,d0.w),a1			;
;	lea	(128-32-3)*pwidth+(20-(numchars>>1*spacing))(a3,d0.w),a1 ; base text position (out of reach)

	moveq	#5-1,d7				; char height
.char	move.b	(a0)+,(a1)			;
	add.w	d1,a1				;
	dbf	d7,.char			;

	addq.w	#delta,d4			; y pos next char
	addq.l	#spacing,a4			; x pos next char
	dbf	d3,.chars			;

	sub.w	#numchars*delta+1,d4		; prepre next frame (+1 = speed)

.wrast	cmp.b	$dff006,d7			;
	bne	.wrast				;

	bra	.main				;


*------	TEXT (FONT) ----------------------------------------------------------*

text	dc.b	%01111100			; digit "6"
	dc.b	%11000000
	dc.b	%11111100
	dc.b	%11000110
	dc.b	%01111100

	dc.b	%01111100			; digit "8"
	dc.b	%11000110
	dc.b	%01111100
	dc.b	%11000110
	dc.b	%01111100

	dc.b	%11000110			; char "K"
	dc.b	%11001100
	dc.b	%11111000
	dc.b	%11001100
	dc.b	%11000110


*------	COPPER INSTRUCTION LIST ----------------------------------------------*

	even

; ! = (Hopefully) set before by OS

clist	dc.w	$009a,$7fff
	dc.w	$0096,$0020	; sprites off

	dc.w	$008e,$2c81
	dc.w	$0090,$2bc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$00e0,bitplane1>>16
	dc.w	$00e2,bitplane1&$ffff
	dc.w	$00e4,bitplane2>>16
	dc.w	$00e6,bitplane2&$ffff

	dc.w	$0100,$2200	; 2 bitplanes
;	dc.w	$0102,$0000	; !
;	dc.w	$0108,$0000	; !
;	dc.w	$010a,$0000	; !

	dc.w	$0180,$0300
	dc.w	$0182,$0f33
	dc.w	$0184,$0fcc
	dc.w	$0186,$0fcc

; followed by $0000,$0000 (stops copper)
