;*************************************************************
;*                     3d-rotations                          *
;*               coded by dr.df0 of .. atd ..                *
;*                   on  4 july 1992                         *
;*          revieved on  6 april 2019                        *
;*************************************************************

; MIT License

; Copyright (c) 2019 Krzysztof Kielak

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

openlibrary = -30-522
allocmem    = -30-168
freemem     = -30-180

startlist   = 38

execbase    = 4
planesize   = 40*256

; memory allocation types
chip        = 2
clear       = chip+$10000

;* enter supervisor mode
;***********************************************
start:	
	move.l	#begin,$80.w
	trap	#0
	moveq	#0,d0
	rts

;* main program in the supervisor mode
;***********************************************
begin:	
	lea	$dff000,a6
	move.w	$1c(a6),save1
	move.w	$02(a6),save2
	move.w	$1e(a6),save3
	ori.w	#$c000,save1
	ori.w	#$8000,save2
	ori.w	#$8000,save3

; wait for raster to raach bottom of the screen
.rastwait:	
	move.l	4(a6),d0
	and.l	#$1ff00,d0
	cmpi.l	#$13700,d0
	bne.s	.rastwait

	move.w	#$7fff,$9a(a6)
	move.w	#$7fff,$96(a6)
	move.w	#$7fff,$9c(a6)
	move.l	$6c.w,save_irq

	move.l	#screen1,planeadr
	move.l	#screen3,clsadr
	move.l	#screen2,midleadr
	move.w	#$8400,$dff096

	bsr.w	init
	
	move.l	planeadr,d0
	move.w	d0,pl1l
	swap	d0
	move.w	d0,pl1h
	swap	d0

	add.l	#40,d0	
	move.w	d0,pl2l
	swap	d0
	move.w	d0,pl2h
	swap	d0

	add.l	#40,d0	
	move.w	d0,pl3l
	swap	d0
	move.w	d0,pl3h

	move.w	#$3081,$dff08e
	move.w	#$30c1,$dff090
	move.w	#$0038,$dff092
	move.w	#$00d0,$dff094
	move.w	#%0011001000000000,$dff100
	clr.w	$dff102
	clr.w	$dff104
	move.w	#80,$dff108
	move.w	#80,$dff10a

	move.l	#irq_routine,$6c.w
	move.l	a7,stack

	move.l	#cladr,$dff080
	clr.w	$dff088

	move.w	#%1000001111000000,$96(a6)
	move.w	#$c020,$9a(a6)

	bsr.w	main_loop

exit:	
	move.l	stack(pc),a7
	lea	$dff000,a6
	bsr.w	waitblit
	move.l	save_irq(pc),$6c.w
	move.w	#$7fff,$9a(a6)
	move.w	#$7fff,$96(a6)
	move.w	#$7fff,$9c(a6)
	move.w	save3(pc),$9c(a6)
	move.w	save2(pc),$96(a6)
	move.w	save1(pc),$9a(a6)
	lea	grname(pc),a1
	moveq	#0,d0
	move.l	$4.w,a6
	jsr	openlibrary(a6)
	move.l	d0,a6
	lea	$dff000,a5
	move.l	startlist(a6),$dff080
	move.w	d0,$dff088
	rte

;* initialization
;***********************************************
init:
; initialize color palette
	move.w	#$0,$dff180
	move.w	#$fff,$dff182
	move.w	#$f00,$dff184
	move.w	#$fff,$dff186
	move.w	#$f00,$dff188
	move.w	#$fff,$dff18a
	move.w	#$800,$dff18c
	move.w	#$fff,$dff18e

	move.w	#24000,observer
	move.w	#00,alfa
	move.w	#00,beta
	move.w	#00,gama

	lea	line_addr_tab(pc),a0
	moveq	#0,d0
	move.w	#120,d1
	move.l	#250,d2
.loop:	move.w	d0,(a0)+
	add.w	d1,d0
	dbf	d2,.loop

	move.l	wsk_sin(pc),a0
	move.w	(a0),wsp_y
	bsr.w	rotate
	rts

;* variables
;***********************************************
planeadr:	dc.l    0
clsadr:		dc.l    0
midleadr:	dc.l    0

newfigur:	dc.l    0
observer:	dc.l    0
flaga:		dc.l    0
counter:	dc.l    0
progadr:	dc.l    0
alfa:		dc.w    0
beta:		dc.w    0
gama:		dc.w    0
wymfg:		dc.l    0
save1:		dc.l    0
save2:		dc.l    0
save3:		dc.l    0
wsp_y:		dc.w    0
wsp_ay:		dc.w    0
wsk_sin:	dc.l    sin2+32

stack:		dc.l    0
save_irq:	dc.l 0

grname:	dc.b    "graphics.library",0
	even

;* interrupt routine
;***********************************************
irq_routine:
	movem.l	d0-d7/a0-a6,-(sp)

	andi.w	#$20,$dff01e
	beq.w	.out
	move.w	#$20,$dff09c

	move.l	wsk_sin(pc),a0
	tst.w	(a0)
	bpl.s	.ll
	lea	sin2(pc),a0
.ll:	
	move.w	(a0)+,d0
	move.l	a0,wsk_sin
	move.w	d0,wsp_y

	bsr.w	display_object
	bsr.w	fill_screen
	bsr.w	clear_screen
	bsr.w	swap_screen
	bsr.w	rotate

; rotations around y-axis
	add.w	#4,beta
	cmp.w	#720,beta
	bne.s	.out
	move.w	#0,beta
.out:	
	movem.l	(sp)+,d0-d7/a0-a6
	rte

;* copper program
;***********************************************
cladr:
	dc.w	$e0
pl1h:	dc.w	0,$e2
pl1l:	dc.w	0,$e4
pl2h:	dc.w	0,$e6
pl2l:	dc.w	0,$e8
pl3h:	dc.w	0,$ea
pl3l:	dc.w	0,$ec
	dc.w	0

	dc.w	$ffff,$fffe

; ... just waiting for the mouse button
main_loop:
	btst	#6,$bfe001
	bne.s	main_loop

	rts
	
;* swap screens
;***********************************************
swap_screen:
	move.l	clsadr,d0
	move.l	planeadr,clsadr
	move.l	midleadr,planeadr
	move.l	midleadr,d1
	move.l	d0,midleadr
	move.l	d1,d0
	move.w	d0,pl1l
	swap	d0
	move.w	d0,pl1h
	swap	d0
	add.l	#40,d0	
	move.w	d0,pl2l
	swap	d0
	move.w	d0,pl2h
	swap	d0
	add.l	#40,d0	
	move.w	d0,pl3l
	swap	d0
	move.w	d0,pl3h
	rts

;* rotate set of 3D points
;***********************************************
rotate:
	move.w	#300,wsp_ay
	lea	sinus(pc),a0
	lea	cosin(pc),a1
	move.l	#points_3d,a2
	move.l	#points_2d,a4

rloop:
	movem.l	(a2)+,d0-d2
	cmp.w	#$7fff,d0	
	beq.w	exit_p

;around x
;	move.w	alfa(pc),d7
;	move.w	(a0,d7.w),d6
;	move.w	(a1,d7.w),d7
;	move.w	d2,d3
;	muls	d6,d3
;	muls	d7,d2
;	move.w	d1,d4
;	muls	d6,d4
;	muls	d7,d1
;	add.l	d1,d1
;	swap	d1
;	add.l	d2,d2
;	swap	d2
;	add.l	d3,d3
;	swap	d3
;	add.l	d4,d4
;	swap	d4
;	add.w	d4,d2
;	sub.w	d3,d1	

;around y	
	move.w	beta(pc),d7
	move.w	(a0,d7.w),d6
	move.w	(a1,d7.w),d7
	move.w	d2,d3
	muls	d6,d3
	muls	d7,d2
	move.w	d0,d4
	muls	d6,d4
	muls	d7,d0
	add.l	d0,d0
	swap	d0
	add.l	d2,d2
	swap	d2
	add.l	d3,d3
	swap	d3
	add.l	d4,d4
	swap	d4
	add.w	d3,d0
	sub.w	d4,d2	

;around z
;	move.w	gama(pc),d7
;	move.w	(a0,d7.w),d6
;	move.w	(a1,d7.w),d7
;	move.w	d0,d3
;	muls	d6,d3
;	muls	d7,d0
;	move.w	d1,d4
;	muls	d6,d4
;	muls	d7,d1
;	add.l	d0,d0
;	swap	d0
;	add.l	d1,d1
;	swap	d1
;	add.l	d3,d3
;	swap	d3
;	add.l	d4,d4
;	swap	d4
;	add.w	d3,d1
;	sub.w	d4,d0

; projection to 2d surface
	add.w	wsp_y,d1
	cmp.w	#10000,d1
	ble.s	.al1
	move.w	#10000,d1
.al1:
	add.w	observer,d2
	ext.l	d1
	ext.l	d0
	lsl.l	#8,d0
	lsl.l	#8,d1
	divs	d2,d0
	divs	d2,d1
	add.w	#150,d0
	add.w	#55,d1

	move.w	d0,(a4)+
	move.w	d1,(a4)+

	cmp.w	wsp_ay,d1
	bge.s	.lll
	move.w	d1,wsp_ay
.lll
	bra rloop
exit_p:
	rts

;     sinus and cosinus tables
sinus:
	dc.w	$0000,$0242,$0484,$06c6,$0907,$0b48,$0d87,$0fc6
	dc.w	$1203,$143f,$1679,$18b2,$1ae8,$1d1d,$1f4f,$217e
	dc.w	$23ab,$25d5,$27fc,$2a20,$2c41,$2e5d,$3077,$328c
	dc.w	$349d,$36ab,$38b3,$3ab8,$3cb7,$3eb2,$40a8,$4298
	dc.w	$4483,$4669,$4849,$4a24,$4bf8,$4dc7,$4f8f,$5151
	dc.w	$530c,$54c1,$566f,$5816,$59b6,$5b4f,$5ce1,$5e6b
	dc.w	$5fee,$6169,$62dc,$6448,$65ab,$6706,$6859,$69a4
	dc.w	$6ae6,$6c20,$6d51,$6e7a,$6f9a,$70b0,$71be,$72c3
	dc.w	$73be,$74b1,$759a,$7679,$774f,$781c,$78df,$7998
	dc.w	$7a48,$7aee,$7b8a,$7c1d,$7ca5,$7d24,$7d98,$7e02
	dc.w	$7e63,$7eb9,$7f05,$7f48,$7f80,$7fad,$7fd1,$7fea
	dc.w	$7ffa,$7fff
cosin:
	dc.w	$7fff,$7ff9,$7fea,$7fd1,$7fae,$7f81,$7f4a,$7f09
	dc.w	$7ebe,$7e69,$7e0a,$7da1,$7d2e,$7cb2,$7c2c,$7b9c
	dc.w	$7b02,$7a5f,$79b2,$78fb,$783b,$7772,$769f,$75c3
	dc.w	$74de,$73f0,$72f8,$71f8,$70ee,$6fdc,$6ec1,$6d9d
	dc.w	$6c70,$6b3c,$69fe,$68b9,$676b,$6615,$64b7,$6351
	dc.w	$61e3,$606e,$5ef1,$5d6d,$5be1,$5a4f,$58b5,$5714
	dc.w	$556c,$53be,$5209,$504d,$4e8c,$4cc4,$4af6,$4922
	dc.w	$4749,$456a,$4385,$419b,$3fac,$3db8,$3bbf,$39c2
	dc.w	$37c0,$35ba,$33af,$31a0,$2f8e,$2d78,$2b5e,$2940
	dc.w	$2720,$24fc,$22d6,$20ad,$1e81,$1c53,$1a23,$17f0
	dc.w	$15bc,$1386,$114f,$0f16,$0cdc,$0aa1,$0865,$0628
	dc.w	$03eb,$01ae,$ff71,$fd34,$faf7,$f8ba,$f67e,$f442
	dc.w	$f208,$efce,$ed96,$eb5f,$e92a,$e6f7,$e4c5,$e296
	dc.w	$e069,$de3e,$dc17,$d9f2,$d7d0,$d5b1,$d395,$d17d
	dc.w	$cf69,$cd58,$cb4b,$c943,$c73f,$c53f,$c344,$c14d
	dc.w	$bf5c,$bd6f,$bb88,$b9a6,$b7ca,$b5f4,$b423,$b258
	dc.w	$b093,$aed4,$ad1c,$ab6b,$a9bf,$a81b,$a67e,$a4e7
	dc.w	$a358,$a1d0,$a04f,$9ed6,$9d65,$9bfb,$9a99,$993f
	dc.w	$97ed,$96a4,$9562,$9429,$92f8,$91d0,$90b1,$8f9a
	dc.w	$8e8c,$8d87,$8c8b,$8b98,$8aae,$89ce,$88f6,$8828
	dc.w	$8763,$86a8,$85f7,$854e,$84b0,$841b,$8390,$830f
	dc.w	$8297,$8229,$81c5,$816b,$811b,$80d5,$8099,$8067
	dc.w	$803f,$8021,$800d,$8003,$8003,$800d,$8021,$803f
	dc.w	$8067,$8099,$80d5,$811b,$816b,$81c5,$8229,$8297
	dc.w	$830e,$8390,$841b,$84b0,$854e,$85f6,$86a8,$8763
	dc.w	$8828,$88f6,$89cd,$8aae,$8b98,$8c8b,$8d87,$8e8c
	dc.w	$8f9a,$90b1,$91d0,$92f8,$9429,$9562,$96a3,$97ed
	dc.w	$993f,$9a99,$9bfb,$9d64,$9ed6,$a04f,$a1d0,$a358
	dc.w	$a4e7,$a67d,$a81b,$a9bf,$ab6a,$ad1c,$aed4,$b092
	dc.w	$b257,$b422,$b5f3,$b7ca,$b9a6,$bb88,$bd6f,$bf5b
	dc.w	$c14d,$c343,$c53e,$c73e,$c942,$cb4b,$cd57,$cf68
	dc.w	$d17c,$d395,$d5b0,$d7cf,$d9f1,$dc16,$de3e,$e068
	dc.w	$e295,$e4c4,$e6f6,$e929,$eb5e,$ed95,$efcd,$f207
	dc.w	$f441,$f67d,$f8b9,$faf6,$fd33,$ff71,$01ad,$03ea
	dc.w	$0627,$0864,$0aa0,$0cdb,$0f15,$114e,$1385,$15bb
	dc.w	$17f0,$1a22,$1c52,$1e80,$20ac,$22d5,$24fc,$271f
	dc.w	$2940,$2b5d,$2d77,$2f8d,$31a0,$33ae,$35b9,$37bf
	dc.w	$39c1,$3bbf,$3db8,$3fac,$419b,$4384,$4569,$4748
	dc.w	$4921,$4af5,$4cc3,$4e8b,$504d,$5208,$53bd,$556c
	dc.w	$5713,$58b4,$5a4e,$5be1,$5d6d,$5ef1,$606e,$61e3
	dc.w	$6351,$64b6,$6614,$676a,$68b8,$69fe,$6b3b,$6c70
	dc.w	$6d9c,$6ec0,$6fdb,$70ee,$71f7,$72f8,$73ef,$74de
	dc.w	$75c3,$769f,$7772,$783b,$78fb,$79b2,$7a5f,$7b02
	dc.w	$7b9c,$7c2c,$7cb2,$7d2e,$7da1,$7e0a,$7e69,$7ebe
	dc.w	$7f09,$7f4a,$7f81,$7fae,$7fd1,$7fea,$7ff9,$7fff

sin2:
	dc.w	$1064,$11e9,$1369,$14e0,$164a,$17a3,$18e9,$1a18,$1b2e,$1c27
	dc.w	$1d01,$1dbb,$1e52,$1ec5,$1f12,$1f3a,$1f3c,$1f17,$1ecd,$1e5d
	dc.w	$1dc9,$1d13,$1c3b,$1b45,$1a32,$1905,$17c1,$1669,$1501,$138b
	dc.w	$120c,$1087,$0eff,$0d79,$0bf9,$0a81,$0916,$07bb,$0673,$0542
	dc.w	$042a,$032e,$0251,$0195,$00fa,$0084,$0033,$0008,$0003,$0024
	dc.w	$006b,$00d7,$0168,$021b,$02f0,$03e4,$04f4,$061f,$0761,$08b7
	dc.w	$0a1f,$0b93,$0d12,$0e97

	dc.w	-1

;* display wireframe of visible faces
;***********************************************
display_object:
	lea	$dff000,a6
	bsr.w	waitblit
	move.l	#-1,$dff044
	move.l	#$ffff8000,$dff072
	move.w	#120,$dff060
	move.w	#120,$dff066
	move.l	#egdes_3d,a0
	move.l	#points_2d,a1
	lea	line_addr_tab(pc),a4
sh_1:
	move.w	(a0)+,modula
	bmi.w	exit_p

	movem.w	(a0),d0-d2
	movem.w	(a1,d2.w),d4-d5
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	sub.w	d2,d4
	sub.w	d3,d5
	sub.w	d0,d2
	sub.w	d1,d3
	muls	d2,d5
	muls	d3,d4
	sub.l	d5,d4
	ble.s	visible	
	add.w	#80,modula
visible:	
	cmp.w	#120,modula
	beq.w	no_vis

	move.w	(a0)+,d0
	movem.w	(a1,d0.w),d0-d1
	move.w	(a0),d2
	movem.w	(a1,d2.w),d2-d3
	bsr.w	draw_line
	
	move.w	(a0)+,d0
	movem.w	(a1,d0.w),d0-d1
	move.w	(a0),d2
	movem.w	(a1,d2.w),d2-d3
	bsr.w	draw_line

	move.w	(a0)+,d0
	movem.w	(a1,d0.w),d0-d1
	move.w	-6(a0),d2
	movem.w	(a1,d2.w),d2-d3
	bsr.w	draw_line

	bra.w	sh_1
no_vis:
	addq.l	#6,a0
	bra.w	sh_1

line_addr_tab:
	blk.l	400,0

;* wait for blitter ready 
;***********************************************
waitblit:
	btst	#6,$dff002
	bne.s	waitblit
	rts

;* fill the bitplane area
;***********************************************
fill_screen:
	move.l	midleadr(pc),a0
	move.w	wsp_ay(pc),d0
	mulu	#120,d0
	lea	(a0,d0.l),a0
	add.l	#120*111-16,a0
	bsr.w	waitblit
	move.l	a0,$dff050
	move.l	a0,$dff054
	move.w	#26,$dff064
	move.w	#26,$dff066
	move.l	#$09f0000a,$dff040
	move.l	#-1,$dff044
	move.w	#3*110*64+7,$dff058

	rts

;* clear the screen area
;***********************************************
clear_screen:
	move.l	clsadr,a0
	move.w	wsp_ay,d0
	mulu	#120,d0
	lea	10(a0,d0.l),a0
	lea	-120*9(a0),a0
	move.l	#$0,d0
	move.l	d0,d1
	move.l	d0,d2
	move.l	d0,d3
	move.l	d0,d4
	move.l	d0,d5
	move.l	#17,d7
.cl01:
	movem.l	d0-d3,(a0)
	movem.l	d0-d3,40(a0)
	movem.l	d0-d3,80(a0)
	movem.l	d0-d3,120(a0)
	movem.l	d0-d3,160(a0)
	movem.l	d0-d3,200(a0)
	movem.l	d0-d3,240(a0)
	movem.l	d0-d3,280(a0)
	movem.l	d0-d3,320(a0)
	movem.l	d0-d3,360(a0)
	movem.l	d0-d3,400(a0)
	movem.l	d0-d3,440(a0)
	movem.l	d0-d3,480(a0)
	movem.l	d0-d3,520(a0)
	movem.l	d0-d3,560(a0)
	movem.l	d0-d3,600(a0)
	movem.l	d0-d3,640(a0)
	movem.l	d0-d3,680(a0)
	movem.l	d0-d3,720(a0)
	movem.l	d0-d3,760(a0)
	lea	800(a0),a0

	dbf	d7,.cl01
	rts

;* draw a line
; d0,d1-x,y coordinates for line start
; d2,d3-x,y coordinates for line end
;***********************************************
draw_line:
	move.l	midleadr(pc),a2
	add.w	modula,a2
	moveq	#15,d4
	cmp.w	d1,d3
	beq.w	exit_p
	ble.s	moon
	exg	d0,d2
	exg	d1,d3
moon:	and.w	d2,d4
	move.w	d4,d5
	not.b	d5
	sub.w	d3,d1
	ext.l	d3
	add.w	d3,d3
	move.w	(a4,d3.w),d3
	sub.w	d2,d0
	blt.s	tron
	cmp.w	d0,d1
	bge.s	prince
	moveq	#$11,d7
	bra.s	speedy
prince:	moveq	#1,d7
	exg	d1,d0
	bra.s	speedy
tron:	neg.w	d0
	cmp.w	d0,d1
	bge.s	only
	moveq	#$15,d7
	bra.s	speedy
only:	moveq	#$9,d7
	exg	d1,d0
speedy:	add.w   d1,d1
	lsr.w	#3,d2
;	ext.l	d2
	add.w	d2,d3
	add.w	d3,a2
	move.w	d1,d2
	swap	d4
	lsr.l	#4,d4
	or.w	#$0b6a,d4
	swap	d7
	move.w	d4,d7
	swap	d7
	move.w	d0,d6
	addq.w	#1,d6
	lsl.w	#6,d6
	addq.w	#2,d6
	sub.w	d0,d2
	or.w	#2,d7
	bge.s	.wblit
	ori.b	#$40,d7
.wblit:	btst	#14,$dff002
	bne.s	.wblit
	move.w	d1,$dff062	;d1=2*sdelta do bltbmod
	move.w	d2,d1		;d2=2*sdelta do d1
	sub.w	d0,d1		;d1-d0=2*sdelta-2*ldelta
	move.w	d1,$dff064	;d1=2*sdelta-2*ldelta=bltamod
	move.l	d7,$dff040
	move.l	a2,$dff048
	move.l	a2,$dff054
	move.w	d2,$dff052
	bchg	d5,(a2)
	move.w	d6,$dff058
	rts

modula:	dc.w	0
	
;* object definiton
;***********************************************
points_3d:
	dc.l	0,0,4000
	dc.l	-4000,-1500,2000
	dc.l	-4000,1500,2000
	dc.l	-1500,4000,2000
	dc.l	1500,4000,2000
	dc.l	4000,1500,2000
	dc.l	4000,-1500,2000
	dc.l	1500,-4000,2000
	dc.l	-1500,-4000,2000

	dc.l	0,0,-4000
	dc.l	-4000,-1500,-2000
	dc.l	-4000,1500,-2000
	dc.l	-1500,4000,-2000
	dc.l	1500,4000,-2000
	dc.l	4000,1500,-2000
	dc.l	4000,-1500,-2000
	dc.l	1500,-4000,-2000
	dc.l	-1500,-4000,-2000

	dc.l	-4000,0,0
	dc.l	-2750,2750,0
	dc.l	0,4000,0
	dc.l	2750,2750,0
	dc.l	4000,0,0
	dc.l	2750,-2750,0
	dc.l	0,-4000,0
	dc.l	-2750,-2750,0
	
	dc.l	$7fff

egdes_3d:
	dc.w	0,2*4,0,1*4
	dc.w	40,3*4,0,2*4
	dc.w	0,4*4,0,3*4
	dc.w	40,5*4,0,4*4
	dc.w	0,6*4,0,5*4
	dc.w	40,7*4,0,6*4
	dc.w	0,8*4,0,7*4
	dc.w	40,1*4,0,8*4

	dc.w	0,11*4,10*4,9*4
	dc.w	40,12*4,11*4,9*4
	dc.w	0,13*4,12*4,9*4
	dc.w	40,14*4,13*4,9*4
	dc.w	0,15*4,14*4,9*4
	dc.w	40,16*4,15*4,9*4
	dc.w	0,17*4,16*4,9*4
	dc.w	40,10*4,17*4,9*4

	dc.w	40,2*4,1*4,18*4
	dc.w	40,11*4,18*4,10*4
	dc.w	0,1*4,10*4,18*4
	dc.w	0,2*4,18*4,11*4

	dc.w	0,3*4,2*4,19*4
	dc.w	0,12*4,19*4,11*4
	dc.w	40,2*4,11*4,19*4
	dc.w	40,3*4,19*4,12*4

	dc.w	40,4*4,3*4,20*4
	dc.w	40,13*4,20*4,12*4
	dc.w	0,3*4,12*4,20*4
	dc.w	0,4*4,20*4,13*4

	dc.w	0,5*4,4*4,21*4
	dc.w	0,14*4,21*4,13*4
	dc.w	40,4*4,13*4,21*4
	dc.w	40,5*4,21*4,14*4
	
	dc.w	40,6*4,5*4,22*4
	dc.w	40,15*4,22*4,14*4
	dc.w	0,5*4,14*4,22*4
	dc.w	0,6*4,22*4,15*4

	dc.w	0,7*4,6*4,23*4
	dc.w	0,16*4,23*4,15*4
	dc.w	40,6*4,15*4,23*4
	dc.w	40,7*4,23*4,16*4

	dc.w	40,8*4,7*4,24*4
	dc.w	40,17*4,24*4,16*4
	dc.w	0,7*4,16*4,24*4
	dc.w	0,8*4,24*4,17*4

	dc.w	0,1*4,8*4,25*4
	dc.w	0,10*4,25*4,17*4
	dc.w	40,8*4,17*4,25*4
	dc.w	40,1*4,25*4,10*4
	
	dc.w	$ffff

points_2d:	 blk.l	100,0

screen1:	 blk.b	3*planesize,0
screen2:	 blk.b	3*planesize,0
screen3:	 blk.b	3*planesize,0
