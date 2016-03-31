;
; MD201603
;


; Code by T.M.R/Cosine
; Graphics by aNdy/Cosine
; Charset by T.M.R/Cosine
; Music by aNdy/Cosine


; This source code is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with PuCrunch which can be downloaded at
; http://csdb.dk/release/?id=6089

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename
		!to "md201603.prg",cbm


; Yank in binary data
		* = $1000
music		!binary "data/lost_in_time.prg",,2

		* = $3800
dypp_chars_1	!binary "data/6px_mcol.chr"

		* = $5800
		!binary "data/sprites.spr"

		* = $6000
		!binary "data/cosine_logo.kla",,2


; Constants
rstr1p		= $00
rstr2p		= $a1
rstr3p		= $d1

dypp_width	= $27


; Label assignments
rn		= $20
sync		= $21

cos_at_1	= $22
cos_at_2	= $23
scroll_x	= $24
dypp_buffer	= $28		; $28 bytes used
dypp_y_pos	= $50		; $a0 bytes used

cos_at_3	= $f0
cos_at_4	= $f1
cos_at_5	= $f2
cos_at_6	= $f3
cos_speed_3	= $f4
cos_speed_4	= $f5
cos_speed_5	= $f6
cos_speed_6	= $f7
anim_tmr	= $f8





dypp_chars_2	= dypp_chars_1+$200
dypp_chars_3	= dypp_chars_1+$400

dypp_workspace	= $5008


; Entry point at $8800
		* = $8800
entry		sei

		lda #$35
		sta $01

		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #rstr1p
		sta $d012

		lda #$0b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Clear the zero page
		ldx #$20
		lda #$00
nuke_zp		sta $00,x
		inx
		bne nuke_zp


; Set up the Koala image's colours
		ldx #$00
koala_init	lda $7f40,x
		sta $5c00,x
		lda $8040,x
		sta $5d00,x
		lda $8140,x
		sta $5e00,x
		lda $8228,x
		sta $5ee8,x

		lda $8328,x
		sta $d800,x
		lda $8428,x
		sta $d900,x
		lda $8528,x
		sta $da00,x
		lda $8710,x
		sta $dae8,x

		inx
		bne koala_init

; Generate the DYPP work area on screen
		ldx #$00
		lda #$01
		clc
dypp_scrn_gen	sta $5e30,x
		adc #$01
		sta $5e58,x
		adc #$01
		sta $5e80,x
		adc #$01
		sta $5ea8,x
		adc #$01
		sta $5ed0,x
		adc #$01
		sta $5ef8,x
		adc #$01
		inx
		cpx #$27
		bne dypp_scrn_gen

		ldx #$00
		lda #$0a
dypp_col_gen	sta $da30,x
		inx
		cpx #$f0
		bne dypp_col_gen

		ldx #$00
		txa
dypp_mask_gen	sta $5308,x
		sta $da08,x
		sta $5f20,x
		sta $db20,x
		inx
		cpx #$28
		bne dypp_mask_gen

; Generate the DYPP fonts
		ldx #$00
dypp_char_gen	ldy dypp_chars_1+$000,x
		tya
		and #%00110000
		sta dypp_chars_2+$000,x
		tya
		and #%00001100
		sta dypp_chars_3+$000,x
		tya
		and #%11000000
		sta dypp_chars_1+$000,x

		ldy dypp_chars_1+$100,x
		tya
		and #%00110000
		sta dypp_chars_2+$100,x
		tya
		and #%00001100
		sta dypp_chars_3+$100,x
		tya
		and #%11000000
		sta dypp_chars_1+$100,x
		inx
		bne dypp_char_gen

; Clear the text buffer
		ldx #$00
		txa
dypp_buffer_clr	sta dypp_buffer,x
		inx
		cpx #$28
		bne dypp_buffer_clr

; Initialise some of the labels
		lda #$01
		sta rn

		lda #$03
		sta cos_speed_3
		lda #$02
		sta cos_speed_4

		lda #$ff
		sta cos_speed_5
		lda #$04
		sta cos_speed_6

; Reset the scroller
		jsr reset

; Initialise the music
		lda #$00
		jsr music+$00

		cli


; Main loop
main_loop	lda #$00
		sta sync
sw_loop		cmp sync
		beq sw_loop

; DYPP clear
!set char_cnt=$00
!set cos_cnt=$00

		lda #$00
!do {
		ldy dypp_y_pos+cos_cnt
		sta dypp_workspace+$00+(char_cnt*$30),y
		sta dypp_workspace+$01+(char_cnt*$30),y
;		sta dypp_workspace+$02+(char_cnt*$30),y
;		sta dypp_workspace+$03+(char_cnt*$30),y
		sta dypp_workspace+$04+(char_cnt*$30),y
		sta dypp_workspace+$05+(char_cnt*$30),y
		!set cos_cnt=cos_cnt+$01

		ldy dypp_y_pos+cos_cnt
		sta dypp_workspace+$00+(char_cnt*$30),y
		sta dypp_workspace+$01+(char_cnt*$30),y
		sta dypp_workspace+$02+(char_cnt*$30),y
		sta dypp_workspace+$03+(char_cnt*$30),y
		sta dypp_workspace+$04+(char_cnt*$30),y
		sta dypp_workspace+$05+(char_cnt*$30),y
		!set cos_cnt=cos_cnt+$01

		ldy dypp_y_pos+cos_cnt
		sta dypp_workspace+$00+(char_cnt*$30),y
		sta dypp_workspace+$01+(char_cnt*$30),y
;		sta dypp_workspace+$02+(char_cnt*$30),y
;		sta dypp_workspace+$03+(char_cnt*$30),y
		sta dypp_workspace+$04+(char_cnt*$30),y
		sta dypp_workspace+$05+(char_cnt*$30),y
		!set cos_cnt=cos_cnt+$02

		!set char_cnt=char_cnt+$01
} until char_cnt=dypp_width

; Move the scroller if needed
		ldx scroll_x
		inx
		cpx #$08
		beq *+$05
		jmp scr_xb

		lda cos_at_1
		sec
		sbc #$08
		sta cos_at_1

		lda cos_at_2
		clc
		adc #$0c
		sta cos_at_2

!set char_cnt=$00
!do {
		lda dypp_buffer+$01+char_cnt
		sta dypp_buffer+$00+char_cnt

		!set char_cnt=char_cnt+$01
} until char_cnt=$26

mread		lda scroll_text
		bne okay
		jsr reset
		jmp mread

okay		cmp #$20
		bne *+$04
		lda #$00
		sta dypp_buffer+$26

		inc mread+$01
		bne *+$05
		inc mread+$02

		ldx #$00
scr_xb		stx scroll_x


; DYPP update
		ldx cos_at_1
		inx
		inx
		stx cos_at_1

		lda cos_at_2
		clc
		adc #$03
		sta cos_at_2
		tay

		lda dypp_cosinus,x
		clc
		adc dypp_cosinus,y
		sta dypp_y_pos

!set curve_cnt=$01
!set curve_skip_cnt=$01
!do {
		dex
		dex
		iny
		iny
		iny

!if curve_skip_cnt<$03 {
		lda dypp_cosinus,x
		clc
		adc dypp_cosinus,y
		sta dypp_y_pos+curve_cnt
}

		!set curve_skip_cnt=curve_skip_cnt+$01
		!if curve_skip_cnt>$03 {
			!set curve_skip_cnt=$00
		}

		!set curve_cnt=curve_cnt+$01
} until curve_cnt=(dypp_width*$04)

; DYPP render
!set char_cnt=$00
!set cos_cnt=$00

!do {
		ldx dypp_buffer+char_cnt
		bne *+$05
		jmp *+$99
		ldy dypp_y_pos+cos_cnt
		lda dypp_chars_1+$000,x
		sta dypp_workspace+$00+(char_cnt*$30),y
		lda dypp_chars_1+$040,x
		sta dypp_workspace+$01+(char_cnt*$30),y
		lda dypp_chars_1+$080,x
		sta dypp_workspace+$02+(char_cnt*$30),y
		lda dypp_chars_1+$0c0,x
		sta dypp_workspace+$03+(char_cnt*$30),y
		lda dypp_chars_1+$100,x
		sta dypp_workspace+$04+(char_cnt*$30),y
		lda dypp_chars_1+$140,x
		sta dypp_workspace+$05+(char_cnt*$30),y
		!set cos_cnt=cos_cnt+$01

		ldy dypp_y_pos+cos_cnt
		lda dypp_chars_2+$000,x
		ora dypp_workspace+$00+(char_cnt*$30),y
		sta dypp_workspace+$00+(char_cnt*$30),y
		lda dypp_chars_2+$040,x
		ora dypp_workspace+$01+(char_cnt*$30),y
		sta dypp_workspace+$01+(char_cnt*$30),y
		lda dypp_chars_2+$080,x
		ora dypp_workspace+$02+(char_cnt*$30),y
		sta dypp_workspace+$02+(char_cnt*$30),y
		lda dypp_chars_2+$0c0,x
		ora dypp_workspace+$03+(char_cnt*$30),y
		sta dypp_workspace+$03+(char_cnt*$30),y
		lda dypp_chars_2+$100,x
		ora dypp_workspace+$04+(char_cnt*$30),y
		sta dypp_workspace+$04+(char_cnt*$30),y
		lda dypp_chars_2+$140,x
		ora dypp_workspace+$05+(char_cnt*$30),y
		sta dypp_workspace+$05+(char_cnt*$30),y
		!set cos_cnt=cos_cnt+$01

		ldy dypp_y_pos+cos_cnt
		lda dypp_chars_3+$000,x
		ora dypp_workspace+$00+(char_cnt*$30),y
		sta dypp_workspace+$00+(char_cnt*$30),y
		lda dypp_chars_3+$040,x
		ora dypp_workspace+$01+(char_cnt*$30),y
		sta dypp_workspace+$01+(char_cnt*$30),y
		lda dypp_chars_3+$080,x
		ora dypp_workspace+$02+(char_cnt*$30),y
		sta dypp_workspace+$02+(char_cnt*$30),y
		lda dypp_chars_3+$0c0,x
		ora dypp_workspace+$03+(char_cnt*$30),y
		sta dypp_workspace+$03+(char_cnt*$30),y
		lda dypp_chars_3+$100,x
		ora dypp_workspace+$04+(char_cnt*$30),y
		sta dypp_workspace+$04+(char_cnt*$30),y
		lda dypp_chars_3+$140,x
		ora dypp_workspace+$05+(char_cnt*$30),y
		sta dypp_workspace+$05+(char_cnt*$30),y
		!set cos_cnt=cos_cnt+$02

		!set char_cnt=char_cnt+$01
} until char_cnt=dypp_width

		jmp main_loop


; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn
		cmp #$02
		bne *+$05
		jmp rout2

		cmp #$03
		bne *+$05
		jmp rout3

; Raster split 1
rout1		lda #$02
		sta rn
		lda #rstr2p
		sta $d012

		lda #$0c
		sta $d020
		lda #$00
		sta $d021
		lda #$0a
		sta $d022
		lda #$07
		sta $d023

		lda #$3b
		sta $d011
		lda #$17
		sta $d016
		lda #$78
		sta $d018

		lda #$c6
		sta $dd00

; Turn on and position the hardware sprites for the upper border
		lda #$ff
		sta $d015
		sta $d01c

		ldx #$00
		ldy #$00
set_sprites	lda sprite_x,x
		clc
		adc #$5c
		sta $d000,y
		lda sprite_y,x
		clc
		adc #$3c
		sta $d001,y
		lda sprite_cols,x
		sta $d027,x
		lda sprite_dps,x
		sta $5ff8,x
		iny
		iny
		inx
		cpx #$08
		bne set_sprites

		lda #$01
		sta $d025
		lda #$06
		sta $d026

		jmp ea31

; Raster split 3
rout2		lda #$1b
		sta $d011

		lda scroll_x
		eor #$17
		sta $d016

		lda #$74
		sta $d018

		lda #$03
		sta rn
		lda #rstr3p
		sta $d012

; Update the curve counters to move the sprites
		ldx #$00
cos_update	lda cos_at_3,x
		clc
		adc cos_speed_3,x
		sta cos_at_3,x
		inx
		cpx #$04
		bne cos_update

; Work out the sprite positions for this frame
		ldx #$00
		ldy cos_at_3
cos_x_gen_1	lda sprite_cosinus,y
		sta sprite_x,x
		tya
		clc
		adc #$20
		tay
		inx
		cpx #$08
		bne cos_x_gen_1

		ldx #$00
		ldy cos_at_4
cos_x_gen_2	lda sprite_cosinus,y
		clc
		adc sprite_x,x
		sta sprite_x,x
		tya
		clc
		adc #$20
		tay
		inx
		cpx #$08
		bne cos_x_gen_2

		ldx #$00
		ldy cos_at_5
cos_y_gen_1	lda sprite_cosinus,y
		sta sprite_y,x
		tya
		clc
		adc #$20
		tay
		inx
		cpx #$08
		bne cos_y_gen_1

		ldx #$00
		ldy cos_at_6
cos_y_gen_2	lda sprite_cosinus,y
		clc
		adc sprite_y,x
		sta sprite_y,x
		tya
		clc
		adc #$20
		tay
		inx
		cpx #$08
		bne cos_y_gen_2

; Update the sprite animations
		ldx anim_tmr
		inx
		cpx #$04
		bne at_xb

		ldx #$00
anim_update	lda sprite_dps,x
		clc
		adc #$01
		cmp #$68
		bne *+$04
		lda #$60
		sta sprite_dps,x
		inx
		cpx #$08
		bne anim_update

		ldx #$00
at_xb		stx anim_tmr

; Play the music
		jsr music+$03

		jmp ea31


; Raster split 3
rout3		lda #$7b
		sta $d011
		lda #$17
		sta $d016
		lda #$78
		sta $d018

		lda #$01
		sta rn
		sta sync
		lda #rstr1p
		sta $d012

		lda #$3b
		sta $d011

ea31		pla
		tay
		pla
		tax
		pla
nmi		rti


; Reset the scroller
reset		lda #<scroll_text
		sta mread+$01
		lda #>scroll_text
		sta mread+$02
		rts


; DYPP cosinus
		* = ((*/$100)+1)*$100

dypp_cosinus	!byte $15,$15,$15,$15,$15,$15,$15,$15
		!byte $15,$15,$15,$15,$15,$15,$15,$15
		!byte $15,$15,$14,$14,$14,$14,$14,$14
		!byte $14,$13,$13,$13,$13,$13,$13,$12
		!byte $12,$12,$12,$12,$11,$11,$11,$11
		!byte $11,$10,$10,$10,$10,$0f,$0f,$0f
		!byte $0f,$0e,$0e,$0e,$0e,$0d,$0d,$0d
		!byte $0d,$0c,$0c,$0c,$0c,$0b,$0b,$0b

		!byte $0a,$0a,$0a,$0a,$09,$09,$09,$09
		!byte $08,$08,$08,$08,$07,$07,$07,$07
		!byte $06,$06,$06,$06,$05,$05,$05,$05
		!byte $04,$04,$04,$04,$03,$03,$03,$03
		!byte $03,$03,$02,$02,$02,$02,$02,$01
		!byte $01,$01,$01,$01,$01,$01,$01,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$01,$01,$01,$01,$01,$01
		!byte $01,$02,$02,$02,$02,$02,$02,$03
		!byte $03,$03,$03,$03,$04,$04,$04,$04
		!byte $04,$05,$05,$05,$05,$06,$06,$06
		!byte $06,$07,$07,$07,$07,$08,$08,$08
		!byte $08,$09,$09,$09,$09,$0a,$0a,$0a

		!byte $0b,$0b,$0b,$0b,$0c,$0c,$0c,$0c
		!byte $0d,$0d,$0d,$0d,$0e,$0e,$0e,$0f
		!byte $0f,$0f,$0f,$10,$10,$10,$10,$10
		!byte $11,$11,$11,$11,$12,$12,$12,$12
		!byte $12,$13,$13,$13,$13,$13,$13,$14
		!byte $14,$14,$14,$14,$14,$14,$14,$15
		!byte $15,$15,$15,$15,$15,$15,$15,$15
		!byte $15,$15,$15,$15,$15,$15,$15,$15

; Sprite cosinus
sprite_cosinus	!byte $4f,$4f,$4f,$4f,$4f,$4f,$4f,$4f
		!byte $4f,$4f,$4e,$4e,$4e,$4d,$4d,$4d
		!byte $4c,$4c,$4c,$4b,$4b,$4a,$4a,$49
		!byte $49,$48,$48,$47,$46,$46,$45,$44
		!byte $44,$43,$42,$42,$41,$40,$3f,$3e
		!byte $3e,$3d,$3c,$3b,$3a,$39,$39,$38
		!byte $37,$36,$35,$34,$33,$32,$31,$30
		!byte $2f,$2e,$2d,$2c,$2b,$2a,$29,$28

		!byte $27,$26,$25,$24,$24,$23,$22,$21
		!byte $20,$1f,$1e,$1d,$1c,$1b,$1a,$19
		!byte $18,$17,$16,$15,$15,$14,$13,$12
		!byte $11,$10,$10,$0f,$0e,$0d,$0d,$0c
		!byte $0b,$0a,$0a,$09,$09,$08,$07,$07
		!byte $06,$06,$05,$05,$04,$04,$03,$03
		!byte $02,$02,$02,$01,$01,$01,$01,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$01,$01,$01,$01,$02,$02,$02
		!byte $03,$03,$03,$04,$04,$05,$05,$06
		!byte $06,$07,$07,$08,$09,$09,$0a,$0b
		!byte $0b,$0c,$0d,$0e,$0e,$0f,$10,$11
		!byte $11,$12,$13,$14,$15,$16,$17,$17
		!byte $18,$19,$1a,$1b,$1c,$1d,$1e,$1f
		!byte $20,$21,$22,$23,$24,$25,$26,$27

		!byte $28,$29,$2a,$2b,$2c,$2d,$2e,$2f
		!byte $30,$30,$31,$32,$33,$34,$35,$36
		!byte $37,$38,$39,$3a,$3b,$3b,$3c,$3d
		!byte $3e,$3f,$40,$40,$41,$42,$43,$43
		!byte $44,$45,$45,$46,$47,$47,$48,$48
		!byte $49,$49,$4a,$4a,$4b,$4b,$4c,$4c
		!byte $4d,$4d,$4d,$4e,$4e,$4e,$4e,$4f
		!byte $4f,$4f,$4f,$4f,$4f,$4f,$4f,$4f

; Sprite position tables
sprite_x	!byte $40,$48,$50,$58,$60,$68,$70,$78
sprite_y	!byte $40,$48,$50,$58,$60,$68,$70,$78
sprite_cols	!byte $0e,$05,$03,$0d,$03,$05,$0e,$04
sprite_dps	!byte $60,$61,$62,$63,$64,$65,$66,$67


scroll_text	!scr "yes girls and boys, it's that time once more so, with "
		!scr "all the usual ",$22,"fanfare",$22,", here comes..."
		!scr "           "
		!scr "-=-  md201603  -=-"
		!scr "           "

		!scr "with the coding and this character set from m'self (t.m.r), "
		!scr "accompanied by a bitmapped logo, sprite animation and "
		!scr "this fantastic cover of ",$22,"lost in time",$22," by andy, "
		!scr "who has recently plonked himself down on the cosine sofa - "
		!scr "welcome aboard, make yourself at home and don't go near the "
		!scr "rather dodgy-looking green stuff in the communal fridge!"
		!scr "        "

		!scr "i did try to talk him into writing some scrolltext after "
		!scr "signing his life away but no joy, so you're all stuck with "
		!scr "me waffling on yet again - sorry about that!"
		!scr "        "

		!scr "i've never previously tried writing a dypp but, since "
		!scr "md201602 was all about playing around with dycps, that "
		!scr "gave me a few ideas to experiment with..."
		!scr "    "

		!scr "i could have got some more optimisations in - just using one "
		!scr "curve like md201602 does would have helped for example - and "
		!scr "there are certainly better dypp scrollers out there for the "
		!scr "c64, but i'm still reasonably happy with the result..."
		!scr "        "

		!scr "and with that said, i shall as usual give up any pretence of "
		!scr "having something interesting to say (or even just something "
		!scr "to say for that matter) and get on with pushing out cosine's "
		!scr "greetings!"
		!scr "    "
		!scr "sort-of-alpha hellos wibble towards the fluffy bunnies in..."
		!scr "        "

		!scr "abyss connection  -  "
		!scr "arkanix labs  -  "
		!scr "artstate  -  "
		!scr "ate bit  -  "
		!scr "atlantis and f4cg  -  "
		!scr "booze design  -  "
		!scr "camelot  -  "
		!scr "chorus  -  "
		!scr "chrome  -  "
		!scr "cncd  -  "
		!scr "cpu  -  "
		!scr "crescent  -  "
		!scr "crest  -  "
		!scr "covert bitops  -  "
		!scr "defence force  -  "
		!scr "dekadence  -  "
		!scr "desire  -  "
		!scr "dac  -  "
		!scr "dmagic  -  "
		!scr "dualcrew  -  "
		!scr "exclusive on  -  "
		!scr "fairlight  -  "
		!scr "fire  -  "
		!scr "focus  -  "
		!scr "french touch  -  "
		!scr "funkscientist productions  -  "
		!scr "genesis project  -  "
		!scr "gheymaid inc.  -  "
		!scr "hitmen  -  "
		!scr "hokuto force  -  "
		!scr "level64  -  "
		!scr "maniacs of noise  -  "
		!scr "mayday  -  "
		!scr "meanteam  -  "
		!scr "metalvotze  -  "
		!scr "noname  -  "
		!scr "nostalgia  -  "
		!scr "nuance  -  "
		!scr "offence  -  "
		!scr "onslaught  -  "
		!scr "orb  -  "
		!scr "oxyron  -  "
		!scr "padua  -  "
		!scr "plush  -  "
		!scr "psytronik  -  "
		!scr "reptilia  -  "
		!scr "resource  -  "
		!scr "rgcd  -  "
		!scr "secure  -  "
		!scr "shape  -  "
		!scr "side b  -  "
		!scr "singular  -  "
		!scr "slash  -  "
		!scr "slipstream  -  "
		!scr "success and trc  -  "
		!scr "style  -  "
		!scr "suicyco industries  -  "
		!scr "taquart  -  "
		!scr "tempest  -  "
		!scr "tek  -  "
		!scr "triad  -  "
		!scr "trsi  -  "
		!scr "viruz  -  "
		!scr "vision  -  "
		!scr "wow  -  "
		!scr "wrath  -  "
		!scr "xenon  -  "
		!scr "and, as ever, the other lovely people who are greeting us "
		!scr "that we've either forgotten or aren't currently aware of "
		!scr "for some reason!"
		!scr "        "

		!scr "and now i'm sorted for yet another month, woohoo!"
		!scr "      "

		!scr "hopefully the next monthly demo will have a more interesting "
		!scr "text if all goes what i'll almost euphamistically call "
		!scr $22,"to plan",$22,", but in the meantime enjoy this great "
		!scr "music, don't forget to prod at the cosine website - "
		!scr "http://cosine.org.uk/ - or laugh at our rather sporadic social "
		!scr "media ",$22,"presence",$22," and this was t.m.r writing "
		!scr "gibberish on the last day of the month once more before "
		!scr "wandering off in a dramatic blaze of full stops"
		!scr ".... ... .. .  .   .    .     ."
		!scr "                        "

		!byte $00
