SECTION "vblank_ivector", ROM0[$0040]
	inc a
	reti
	ds $0048 - @, 0

SECTION "stat_ivector", ROM0[$0048]
	inc b
	reti
	ds $0050 - @, 0

SECTION "timer_ivector", ROM0[$0050]
	inc c
	reti
	ds $0058 - @, 0

SECTION "serial_ivector", ROM0[$0058]
	inc d
	reti
	ds $0060 - @, 0

SECTION "joypad_ivector", ROM0[$0060]
	inc e
	reti
	ds $0100 - @, 0

SECTION "Header", ROM0[$0100]
	jp start
	reti
	ds $0150 - @, 0 ; Make room for the header

start:
	; Write a 0 into LCDC.7, so we have full access to VRAM and OAM
	ld hl, $FF40
	ld a, [hl]
	ld b, $7f
	and a, b
	ld [hl], a

	ld hl, $9800
	ld a, $0
	ld de, $0002
	ld c, $0
tile_map_write_top:
	ld [hl], a
	add hl, de
	inc c
	jr nz, tile_map_write_top

write_tile:
	; Load a smiley tile into vram
	ld hl, $8000

	ld a, $00
	ld [hl], a
	inc hl
	ld a, $00
	ld [hl], a
	inc hl
	ld a, $24
	ld [hl], a
	inc hl
	ld a, $24
	ld [hl], a
	inc hl
	ld a, $24
	ld [hl], a
	inc hl
	ld a, $24
	ld [hl], a
	inc hl
	ld a, $00
	ld [hl], a
	inc hl
	ld a, $00
	ld [hl], a
	inc hl
	ld a, $42
	ld [hl], a
	inc hl
	ld a, $42
	ld [hl], a
	inc hl
	ld a, $3C
	ld [hl], a
	inc hl
	ld a, $3C
	ld [hl], a
	inc hl
	ld a, $00
	ld [hl], a
	inc hl
	ld a, $08
	ld [hl], a
	inc hl
	ld a, $00
	ld [hl], a
	inc hl
	ld a, $00
	ld [hl], a
	inc hl

	; Write a 1 into LCDC.7, so everything will draw
	ld hl, $FF40
	ld a, [hl]
	ld b, $80
	or a, b
	ld [hl], a

	; enable the timer (TAC.2)
	ld hl, $FF07
	ld a, [hl]
	or a, $4
	ld [hl], a

	; enable the timer interrupt (IE.2)
	; and disable all others
	ld hl, $FFFF
	ld[hl], $4

	; enable interrupts
	ei

spin:
	; ld hl, $FF42 ; SCY
	; inc [hl]
	; ld hl, $FF43 ; SCX
	; inc [hl]
	jp spin
