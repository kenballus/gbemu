SECTION "Header", ROM0[$100]
	jp start
	ds $150 - @, 0 ; Make room for the header

start:
	; Write a 0 into LCDC.7, so we have full access to VRAM and OAM
	ld hl, $FF40
	ld a, [hl]
	ld b, $7f
	and a, b
	ld [hl], a

	; Write a 1 into the first place in the tilemap
	ld hl, $9800
	ld [hl], 1

	; Put a tile into character ram
	ld hl, $8010
	ld [hl], $00
	inc hl
	ld [hl], $01
	inc hl
	ld [hl], $02
	inc hl
	ld [hl], $03
	inc hl
	ld [hl], $04
	inc hl
	ld [hl], $05
	inc hl
	ld [hl], $06
	inc hl
	ld [hl], $07
	inc hl
	ld [hl], $08
	inc hl
	ld [hl], $09
	inc hl
	ld [hl], $0a
	inc hl
	ld [hl], $0b
	inc hl
	ld [hl], $0c
	inc hl
	ld [hl], $0d
	inc hl
	ld [hl], $0e
	inc hl
	ld [hl], $0f

	; Write a 1 into LCDC.7, so everything will draw
	ld hl, $FF40
	ld a, [hl]
	ld b, $80
	or a, b
	ld [hl], a

loop:
	call doodad
	jp loop

doodad:
	inc a
	ret