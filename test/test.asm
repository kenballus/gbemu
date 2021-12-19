SECTION "Header", ROM0[$100]

	jp start

	ds $150 - @, 0 ; Make room for the header

start:
	ld a, 16  ; 0b00010000
	scf
	sbc a, 15 ; 0b00001111
	halt