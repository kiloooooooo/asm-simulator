; Swap WW and XX
	LDA	WW	; Load WW into reg A
	MOV	D,A	; Move reg A to reg D
	LDA	XX	; Load XX into reg A
	MOV	H,A	; Move reg A to reg H
	XCHG		; Exchange reg D and reg H
	MOV	A,H	; Move reg H to reg A
	STA	XX	; Store reg A into XX
	MOV	A,D	; Move reg D to reg A
	STA	WW	; Store reg A into WW

; Swap ZZ and YY
	LDA	ZZ	; Load ZZ into reg A
	MOV	D,A	; Move reg A to reg D
	LDA	YY	; Load YY into reg A
	MOV	H,A	; Move reg A to reg H
	XCHG		; Exchange reg D and reg H
	MOV	A,H	; Move reg H to reg A
	STA	YY	; Store reg A into YY
	MOV	A,D	; Move reg D to reg A
	STA	ZZ	; Store reg A into ZZ

	HLT

; Labels
XX:	DB	1
YY:	DB	2
ZZ:	DB	3
WW:	DB	4
	END
