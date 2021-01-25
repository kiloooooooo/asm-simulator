; フィボナッチ数列
	MVI	D,0	; a(n-2)
	MVI	E,1	; a(n-1)
	CALL	lp

lp:	MOV	A,E	; A <- a(n-1)
	ADD	D	; A <- a(n-1) + a(n-2) = a(n)
	MOV	D,E
	MOV	E,A
	CPI	233
	JM	lp
	HLT

END
