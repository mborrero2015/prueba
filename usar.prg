PARAMETERS tabla,exclusivo
IF !USED(tabla)
	IF exclusivo =.T.
		select 0
		USE &tabla EXCLU
	ELSE
		USE &tabla
	ENDIF
ELSE
	IF exclusivo =.T.
		USE IN &TABLA
		select 0
		USE &tabla EXCLU
	ELSE
		SELECT &tabla
	ENDIF
ENDIF
