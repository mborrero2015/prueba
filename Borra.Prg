PARAMETERS a,b,c

respu = MESSAGEBOX("Desea Borrar "+CHR(13)+ ;
    "Esta Cuenta",68,"¡Borrar!")

IF respu = 6 then
    REPLACE &a WITH " "
    REPLACE &b WITH " "
    IF c != " " then
        REPLACE &c WITH " "
    ENDIF
ENDIF
