***************************************************************************
***Procedimiento que permite MOSTRAR EL ESTADO DE PERDIDAS Y GANANCIAS*****
**************  **************  **************  **************  ***********
**************  ******  INVENTARIOS PERMANENTES **************  ***********
**************  **************  **************  **************  ***********
PARAMETERS nivel

FOR cer = 5 TO 250
    lsd = "Use in " + ALLTRIM(STR(cer))
    &lsd
ENDFOR
SET NEAR ON
SET SAFETY OFF

******  VARIABLES SIMULADAS &&/// ELIMINAR
*tipoconta=1  &&/// POR FINCA
*******

PRIVATE a,b,m,pant,cactual,auxcod,prov,auxnum1,qreg,qrfin,lini,lifin,FINCA

SELECT 18
USE pgi1 EXCLUSIVE
ZAP

SELECT 18
USE IPROV
INDEX ON CODPROV TO ip2

SELECT 58
USE puc
&& // INDEX ON CLASE+GRP+CUENTA+SUBCUENTA TO PPUPC
INDEX ON Codigo TO PPUPC

SELECT 113
USE CICLOS
fech1=CICLOS.fecha1
fechi=CICLOS.fechai

SELECT 115
USE PYG EXCLUSIVE
ZAP
**************  **************  **************  **************  ********
***********  ************** PASANDO A PYG.DBF **************  **************
SELECT 120
USE PG  &&///  ESTRUCTURA GENERAL DEL PYG &&/// INVENTARIOS PERMANENTES

DO WHILE .NOT. EOF()
    SELECT 115
    APPEND BLANK
    REPLACE PYG.NOMBRECTA WITH PG.ITEM
    REPLACE PG.R WITH RECNO()
    traer=ALLTRIM(PG.codcontabl)
    IF VAL(traer)>0
        traer=ALLTRIM(PG.codcontabl)
        APPEND FROM PUC1 FOR LEFT(codcontabl,LEN(traer))=traer;
            .AND. PYG1<>0
    ELSE
        IF ALLTRIM(PG.codcontabl)="="
        	go top
            SUM PYG.PYG1 TO XXX
            GO BOTTOM
            REPLACE PYG.TOTAL WITH (XXX * -1)
        ENDIF
    ENDIF
    SELECT 120
    SKIP
ENDDO

SELECT 115
REPLACE PYG.PYG1  WITH PYG.PYG1 * -1 ALL
GO TOP

SELECT 120
GO TOP
DO WHILE .NOT. EOF()
    SELECT 115
    IF VAL(PG.codcontabl)>0
    	  	
        SUM PYG.PYG1 TO XXX FOR LEFT(PYG.codcontabl,LEN(ALLTRIM(PG.codcontabl)))=ALLTRIM(PG.codcontabl)
        GO PG.R
        REPLACE PYG.TOTAL WITH XXX
    ENDIF
    SELECT 120
    SKIP
ENDDO

SELECT 115
SET RELATION TO CODPROV INTO 18

MESSAGEBOX("Este P&G corresponde al ultimo cierre hecho",0,"   *** IMPORTANTE ***  ")

*SELECT 114
*USE PUC1
*INDEX ON puc1.CODCONTABL TO CC


SELECT 115

IF nivel=1
    SET FILTER TO PYG.codcontabl=SPAC(10)
    GO TOP
ELSE
    SET FILTER TO
    GO TOP
ENDIF


DO IMPRE4

FOR E = 5 TO 150

    USE IN E

ENDFOR


RETURN

*************  *************  *************  *************  *************






*************  *************  *************  *************  *************
PROCEDURE IMPRE4

    GO TOP

    claimp   = SPAC(1)
    grpimp   = SPAC(2)
    ctaimp   = SPAC(4)
    sbctaimp = SPAC(6)

    DO WHILE .NOT. EOF()

        SELECT 115

        A1 = PYG.codcontabl
        A2 = LEFT(SPAC(LEN(ALLTRIM(PYG.codcontabl)))+LEFT(PYG.NOMBRECTA,60),60)

        C1 = PYG.PYG1
        C2 = PYG.TOTAL

        INSERT INTO pgi1.DBF (AA1, AA2, CC1, CC2);
            VALUES (A1, A2, C1, C2)

        SKIP
    ENDDO

    IF RECCOUNT("pgi1") > 2

        DO FORM imprimir WITH "pg.frx",""

    ENDIF
    RETURN