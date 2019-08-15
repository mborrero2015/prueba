************************************************************************
*Procedimiento que permite HACER CIERRE DE PERIODO.

*PROCEDURE CERRARC
******VARIABLES SIMULADAS /// ELIMINAR
*tipoconta=1  /// POR FINCA
*******

PRIVATE a,b,m,pant,cactual,auxcod,prov,auxnum1,qreg,qrfin,lini,lifin

*!*	var1 = MESSAGEBOX("Este procedimiento es de cuidado, realiza "+CHR(13)+;
*!*	    "el CIERRE DEFINITIVO del periodo en curso."+CHR(13)+;
*!*	    "La fecha de cierre, es la que aparece hoy"+CHR(13)+;
*!*	    "en su sistema."+CHR(13)+;
*!*	    "   "+CHR(13)+;
*!*	    "Si no desea realizar el cierre, consulte"+CHR(13)+;
*!*	    "su Balance o Resultado preliminar, con la"+CHR(13)+;
*!*	    "opcion del -BALANCE DE PRUEBA.- ",1,"       >>>>>  IMPORTANTE  <<<<< ")

*!*	IF var1=2
*!*	    RETURN
*!*	ENDIF


IF !FILE("CPD.DBF")
    MESSAGEBOX ("IMPOSIBLE HACER CIERRE "+CHR (13);
        +"PERIODO SIN MOVIMIENTOS,"+CHR (13)+CHR (13);
        + " >>> ¡ NO HAY CIERRE ! <<<",64," No Se Puede")

*!*	    FOR cer = 5 TO 250
*!*	        lsd = "Use in " + ALLTRIM(STR(cer))
*!*	        &lsd
*!*	    ENDFOR

    RETURN
ENDIF

********  SE ACTUALIZA PUC1 CON LA INFORMACION DEL CENTRO DE *******
********  COSTO SELECCIONADO
*g_numcontrol = -2
DO actpuc1
*******************

SELECT 109
USE CPD EXCLUSIVE
CURSORSETPROP("Buffering", 5)
SET ORDER TO 3
GO TOP

SELECT 104
USE CPG EXCLUSIVE
CURSORSETPROP("Buffering", 5)
SET ORDER TO 3
GO TOP


SELECT 114
USE PUC1
GO TOP


&&//&&// CALCULANDO SALDO FINAL SIN CIERRES &&//&&//&&//&&//
REPLACE PUC1.SALDOTR1 WITH PUC1.SALDOINIC+PUC1.MDEBITO-PUC1.MCREDITO ALL
SUM PUC1.SALDOTR1 TO cero

IF cero <> 0
    MESSAGEBOX("IMPOSIBLE CIERRE "+ CHR(13)+"BALANCE DESCUADRADO";
        + CHR(13)+CHR(13)+">>> ¡ NO HAY CIERRE ! <<<",16,"No Se Puede")

    FOR cer = 5 TO 250
        lsd = "Use in " + ALLTRIM(STR(cer))
        &lsd
    ENDFOR

    RETURN
ELSE
    SELECT 104
    GO BOTTOM
    auxcod = CPG.CODcp
    auxcod = auxcod + 1
    APPEND BLANK
    REPLACE CPG.CODcp WITH auxcod

    ****** COMPROBANTE DE CIERRE == -99
    REPLACE CPG.FUENTE WITH -99
    ***************************************************************************

    REPLACE CPG.fecha WITH DATE()

    SELECT 109
    GO BOTTOM
    aux = CPD.CODCOMP
ENDIF


SELECT 114
***************************************************************************
**************** CALCULANDO SALDO FINAL CON CIERRES ***********************
***************************************************************************

SET ORDER TO 1 DESCENDING

GO TOP

REPLACE PUC1.CDB WITH 0 ALL
REPLACE PUC1.CCR WITH 0 ALL



cupasos = 0
DO WHILE .T.
    GO TOP
    REPLACE PUC1.SALDOTR2 WITH PUC1.SALDOTR1+PUC1.CDB-PUC1.CCR ALL
    COUNT FOR PUC1.CTACIERRE<>SPAC(10) .AND. PUC1.SALDOTR2<>0 TO cuantas
    GO TOP

    IF cuantas>0 .AND. cupasos < RECCOUNT()
        DO WHILE !EOF()
*!*	            WAIT WINDOWS AT 15,30 "Cerrando Cuenta No...."+ PUC1.CODCONTABL;
*!*	                TIMEOUT .5

            IF PUC1.CTACIERRE <> SPAC(10)
                nureg = RECNO()
                cuentac = PUC1.CODCONTABL
                mrec = 0
                SKIP
                DO WHILE !EOF()
                    REPLACE PUC1.SALDOTR2 WITH PUC1.SALDOTR1+PUC1.CDB-PUC1.CCR
                    IF PUC1.CTACIERRE=cuentac .AND. PUC1.SALDOTR2<>0
                        mrec = mrec+PUC1.SALDOTR2

                        IF PUC1.SALDOTR2>0
                            REPLACE PUC1.CCR WITH PUC1.CCR+PUC1.SALDOTR2

                            DO MVTO WITH 0,PUC1.SALDOTR2,PUC1.CODCONTABL,"A CTA. # "+ cuentac

                            DO MVTO WITH PUC1.SALDOTR2,0,cuentac,"DE CTA. # "+PUC1.CODCONTABL

                        ELSE
                            REPLACE PUC1.CDB WITH PUC1.CDB+ABS(PUC1.SALDOTR2)

                            DO MVTO WITH ABS(PUC1.SALDOTR2),0,PUC1.CODCONTABL,"A CTA. # " + cuentac

                            DO MVTO WITH 0,ABS(PUC1.SALDOTR2),cuentac,"DE CTA. # "+ PUC1.CODCONTABL

                        ENDIF
                    ENDIF
                    SKIP
                ENDDO
                GO nureg
                IF mrec > 0
                    REPLACE PUC1.CDB WITH PUC1.CDB+mrec
                ELSE
                    REPLACE PUC1.CCR WITH PUC1.CCR+ABS(mrec)
                ENDIF

                REPLACE PUC1.SALDOTR2 WITH PUC1.SALDOTR1+PUC1.CDB - PUC1.CCR

                IF PUC1.SALDOTR2 > 0
                    REPLACE PUC1.CCR  WITH PUC1.CCR+PUC1.SALDOTR2

                    DO MVTO WITH 0,PUC1.SALDOTR2,PUC1.CODCONTABL,"A CTA. # " + PUC1.CTACIERRE

                ELSE
                    IF PUC1.SALDOTR2 < 0
                        REPLACE PUC1.CDB WITH PUC1.CDB+ABS(PUC1.SALDOTR2)

                        DO MVTO WITH ABS(PUC1.SALDOTR2),0,PUC1.CODCONTABL,"A CTA. # " + PUC1.CTACIERRE
                    ENDIF

                ENDIF
                mmcta = PUC1.CODCONTABL
                mrec  = PUC1.SALDOTR2

                SEEK PUC1.CTACIERRE
                IF FOUND()
                    IF mrec>0
                        REPLACE PUC1.CDB WITH PUC1.CDB+mrec

                        DO MVTO WITH mrec,0,PUC1.CODCONTABL,"DE CTA. # " + mmcta

                    ELSE
                        IF mrec<0
                            REPLACE PUC1.CCR WITH PUC1.CCR+ABS(mrec)

                            DO MVTO WITH 0,ABS(mrec),PUC1.CODCONTABL,"DE CTA. # " + mmcta
                        ENDIF

                    ENDIF
                ENDIF
                GO nureg
            ENDIF
            SKIP
        ENDDO
        cupasos = cupasos + 1
    ELSE
        EXIT
    ENDIF

ENDDO

IF cuantas > 0
    MESSAGEBOX(" **CUENTAS CICLICAS** - No hay Cierre. ",0," *** Imposible Cierre ***")

    SELECT 109
    CLOSE INDEX
    DELETE FOR CPD.CODcp = auxcod
    PACK

    SELECT 104
    DELETE
    PACK

    FOR cer = 5 TO 250
        lsd = "Use in " + ALLTRIM(STR(cer))
        &lsd
    ENDFOR

    RETURN

ENDIF

****************************************************************************

USE IN 114

SELECT 109
SET FILTER TO CPD.CODcp = auxcod
GO TOP
SUM CPD.DB,CPD.CR TO sdbs,scrs

SET FILTER TO
GO TOP

SELECT 104
REPLACE CPG.dbs WITH sdbs
REPLACE CPG.crs WITH scrs
REPLACE CPG.DFs WITH ABS(sdbs-scrs)
REPLACE CPG.codcc WITH "1"
REPLACE CPG.numcontrol WITH -1
REPLACE CPG.fecha WITH DATE()


SELECT 113
USE ciclos
CURSORSETPROP("Buffering", 3)

SELECT 114
USE PUC1
GO TOP

REPLACE PUC1.SALDO5 WITH PUC1.SALDO4 ALL
REPLACE PUC1.SALDO4 WITH PUC1.SALDO3 ALL
REPLACE PUC1.SALDO3 WITH PUC1.SALDO2 ALL
REPLACE PUC1.SALDO2 WITH PUC1.SALDO1 ALL
REPLACE PUC1.SALDO1 WITH PUC1.SALDOINIC ALL
REPLACE PUC1.SALDOINIC WITH PUC1.SALDOTR2 ALL

REPLACE PUC1.PYG5 WITH PUC1.PYG4 ALL
REPLACE PUC1.PYG4 WITH PUC1.PYG3 ALL
REPLACE PUC1.PYG3 WITH PUC1.PYG2 ALL
REPLACE PUC1.PYG2 WITH PUC1.PYG1 ALL
REPLACE PUC1.PYG1 WITH PUC1.SALDOTR1 FOR LEFT(PUC1.CODCONTABL,1)>"3" .AND. LEFT(PUC1.CODCONTABL,1)<"8"

REPLACE PUC1.MDEBITO WITH 0 ALL
REPLACE PUC1.MCREDITO WITH 0 ALL

SELECT 113

REPLACE ciclos.FECHA5 WITH ciclos.FECHA4
REPLACE ciclos.FECHA4 WITH ciclos.FECHA3
REPLACE ciclos.FECHA3 WITH ciclos.FECHA2
REPLACE ciclos.FECHA2 WITH ciclos.FECHA1
REPLACE ciclos.FECHA1 WITH ciclos.FECHAI
REPLACE ciclos.FECHAI WITH DATE() + 1

REPLACE ciclos.CIERR WITH ciclos.CIERR+1
qcierr = ALLTRIM(STR(ciclos.CIERR))


*****  borrando y haciendo copia de comprobantes por cierre ****

SELECT 104
**************

DO WHILE !EOF()
    SELECT 109
    REPLACE CPG.cierre WITH .T. FOR CPD.CODcp = CPG.CODcp
    SELECT 104
    SKIP
ENDDO


SET DELETE OFF


SELECT 104
SET FILTER TO
GO TOP

******
*!*	REPLACE CPD.cierre WITH .T. ALL



*!*	qcierr="000000"+qcierr
*!*	nnvo="D"+RIGHT(qcierr,7)+".DBF"
*!*	SELECT 109
*!*	COPY TO &nnvo FOR DELETE()
*!*	PACK

*!*	nnvo="G"+RIGHT(qcierr,7)+".DBF"
*!*	SELECT 104
*!*	COPY TO &nnvo FOR DELETE()
*!*	PACK

******************

*!*	FOR cer = 5 TO 250
*!*	    lsd = "Use in " + ALLTRIM(STR(cer))
*!*	    &lsd
*!*	ENDFOR


SET DELETE ON

RETURN
*******************************




****************************************
PROCEDURE MVTO

    PARAMETERS db1,cr1,cta,cie
    aux=aux + 1
    SELECT 109
    APPEND BLANK
    REPLACE CPD.CODCOMP WITH aux
    REPLACE CPD.CODcp WITH auxcod
    REPLACE CPD.CODCONTABL WITH cta
    REPLACE CPD.DETALLE WITH "CIERRE DE PERIODO " + cie
    REPLACE CPD.DB WITH db1
    REPLACE CPD.CR WITH cr1
    REPLACE CPD.codcc WITH "1"
    REPLACE CPD.fechacp WITH DATE()

    SELECT 114

    RETURN
