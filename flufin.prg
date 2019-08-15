****************************************************************************
*Programa que genera el flujo financiero y flujo de caja
****************************************************************************
PARAMETERS qsali

SET procedure TO libreria.prg

DECLARE pie[4]


SELECT Infculti.numcontrol, Infculti.codlote, Infculti.nomlote, ;
    Lotesfin.area AS area, Infculti.cultivo, Infculti.codcultivo, ;
    Ubica.nomfin, Infculti.ano, Infculti.semestre ;
    FROM Infculti, Lotesfin , Ubica WHERE (Infculti.codfin = g_codfin AND ;
    Infculti.ano = g_ano AND Infculti.semestre = g_semestre) AND ;
    Infculti.marcainf AND Lotesfin.codlote = Infculti.codlote AND ;
    Ubica.codfin = g_codfin INTO TABLE consoli

USE consoli
COUNT TO cuantos
GO TOP

IF cuantos <> 0

    finca = consoli.nomfin
    anno  = consoli.ano
    semestre = consoli.semestre
    cultivo  = "CULTIVO: "+Infculti.cultivo

    pie[1] = ""
    pie[2] = ""
    pie[3] = ""
    pie[4] = ""

    IF cuantos = 1
        lote = ALLTRIM(consoli.nomlote)+" ("+ALLTRIM(STR(consoli.area,10,2))+")"

        nconts1 = "numcontrol = " + ALLTRIM(STR(consoli.numcontrol))
        lotesp1 = "codlote = " + ALLTRIM(STR(consoli.codlote))
    ELSE
        lote = ">>> CONSOLIDADOS <<<"
        cultivo = ""

        nconts1 = ""
        lotesp1 = ""

        npie = 1
        DO WHILE !EOF()
            nconts1 = nconts1+"numcontrol = " + ALLTRIM(STR(consoli.numcontrol))+" OR "
            lotesp1 = lotesp1+"codlote = " + ALLTRIM(STR(consoli.codlote))+" OR "

            IF LEN(pie[npie]) => 130
                npie = npie + 1
            ELSE
                pie[npie] = pie[npie] + consoli.nomlote + ALLTRIM(STR(consoli.area,10,2))+" ha ("+consoli.cultivo+") *"
            ENDIF

            SKIP
        ENDDO

        nconts1 = LEFT(nconts1,LEN(nconts1)-4)
        lotesp1 = LEFT(lotesp1,LEN(lotesp1)-4)

    ENDIF

    pie1 = pie[1]
    pie2 = pie[2]
    pie3 = pie[3]
    pie4 = pie[4]

    **********
    feme1 = FLUCOMUN(nconts1,lotesp1,anno,semestre)
    **********

    IF feme1=CTOD("31/12/2099")

        MESSAGEBOX("Imposible Emitir Resultado"+CHR(13);
            +"- NO HAY ACTIVIDADES -",16,"!No se Puede�")

    ELSE
        DO CASE
            CASE qsali=5
                titulo="Flujo Financiero"
                USE flufin
            CASE qsali=4
                DO HACECAJA
                titulo="Flujo de Ingresos y Egresos"
        ENDCASE

        DO FORM imprimir WITH "flufin.frx",""
    ENDIF
ENDIF


RETURN

************************


************************
PROCEDURE HACECAJA

    ********* sumando registros FLUFIN **********

    SELECT SUM(VAL(STRTRAN(m1,",",""))) AS m1,SUM(VAL(STRTRAN(m2,",",""))) AS m2,;
        SUM(VAL(STRTRAN(m3,",",""))) AS m3,SUM(VAL(STRTRAN(m4,",",""))) AS m4,;
        SUM(VAL(STRTRAN(m5,",",""))) AS m5,SUM(VAL(STRTRAN(m6,",",""))) AS m6,;
        SUM(VAL(STRTRAN(m7,",",""))) AS m7,SUM(VAL(STRTRAN(m8,",",""))) AS m8,;
        SUM(VAL(STRTRAN(m9,",",""))) AS m9,SUM(VAL(STRTRAN(m10,",",""))) AS m10,;
        SUM(VAL(STRTRAN(m11,",",""))) AS m11,SUM(VAL(STRTRAN(m12,",",""))) AS m12,;
        05 AS REGIS;
        FROM sacfadb!flufin;
        WHERE RECNO() = 4;
        INTO TABLE FCAJA7
	
    SELECT SUM(VAL(STRTRAN(m1,",",""))) AS m1,SUM(VAL(STRTRAN(m2,",",""))) AS m2,;
        SUM(VAL(STRTRAN(m3,",",""))) AS m3,SUM(VAL(STRTRAN(m4,",",""))) AS m4,;
        SUM(VAL(STRTRAN(m5,",",""))) AS m5,SUM(VAL(STRTRAN(m6,",",""))) AS m6,;
        SUM(VAL(STRTRAN(m7,",",""))) AS m7,SUM(VAL(STRTRAN(m8,",",""))) AS m8,;
        SUM(VAL(STRTRAN(m9,",",""))) AS m9,SUM(VAL(STRTRAN(m10,",",""))) AS m10,;
        SUM(VAL(STRTRAN(m11,",",""))) AS m11,SUM(VAL(STRTRAN(m12,",",""))) AS m12,;
        06 AS REGIS;
        FROM sacfadb!flufin;
        WHERE RECNO() = 5;
        OR (RECNO() = 6);
        INTO TABLE FCAJA


    SELECT SUM(VAL(STRTRAN(m1,",",""))) AS m1,SUM(VAL(STRTRAN(m2,",",""))) AS m2,;
        SUM(VAL(STRTRAN(m3,",",""))) AS m3,SUM(VAL(STRTRAN(m4,",",""))) AS m4,;
        SUM(VAL(STRTRAN(m5,",",""))) AS m5,SUM(VAL(STRTRAN(m6,",",""))) AS m6,;
        SUM(VAL(STRTRAN(m7,",",""))) AS m7,SUM(VAL(STRTRAN(m8,",",""))) AS m8,;
        SUM(VAL(STRTRAN(m9,",",""))) AS m9,SUM(VAL(STRTRAN(m10,",",""))) AS m10,;
        SUM(VAL(STRTRAN(m11,",",""))) AS m11,SUM(VAL(STRTRAN(m12,",",""))) AS m12,;
        07 AS REGIS;
        FROM sacfadb!flufin;
        WHERE RECNO() = 13;
        OR (RECNO() = 16);
        INTO TABLE FCAJA1

    SELECT SUM(VAL(STRTRAN(m1,",",""))) AS m1,SUM(VAL(STRTRAN(m2,",",""))) AS m2,;
        SUM(VAL(STRTRAN(m3,",",""))) AS m3,SUM(VAL(STRTRAN(m4,",",""))) AS m4,;
        SUM(VAL(STRTRAN(m5,",",""))) AS m5,SUM(VAL(STRTRAN(m6,",",""))) AS m6,;
        SUM(VAL(STRTRAN(m7,",",""))) AS m7,SUM(VAL(STRTRAN(m8,",",""))) AS m8,;
        SUM(VAL(STRTRAN(m9,",",""))) AS m9,SUM(VAL(STRTRAN(m10,",",""))) AS m10,;
        SUM(VAL(STRTRAN(m11,",",""))) AS m11,SUM(VAL(STRTRAN(m12,",",""))) AS m12,;
        08 AS REGIS;
        FROM sacfadb!flufin;
        WHERE RECNO() = 14;
        OR (RECNO() = 17);
        INTO TABLE FCAJA2

    SELECT SUM(VAL(STRTRAN(m1,",",""))) AS m1,SUM(VAL(STRTRAN(m2,",",""))) AS m2,;
        SUM(VAL(STRTRAN(m3,",",""))) AS m3,SUM(VAL(STRTRAN(m4,",",""))) AS m4,;
        SUM(VAL(STRTRAN(m5,",",""))) AS m5,SUM(VAL(STRTRAN(m6,",",""))) AS m6,;
        SUM(VAL(STRTRAN(m7,",",""))) AS m7,SUM(VAL(STRTRAN(m8,",",""))) AS m8,;
        SUM(VAL(STRTRAN(m9,",",""))) AS m9,SUM(VAL(STRTRAN(m10,",",""))) AS m10,;
        SUM(VAL(STRTRAN(m11,",",""))) AS m11,SUM(VAL(STRTRAN(m12,",",""))) AS m12,;
        14 AS REGIS;
        FROM sacfadb!flufin;
        WHERE RECNO() = 12;
        OR (RECNO() = 15);
        INTO TABLE FCAJA3

    SELECT VAL(STRTRAN(m1,",","")) AS m1,VAL(STRTRAN(m2,",","")) AS m2,;
        VAL(STRTRAN(m3,",","")) AS m3,VAL(STRTRAN(m4,",","")) AS m4,;
        VAL(STRTRAN(m5,",","")) AS m5,VAL(STRTRAN(m6,",","")) AS m6,;
        VAL(STRTRAN(m7,",","")) AS m7,VAL(STRTRAN(m8,",","")) AS m8,;
        VAL(STRTRAN(m9,",","")) AS m9,VAL(STRTRAN(m10,",","")) AS m10,;
        VAL(STRTRAN(m11,",","")) AS m11,VAL(STRTRAN(m12,",","")) AS m12,;
        15 AS REGIS;
        FROM sacfadb!flufin;
        WHERE RECNO() = 7;
        INTO TABLE FCAJA4

    SELECT SUM(VAL(STRTRAN(m1,",",""))) AS m1,SUM(VAL(STRTRAN(m2,",",""))) AS m2,;
        SUM(VAL(STRTRAN(m3,",",""))) AS m3,SUM(VAL(STRTRAN(m4,",",""))) AS m4,;
        SUM(VAL(STRTRAN(m5,",",""))) AS m5,SUM(VAL(STRTRAN(m6,",",""))) AS m6,;
        SUM(VAL(STRTRAN(m7,",",""))) AS m7,SUM(VAL(STRTRAN(m8,",",""))) AS m8,;
        SUM(VAL(STRTRAN(m9,",",""))) AS m9,SUM(VAL(STRTRAN(m10,",",""))) AS m10,;
        SUM(VAL(STRTRAN(m11,",",""))) AS m11,SUM(VAL(STRTRAN(m12,",",""))) AS m12,;
        09 AS REGIS;
        FROM sacfadb!flufin;
        WHERE RECNO() = 4 OR RECNO() = 5 OR RECNO() = 6 OR RECNO() = 13 OR ;
        RECNO() = 16 OR RECNO() = 14 OR RECNO() = 17;
        INTO TABLE FCAJA5

    SELECT SUM(VAL(STRTRAN(m1,",",""))) AS m1,SUM(VAL(STRTRAN(m2,",",""))) AS m2,;
        SUM(VAL(STRTRAN(m3,",",""))) AS m3,SUM(VAL(STRTRAN(m4,",",""))) AS m4,;
        SUM(VAL(STRTRAN(m5,",",""))) AS m5,SUM(VAL(STRTRAN(m6,",",""))) AS m6,;
        SUM(VAL(STRTRAN(m7,",",""))) AS m7,SUM(VAL(STRTRAN(m8,",",""))) AS m8,;
        SUM(VAL(STRTRAN(m9,",",""))) AS m9,SUM(VAL(STRTRAN(m10,",",""))) AS m10,;
        SUM(VAL(STRTRAN(m11,",",""))) AS m11,SUM(VAL(STRTRAN(m12,",",""))) AS m12,;
        16 AS REGIS;
        FROM sacfadb!flufin;
        WHERE RECNO() = 12 OR RECNO() = 15 OR RECNO() = 7;
        INTO TABLE FCAJA6

    UPDATE flucaja SET m1 = " ", m2 = " ", m3 = " ", m4 = " ", ;
        m5 = " ", m6 = " ", m7 = " ", m8 = " ", m9  = " ", m10 = " ", ;
        m11 = " ", m12 = " "

    SELECT FCAJA
    APPEND FROM FCAJA1
    APPEND FROM FCAJA2
    APPEND FROM FCAJA3
    APPEND FROM FCAJA4
    APPEND FROM FCAJA5
    APPEND FROM FCAJA6
    APPEND FROM FCAJA7

    ************  ACTUALIZANDO FLUJO DE CAJA *********

    GO TOP
    DO WHILE !EOF()

        UPDATE sacfadb!flucaja;
            SET m1=TRANSFORM(FCAJA.m1,"@Z 99,999,999,999"),;
            m2=TRANSFORM(FCAJA.m2,"@Z 99,999,999,999"),;
            m3=TRANSFORM(FCAJA.m3,"@Z 99,999,999,999"),;
            m4=TRANSFORM(FCAJA.m4,"@Z 99,999,999,999"),;
            m5=TRANSFORM(FCAJA.m5,"@Z 99,999,999,999"),;
            m6=TRANSFORM(FCAJA.m6,"@Z 99,999,999,999"),;
            m7=TRANSFORM(FCAJA.m7,"@Z 99,999,999,999"),;
            m8=TRANSFORM(FCAJA.m8,"@Z 99,999,999,999"),;
            m9=TRANSFORM(FCAJA.m9,"@Z 99,999,999,999"),;
            m10=TRANSFORM(FCAJA.m10,"@Z 99,999,999,999"),;
            m11=TRANSFORM(FCAJA.m11,"@Z 99,999,999,999"),;
            m12=TRANSFORM(FCAJA.m12,"@Z 99,999,999,999");
            WHERE RECNO()=FCAJA.REGIS

        SKIP
    ENDDO

    SELECT flufin
    GO 1

    UPDATE sacfadb!flucaja;
        SET m1=flufin.m1,;
        m2=flufin.m2,;
        m3=flufin.m3,;
        m4=flufin.m4,;
        m5=flufin.m5,;
        m6=flufin.m6,;
        m7=flufin.m7,;
        m8=flufin.m8,;
        m9=flufin.m9,;
        m10=flufin.m10,;
        m11=flufin.m11,;
        m12=flufin.m12;
        WHERE RECNO()=1

    SELECT flucaja

    FOR I=2 TO 13
        qcampo=FIELD(I)
        GO 16
        qsuma=VAL(STRTRAN(&qcampo,",",""))

        IF I=2
            acum1=VAL(STRTRAN(&qcampo,",",""))
        ELSE
            acum1=acum1+VAL(STRTRAN(&qcampo,",",""))
        ENDIF

        GO 9
        qsuma=qsuma-VAL(STRTRAN(&qcampo,",",""))

        IF I=2
            acum2=VAL(STRTRAN(&qcampo,",",""))
        ELSE
            acum2=acum2+VAL(STRTRAN(&qcampo,",",""))
        ENDIF

        GO 20
        REPLACE &qcampo WITH TRANSFORM(qsuma,"@Z 99,999,999,999")

        IF I=2
            acum3=qsuma
        ELSE
            acum3=acum3+qsuma
        ENDIF

        GO 10
        REPLACE &qcampo WITH TRANSFORM(acum2,"@Z 99,999,999,999")

        GO 17
        REPLACE &qcampo WITH TRANSFORM(acum1,"@Z 99,999,999,999")

        GO 21
        REPLACE &qcampo WITH TRANSFORM(acum3,"@Z 99,999,999,999")

    ENDFOR
    RETURN
