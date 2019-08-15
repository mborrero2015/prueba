****************************************************************************
*Programa que genera el flujo financiero y flujo de caja
****************************************************************************
PARAMETERS qsali
PUBLIC g_estado,v_estado
g_estado=2
v_estado=" "
DECLARE arreglo[12]

SET procedure TO libreria.prg

IF !SELECT ("indf") <> 0
	SELECT 211
	USE indf
ENDIF

UPDATE sacfadb!indf ;
SET indf.costoconc = 0, indf.costosinc = 0
USE IN indf


SELECT Infculti.numcontrol, Infculti.codlote, Infculti.nomlote, ;
Lotesfin.area AS area, Infculti.cultivo, Infculti.codcultivo, ;
Ubica.nomfin, Infculti.ano, Infculti.semestre, Infculti.toneladas ;
FROM Infculti, Lotesfin , Ubica WHERE (Infculti.codfin = g_codfin AND ;
Infculti.ano = g_ano AND Infculti.semestre = g_semestre) AND ;
Infculti.marcainf AND Lotesfin.codlote = Infculti.codlote AND ;
Ubica.codfin = g_codfin INTO TABLE consoli
GO TOP

DO WHILE !EOF()

	arealo   = consoli.area
	tonlo    = consoli.toneladas
	finca    = consoli.nomfin
	anno     = consoli.ano
	semestre = consoli.semestre
	cultivo  = "CULTIVO: "+Infculti.cultivo

	lote = ALLTRIM(consoli.nomlote)+" ("+ALLTRIM(STR(consoli.area,10,2))+")"

	nconts1 = "numcontrol="+ALLTRIM(STR(consoli.numcontrol))
	lotesp1 = "codlote="+ALLTRIM(STR(consoli.codlote))

	nmctr = consoli.numcontrol
**********
	feme1 = FLUCOMUN(nconts1,lotesp1,anno,semestre)
**********

	IF feme1 = CTOD("31/12/2099")

		MESSAGEBOX("Imposible Emitir Resultado"+CHR(13);
		+"- NO HAY ACTIVIDADES -",16,"!No se Puede�")

	ELSE
		DO CASE
		CASE qsali=7 && indicadores financieros
			DO HACEINDF
			DO FORM imprimir WITH "indf.frx", ""


		CASE qsali=6 && resumen financiero
			DO HACEREFI
			DO FORM imprimir WITH "resufin.frx",""
		ENDCASE

	ENDIF

	SELECT consoli
	SKIP
ENDDO

RETURN

************************


******************************
PROCEDURE HACEREFI

SELECT cuenta,VAL(STRTRAN(m1,",",""))+VAL(STRTRAN(m2,",",""))+;
VAL(STRTRAN(m3,",",""))+VAL(STRTRAN(m4,",",""))+;
VAL(STRTRAN(m5,",",""))+VAL(STRTRAN(m6,",",""))+;
VAL(STRTRAN(m7,",",""))+VAL(STRTRAN(m8,",",""))+;
VAL(STRTRAN(m9,",",""))+VAL(STRTRAN(m10,",",""))+;
VAL(STRTRAN(m11,",",""))+VAL(STRTRAN(m12,",","")) AS TOTAL,;
0000000000000000 AS hecta,000000000000 AS tonel;
FROM  sacfadb!flufin;
WHERE RECNO()<>1 AND RECNO()<>2 AND RECNO()<>3 AND RECNO()<>10 AND RECNO()<>20;
INTO TABLE flufin1

USE flufin1

REPLACE hecta WITH TOTAL/arealo ALL

IF tonlo <> 0
	REPLACE tonel WITH TOTAL/tonlo ALL
ENDIF

RETURN



PROCEDURE HACEINDF

DECLARE arreg1[12],tir[2],vp[2]

SELECT cuenta,VAL(STRTRAN(m1,",",""))+VAL(STRTRAN(m2,",",""))+;
VAL(STRTRAN(m3,",",""))+VAL(STRTRAN(m4,",",""))+;
VAL(STRTRAN(m5,",",""))+VAL(STRTRAN(m6,",",""))+;
VAL(STRTRAN(m7,",",""))+VAL(STRTRAN(m8,",",""))+;
VAL(STRTRAN(m9,",",""))+VAL(STRTRAN(m10,",",""))+;
VAL(STRTRAN(m11,",",""))+VAL(STRTRAN(m12,",","")) AS TOTAL;
FROM  sacfadb!flufin;
WHERE RECNO()=4 OR RECNO()=5 OR RECNO()=6 OR RECNO()=9 OR;
RECNO()=13 OR RECNO()=16 OR RECNO()=19 OR RECNO()=7;
INTO TABLE flufin2

USE flufin2
SUM TOTAL TO ctsc FOR RECNO() < 4
SUM TOTAL TO ctcc FOR RECNO() < 4 OR RECNO()= 6 OR RECNO()= 7

SUM TOTAL TO insc FOR RECNO()= 5
SUM TOTAL TO incc FOR RECNO()= 8

****** costos ha  y ton.
UPDATE sacfadb!indf;
SET costoconc=ctcc/arealo,;
costosinc=ctsc/arealo;
WHERE RECNO()=1

IF tonlo <> 0
	UPDATE sacfadb!indf;
	SET costoconc=ctcc/tonlo,;
	costosinc=ctsc/tonlo;
	WHERE RECNO()=2
ENDIF

****  ingreso neto
UPDATE sacfadb!indf;
SET costoconc = incc/arealo,;
costosinc = insc/arealo;
WHERE RECNO()= 4

*** rendimiento ha.

UPDATE sacfadb!indf;
SET costosinc=(tonlo/arealo)*1000;
WHERE RECNO()=10


*** tir / vpn / R/c
IF !USED("genpara")
	USE genpara IN  0
endif
SELECT genpara
oportu = 00.0
vcosto_dolares =0
oportu = tasaoport/100
vtrm= trm
vsecamiento		= secamiento
vconversion     = conversion
vcompetitividad = competitividad
vflete			= flete
vpuertos		=puertos
vfinanciacion	= financiacion
vmermas 		=mermas
vimportacion	=importacion
vcostocif	= (vcompetitividad+vflete)*vconversion/100+(vcompetitividad+vflete)
vcostopuerto= 	vcostocif+vpuertos
vcostosinarancel=vcostopuerto+ vcostocif*vmermas/100 +vfinanciacion
vcostotalpuerto=  vcostocif*vimportacion
vcomisionimportador=  vcostocif*3/100
vcosto_dolares =vcostosinarancel+vcostotalpuerto+vcomisionimportador
*	vcosto_dolares = (vcompetitividad+vflete)*vconversion/100+(vcompetitividad+vflete)+vpuertos+vfinanciacion+vmermas*((vcompetitividad+vflete)*(vconversion/100)+vcompetitividad+vflete)
*+((vcompetitividad+vflete)*(vconversion/100)+vcompetitividad+vflete)*vimportacion+((vcompetitividad+vflete)*(vconversion/100)+(vcompetitividad+vflete))*3%
*	=+(C18+C17)*0.86%+C18+C17+C21+C23+(C24)*((C18+C17)*0.86%+C18+C17)+((C18+C17)*0.86%+C18+C17)*C26+((C18+C17)*0.86%+C18+C17)*3%
SELECT indf
UPDATE sacfadb!indf;
SET costoconc=vtrm;
WHERE RECNO()=12
UPDATE sacfadb!indf;
SET costoconc=vcosto_dolares ;
WHERE RECNO()=14
*!*		GO 1
*!*		costo_usd_ha_s=indf.COSTOsinc/vtrm
*!*		costo_usd_ha_c=indf.COSTOCONC/vtrm
*!*		 UPDATE sacfadb!indf;
*!*	        SET costosinc=costo_usd_ha_s,costoconc=costo_usd_ha_c;
*!*	        WHERE RECNO()=13

GO 2
*costo_usd_ton_s=indf.costosinc/(0.86*vtrm)
*costo_usd_ton_c=indf.costoconc/(0.86*vtrm)
costo_usd_ton_s=((indf.costosinc/0.85)+vsecamiento)/vtrm
costo_usd_ton_c=((indf.costoconc/0.85)+vsecamiento)/vtrm

UPDATE sacfadb!indf;
SET costosinc=costo_usd_ton_s,costoconc=costo_usd_ton_c;
WHERE RECNO()=13


vcompetitivo=2
IF costo_usd_ton_c>0
	IF costo_usd_ton_c<=vcosto_dolares
		g_estado=1
		v_estado="Competitivo"
	ELSE
		g_estado=0
		v_estado="No Competitivo"
	ENDIF
else
	v_estado="No ha vendido"
ENDIF

*SET REPORTBEHAVIOR 90
SELECT flufin
ii = 1
vpisin = 0
vpesin = 0
vpicon = 0
vpecon = 0

FOR ix = 9 TO 19 STEP 10
	GO ix

	FOR iy = 2 TO 13
		quemes = FIELD(iy)
		arreg1[iy-1] = VAL(STRT(&quemes,",",""))

		UPDATE sacfadb!vpns;
		SET TOTAL = arreg1[iy-1];
		WHERE RECNO()= iy-1

		IF ix = 9
			IF arreg1[iy-1] > 0   &&//Valor presente de ingresos y egresos sin credito.
				vpisin = vpisin + arreg1[iy-1]/((1+oportu)**(iy-2))
*vpisin = vpisin + arreg1[iy-1]/((1+oportu)**(iy-1))
			ELSE
				vpesin = vpesin + arreg1[iy-1]/((1+oportu)**(iy-2))
*vpesin = vpesin + arreg1[iy-1]/((1+oportu)**(iy-1))
			ENDIF
		ELSE
			IF arreg1[iy-1] > 0   &&//Valor presente de ingresos y egresos con credito.
				vpicon = vpicon + arreg1[iy-1]/((1+oportu)**(iy-2))
*vpicon = vpicon + arreg1[iy-1]/((1+oportu)**(iy-1))
			ELSE
				vpecon = vpecon + arreg1[iy-1]/((1+oportu)**(iy-2))
*vpecon = vpecon + arreg1[iy-1]/((1+oportu)**(iy-1))
			ENDIF
		ENDIF

	NEXT

	tir[ii]= TASAINT(@arreg1)

*** vpn
	SELECT vpns
	GO 1
	vin = TOTAL
	CALCULATE NPV(oportu,TOTAL,vin) TO vp[ii] FOR RECNO()>1

*****  ponerlas en infculti ****
*** 1 sin credito
***  2 con credito

	ii=ii+1

	SELECT flufin
NEXT

UPDATE sacfadb!indf;
SET costoconc = vp[2],;
costosinc = vp[1],;
porc = genpara.tasaoport;
WHERE RECNO()=7

UPDATE sacfadb!indf;
SET costoconc = tir[2],;
costosinc = tir[1];
WHERE RECNO()= 6

*** B/C
IF .NOT. (vpisin = 0  .AND. vpesin = 0)
	UPDATE sacfadb!indf ;
	SET costosinc = vpisin / (vpesin * -1),;
	porc = genpara.tasaoport;
	WHERE RECNO() = 8
**   //Relacion beneficio/costo sin credito.
ENDIF

IF .NOT. (vpicon = 0 .AND. vpecon = 0)
	UPDATE sacfadb!indf;
	SET costoconc=vpicon / (vpecon * -1),;
	porc = genpara.tasaoport;
	WHERE RECNO()= 8
**    //Relacion beneficio/costo con credito.
ENDIF


****  $$$ venta.. (ponderado.)
***************

SELECT flufin2
GO 5
IF tonlo <> 0
	SELECT SUM(Ventas.valventa*Ventas.tonels) AS vavent, SUM(Ventas.tonels) AS ton ;
	FROM  sacfadb!Infculti INNER JOIN sacfadb!Ventas ;
	ON  Infculti.numcontrol = Ventas.numcontrol;
	WHERE Infculti.marcainf = .T. INTO CURSOR VENT123

	PRECPVT = VENT123.vavent/VENT123.ton
*!* PRECPVT = VENT123.vavent/VENT123.cuant

	UPDATE sacfadb!indf;
	SET costosinc = PRECPVT;
	WHERE RECNO() = 11
ENDIF
**************************
SELECT indf
GO 1

UPDATE sacfadb!Infculti;
SET Infculti.costotsc = indf.costosinc * arealo,;
Infculti.costotcc = indf.costoconc * arealo;
WHERE Infculti.numcontrol=nmctr

GO 4
UPDATE sacfadb!Infculti;
SET Infculti.ingtsc = indf.costosinc * arealo,;
Infculti.ingtcc = indf.costoconc * arealo;
WHERE Infculti.numcontrol=nmctr

GO 6
UPDATE sacfadb!Infculti;
SET Infculti.tirsc = indf.costosinc ,;
Infculti.tircc = indf.costoconc;
WHERE Infculti.numcontrol = nmctr

GO 7
UPDATE sacfadb!Infculti;
SET Infculti.vpnsc = indf.costosinc,;
Infculti.vpncc = indf.costoconc;
WHERE Infculti.numcontrol = nmctr

GO 8
UPDATE sacfadb!Infculti;
SET Infculti.rbcsc = indf.costosinc,;
Infculti.rbccc = indf.costoconc;
WHERE Infculti.numcontrol = nmctr

RETURN

*****************************


**********************************************************************
*Procedimiento que genera la tasa interna de retorno.
**********************************************************************

FUNCTION TASAINT
PARAMETER arreglo

LOCAL tonu[2],i,x

IF arreglo[1]>0
	FOR x=1 TO 12
		arreglo[X]=arreglo[X]*-1
	NEXT
ENDIF

i=-50
FOR x=1 TO 2
	FAC=1+(i/100)
	TOT=arreglo[1]
	FOR Z=2 TO 12
		TOT=TOT+(arreglo[Z]/(FAC^(Z-1)))
	NEXT Z

	tonu[X]=TOT
	IF TOT=0
		EXIT
	ENDIF
	i=100
NEXT

IF TOT=0
	RETURN i
ENDIF

IF  (tonu[1]<0 .AND. tonu[2]<0) .OR. (tonu[1]>0 .AND. tonu[2]>0)
	RETURN 0  &&// *** ERROR ***
ENDIF

IF TOT<0
	SWP=0

	DO WHILE SWP=0
		FAC=1+(i/100)
		TOT=arreglo[1]
		FOR Z=2 TO 12
			TOT=TOT+(arreglo[Z]/(FAC^(Z-1)))
		NEXT Z
		IF TOT>=0
			SWP=1
		ELSE
			i=i-1
		ENDIF
	ENDDO
	LI=i
	TOI=TOT

	IF TOT<>0
		SWP=0
		DO WHILE SWP=0
			FAC=1+(i/100)
			TOT=arreglo[1]
			FOR Z=2 TO 12
				TOT=TOT+(arreglo[Z]/(FAC^(Z-1)))
			NEXT Z
			IF TOT<=0
				SWP=1
			ELSE
				i=i+1
			ENDIF
		ENDDO
		LS=i
		TOS=TOT
	ENDIF
ELSE

	SWP=0
	DO WHILE SWP=0
		FAC=1+(i/100)
		TOT=arreglo[1]
		FOR Z=2 TO 12
			TOT=TOT+(arreglo[Z]/(FAC^(Z-1)))
		NEXT Z
		IF TOT<=0
			SWP=1
		ELSE
			i=i+1
		ENDIF
	ENDDO
	LS=i
	TOS=TOT

	IF TOT<>0
		SWP=0
		DO WHILE SWP=0
			FAC=1+(i/100)
			TOT=arreglo[1]
			FOR Z=2 TO 12
				TOT=TOT+(arreglo[Z]/(FAC^(Z-1)))
			NEXT Z
			IF TOT>=0
				SWP=1
			ELSE
				i=i-1
			ENDIF
		ENDDO
		LI=i
		TOI=TOT
	ENDIF

ENDIF


IF TOT=0
	RETURN i
ENDIF

i=LI+(TOI/(TOI-TOS))

RETURN i
***********************