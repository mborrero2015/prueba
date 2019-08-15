****************************************************************************
*****************FLUJO COMUN PARA LOS RESULTADOS FINANCIEROS****************
****************************************************************************

FUNCTION FLUCOMUN

	PARAMETERS nconts,lotesp,anofl,semfl

	DECLARE vari[12],fij[12],imp[12],ing[12],bruto[12],neto[12],linea[12],abruto[12],aneto[12]
	DECLARE desotc[12],amorotc[12],intotc[12]
	DECLARE fecprob[7],nmes[12],donde[17]

	fecprob[1] = 1
	fecprob[2] = 3
	fecprob[3] = 5
	fecprob[4] = 7
	fecprob[5] = 8
	fecprob[6] = 10
	fecprob[7] = 12

	*!* VARIABLES DEL LOTE(S) A PROCESAR

	FOR i=1 TO 12
		vari[i] = 0     &&//Costos variables.  cambio de -var-
		fij[i] = 0      &&//Costos fijos.
		
		desotc[i] = 0   &&//desembolsos otros cr‚ditos.
		intotc[i] = 0   &&//intereses otros cr‚ditos.
		amorotc[i] = 0  &&//amortizaciones otros cr‚ditos.
		ing[i] = 0      &&//Ingresos totales.
		imp[i] = 0      &&//Impuestos.
		bruto[i] = 0    &&//total de margen bruto.
		neto[i] = 0     &&//total de margen neto.
		linea[i] = 0    &&// linea sola...
		abruto[i] = 0   &&//acumulado bruto
		aneto[i] = 0    &&//acumulado neto
	NEXT


	****************************************************************************
	********************** COSTOS VARIABLES - CONTRATOS ************************
	****************************************************************************

	** Labculti.fijo = 1 SI EL LOTE ES ARRENDADO
	** CODLABOR * Labculti.fijo &&& PARA ARRIENDO COMO COSTO VARIABLE
	*****************************************************************
	Vcontinuar = .F.

	SELECT COUNT(*) FROM  sacfadb!labculti ;
		WHERE (&nconts) INTO ARRAY vcantidad

	feme = CTOD("31/12/2099")
	ULTMES = CTOD("01/01/1900")

	IF vcantidad > 0
		Vcontinuar = .T.
		nosera = .T.

		SELECT COUNT(*) FROM  sacfadb!labculti ;
			WHERE (&nconts) AND labculti.codlabor != 804 INTO ARRAY vcantidad

		IF vcantidad > 0

			SELECT SUM(labculti.costotal ) AS costotal ;
				FROM  sacfadb!labculti   WHERE (&nconts) AND labculti.codlabor != 804 ;
				INTO CURSOR Curpago1

			SELECT  SUM(LABCULTI.costotal) as valor;
				FROM  sacfadb!labculti ;
				WHERE (&nconts) AND labculti.codlabor != 804 INTO CURSOR Curpago2

			Curpago =  Curpago1.costotal - Curpago2.valor

			IF Curpago  != 0
				nosera = .F.
			ENDIF
		ELSE
			nosera = .F.
		ENDIF

		IF !nosera
			MESSAGEBOX("  La Información no sera la Correcta no "+CHR(13);
				+"se han realizado pagos en el Desarrollo"+CHR(13);
				+"  de Cultivos      CONTRATOS "+CHR(13)+CHR(13)+;
				"             INGRESE PAGOS",16,"¡Ojo falta!")
		ENDIF
	ENDIF

	IF Vcontinuar

		SELECT fechalab AS MES, SUM(costotal) AS VALORT;
			FROM  sacfadb!labculti;
			WHERE (&nconts) AND  ;
			labculti.codlabor <> 400 AND labculti.codlabor <> 506 AND;
			labculti.codlabor * labculti.fijo <> 1802 ;
			GROUP BY 1 ORDER BY 1 INTO TABLE vvvvvv
			
&&AND ;         (YEAR(Dfechas.fecha) = g_ano OR YEAR(Dfechas.fecha) = g_ano + 1)
		**********  **********  **********  **********  **********

		feme = CTOD("31/12/2099")
		ULTMES = CTOD("01/01/1900")


		USE vvvvvv
		GO BOTTOM
		IF !EMPTY (vvvvvv.MES)
			ULTMES = vvvvvv.MES  && ultimo mes para admon. de GENPARA.
		ENDIF

		GO TOP
		IF !EMPTY(vvvvvv.MES)
			feme = vvvvvv.MES
		ENDIF

		DO WHILE !EOF()
			vari[month(vvvvvv.MES)] = vari[month(vvvvvv.MES)] + vvvvvv.VALORT
			Skip
		ENDDO

		****************************************************************************
		***************************** PARA LOS FIJOS *******************************
		****************************************************************************
		** LABOR = 506 .... AGUA TARIFA FIJA
		** Labculti.fijo = 2 ---- ARRIENDO DEL LOTE PROPIO COMO COSTO FIJO
		*********  *********  *********  *********  *********  *********

		SELECT fechalab AS MES, SUM(costotal) AS VALORT;
			FROM  sacfadb!labculti;
			WHERE (&nconts) AND;
			(labculti.codlabor = 506 OR labculti.codlabor * ;
			labculti.fijo = 1802);
			GROUP BY 1 ORDER BY 1 INTO TABLE fijo

&&AND ;
&&(YEAR(Dfechas.fecha) = g_ano OR YEAR(Dfechas.fecha) = g_ano + 1)

		USE fijo
		GO BOTTOM
		IF fijo.MES>ULTMES
			ULTMES=fijo.MES
		ENDIF

		GO TOP
		IF fijo.MES<feme AND !EMPTY(fijo.MES)
			feme=fijo.MES
		ENDIF

		DO WHILE !EOF()
			fij[month(FIJO.MES)] = fij[month(FIJO.MES)] + fijo.VALORT
			Skip
		ENDDO
	ENDIF

	
	*************************************************************************
	************ COSTO VARIABLE POR ACTIVIDAD MANUAL. "JORNALES" ************
	*************************************************************************
	Vcontinuar = .F.

	SELECT COUNT(*) FROM  sacfadb!jorculti ;
		WHERE (&nconts) INTO ARRAY vcantidad

	IF vcantidad > 0

		Vcontinuar = .T.

		SELECT fechalab AS MES, SUM(costotal) AS VALORT;
			FROM  sacfadb!jorculti;
			WHERE (&nconts) ;
			GROUP BY 1 ORDER BY 1 INTO TABLE vvvvvv
&&AND ;
&&    (YEAR(Dfechas.fecha) = g_ano OR YEAR(Dfechas.fecha) = g_ano + 1)

		USE vvvvvv
		GO BOTTOM
		IF vvvvvv.MES>ULTMES
			ULTMES=vvvvvv.MES
		ENDIF

		GO TOP
		IF vvvvvv.MES < feme AND !EMPTY(vvvvvv.MES)
			feme = vvvvvv.MES
		ENDIF

		DO WHILE !EOF()
			vari[MONTH(vvvvvv.MES)] = vari[MONTH(vvvvvv.MES)] + vvvvvv.VALORT
			Skip
		ENDDO
	ENDIF
	
	
		
	************************ Costos para gastos generales.**********************
	****************************************************************************

	****  CAMPOS costovar  ****
	
	IF !used("gast_gene")
		USE gast_gene in 0
	ENDIF
				
	SELECT sum(costo) as costo , fecha as MES, month(fecha) as mes1 from gast_gene where ;
		g_ano= year(fecha) and g_codfin=codfin and g_semestre=iif(month(fecha)<7,1,2) ;
		GROUP BY MES order BY MES  into cursor c_gastos
	SELECT sum(costo) as VALORT, MES from c_gastos GROUP BY mes1 into table t_cosnom

	USE t_cosnom
	GO BOTTOM
	IF t_cosnom.MES>ULTMES
		ULTMES=t_cosnom.MES
	ENDIF

	GO TOP
	IF t_cosnom.MES<feme AND !EMPTY(t_cosnom.MES)
		feme=t_cosnom.MES
	ENDIF

	v_prorrata =0
	SELECT sum(area) from lotesfin INNER join infculti on lotesfin.codlote = infculti.codlote ;
		where lotesfin.codfin = infculti .codfin  and lotesfin.codfin = g_codfin ;
		and semestre= g_semestre and ano = g_ano into array area_t

	SELECT sum(area) from lotesfin INNER join infculti on lotesfin.codlote = infculti.codlote ;
		where lotesfin.codfin = infculti .codfin  and lotesfin.codfin = g_codfin ;
		and semestre= g_semestre and ano = g_ano  and marcainf = .t. into array area_sel


	DO WHILE NOT EOF()
		v_prorrata = t_cosnom.VALORT*area_sel/area_t
		vari[MONTH(t_cosnom.MES)] = vari[MONTH(t_cosnom.MES)] + v_prorrata
		Skip
	ENDDO
	
	
	****************************************************************************
	************ LLENA CON LOS COSTOS DE LOS INSUMOS APLICADOS ***********
	
	****************************************************************************

	SELECT insaplica.fecha1 AS MES,;
		SUM(insaplica.peso*insaplica.precio) AS VALORT ;
		FROM  sacfadb!insaplica;
		WHERE (&nconts) ;
		GROUP BY 1 ORDER BY 1 INTO TABLE vvvvvv

&&AND ;
&&(YEAR(Aplica.fecaplic) = g_ano OR YEAR(Aplica.fecaplic) = g_ano + 1)

	USE vvvvvv
	GO BOTTOM
	IF vvvvvv.MES>ULTMES
		ULTMES=vvvvvv.MES
	ENDIF

	GO TOP
	IF vvvvvv.MES<feme AND !EMPTY(vvvvvv.MES)
		feme=vvvvvv.MES
	ENDIF

	DO WHILE NOT EOF()
		vari[MONTH(vvvvvv.MES)] = vari[MONTH(vvvvvv.MES)] + vvvvvv.VALORT
		Skip
	ENDDO

	

	****    Ingresos totales del ejercicio.   ****

	SELECT Ventas.fecha AS MES,;
		SUM(Ventas.VALVENTA * Ventas.TONELS) AS ingreso,;
		SUM((Ventas.VALVENTA * Ventas.TONELS)*((Ventas.RETEN+Ventas.CUOTA+Ventas.OTROS)/100)) AS impuesto;
		FROM  sacfadb!Ventas WHERE (&nconts) ;
		GROUP BY 1 ORDER BY 1 INTO TABLE VVENTAS

&&AND ;
&&(YEAR(Ventas.fecha) = g_ano OR YEAR(Ventas.fecha) = g_ano + 1)

	USE VVENTAS
	GO BOTTOM
	IF VVENTAS.MES>ULTMES
		ULTMES=VVENTAS.MES
	ENDIF

	GO TOP
	IF VVENTAS.MES<feme AND !EMPTY(VVENTAS.MES)
		feme=VVENTAS.MES
	ENDIF

	DO WHILE !EOF()
		ing[MONTH(VVENTAS.MES)] = ing[MONTH(VVENTAS.MES)] + VVENTAS.ingreso
		imp[MONTH(VVENTAS.MES)] = imp[MONTH(VVENTAS.MES)] + VVENTAS.impuesto
		Skip
	ENDDO

	*****************************************************************************
	*********  LLENA CON LOS VALORES CORRESPONDIENTES A LOS CREDITOS *******
	**** PASAR vvvvvvS DE:
	**** AÑO, SEMESTRE  Y LOTES A PROCESAR...  (OR... OR..)
	*****************************************************************************

	*******  2 SELECTS  - PLALOTS Y CREDS *******
	DO PLACRED WITH anofl,semfl,lotesp
	*********************************************

	USE CREDS
	GO TOP

	DO WHILE !EOF()
					** desembolsos
					desotc[MONTH(mes_asig)] = desotc[MONTH(mes_asig)] + CREDS.val_cred

					** pagos
					amorotc[MONTH(mes_pago)] = amorotc[MONTH(mes_pago)] + CREDS.val_cred

					** intereses
					intotc[MONTH(mes_inter)] = intotc[MONTH(mes_inter)] + creds.val_inter

		
		IF mes_asig<feme AND !EMPTY(mes_asig)
			feme = mes_asig
		ENDIF

		IF mes_asig>ULTMES
			ULTMES = mes_asig
		ENDIF


		IF mes_pago<feme AND !EMPTY(mes_pago)
			feme = mes_pago
		ENDIF

		IF mes_pago>ULTMES
			ULTMES = mes_pago
		ENDIF


		Skip

	ENDDO
	
	**************************************************************************


	FOR i=1 TO 12

		bruto[i] = ing[i] - imp[i] - fij[i] - vari[i]

	NEXT

	FOR i=1 TO 12

		neto[i]=bruto[i]+desotc[i]-amorotc[i]-intotc[i]

	NEXT

	****************************************************************************
	****** llenado de flufin.dbf -  para informe  ******
	*****************************************************************************

	donde[1]="fij"
	donde[2]="vari"
	donde[3]="imp"
	donde[4]="ing"
	donde[5]="linea"
	donde[6]="bruto"
	donde[7]="abruto"
	donde[8]="linea"
	
	donde[9]="linea"
	donde[10]="linea"
	donde[11]="linea"
	
	donde[12]="desotc"
	donde[13]="intotc"
	donde[14]="amorotc"
	donde[15]="linea"
	donde[16]="neto"
	donde[17]="aneto"

	*********

	nmes[1]="ENERO"
	nmes[2]="FEBRERO"
	nmes[3]="MARZO"
	nmes[4]="ABRIL"
	nmes[5]="MAYO"
	nmes[6]="JUNIO"
	nmes[7]="JULIO"
	nmes[8]="AGOSTO"
	nmes[9]="SEPTIEMB"
	nmes[10]="OCTUBRE"
	nmes[11]="NOVIEMBRE"
	nmes[12]="DICIEMBRE"

	IF SELECT ("flufin") = 0
		USE flufin IN 0
	ENDIF
	SELECT flufin
	GO TOP

	**** ubicando primer mes con datos *****
	MES   = MONTH(feme)
	mes1  = MES
	deano = YEAR(feme)

	FOR i = 1 TO 12
		qcampo=FIELD(i+1)
		REPLACE &qcampo WITH nmes[mes]+"/"+RIGHT(STR(deano),4)

		MES=MES+1
		IF MES=13
			MES=1
			deano=deano+1
		ENDIF
	NEXT

	FOR i=4 TO 20
		GO i
		CUALES=donde[i-3]
		MES=mes1
		FOR j=1 TO 12
			qcampo=FIELD(j+1)
			REPLACE &qcampo WITH TRANSFORM(&CUALES[mes],"@Z 99,999,999,999")
			*.* REPLACE &qcampo WITH right(spac(16)+TRANSFORM(&CUALES[mes],"@Z 999,999,999,999"),16)
			MES=MES+1
			IF MES=13
				MES=1
			ENDIF
		NEXT
	NEXT

	SELECT flufin

	FOR i=2 TO 13
		qcampo=FIELD(i)

		GO 9
		IF i=2
			acum1=VAL(ALLTRIM(STRTRAN(&qcampo,",","")))
		ELSE
			acum1=acum1+VAL(ALLTRIM(STRTRAN(&qcampo,",","")))
		ENDIF

		GO 19
		IF i=2
			acum2=VAL(ALLTRIM(STRTRAN(&qcampo,",","")))
		ELSE
			acum2=acum2+VAL(ALLTRIM(STRTRAN(&qcampo,",","")))
		ENDIF

		GO 10
		REPLACE &qcampo WITH TRANSFORM(acum1,"@Z 99,999,999,999")

		GO 20
		REPLACE &qcampo WITH TRANSFORM(acum2,"@Z 99,999,999,999")

	ENDFOR

	RETURN feme

	****************************************************************************
PROCEDURE PLACRED
	****************************************************************************

	PARAMETERS anofl1,semfl1,lotesp1


***  TODAS LAS ASIGNACIONES DE CREDITO($$) A LOS LOTES SELECCIONADOS,
***    POR AÑO Y SEMESTRE

	SELECT dcredito.fecha as mes_asig, (dcredito.porcentaje * credito.valcred / 100) AS val_cred,;
		   credito.fecha_interes as mes_inter,(dcredito.porcentaje * credito.intereses / 100) AS val_inter,;
		   credito.fecha_pago as mes_pago;
	  FROM dcredito INNER JOIN credito ON dcredito.codcred = credito.codcred;
		WHERE (&lotesp1) AND Credito.ano = g_ano AND Credito.semestre = g_semestre AND Credito.codfin = g_codfin;
		  INTO TABLE CREDS

	RETURN



