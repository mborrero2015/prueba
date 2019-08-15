****************************************************************************
*****************************************************************************
************  C O N T A B I L I D A D   D E S D E   A Q U I   ***************
*****************************************************************************
*****************************************************************************
*****  ELABORA COMPROBANTE CONTABLE - MANO DE OBRA JORNALES - EMPLEADOS *****
PROCEDURE hacecpjoem
	*****************************************************************************

	PARAMETERS quemano

	SELECT 248
	USE cpdtemp

	quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))
	detal = ALLTRIM(g_nomlab)+ " " + quecp
	SELECT 246
	USE contador
	SET ORDER TO  codigo

	SEEK 6  &&///  Cxp a Contratistas

	*  //  VARIABLE PARA CONTRAPARTIDA AL COSTO (SUPONE CONTRA >COSTOS INDIRECTOS)
	*  //  POR AQUI LA CTA. DE COSTOS INDIRECTOS SE VA ACREDITANDO - DISMINUYENDO.
	*  //  SE VA DISMINUYENDO PORQUE ALLI SE HAN IDO COLOCANDO TODOS LOS gastos
	*  //  QUE SON GENERALES A TODOS LOS LOTES O CULTIVOS Y  QUE EL PROGRAMA
	*  //  PRORRATEA, ESTIMA, ETC . AQUI SON:HORAS EMPLEADOS O/Y JORNALES.

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH jorculti.costotal
	REPLACE cpdtemp.detalle WITH quemano+" "+detal
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	REPLACE cpdtemp.codprov WITH g_codprov

	IF VAL(contador.cuenta) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH contador.cuenta

		IF ALLTRIM(contador.natural) = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF

		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
	ENDIF

	***************** CONTRAPARTIDA EN COSTOS DE PRODUCCION *********************
	SELECT contador
	SEEK 8

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH jorculti.costotal
	REPLACE cpdtemp.detalle WITH "COSTO"+" "+detal
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	REPLACE cpdtemp.codprov WITH g_codprov

	IF VAL(contador.cuenta) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH contador.cuenta

		IF ALLTRIM(contador.natural) = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF
		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
	ENDIF

	USE IN 246
	USE IN 248
	RETURN

	*****************************************************************************
	*****************************************************************************
	*******  ELABORA COMPROBANTE CONTABLE DE --- USO DE INSUMOS EN CULTIVOS ***
PROCEDURE hacecpui
	*****************************************************************************
	
	IF SELECT("Contador") = 0
		USE contador IN 0
	ENDIF
	SELECT contador
	SET ORDER TO codigo

	IF SELECT("Tipinsu") <> 0
		SELECT tipinsu
	ELSE
		SELECT 200
		USE tipinsu
	ENDIF
	SET ORDER TO 1


	SELECT 248
	USE cpdtemp
	SET ORDER TO 1

	quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))
	detal = quecp

	valtodos  = 0 &&&// costo todos los insumos SEGUN INVENTARIOS
	descuento = 0 &&&// valor descuentos
	dedemas   = 0 &&&// valor sobrecostos


	** CONTABILIZACION DE USOS POR GRUPO DE INSUMOS  (DISMINUCION INVENTARIOS) **
	*****************************************************************************

	IF SELECT("COMPINS") <> 0
		*use in COMPINS
	ELSE
		SELECT 187
		USE compins
		SET ORDER TO 7
	ENDIF


	SELECT insaplica
	SET FILTER TO insaplica.codaplic = g_aplica
	GO TOP

	DO WHILE !EOF()
		coma1 = SELECT("COMPINS")
		inva1 = SELECT("INVENINI")
		g_actual1 = RECNO()

		IF inva1 <> 0
			SELECT invenini
			SET ORDER TO 2
		ENDIF

		SELECT auxapli
		SET FILTER TO auxapli.codinsap = insaplica.codinsap
		GO TOP

		costins = 0  &&// costo del insumo
		ctipo   = auxapli.codtipo   &&// codigo tipo de insumo
		ccomp   = auxapli.codcomp
		coins   = auxapli.codinsap
		valocost = 0

		DO WHILE !EOF()
			g_actual = RECNO()
			ccomp   = auxapli.codcomp
			IF auxapli.indica = 0 AND coma1 <> 0

				SELECT auxapli.cantusada*((compins.costunit/compins.equivale) ;
					+ compins.sobrecosto/compins.cantins) AS valor  FROM  sacfadb!compins ;
					inner JOIN sacfadb!auxapli ON  compins.codcomp = auxapli.codcomp ;
					WHERE compins.codcomp = ccomp AND auxapli.codinsap = coins ;
					INTO CURSOR cost22

				costins =  cost22.valor
				valocost = costins + valocost

			ENDIF

			IF auxapli.indica = 1 AND inva1 <> 0
				SELECT auxapli.cantusada * ;
					((compins.costunit / compins.equivale) + compins.sobreconta/compins.cantins) AS ;
					valor FROM  sacfadb!auxapli inner JOIN sacfadb!invenini ;
					ON  auxapli.codcomp = invenini.codinv WHERE compins.codcomp ;
					= ccomp AND auxapli.codinsap = coins INTO CURSOR cost22

				costins = cost22.valor
				valocost = costins + valocost

			ENDIF
			SELECT auxapli
			GO g_actual
			Skip
		ENDDO
		costins = valocost
		SELECT tipinsu  &&/// tipinsu....
		SEEK ctipo
		s_ctipo = ALLTRIM(STR(ctipo))

		SELECT cpdtemp
		SEEK s_ctipo

		IF !FOUND()
			APPEND BLANK

			REPLACE cpdtemp.codcontabl WITH s_ctipo
			REPLACE cpdtemp.detalle WITH "SALIDA INV.-" + LEFT(tipinsu.tipinsumo,10) + " " + detal
			REPLACE cpdtemp.codcc WITH g_codcc
			REPLACE cpdtemp.numcontrol WITH g_numcontrol
			*REPLACE CPDTEMP.codprov with g_codprov
		ENDIF

		SELECT  cpdtemp
		*REPLACE CPDTEMP.VALOR WITH round(CPDTEMP.VALOR + costins,0)
		REPLACE cpdtemp.valor WITH cpdtemp.valor + costins

		valtodos = valtodos + costins  &&// para todo el costo de insumo

		SELECT auxapli
		SET FILTER TO auxapli.codinsap = insaplica.codinsap
		GO TOP

		IF SELECT("INVENINI.DBF") <> 0
			SELECT invenini
			SET ORDER TO 1
		ENDIF

		SELECT insaplica
		GO g_actual1
		Skip
	ENDDO

	SET FILTER TO insaplica.codaplic = g_aplica
	GO TOP

	***************** PARA LOS COSTOS DE PRODUCCION - COSTOS INDIRECTOS ******************
	*****************************************************************************

	SELECT * FROM cpdtemp INTO TABLE auxcpd1

	*****************************************************************************
	***** COLOCA EL CODIGO CONTABLE, DB o CR, A TIPOS DE INSUMO -SI SE PUEDE *******
	************** DESCARGA INVENTARIOS *********
	*****************************************************************************
	SELECT cpdtemp
	SET FILTER TO
	GO TOP

	DO WHILE !EOF()
		g_actual = RECNO()
		SELECT tipinsu
		SEEK VAL(ALLTRIM(cpdtemp.codcontabl))

		IF VAL(tipinsu.cuenta) <> 0   &&// DEFINIDA LA CUENTA Y DB  o CR
			REPLACE cpdtemp.codcontabl WITH  tipinsu.cuenta

			IF tipinsu.nat1 = "Debito" &&//  DESCARGA -  INVERSO - SALIDA
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			*REPLACE CPDTEMP.codprov with g_codprov
		ELSE
			REPLACE cpdtemp.codcontabl WITH SPAC(10)
		ENDIF

		SELECT cpdtemp
		GO g_actual
		Skip

	ENDDO

	*************** CONTRAPARTIDAS A LAS SALIDAS DE INVENTARIOS *****************
	***************** COSTOS DE PRODUCCION - COSTOS INDIRECTOS ******************
	*****************************************************************************
	SELECT cpdtemp
	APPEND FROM auxcpd1
	SET FILTER TO
	SET FILTER TO LEN(ALLTRIM(cpdtemp.codcontabl)) =< 2
	GO TOP

	DO WHILE !EOF()

		SELECT tipinsu
		SEEK VAL(ALLTRIM(cpdtemp.codcontabl))

		parte = "COSTO " + LEFT(tipinsu.tipinsumo,10) + " " + detal
		REPLACE cpdtemp.detalle WITH parte

		IF VAL(tipinsu.costoinsu) <> 0   &&// DEFINIDA LA CUENTA Y DB  o CR
			REPLACE cpdtemp.codcontabl WITH  tipinsu.costoinsu

			IF tipinsu.nat3 = "Debito" &&//  DESCARGA -  INVERSO - SALIDA
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF
			*REPLACE CPDTEMP.codprov with g_codprov
		ELSE
			REPLACE cpdtemp.codcontabl WITH SPAC(10)
		ENDIF

		SELECT cpdtemp
		IF !EMPTY(cpdtemp.codcontabl)
			GO TOP
		ELSE
			Skip
		ENDIF

	ENDDO

	USE IN 248
	USE IN contador

	g_costo = valtodos &&&Para que sume en el campo sumacosto Total de Insumos

	RETURN

	**************************************************************************
	**************************************************************************
	********  ELABORA COMPROBANTE CONTABLE - CONTRATO DE SERVICIOS ***********
PROCEDURE hacecpct
	****************************************************************************
	tipomodo = "CONTRATO"
	quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))
	detal = "CONTRATO" + "  " + ALLTRIM(g_nomlab) + "  " + quecp

	SELECT 248
	USE cpdtemp

	********************* CUENTAS POR PAGAR A CONTRATISTAS **********************
	*****************************************************************************
	SELECT 246
	USE contador
	SET ORDER TO  codigo

	SEEK 6  &&// CUENTAS POR PAGAR CONTRATISTAS

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH valtotal
	REPLACE cpdtemp.detalle WITH "CxP a " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) +" "+ detal
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	REPLACE cpdtemp.codprov WITH g_codprov

	IF VAL(contador.cuenta) <> 0  &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH  contador.cuenta

		IF contador.natural = "Debito"   &&// DEBITO o CREDITO *********
			REPLACE cpdtemp.db WITH valtotal
		ELSE
			REPLACE cpdtemp.cr WITH valtotal
		ENDIF
		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
	ELSE
		REPLACE cpdtemp.guia WITH  contador.guia
		REPLACE cpdtemp.ft1 WITH  LEFT(contador.guia,2)
	ENDIF

	REPLACE cpdtemp.subcta WITH contador.modoct
	REPLACE cpdtemp.codprovsb WITH labculti.codprov

	DO CASE
		CASE g_codlab = 901 AND g_arriendo = 1 &&&&ARRIENDO PAGOS ANTICIPADO

			SELECT contador
			SEEK 31  &&// pago x anticipado

			SELECT cpdtemp
			APPEND BLANK

			REPLACE cpdtemp.valor WITH valtotal
			REPLACE cpdtemp.detalle WITH "PAGO X ANTICIPADO " + ;
				PROPER(ALLTRIM(LEFT(g_nomprov,15))) +" "+ alltrim(detal)+"  CH-" +  g_numcheq
			REPLACE cpdtemp.codcc WITH g_codcc
			REPLACE cpdtemp.numcontrol WITH g_numcontrol
			REPLACE cpdtemp.codprov WITH g_codprov

			IF VAL(contador.cuenta) <> 0  &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
				REPLACE cpdtemp.codcontabl WITH  contador.cuenta

				IF contador.natural = "Debito"   &&// DEBITO o CREDITO *********
					REPLACE cpdtemp.db WITH valtotal
				ELSE
					REPLACE cpdtemp.cr WITH valtotal
				ENDIF
				REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			ELSE
				REPLACE cpdtemp.guia WITH  contador.guia
				REPLACE cpdtemp.ft1 WITH  LEFT(contador.guia,2)
			ENDIF

			USE IN 246
		CASE g_codlab = 803  &&&&GASTOS DE ADMINISTRACION

			SELECT contador
			SEEK 49  &&// GASTOS ADMINISTRACION

			SELECT cpdtemp
			APPEND BLANK

			REPLACE cpdtemp.valor WITH valtotal
			REPLACE cpdtemp.detalle WITH "GASTOS " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) +" "+ detal
			REPLACE cpdtemp.codcc WITH g_codcc
			REPLACE cpdtemp.numcontrol WITH g_numcontrol
			REPLACE cpdtemp.codprov WITH g_codprov

			IF VAL(contador.cuenta) <> 0  &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
				REPLACE cpdtemp.codcontabl WITH  contador.cuenta

				IF contador.natural = "Debito"   &&// DEBITO o CREDITO *********
					REPLACE cpdtemp.db WITH valtotal
				ELSE
					REPLACE cpdtemp.cr WITH valtotal
				ENDIF
				REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			ELSE
				REPLACE cpdtemp.guia WITH  contador.guia
				REPLACE cpdtemp.ft1 WITH  LEFT(contador.guia,2)
			ENDIF

			USE IN 246

		OTHERWISE

			USE IN 246
			**************** CONTRAPARTIDA EN COSTOS DE PRODUCCION***********************
			DO costoconta WITH valtotal,"COSCUL" && // COSTO DE PROD.
			*****************************************************************************

	ENDCASE


	USE IN 248

	RETURN

	****************************************************************************
	****************************************************************************
	********  ELABORA COMPROBANTE CONTABLE - CREDITOS ... ***********
	****************************************************************************
	*  -CAJA
	*     contra:
	*  -CxP  / BANCOS /CORPORACIONES ... .... (manual)
	****************************************************************************
PROCEDURE hacecpcr
	****************************************************************************
	SELECT 246
	USE contador
	SET ORDER TO  codigo
	SEEK 20  &&///  CUENTA DE BANCO

	*** EL CREDITO ENTRA A BANCO .......


	SELECT 248
	USE cpdtemp

	SELECT cpdtemp
	APPEND BLANK
	REPLACE cpdtemp.valor WITH g_valcred
	REPLACE cpdtemp.detalle WITH "ING. CREDITO " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) + credito.numcred
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH -1

	IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH  contador.cuenta
		IF contador.natural = "Debito"  &&&// DEBITO o CREDITO *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF
		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
	ELSE
		REPLACE cpdtemp.guia WITH contador.guia
		REPLACE cpdtemp.ft1 WITH  LEFT(contador.guia,2)
	ENDIF

	IF g_gascred != 0

		SELECT contador
		SEEK 30  &&///  GASTOS DEL CREDITO

		SELECT cpdtemp
		APPEND BLANK
		REPLACE cpdtemp.valor WITH g_gascred
		REPLACE cpdtemp.detalle WITH "GASTOS CREDITO " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) + credito.numcred
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH -1

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH  contador.cuenta
			IF contador.natural = "Debito"  &&&// DEBITO o CREDITO *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF
			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ELSE
			REPLACE cpdtemp.guia WITH contador.guia
			REPLACE cpdtemp.ft1 WITH  LEFT(contador.guia,2)
		ENDIF

	ENDIF

	USE IN 246

	************* Seleccionar ENTIDAD BANCO, CORPORACION , ETC...****************
	*****************************************************************************
	SELECT cpdtemp
	APPEND BLANK
	REPLACE cpdtemp.valor WITH valtotal
	REPLACE cpdtemp.detalle WITH "CxP Crédito "+ PROPER(ALLTRIM(LEFT(g_nomprov,15)))+"/" + credito.numcred
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH -1
	REPLACE cpdtemp.codprov WITH g_codprov
	REPLACE cpdtemp.cr WITH cpdtemp.valor
	REPLACE cpdtemp.ft1 WITH "21"

	USE IN 248

	RETURN

	*****************************************************************************
	*****************************************************************************
	********  ELABORA COMPROBANTE CONTABLE - VENTA DE PRODUCCION CULTIVO *********
PROCEDURE hacecpvt
	*****************************************************************************

	SELECT 248
	USE cpdtemp

	SELECT 246
	USE contador
	SET ORDER TO  codigo

	quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))

	*********************** CUENTAS POR COBRAR A CLIENTES ***********************
	*****************************************************************************

	SEEK 14  &&// CUENTAS POR COBRAR CLIENTES

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH cxc
	REPLACE cpdtemp.detalle WITH "CxC-Produccion a "+ PROPER(ALLTRIM(LEFT(g_nomemp,15))) +" " + quecp
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	*REPLACE CPDTEMP.codprov WITH g_codemp
	REPLACE cpdtemp.codprov with g_codprov

	IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH contador.cuenta

		IF contador.natural = "Debito"   &&// DEBITO o CREDITO *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF

		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
	ELSE
		REPLACE cpdtemp.guia WITH  contador.guia
		REPLACE cpdtemp.ft1 WITH  LEFT(contador.guia,2)
	ENDIF

	REPLACE cpdtemp.subcta WITH contador.modoct
	REPLACE cpdtemp.codprovsb WITH ventas.codprov


	*************************** V E N T A S  ***********************************
	*****************************************************************************

	SELECT cultivo
&&&// VENTAS

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH vtavta
	REPLACE cpdtemp.detalle WITH "Venta de Prod. " + quecp
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	REPLACE cpdtemp.codprov with g_codprov

	IF VAL(cultivo.ingven) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH cultivo.ingven

		IF cultivo.nat3 = "Debito"   &&// DEBITO o CREDITO *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF

		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
	ELSE
		REPLACE cpdtemp.guia WITH  contador.guia
		REPLACE cpdtemp.ft1 WITH  LEFT(contador.guia,2)
	ENDIF

	***************************** RETEFUENTE  ***********************************
	*****************************************************************************

	IF rterte <> 0
		SELECT contador
		SEEK 15  &&// RETEFUENTE

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH rterte
		REPLACE cpdtemp.detalle WITH "R/fuente Venta Prod. "+quecp
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol
		REPLACE cpdtemp.codprov with g_codprov

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Debito"   &&// DEBITO o CREDITO *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF
			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ELSE
			REPLACE cpdtemp.guia WITH  contador.guia
			REPLACE cpdtemp.ft1 WITH  LEFT(contador.guia,2)
		ENDIF

	ENDIF

	*****************************************************************************
	****** CONTRAPARTIDA EN GASTOS PORQUE EN VENTA YA NO SE CARGA NADA MAS ********
	****** A LOS COSTOS YA QUE SE HACE LA SALIDA DE COSTO CONTABLE
	*****************************************************************************

	detal = "Cuota Fomento "

	IF costo <> 0
		SELECT contador
		SEEK 16  &&// COSTO DE CUOTA FOMENTO

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH cuota
		REPLACE cpdtemp.detalle WITH "GASTO "+ detal + quecp
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol
		REPLACE cpdtemp.codprov with g_codprov

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta
			IF contador.natural = "Debito"   &&// DEBITO o CREDITO *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF
			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF

	ENDIF

	************************************************************
	***************** GASTOS DE OTROS DESCUENTOS  **************
	************************************************************
	detal = "-Otros Descuentos "

	IF otodes <> 0
		SELECT contador
		SEEK 17  &&// COSTO DE OTROS DESCUENTOS

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH otodes
		REPLACE cpdtemp.detalle WITH "GASTO "+ detal + quecp
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol
		REPLACE cpdtemp.codprov with g_codprov

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta
			IF contador.natural = "Debito"   &&// DEBITO o CREDITO *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF
			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF

	************************  COSTO DE VENTA  **********************************
	*****************************************************************************

	SELECT contador
	SEEK 13  &&&// COSTO DE VENTA

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH cstovta
	REPLACE cpdtemp.detalle WITH "Costo Venta Prod. " + quecp
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	REPLACE cpdtemp.codprov with g_codprov

	IF VAL(cultivo.cosven) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH cultivo.cosven

		IF cultivo.nat2 = "Debito"   &&// DEBITO o CREDITO *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF
		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
	ENDIF

	USE IN 246

	*****************************************************************************
	*CONTRAPARTIDA AL COSTO DE VENTAS-DESCARGUE Al costo CULTIVOS EN DESARROLLO*
	*SALIDA DEL CULTIVO LO QUE CUESTA TODO EL DESARROLLO DE CULTIVOS Y QUE GENERAN
	*COTOS
	*****************************************************************************

	yaesta = SELECT("CULTIVO")

	IF yaesta <> 0
		SELECT (yaesta)
		SET ORDER TO 1
	ELSE
		SELECT 246
		USE cultivo
		SET ORDER TO 1
	ENDIF

	SEEK g_codcul

	cta = "Cultivo.salicult"
	nat = "Cultivo.nat4"

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH cstovta
	REPLACE cpdtemp.detalle WITH "Salida Cultivo " + quecp
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	REPLACE cpdtemp.codprov with g_codprov

	IF VAL(&cta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH &cta

		IF cultivo.nat4 = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF

		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
	ENDIF

	IF yaesta = 0
		USE IN 246
	ENDIF


	USE IN 248

	RETURN


	*****************************************************************************
	*****************************************************************************
	********  ELABORA COMPROBANTE CONTABLE - MANTENIMIENTO MAQUINARIA . *********
	*****************************************************************************
	* -CxP PROVEEDORES (A credito)
	*     CONTRA
	* - COSTOS INDIRECTOS GENERALES - -  pedir confirmacion.....
	*****************************************************************************
PROCEDURE hacecpmm
	*****************************************************************************

	SELECT 248
	USE cpdtemp
	SET ORDER TO 1

	********************** CONTABILIZACION   C x P ******************************
	*****************************************************************************

	SELECT 246
	USE contador
	SET ORDER TO  codigo
	aquien = ALLTRIM(g_nommaq)+" / " + ALLTRIM(maquinas.seriemaq)
	******** EL TOTAL NETO, CREDITO , DE mantenimiento SE CONTABILIZA:***********
	*****************************************************************************

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH valtotal   &&&&Maqmant.COSTOMANT

	SELECT contador
	SEEK 7  &&//  2(Credito) - CUENTAS POR PAGAR PROVEEDORES-

	SELECT cpdtemp
	REPLACE cpdtemp.detalle WITH "CxP " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) + " MANT. A: " + aquien
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	REPLACE cpdtemp.codprov WITH g_codprov

	IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH  contador.cuenta

		IF contador.natural = "Debito"   &&// DEBITO o CREDITO *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF

		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)

	ENDIF

	REPLACE cpdtemp.subcta WITH contador.modoct
	REPLACE cpdtemp.codprovsb WITH maqmant.codprov

	***************  CONTRA ...  POSIBLES  COSTOS INDIRECTOS *******************
	************************ SELECCIONAR CUENTA .... ***************************
	*****************************************************************************

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH valtotal &&&&Maqmant.COSTOMANT

	REPLACE cpdtemp.detalle WITH "Costo Mantenimiento: " + vdetalle
	REPLACE cpdtemp.db WITH cpdtemp.valor
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol

	USE IN 248
	USE IN 246

	RETURN

	*****************************************************************************
	*****************************************************************************
	***** ELABORA COMPROBANTE CONTABLE DE --- USO DE INSUMOS fuera CULTIVOS *****
	*****************************************************************************
PROCEDURE hacecpufi
	*****************************************************************************

	SELECT 248
	USE cpdtemp
	SET ORDER TO 1

	IF UPPER(qllama) = "UFC"
		detal = "Usado en: " + LEFT(ufc.motufc,32)
	ELSE
		quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))
		detal = "Usado en: "  + g_nommaq + "/" + ALLTRIM(g_serie) + " " + quecp
	ENDIF


	** CONTABILIZACION DE USOS POR GRUPO DE INSUMOS  (DISMINUCION INVENTARIOS) **
	*****************************************************************************
	IF UPPER(qllama) = "UFC"
		SELECT auxufc
		SET FILTER TO auxufc.codufc = g_codufc
		GO TOP
		scatter memvar
		if !used("TEMPAUX")
			COPY STRUCTURE TO tempaux
			USE tempaux IN 0
		endif
		SELECT tempaux
		append blank
		gather memvar
		costins = 0  &&// costo del insumo
		ctipo = ufc.codtipo && // codigo tipo de insumo
		*!*		ccomp   = Auxufc.codcomp
		*!*		coins   = Auxufc.CODUFC
		*!*		CANTUSADA = Auxufc.CANTUSADA
		coma1 = SELECT("COMPINS")
		inva1 = SELECT("INVENINI")
		DO WHILE !EOF()
			ccomp   = tempaux.codcomp
			coins   = tempaux.codufc
			g_actual = RECNO()

			IF tempaux.indica = 0 AND  coma1 <> 0

				SELECT tempaux.cantusada*((compins.costunit/compins.equivale) ;
					+ compins.sobrecosto/compins.cantins) AS valor  FROM  sacfadb!compins ;
					inner JOIN tempaux ON  compins.codcomp = tempaux.codcomp ;
					WHERE compins.codcomp = ccomp AND tempaux.codufc = coins ;
					INTO CURSOR cost22

				costins =  cost22.valor
			ENDIF
			IF tempaux.indica = 1 AND inva1 <> 0

				SELECT tempaux.cantusada * ;
					((compins.costunit / compins.equivale) + compins.sobreconta/compins.cantins) AS ;
					valor FROM  tempaux inner JOIN sacfadb!invenini ;
					ON  tempaux.codcomp = invenini.codinv WHERE compins.codcomp ;
					= ccomp AND tempaux.codufc = coins INTO CURSOR cost22
				costins = cost22.valor
			ENDIF
			*SELECT Auxufc
			SELECT tempaux
			GO g_actual
			Skip
		ENDDO
		rot = "Uso Fuera del Cultivo-"
	ELSE
		SELECT auxcomb
		SET FILTER TO auxcomb.codufc = g_codcomb
		GO TOP
		costins = 0  &&// costo del insumo
		ctipo = combust.codtipo && // codigo tipo de insumo
		ccomp = auxcomb.codcomp
		coins = auxcomb.codufc
		coma1 = SELECT("COMPINS")
		inva1 = SELECT("INVENINI")


		DO WHILE !EOF()
			g_actual = RECNO()

			IF auxcomb.indica = 0 AND  coma1 <> 0
				SELECT auxcomb.cantusada*((compins.costunit/compins.equivale) ;
					+ compins.sobrecosto/compins.cantins) AS valor  FROM  sacfadb!compins ;
					inner JOIN sacfadb!auxcomb ON  compins.codcomp = auxcomb.codcomp ;
					WHERE compins.codcomp = ccomp AND auxcomb.codufc = coins ;
					INTO CURSOR cost22

				costins =  cost22.valor
			ENDIF
			IF auxcomb.indica = 1 AND  inva1 <> 0

				SELECT auxcomb.cantusada * ;
					((compins.costunit / compins.equivale) + compins.sobreconta/compins.cantins) AS ;
					valor FROM  sacfadb!auxcomb inner JOIN sacfadb!invenini ;
					ON  auxcomb.codcomp = invenini.codinv WHERE compins.codcomp ;
					= ccomp AND auxcomb.codufc = coins INTO CURSOR cost22

				costins = cost22.valor
			ENDIF
			SELECT auxcomb
			GO g_actual
			Skip
		ENDDO
		rot = "SALIDA INV.-"
	ENDIF

	SELECT tipinsu  &&/// tipinsu....
	SET ORDER TO 1
	SEEK ctipo

	SELECT cpdtemp
	SEEK ALLTRIM(STR(ctipo))

	IF !FOUND()
		APPEND BLANK
		REPLACE cpdtemp.codcontabl WITH STR(ctipo)
		REPLACE cpdtemp.detalle WITH rot + " " + LEFT(tipinsu.tipinsumo,10)
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol
	ENDIF

	REPLACE cpdtemp.valor WITH cpdtemp.valor + costins
	*REPLACE CPDTEMP.VALOR WITH round(CPDTEMP.VALOR + costins,0)

	IF UPPER(qllama) = "UFC"
		SELECT auxufc
		SET FILTER TO auxufc.codufc = g_codufc
		GO TOP
	ELSE
		SELECT auxcomb
		SET FILTER TO auxcomb.codufc = g_codcomb
		GO TOP
	ENDIF

	*** COLOCA EL CODIGO CONTABLE, DB o CR, A TIPOS DE INSUMO -SI SE PUEDE... ***
	**************************************************************************
	SELECT 248
	USE cpdtemp
	GO TOP
	DO WHILE !EOF()

		SELECT tipinsu
		SEEK VAL(cpdtemp.codcontabl)

		IF VAL(tipinsu.cuenta) <> 0   &&// DEFINIDA LA CUENTA Y DB  o CR
			REPLACE cpdtemp.codcontabl WITH  tipinsu.cuenta

			IF tipinsu.nat1 = "Debito" &&//  DESCARGA -  INVERSO - SALIDA
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			*REPLACE CPDTEMP.codprov with g_codprov
		ELSE
			REPLACE cpdtemp.codcontabl WITH SPAC(10)
		ENDIF

		SELECT cpdtemp
		Skip

	ENDDO
	*************** CONTRAPARTIDAS A LAS SALIDAS DE INVENTARIOS ****************
	************************* SELECCIONAR CUENTA .... **************************

	SELECT cpdtemp
	APPEND BLANK

	*REPLACE CPDTEMP.VALOR   WITH round(costins,0)
	REPLACE cpdtemp.valor   WITH costins
	REPLACE cpdtemp.detalle WITH detal
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	*REPLACE CPDTEMP.db      WITH round(costins,0)
	REPLACE cpdtemp.db  WITH costins
	REPLACE cpdtemp.guia    WITH "7301"

	***
	*********** Contrapartida en UFC

	IF UPPER(qllama) = "UFC"
		IF !used("contador")
			USE contador in 0
			SELECT contador
		ELSE
			SELECT contador
		ENDIF
		SET ORDER TO  codigo
		SEEK 50  &&///

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF

	***
	USE IN 248

	RETURN



	*****************************************************************************
	**************** CONTRAPARTIDA EN COSTOS DE PRODUCCION **********************
	*!*/ COSTO DE PROD. GASTO ADMON..
	DO costoconta WITH labculti.costotal,"COSCUL"  &&// COSTO DE PROD. GASTO ADMON..
	*****************************************************************************

	USE IN 248

	RETURN

	*****************************************************************************
	*****************************************************************************
	***********  ELABORA COMPROBANTE CONTABLE - USO MAQUINARIA PROPIA ***********
PROCEDURE hacecpmq
	*****************************************************************************
	PARAMETERS totalmaq

	SELECT 248
	USE cpdtemp

	quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))

	detal = ALLTRIM(g_nomlab) + " " + quecp

	SELECT 246
	USE contador
	SET ORDER TO  codigo


	IF depre != 0

		SEEK 24 &&& Depreciacion Acumulada

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH depre
		REPLACE cpdtemp.detalle WITH "DEPRECIACION MQ/PROPIA " + detal
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Credito"  &&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF

	IF imp != 0

		SELECT contador
		SEEK 25 &&& Impuestos Pagos por Anticipado

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH imp
		REPLACE cpdtemp.detalle WITH "IMPUESTOS MQ/PROPIA " + detal
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Credito"  &&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF

	IF inter1 != 0

		SELECT contador
		SEEK 26 &&& Intereses Pago por Anticipado

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH inter1
		REPLACE cpdtemp.detalle WITH "INTERESES MQ/PROPIA " + detal
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Credito"  &&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF

	IF repar != 0

		SELECT contador
		SEEK 27 &&& Provisión Reparación

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH repar
		REPLACE cpdtemp.detalle WITH "PROV. REPA. MQ/PROPIA " + detal
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Credito"  &&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF

	IF opera != 0

		SELECT contador
		SEEK 29 &&& Nomina para la parte del uso del empleado

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH opera
		REPLACE cpdtemp.detalle WITH "COSTO NOMINA MQ/PROPIA " + detal
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Dedito"  &&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF

	ENDIF

	***************** CONTRAPARTIDA EN COSTOS DE PRODUCCION*********************
	*****************************************************************************
	SUM db all to totalmaq
	IF totalmaq != 0

		SELECT contador
		SEEK 28 &&& Costos de Mecanizacion

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH totalmaq
		REPLACE cpdtemp.detalle WITH "COSTOS MECANIZACION MQ/PROPIA " + detal
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Dedito"  &&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF

	ENDIF

	USE IN 246
	USE IN 248
	RETURN


	*****************************************************************************
	*****************************************************************************
	********  ELABORA COMPROBANTE CONTABLE - FACTURA DE INSUMOS ... ***********
	*****************************************************************************
	*  -INVENTARIOS (+ FLETE) ..
	*     contra:
	*  -CxP PROVEEDORES (A credito)  o  CAJA / BANCOS /.... (A contado) -(manual)
	*  -DESCUENTOS
	*  -FINANCION
	*  -COMISION
	*  -IVA
	*****************************************************************************
PROCEDURE hacecp
	*****************************************************************************
	DECLARE cmp[4,2]
	PRIVATE cmp[4,2]

	SELECT 248
	USE cpdtemp
	SET ORDER TO 1
	****** CONTABILIZACION POR GRUPO DE INSUMOS (ENTRADA A INVENTARIOS) *********
	*****************************************************************************

	******** COLOCA VALORES Y TIPOS DE INSUMO QUE SE VA A CONTABILIZAR **********
	*****************************************************************************
	SELECT tipinsu
	SET ORDER TO 1

	SELECT compins
	SET ORDER TO 6
	SET FILTER TO compins.codfact = g_codfact
	GO TOP

	DO WHILE !EOF()
		g_actual = RECNO ()
		g_numfac = ALLTRIM(facins.numfact)
		*!*	        cuanto = ROUND((compins.CANTUNIT * compins.costunit) + ;
		*!*	            (compins.CANTINS * compins.SOBRECONTA),0)
		cuanto = (compins.total+compins.sobreconta)

		SELECT LEFT(ALLTRIM(tipinsu.tipinsumo),10) AS tipin FROM tipinsu ;
			WHERE tipinsu.codtipo = compins.codtipo INTO CURSOR tipo

		nomti = tipo.tipin

		SELECT COUNT(*)AS cuan FROM cpdtemp INTO CURSOR vcanti ;
			WHERE compins.codtipo = VAL(ALLTRIM(cpdtemp.codcontabl))

		canti = vcanti.cuan

		IF canti = 0 then
			INSERT INTO cpdtemp (codcontabl, detalle, codcc, numcontrol,codprov) ;
				VALUES (STR(compins.codtipo), ;
				"invent- " + nomti + " " + g_nomprov + " -FC." + g_numfac, ;
				g_codcc, g_numcontrol,g_codprov )
		ENDIF

		SELECT cpdtemp
		REPLACE cpdtemp.valor WITH cpdtemp.valor + cuanto

		SELECT compins
		GO g_actual
		Skip
	ENDDO
	***** COLOCA EL CODIGO CONTABLE, DB o CR, A TIPOS DE INSUMO -SI SE PUEDE.****
	*****************************************************************************

	SELECT 248
	USE cpdtemp
	GO TOP

	DO WHILE !EOF()

		SELECT tipinsu
		SEEK VAL(cpdtemp.codcontabl)

		IF VAL(tipinsu.cuenta) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR
			REPLACE cpdtemp.codcontabl WITH  tipinsu.cuenta

			IF tipinsu.nat1 = "Debito"    &&&// DEBITO o CREDITO **************
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF
			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ELSE
			REPLACE cpdtemp.codcontabl WITH SPAC(10)
		ENDIF

		SELECT cpdtemp
		Skip

	ENDDO
	SUM valor all to g_val_factu
	*************** CONTABILIZACION CONTRAPARTIDAS A INVENTARIOS ***************
	****************************************************************************

	SELECT 246
	USE contador
	SET ORDER TO  codigo


	******* EL TOTAL NETO, CREDITO O CONTADO, DE FACTURA SE CONTABILIZA: *******
	****************************************************************************

	SELECT cpdtemp
	APPEND BLANK

	*REPLACE CPDTEMP.VALOR WITH Facins.VALORTOT
	REPLACE cpdtemp.valor WITH g_val_factu

	IF g_forpago = 2     &&&// FACTURA A CREDITO ***********
		SELECT contador
		SEEK 7  &&&//  2(Credito) - CUENTAS POR PAGAR PROVEEDORES-

		SELECT cpdtemp
		REPLACE cpdtemp.detalle WITH "CxP " + PROPER(ALLTRIM(LEFT(g_nomprov,15)));
			+ " FACT. " + g_numfac
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH  contador.cuenta

			IF contador.natural = "Debito"   &&&// DEBITO o CREDITO *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF
			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ELSE
			REPLACE cpdtemp.guia WITH  contador.guia
			REPLACE cpdtemp.ft1 WITH  LEFT(contador.guia,2)
		ENDIF

		REPLACE cpdtemp.subcta WITH contador.modoct
		REPLACE cpdtemp.codprov WITH facins.codprov

	ELSE  &&&&// FACT. DE CONTADO - Seleccionar cja,bancos,obl. financieras (Tarj. credito)
		*************************************************

		REPLACE cpdtemp.detalle WITH "PAGO A " + ;
			PROPER(ALLTRIM(LEFT(g_nomprov,15))) + " FACT. " + Alltrim(facins.numfact) +"  CH-" + g_numcheq
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol
		SELECT contador
		SEEK 23  &&&///  CUENTA DE BANCO
		SELECT cpdtemp
		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR
			REPLACE cpdtemp.codcontabl WITH  contador.cuenta

			IF contador.natural = "Debito" &&&// DEBITO o CREDITO *** //naturaleza inversa .....
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ENDIF
		ENDIF

		REPLACE cpdtemp.ft1 WITH LEFT(contador.guia,2) &&// CUENTAS DE DISPONIBLE - CAJA Y BANCOS
		REPLACE cpdtemp.ft2 WITH "21" &&// CUENTAS DE OBLIG. FINANCIERAS-T.CREDITO
		REPLACE cpdtemp.codprov WITH facins.codprov

	ENDIF
	USE IN 248
	USE IN 246

	RETURN

	*****************************************************************************
	*****************************************************************************
	****************  ELABORA COMPROBANTE CONTABLE - PAGOS **********************
PROCEDURE hacecpag
	*****************************************************************************
	SELECT 208
	USE cpd

	SELECT 248
	USE cpdtemp

	SELECT 246
	USE contador
	SET ORDER TO codigo

	DO CASE
		CASE LEFT(ALLTRIM(detal1),2) = "23"
			v_bustab = "Facturas"

		CASE LEFT(ALLTRIM(detal1),2) = "22"
			v_bustab = "Credito"

		CASE LEFT(ALLTRIM(detal1),2) = "53"
			v_bustab = "OtroCre"
			ldetal = RIGHT(ALLTRIM(detal1),LEN(ALLTRIM(detal1))-2)

		CASE LEFT(ALLTRIM(detal1),2) = "21"
			v_bustab = "Contratistas"
	ENDCASE

	DO CASE
		CASE v_bustab = "Credito"

			SEEK 21 &&&& PAGO creditos
			SELECT detalle, codprov, codcontabl FROM cpd WHERE cpd.codcp = g_codcp ;
				AND LEFT(cpd.codcontabl,2)= "21" INTO CURSOR pdet
			g_codprov= pdet.codprov
		CASE v_bustab = "OtroCre"

			SEEK 48&&&& PAGO Intereses del credito y otros
			SELECT cpdtemp
			APPEND BLANK

			REPLACE cpdtemp.valor WITH vvalor
			REPLACE cpdtemp.detalle WITH RIGHT(ALLTRIM(detal1),LEN(ALLTRIM(detal1))-2)
			REPLACE cpdtemp.codcc WITH g_codcc
			REPLACE cpdtemp.numcontrol WITH g_numcontrol

			IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
				REPLACE cpdtemp.codcontabl WITH contador.cuenta

				IF contador.natural = "Debito"
					REPLACE cpdtemp.db WITH cpdtemp.valor
				ELSE
					REPLACE cpdtemp.cr WITH cpdtemp.valor
				ENDIF
				REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			ENDIF


		CASE v_bustab = "Facturas"

			SEEK 23 &&&& PAGO PROVEEDORES
			SELECT detalle, codprov, codcontabl FROM cpd WHERE cpd.codcp = g_codcp ;
				AND LEFT(cpd.codcontabl,2)= "22" INTO CURSOR pdet
			g_codprov= pdet.codprov	

		CASE v_bustab = "Contratistas"

			SEEK 22 &&&& PAGO CONTRATISTAS
			SELECT detalle, codprov, codcontabl FROM cpd WHERE cpd.codcp = g_codcp ;
				AND LEFT(cpd.codcontabl,2)= "23" INTO CURSOR pdet
			g_codprov= pdet.codprov	

	ENDCASE

	IF v_bustab != "OtroCre"


		INSERT INTO cpdtemp (valor, detalle, codcc, numcontrol, db, codprov, ;
			codcontabl) VALUES(vvalor, pdet.detalle, g_codcc, g_numcontrol, ;
			vvalor, g_codprov, pdet.codcontabl)

		***************  CONTRA ...  POSIBLES  Bancos, cajas ,etc.******************
		************************ SELECCIONAR CUENTA .... ***************************
		*****************************************************************************


		ldetal = "PAGO " + RIGHT(ALLTRIM(pdet.detalle), ;
			LEN(ALLTRIM(pdet.detalle))-3)+"  CH-"+ g_numcheq
	ELSE

		ldetal = "PAGO " + RIGHT(ALLTRIM(detal1),LEN(ALLTRIM(detal1))-2)+"  CH-"+ g_numcheq
		SELECT contador
		SEEK 22

	ENDIF


	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH vvalor
	REPLACE cpdtemp.detalle WITH ldetal
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol

	IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH contador.cuenta

		IF contador.natural = "Credito"
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF
		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		replace cpdtemp.codprov with g_codprov
	ENDIF

	USE IN 208
	USE IN 246
	USE IN 248

	RETURN

	****************************************************************************
	****************************************************************************
	************  ELABORA COMPROBANTE CONTABLE - PAGO DE NOMINA ****************
PROCEDURE haceplani
	****************************************************************************

	SELECT 248
	USE cpdtemp

	quecp = "  "+g_fechaplani
	detal = "Pago de "

	SELECT 246
	USE contador
	SET ORDER TO codigo

	FOR i= 1 TO  2
		SELECT contador
		IF i = 1
			SEEK 32  &&///  Nomina Administracion Auxilio de transporte
			v_auxtra = "g_nauxtra" + ALLTRIM(STR(i))
		ELSE
			SEEK 41  &&///  Nomina Produccion Auxilio de transporte
			v_auxtra = "g_nauxtra" + ALLTRIM(STR(i))
		ENDIF
		IF &v_auxtra > 0
			SELECT cpdtemp
			APPEND BLANK

			REPLACE cpdtemp.valor WITH &v_auxtra
			REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
			REPLACE cpdtemp.codcc WITH g_codcc
			REPLACE cpdtemp.numcontrol WITH g_numcontrol

			IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
				REPLACE cpdtemp.codcontabl WITH contador.cuenta

				IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
					REPLACE cpdtemp.db WITH cpdtemp.valor
				ELSE
					REPLACE cpdtemp.cr WITH cpdtemp.valor
				ENDIF

				REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			ENDIF
		ENDIF

		****** Alimentacion

		SELECT contador

		IF i=1
			SEEK 33  &&///  Nomina Administracion Auxilio de Alimentacion
			v_auxali = "g_nauxalim" + ALLTRIM(STR(i))
		ELSE
			SEEK 42  &&///  Nomina Produccion Auxilio de Alimentacion
			v_auxali = "g_nauxalim" + ALLTRIM(STR(i))
		ENDIF
		IF &v_auxali>0
			SELECT cpdtemp
			APPEND BLANK

			REPLACE cpdtemp.valor WITH  &v_auxali
			REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
			REPLACE cpdtemp.codcc WITH g_codcc
			REPLACE cpdtemp.numcontrol WITH g_numcontrol

			IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
				REPLACE cpdtemp.codcontabl WITH contador.cuenta

				IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
					REPLACE cpdtemp.db WITH cpdtemp.valor
				ELSE
					REPLACE cpdtemp.cr WITH cpdtemp.valor
				ENDIF

				REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			ENDIF
		ENDIF

		****** Sueldos

		SELECT contador

		IF i= 1
			SEEK 34  &&///  Nomina Administracion Sueldos
			v_auxsue = "g_nsueldo" + ALLTRIM(STR(i))
		ELSE
			SEEK 43  &&///  Nomina Produccion Sueldos
			v_auxsue = "g_nsueldo" + ALLTRIM(STR(i))
		ENDIF
		IF &v_auxsue>0
			SELECT cpdtemp
			APPEND BLANK

			REPLACE cpdtemp.valor WITH &v_auxsue
			REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
			REPLACE cpdtemp.codcc WITH g_codcc
			REPLACE cpdtemp.numcontrol WITH g_numcontrol

			IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
				REPLACE cpdtemp.codcontabl WITH contador.cuenta

				IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
					REPLACE cpdtemp.db WITH cpdtemp.valor
				ELSE
					REPLACE cpdtemp.cr WITH cpdtemp.valor
				ENDIF

				REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			ENDIF
		ENDIF
		****** Horas Extras y Recargos

		SELECT contador

		IF i= 1
			SEEK 35  &&///  Nomina Administracion horas extras y recargos
			v_auxner = "g_nhorrec" + ALLTRIM(STR(i))
		ELSE
			SEEK 44  &&///  Nomina Produccion horas extras y recargos
			v_auxner = "g_nhorrec" + ALLTRIM(STR(i))
		ENDIF
		IF &v_auxner>0
			SELECT cpdtemp
			APPEND BLANK

			REPLACE cpdtemp.valor WITH &v_auxner
			REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
			REPLACE cpdtemp.codcc WITH g_codcc
			REPLACE cpdtemp.numcontrol WITH g_numcontrol

			IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
				REPLACE cpdtemp.codcontabl WITH contador.cuenta

				IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
					REPLACE cpdtemp.db WITH cpdtemp.valor
				ELSE
					REPLACE cpdtemp.cr WITH cpdtemp.valor
				ENDIF

				REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			ENDIF
		ENDIF

		****** Otros Devengos

		SELECT contador

		IF i= 1
			SEEK 36  &&///  Nomina Administracion Otros devengos
			v_auxod = "g_notrdev" + ALLTRIM(STR(i))
		ELSE
			SEEK 45  &&///  Nomina Produccion Otros devengos
			v_auxod = "g_notrdev" + ALLTRIM(STR(i))
		ENDIF
		IF &v_auxod>0
			SELECT cpdtemp
			APPEND BLANK

			REPLACE cpdtemp.valor WITH &v_auxod
			REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
			REPLACE cpdtemp.codcc WITH g_codcc
			REPLACE cpdtemp.numcontrol WITH g_numcontrol

			IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
				REPLACE cpdtemp.codcontabl WITH contador.cuenta

				IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
					REPLACE cpdtemp.db WITH cpdtemp.valor
				ELSE
					REPLACE cpdtemp.cr WITH cpdtemp.valor
				ENDIF

				REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
			ENDIF
		ENDIF
	ENDFOR

	****** Otros Descuentos

	SELECT contador

	SEEK 37  &&///  Nomina Otros Descuentos

	IF g_ndescu>0
		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH g_ndescu
		REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF

	****** Salud

	SELECT contador

	SEEK 38  &&///  Nomina Salud

	IF g_nsalud>0
		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH g_nsalud
		REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF

	****** Pension

	SELECT contador

	SEEK 39  &&///  Nomina Pension

	IF g_npension>0
		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH g_npension
		REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF


	SELECT contador

	SEEK 40  &&///  Retefuente
	IF g_nretefuen > 0

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH g_nretefuen
		REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF


	****** Fdo de Solidaridad

	SELECT contador

	SEEK 46  &&///  Fdo de Solidaridad
	IF g_nfsolida>0

		SELECT cpdtemp
		APPEND BLANK

		REPLACE cpdtemp.valor WITH g_nfsolida
		REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
		REPLACE cpdtemp.codcc WITH g_codcc
		REPLACE cpdtemp.numcontrol WITH g_numcontrol

		IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
			REPLACE cpdtemp.codcontabl WITH contador.cuenta

			IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
				REPLACE cpdtemp.db WITH cpdtemp.valor
			ELSE
				REPLACE cpdtemp.cr WITH cpdtemp.valor
			ENDIF

			REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		ENDIF
	ENDIF
	*********** Contrapartida en Nómina por pagar

	SELECT contador

	SEEK 47  &&///  Nomina por pagar
	SELECT cpdtemp
	APPEND BLANK
	v_nomxpag=g_nsueldo1+g_nsueldo2+g_nhorrec1+g_nhorrec2+g_nauxtra1+g_nauxtra2+g_nauxalim1+g_nauxalim2;
		+g_notrdev1+g_notrdev2-(g_ndescu+g_nsalud+g_npension+g_nfsolida)
	REPLACE cpdtemp.valor WITH v_nomxpag
	REPLACE cpdtemp.detalle WITH detal + ALLTRIM(contador.concepto) + quecp
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol

	IF VAL(contador.cuenta) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH contador.cuenta

		IF contador.natural = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF

		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
	ENDIF



	**********Nomina por pagar
	USE IN 246
	USE IN 248

	RETURN


	****************************************************************************
	****************************************************************************
	************  ELABORA COMPROBANTE CONTABLE - TRASLADO DE VENTA ****************
PROCEDURE hacetrasv
	****************************************************************************

	SELECT 248
	USE cpdtemp

	quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))

	IF g_trasinv = 1
		
		detal = "Traslado a Inv. " + quecp
		select top 1 allt(codcontabl) as codcontabl from puc1 where upper(nombrecta)="ARROZ" and left(codcontabl,4)="1425";
				order by codcontabl into array ncuen
		if type("ncuen") ="U"		
			messagebox("Error cuenta no encontrada la contabilidad se corrompera comuniquese con asistencia Fedearroz",16,"Cuenta no encontrada")
		else
			ncuen = "14250501"
		endif
		SELECT codcontabl, SUM(db-cr) AS cr, codcc, numcontrol, codprov;
			FROM cpd INTO TABLE trscpd ;
			WHERE LEFT(cpd.codcontabl,1) = "7" AND numcontrol = g_numcontrol ;
			AND db != 0  ORDER BY codcontabl GROUP BY codcontabl

		SUM (trscpd.cr) TO s_trs

		SELECT cpdtemp
		APPEND FROM trscpd
		GO TOP

		REPLACE cpdtemp.detalle WITH detal ALL


	ELSE

		detal = "Traslado a Prod. Term. " + quecp
		
		select top 1 allt(codcontabl) as codcontabl from puc1 where upper(nombrecta)="ARROZ" and left(codcontabl,4)="1430";
				order by codcontabl into array ncuen
		if type("ncuen") ="U"		
			messagebox("Error cuenta no encontrada la contabilidad se corrompera comuniquese con asistencia Fedearroz",16,"Cuenta no encontrada")
		else
			ncuen = "14301501"
		endif

		SELECT codcontabl, SUM(db) AS cr, codcc, numcontrol, codprov;
			FROM cpd INTO TABLE trscpd ;
			WHERE LEFT(cpd.codcontabl,4) = "1425" AND numcontrol = g_numcontrol ;
			AND db != 0 ORDER BY codcontabl GROUP BY codcontabl

		SUM (trscpd.cr) TO s_trs

		SELECT cpdtemp
		APPEND FROM trscpd
		GO TOP

		REPLACE cpdtemp.detalle WITH detal ALL

	ENDIF


	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH s_trs
	REPLACE cpdtemp.detalle WITH detal
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	REPLACE cpdtemp.codcontabl WITH ncuen
	REPLACE cpdtemp.db WITH s_trs

	USE IN cpdtemp

	RETURN











	*****************************************************************************
	*///  DESDE AQUI --  FUNCIONES  GENERICAS , pueden usarse para todo/  -------
	*****************************************************************************






	****************************************************************************
	*creacion de COMPROBANTES AUTOMATICOS...
	****************************************************************************
FUNCTION comproa
	*****************************************************************************

	PARAMETERS cptipo,cpfecha,numero

	PRIVATE auxcod,aux,pant,ctapuc1,ya18

	********************  APERTURA DE ARCHIVOS CDP Y CPG ***********************
	*****************************************************************************

	IF !FILE("CPD.DBF")
		SELECT 100
		USE cpd
	ENDIF
	IF !FILE("CPG.DBF")
		SELECT 100
		USE cpg
	ENDIF
	*****************************************************************************
	ya18 = 0
	ya18 = SELECT("IPROV")

	IF ya18 = 0
		SELECT 18
		USE iprov
		ya18 = SELECT("IPROV")
		SET ORDER TO 0

	ENDIF

	*****************************************************************************
	SELECT 58
	USE puc
	SET ORDER TO 1

	yaesta = SELECT("CPG")

	IF yaesta <> 0
		SELECT (yaesta)
	ELSE
		SELECT 104
		USE cpg
	ENDIF

	****************** CALCULANDO NUMERO DE COMPROBANTE    **********************
	*****************************************************************************

	IF numero = 0
		SELECT COUNT(*) FROM cpg INTO ARRAY vtotal
		IF vtotal = 0 then
			auxcod = 1
		ELSE
			SELECT MAX(codcp) FROM cpg  ;
				INTO ARRAY valorcp
			auxcod = valorcp + 1
		ENDIF
	ELSE
		SELECT MAX(codcp) FROM cpg  ;
			INTO ARRAY valorcp
		auxcod = valorcp + 1
	ENDIF

	*****************************************************************************
	****************** CONFIRMACION DE CUENTAS POR PANTALLA *********************
	*****************************************************************************
	SELECT 248
	USE cpdtemp
	GO TOP
	DO WHILE !EOF()
		g_actual = RECNO()
		IF  cpdtemp.codcontabl = SPAC(10)
			rotcab = cpdtemp.detalle
			IF cpdtemp.codcontabl <> SPAC(10)
				donde = cpdtemp.codcontabl
			ELSE
				donde = cpdtemp.guia
			ENDIF
			*****************************************************************************
			**** Esta parte es con el formulario "DONCON" que pide las cuentas para *****
			**** Contabilizar
			sig1 = 1
			sig2 = 1
			rotulo = ALLTRIM(cpdtemp.detalle)
			valor  = cpdtemp.valor
			DO WHILE sig2 = 1
				DO FORM doncon.scx WITH sig1, rotulo, valor  TO sig2
			ENDDO
			****************************************************************************
		ENDIF
		SELECT cpdtemp
		GO g_actual
		Skip
	ENDDO
	USE IN 248
	*************** REGISTRANDO EL DETALLE DEL COMPROBANTE **********************
	IF SELECT("CPD") <> 0
		SELECT cpd

	ELSE
		SELECT 109
		USE cpd
	ENDIF
	***************** ADICIONANDO REGISTROS DEL TEMPORAL ************************
	APPEND FROM cpdtemp
	*****************************************************************************
	******************  COLOCANDO NUMERO DE COMPROBANTE **********************
	*******************  COLOCANDO FECHA DE COMPROBANTE **********************

	REPLACE cpd.fechacp WITH cpfecha FOR cpd.codcp = 0
	REPLACE cpd.codcp WITH auxcod FOR cpd.codcp = 0
	*****************************************************************************

	******************* SUMADO DEBITOS Y CREDITOS *******************************

	SUM cpd.db,cpd.cr TO sdbs,scrs FOR cpd.codcp = auxcod &&AND CPD.codprov = -20

	REPLACE cpd.codprov WITH 0 FOR cpd.codprov < 0
	*****************************************************************************

	******************  COLOCANDO CONSECUTIVO DE REGISTROS **********************
	*****************   Y ACTUALIZANDO MOVIMIENTOS A PUC1 ***********************

	SET ORDER TO 3
	GO BOTTOM
	aux = cpd.codcomp
	aux = aux + 1
	CLOSE INDEXES

	GO TOP
	LOCATE FOR cpd.codcomp = 0
	DO WHILE !EOF()

		REPLACE cpd.codcomp WITH aux
		aux = aux + 1
		Skip
	ENDDO
	******************************************************************************
	SET ORDER TO 4
	SET ORDER TO 2
	****************** REGISTRO GENERAL DEL COMPROBANTE *************************
	SELECT cpg
	APPEND BLANK
	REPLACE cpg.codcp WITH auxcod
	REPLACE cpg.codcc WITH g_codcc
	REPLACE cpg.numcontrol WITH g_numcontrol

	REPLACE cpg.fuente WITH cptipo
	REPLACE cpg.fecha WITH cpfecha
	REPLACE cpg.dbs WITH sdbs
	REPLACE cpg.crs WITH scrs
	REPLACE cpg.dfs WITH ABS(sdbs-scrs)
	*****************************************************************************
	*********************** BORRANDO DEL TEMPORAL *******************************
	SELECT 250
	USE cpdtemp EXCLUSIVE
	ZAP
	*****************************************************************************

	IF ya18 = 0
		USE IN 18
	ENDIF

	USE IN puc
	*    USE IN PUC1
	USE IN cpg
	USE IN cpd
	USE IN cpdtemp

	RETURN auxcod


	********** AUMENTA O DISMINUYE  EL COSTO CONTABLE, ACUMULADO ****************
	*****************************************************************************
FUNCTION sumacosto
	*****************************************************************************

	areact = SELECT()
	acucosto = 0

	SELECT SUM(db)AS tot FROM cpd WHERE codcc = g_codcc AND ;
		LEFT(cpd.codcontabl,6) = "143015" AND numcontrol = g_numcontrol ;
		INTO CURSOR coscontbl

	acucosto = (coscontbl.tot*vporc)/100


	SELECT (areact)
	RETURN acucosto
ENDFUNC

*****************************************************************************
****************** ELIMINA COMPROBANTE AUTOMATICO  **************************
*****************************************************************************
FUNCTION elimcp
	*****************************************************************************

	PARAMETERS numecp

	yaesta = SELECT("CPD")

	IF yaesta <> 0
		SELECT (yaesta)
	ELSE
		SELECT 109
		USE cpd
	ENDIF

	SET FILTER TO cpd.codcp = numecp
	GO TOP

	Delete FROM cpd WHERE cpd.codcp = numecp
	*!*	*CPD.codcp = numecp - 2 INTO ARRAY vcantidad  **LEFT(ALLTRIM(DETALLE),15) == "Traslado a Inv."
	**&&AND CPD.codcp = numecp - 2
	*!*	*!*				SELECT COUNT(*) FROM CPD WHERE codcc = g_codcc AND !deleted and ;
	*!*	*!*					numcontrol = g_numcontrol INTO ARRAY vcantidad
	*!*	* AND LEFT(ALLTRIM(DETALLE),16) == "Traslado a Prod."; *AND CPD.codcp = numecp - 1 INTO ARRAY vcantidad
	*!*				IF vcantidad = 0

	IF qtabla = "Venta"
		IF g_modo != "M"
			SELECT COUNT(*) FROM cpd WHERE codcc = g_codcc AND !deleted() and ;
				numcontrol = g_numcontrol and left(alltrim(cpd.detalle),5)=="Venta" INTO ARRAY vcantidad
			IF vcantidad = 0
				Delete FROM cpd WHERE codcc = g_codcc AND numcontrol = g_numcontrol and ;
					Left(ALLTRIM(detalle),15) == "Traslado a Inv."
				Delete FROM cpd WHERE codcc = g_codcc AND ;
					numcontrol = g_numcontrol  AND LEFT(ALLTRIM(detalle),16) == "Traslado a Prod."
&&CPD.codcp = numecp - 1
				*!*	*!*				ENDIF
			ENDIF
		ENDIF
	ENDIF

	yaesta = SELECT("CPG")

	IF yaesta <> 0
		SELECT (yaesta)
	ELSE
		SELECT 104
		USE cpg
	ENDIF


	Delete FROM cpg WHERE codcp = numecp
	IF g_modo != "M" &&es B
		IF qtabla = "Venta" AND vcantidad = 0

			Delete FROM cpg WHERE codcc = g_codcc AND;
				numcontrol = g_numcontrol AND fuente = -84 &&AND CPD.codcp = numecp - 1 OR CPD.codcp = numecp - 2

		ENDIF
	ENDIF

	USE IN cpd
	USE IN cpg
	*   USE IN PUC

	RETURN

	*****************************************************************************
	*****************************************************************************
	*************************** COSTOS DE PRODUCCION  ***************************
PROCEDURE costoconta
	*****************************************************************************
	PARAMETERS valct, cualct

	yaesta = SELECT("CULTIVO")

	IF yaesta <> 0
		SELECT (yaesta)
		SET ORDER TO 1
	ELSE
		SELECT 246
		USE cultivo
		SET ORDER TO 1
	ENDIF

	SEEK g_codcul

	*   //  VARIABLES PARA COSTO
	cta = "CULTIVO."+cualct
	nat = "CULTIVO.NAT1"

	SELECT cpdtemp
	APPEND BLANK

	REPLACE cpdtemp.valor WITH valct
	REPLACE cpdtemp.detalle WITH "COSTO "+ detal
	REPLACE cpdtemp.codcc WITH g_codcc
	REPLACE cpdtemp.numcontrol WITH g_numcontrol
	REPLACE cpdtemp.codprov WITH g_codprov

	IF VAL(&cta) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
		REPLACE cpdtemp.codcontabl WITH &cta

		IF &nat = "Debito"   &&&// DEBITO o CREDITO *********
			REPLACE cpdtemp.db WITH cpdtemp.valor
		ELSE
			REPLACE cpdtemp.cr WITH cpdtemp.valor
		ENDIF
		REPLACE cpdtemp.ft1 WITH  LEFT(cpdtemp.codcontabl,2)
		REPLACE cpdtemp.codprov with g_codprov
	ENDIF


	IF yaesta = 0
		USE IN 246
	ENDIF

	RETURN

