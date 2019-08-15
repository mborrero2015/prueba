****************************************************************************
*****************************************************************************
************  C O N T A B I L I D A D   D E S D E   A Q U I   ***************
*****************************************************************************
*****************************************************************************
*****  ELABORA COMPROBANTE CONTABLE - MANO DE OBRA JORNALES - EMPLEADOS *****
PROCEDURE HACECPJOEM
    *****************************************************************************

    PARAMETERS quemano

    SELECT 248
    USE CPDTEMP

    quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))
    detal = ALLTRIM(g_nomlab)+ " " + quecp
    SELECT 246
    USE CONTADOR
    SET ORDER TO  Codigo

    SEEK 6  &&///  Cxp a Contratistas

    *  //  VARIABLE PARA CONTRAPARTIDA AL COSTO (SUPONE CONTRA >COSTOS INDIRECTOS)
    *  //  POR AQUI LA CTA. DE COSTOS INDIRECTOS SE VA ACREDITANDO - DISMINUYENDO.
    *  //  SE VA DISMINUYENDO PORQUE ALLI SE HAN IDO COLOCANDO TODOS LOS gastos
    *  //  QUE SON GENERALES A TODOS LOS LOTES O CULTIVOS Y  QUE EL PROGRAMA
    *  //  PRORRATEA, ESTIMA, ETC . AQUI SON:HORAS EMPLEADOS O/Y JORNALES.

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH Jorculti.COSTOTAL
    REPLACE CPDTEMP.DETALLE WITH quemano+" "+detal
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol
    REPLACE CPDTEMP.codprov WITH g_codprov

    IF VAL(CONTADOR.CUENTA) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

        IF ALLTRIM(CONTADOR.NATURAL) = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF

        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ENDIF
    
    ***************** CONTRAPARTIDA EN COSTOS DE PRODUCCION *********************
    SELECT CONTADOR
    SEEK 8  

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH Jorculti.COSTOTAL
    REPLACE CPDTEMP.DETALLE WITH "COSTO"+" "+detal
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol
    REPLACE CPDTEMP.codprov WITH g_codprov

    IF VAL(CONTADOR.CUENTA) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

        IF ALLTRIM(CONTADOR.NATURAL) = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF
        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ENDIF
 
    USE IN 246
    USE IN 248
    RETURN

    *****************************************************************************
    *****************************************************************************
    *******  ELABORA COMPROBANTE CONTABLE DE --- USO DE INSUMOS EN CULTIVOS ***
PROCEDURE HACECPUI
    *****************************************************************************

    IF SELECT("Contador") = 0
        USE CONTADOR IN 0
    ENDIF
    SELECT CONTADOR
    SET ORDER TO Codigo

    IF SELECT("Tipinsu") <> 0
        SELECT tipinsu
    ELSE
        SELECT 200
        USE tipinsu
    ENDIF
    SET ORDER TO 1


    SELECT 248
    USE CPDTEMP
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


    SELECT Insaplica
    SET FILTER TO Insaplica.CODAPLIC = g_aplica
    GO TOP

    DO WHILE !EOF()
        coma1 = SELECT("COMPINS")
        inva1 = SELECT("INVENINI")
        g_actual1 = RECNO()

        IF inva1 <> 0
            SELECT INVENINI
            SET ORDER TO 2
        ENDIF

        SELECT Auxapli
        SET FILTER TO Auxapli.CODINSAP = Insaplica.CODINSAP
        GO TOP

        costins = 0  &&// costo del insumo
        ctipo   = Auxapli.codtipo   &&// codigo tipo de insumo
        ccomp   = Auxapli.codcomp
        coins   = Auxapli.CODINSAP
        valocost = 0

        DO WHILE !EOF()
            g_actual = RECNO()
            ccomp   = Auxapli.codcomp
            IF Auxapli.INDICA = 0 AND coma1 <> 0

                SELECT Auxapli.cantusada*((compins.costunit/compins.equivale) ;
                    + compins.sobrecosto) AS VALOR  FROM  sacfadb!compins ;
                    INNER JOIN sacfadb!Auxapli ON  compins.codcomp = Auxapli.codcomp ;
                    WHERE compins.codcomp = ccomp AND Auxapli.CODINSAP = coins ;
                    INTO CURSOR Cost22

                costins =  ROUND(Cost22.VALOR,5)
                valocost = costins + valocost

            ENDIF

            IF Auxapli.INDICA = 1 AND inva1 <> 0
                SELECT Auxapli.cantusada * ;
                    ((compins.costunit / compins.equivale) + compins.SOBRECONTA) AS ;
                    VALOR FROM  sacfadb!Auxapli INNER JOIN sacfadb!INVENINI ;
                    ON  Auxapli.codcomp = INVENINI.codinv WHERE compins.codcomp ;
                    = ccomp AND Auxapli.CODINSAP = coins INTO CURSOR Cost22

                costins = ROUND(Cost22.VALOR,5)
                valocost = costins + valocost

            ENDIF
            SELECT Auxapli
            GO g_actual
            SKIP
        ENDDO
        costins = valocost
        SELECT tipinsu  &&/// tipinsu....
        SEEK ctipo
        s_ctipo = ALLTRIM(STR(ctipo))

        SELECT CPDTEMP
        SEEK s_ctipo

        IF !FOUND()
            APPEND BLANK

            REPLACE CPDTEMP.CODCONTABL WITH s_ctipo
            REPLACE CPDTEMP.DETALLE WITH "SALIDA INV.-" + LEFT(tipinsu.TIPINSUMO,10) + " " + detal
            REPLACE CPDTEMP.codcc WITH g_codcc
            REPLACE CPDTEMP.numcontrol WITH g_numcontrol
        ENDIF

        SELECT  CPDTEMP
        REPLACE CPDTEMP.VALOR WITH CPDTEMP.VALOR + costins

        valtodos = valtodos + costins  &&// para todo el costo de insumo

        SELECT Auxapli
        SET FILTER TO Auxapli.CODINSAP = Insaplica.CODINSAP
        GO TOP

        IF SELECT("INVENINI.DBF") <> 0
            SELECT INVENINI
            SET ORDER TO 1
        ENDIF

        SELECT Insaplica
        GO g_actual1
        SKIP
    ENDDO

    SET FILTER TO Insaplica.CODAPLIC = g_aplica
    GO TOP

    ***************** PARA LOS COSTOS DE PRODUCCION - COSTOS INDIRECTOS ******************
    *****************************************************************************

    SELECT * FROM CPDTEMP INTO TABLE auxcpd1

    *****************************************************************************
    ***** COLOCA EL CODIGO CONTABLE, DB o CR, A TIPOS DE INSUMO -SI SE PUEDE *******
    ************** DESCARGA INVENTARIOS *********
    *****************************************************************************
    SELECT CPDTEMP
    SET FILTER TO
    GO TOP

    DO WHILE !EOF()
        g_actual = RECNO()
        SELECT tipinsu
        SEEK VAL(ALLTRIM(CPDTEMP.CODCONTABL))

        IF VAL(tipinsu.CUENTA) <> 0   &&// DEFINIDA LA CUENTA Y DB  o CR
            REPLACE CPDTEMP.CODCONTABL WITH  tipinsu.CUENTA

            IF tipinsu.NAT1 = "Debito" &&//  DESCARGA -  INVERSO - SALIDA
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ELSE
            REPLACE CPDTEMP.CODCONTABL WITH SPAC(10)
        ENDIF

        SELECT CPDTEMP
        GO g_actual
        SKIP

    ENDDO

    *************** CONTRAPARTIDAS A LAS SALIDAS DE INVENTARIOS *****************
    ***************** COSTOS DE PRODUCCION - COSTOS INDIRECTOS ******************
    *****************************************************************************
    SELECT CPDTEMP
    APPEND FROM auxcpd1
    SET FILTER TO
    SET FILTER TO LEN(ALLTRIM(CPDTEMP.CODCONTABL)) =< 2
    GO TOP

    DO WHILE !EOF()

        SELECT tipinsu
        SEEK VAL(ALLTRIM(CPDTEMP.CODCONTABL))

        PARTE = "COSTO " + LEFT(tipinsu.TIPINSUMO,10) + " " + detal
        REPLACE CPDTEMP.DETALLE WITH PARTE

        IF VAL(tipinsu.costoinsu) <> 0   &&// DEFINIDA LA CUENTA Y DB  o CR
            REPLACE CPDTEMP.CODCONTABL WITH  tipinsu.costoinsu

            IF tipinsu.nat3 = "Debito" &&//  DESCARGA -  INVERSO - SALIDA
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ENDIF
        ELSE
            REPLACE CPDTEMP.CODCONTABL WITH SPAC(10)
        ENDIF

        SELECT CPDTEMP
        IF !EMPTY(CPDTEMP.CODCONTABL)
            GO TOP
        ELSE
            SKIP
        ENDIF

    ENDDO

    USE IN 248
    USE IN CONTADOR

    g_costo = valtodos &&&Para que sume en el campo sumacosto Total de Insumos

    RETURN

    **************************************************************************
    **************************************************************************
    ********  ELABORA COMPROBANTE CONTABLE - CONTRATO DE SERVICIOS ***********
PROCEDURE HACECPCT
    ****************************************************************************
    tipomodo = "CONTRATO"
    quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))
    detal = "CONTRATO" + "  " + ALLTRIM(g_nomlab) + "  " + quecp

    SELECT 248
    USE CPDTEMP

    ********************* CUENTAS POR PAGAR A CONTRATISTAS **********************
    *****************************************************************************
    SELECT 246
    USE CONTADOR
    SET ORDER TO  Codigo

    SEEK 6  &&// CUENTAS POR PAGAR CONTRATISTAS

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH valtotal
    REPLACE CPDTEMP.DETALLE WITH "CxP a " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) +" "+ detal
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol
    REPLACE CPDTEMP.codprov WITH g_codprov

    IF VAL(CONTADOR.CUENTA) <> 0  &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH  CONTADOR.CUENTA

        IF CONTADOR.NATURAL = "Debito"   &&// DEBITO o CREDITO *********
            REPLACE CPDTEMP.db WITH valtotal
        ELSE
            REPLACE CPDTEMP.cr WITH valtotal
        ENDIF
        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ELSE
        REPLACE CPDTEMP.GUIA WITH  CONTADOR.GUIA
        REPLACE CPDTEMP.FT1 WITH  LEFT(CONTADOR.GUIA,2)
    ENDIF

    REPLACE CPDTEMP.SUBCTA WITH CONTADOR.MODOCT
    REPLACE CPDTEMP.CODPROVSB WITH Labculti.codprov

    IF g_codlab = 901 AND g_arriendo = 1 &&&&ARRIENDO PAGOS ANTICIPADO

        SELECT CONTADOR
        SEEK 31  &&// pago x anticipado

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH valtotal
        REPLACE CPDTEMP.DETALLE WITH "PAGO X ANTICIPADO " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) +" "+ detal
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol
        REPLACE CPDTEMP.codprov WITH g_codprov

        IF VAL(CONTADOR.CUENTA) <> 0  &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH  CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito"   &&// DEBITO o CREDITO *********
                REPLACE CPDTEMP.db WITH valtotal
            ELSE
                REPLACE CPDTEMP.cr WITH valtotal
            ENDIF
            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ELSE
            REPLACE CPDTEMP.GUIA WITH  CONTADOR.GUIA
            REPLACE CPDTEMP.FT1 WITH  LEFT(CONTADOR.GUIA,2)
        ENDIF

        USE IN 246
    ELSE
        USE IN 246
        **************** CONTRAPARTIDA EN COSTOS DE PRODUCCION***********************
        DO COSTOCONTA WITH valtotal,"COSCUL" && // COSTO DE PROD.
        *****************************************************************************
    ENDIF


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
PROCEDURE HACECPCR
    ****************************************************************************
    SELECT 246
    USE CONTADOR
    SET ORDER TO  Codigo
    SEEK 20  &&///  CUENTA DE CAJA.....

    *** EL CREDITO ENTRA A BANCO .......


    SELECT 248
    USE CPDTEMP

    SELECT CPDTEMP
    APPEND BLANK
    REPLACE CPDTEMP.VALOR WITH g_valcred
    REPLACE CPDTEMP.DETALLE WITH "ING. CREDITO " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) + Credito.NUMCRED
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH -1

    IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH  CONTADOR.CUENTA
        IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF
        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ELSE
        REPLACE CPDTEMP.GUIA WITH CONTADOR.GUIA
        REPLACE CPDTEMP.FT1 WITH  LEFT(CONTADOR.GUIA,2)
    ENDIF

    SELECT CONTADOR
    SEEK 30  &&///  GASTOS DEL CREDITO

    SELECT CPDTEMP
    APPEND BLANK
    REPLACE CPDTEMP.VALOR WITH g_gascred
    REPLACE CPDTEMP.DETALLE WITH "GASTOS CREDITO " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) + Credito.NUMCRED
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH -1

    IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH  CONTADOR.CUENTA
        IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF
        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ELSE
        REPLACE CPDTEMP.GUIA WITH CONTADOR.GUIA
        REPLACE CPDTEMP.FT1 WITH  LEFT(CONTADOR.GUIA,2)
    ENDIF

    USE IN 246

    ************* Seleccionar ENTIDAD BANCO, CORPORACION , ETC...****************
    *****************************************************************************
    SELECT CPDTEMP
    APPEND BLANK
    REPLACE CPDTEMP.VALOR WITH valtotal
    REPLACE CPDTEMP.DETALLE WITH "CxP Crédito "+ PROPER(ALLTRIM(LEFT(g_nomprov,15)))+"/" + Credito.NUMCRED
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol
    REPLACE CPDTEMP.codprov WITH g_codprov
    REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
    REPLACE CPDTEMP.FT1 WITH "21"

    USE IN 248

    RETURN

    *****************************************************************************
    *****************************************************************************
    ********  ELABORA COMPROBANTE CONTABLE - VENTA DE PRODUCCION CULTIVO *********
PROCEDURE HACECPVT
    *****************************************************************************

    SELECT 248
    USE CPDTEMP

    SELECT 246
    USE CONTADOR
    SET ORDER TO  Codigo

    quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))

    *********************** CUENTAS POR COBRAR A CLIENTES ***********************
    *****************************************************************************

    SEEK 14  &&// CUENTAS POR COBRAR CLIENTES

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH cxc
    REPLACE CPDTEMP.DETALLE WITH "CxC-Produccion a "+ PROPER(ALLTRIM(LEFT(g_nomemp,15))) +" " + quecp
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol
    REPLACE CPDTEMP.codprov WITH g_codemp

    IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

        IF CONTADOR.NATURAL = "Debito"   &&// DEBITO o CREDITO *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF

        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ELSE
        REPLACE CPDTEMP.GUIA WITH  CONTADOR.GUIA
        REPLACE CPDTEMP.FT1 WITH  LEFT(CONTADOR.GUIA,2)
    ENDIF

    REPLACE CPDTEMP.SUBCTA WITH CONTADOR.MODOCT
    REPLACE CPDTEMP.CODPROVSB WITH Ventas.codprov


    *************************** V E N T A S  ***********************************
    *****************************************************************************

    SELECT Cultivo
&&&// VENTAS

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH vtavta
    REPLACE CPDTEMP.DETALLE WITH "Venta de Prod. " + quecp
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol

    IF VAL(Cultivo.ingven) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH Cultivo.ingven

        IF Cultivo.nat3 = "Debito"   &&// DEBITO o CREDITO *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF

        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ELSE
        REPLACE CPDTEMP.GUIA WITH  CONTADOR.GUIA
        REPLACE CPDTEMP.FT1 WITH  LEFT(CONTADOR.GUIA,2)
    ENDIF

    ***************************** RETEFUENTE  ***********************************
    *****************************************************************************

    IF rterte <> 0
        SELECT CONTADOR
        SEEK 15  &&// RETEFUENTE

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH rterte
        REPLACE CPDTEMP.DETALLE WITH "R/fuente Venta Prod. "+quecp
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito"   &&// DEBITO o CREDITO *********
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ELSE
            REPLACE CPDTEMP.GUIA WITH  CONTADOR.GUIA
            REPLACE CPDTEMP.FT1 WITH  LEFT(CONTADOR.GUIA,2)
        ENDIF

    ENDIF

    *****************************************************************************
    ****** CONTRAPARTIDA EN GASTOS PORQUE EN VENTA YA NO SE CARGA NADA MAS ********
    ****** A LOS COSTOS YA QUE SE HACE LA SALIDA DE COSTO CONTABLE
    *****************************************************************************

    detal = "Cuota Fomento "

    IF costo <> 0
        SELECT CONTADOR
        SEEK 16  &&// COSTO DE CUOTA FOMENTO

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH cuota
        REPLACE CPDTEMP.DETALLE WITH "GASTO "+ detal + quecp
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito"   &&// DEBITO o CREDITO *********
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF

    ENDIF

    ************************************************************
    ***************** GASTOS DE OTROS DESCUENTOS  **************
    ************************************************************
    detal = "-Otros Descuentos "

    IF otodes <> 0
        SELECT CONTADOR
        SEEK 17  &&// COSTO DE OTROS DESCUENTOS

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH otodes
        REPLACE CPDTEMP.DETALLE WITH "GASTO "+ detal + quecp
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito"   &&// DEBITO o CREDITO *********
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF

    ENDIF

    ************************  COSTO DE VENTA  **********************************
    *****************************************************************************

    SELECT CONTADOR
    SEEK 13  &&&// COSTO DE VENTA

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH cstovta
    REPLACE CPDTEMP.DETALLE WITH "Costo Venta Prod. " + quecp
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol

    IF VAL(Cultivo.cosven) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH Cultivo.cosven

        IF Cultivo.nat2 = "Debito"   &&// DEBITO o CREDITO *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF
        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
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
        USE Cultivo
        SET ORDER TO 1
    ENDIF

    SEEK g_codcul

    CTA = "Cultivo.salicult"
    NAT = "Cultivo.nat4"

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH cstovta
    REPLACE CPDTEMP.DETALLE WITH "Salida Cultivo " + quecp
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol

    IF VAL(&CTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH &CTA

        IF Cultivo.nat4 = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF

        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
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
PROCEDURE HACECPMM
    *****************************************************************************

    SELECT 248
    USE CPDTEMP
    SET ORDER TO 1

    ********************** CONTABILIZACION   C x P ******************************
    *****************************************************************************

    SELECT 246
    USE CONTADOR
    SET ORDER TO  Codigo
    aquien = ALLTRIM(g_nommaq)+" / " + ALLTRIM(Maquinas.seriemaq)
    ******** EL TOTAL NETO, CREDITO , DE mantenimiento SE CONTABILIZA:***********
    *****************************************************************************

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH valtotal   &&&&Maqmant.COSTOMANT

    SELECT CONTADOR
    SEEK 7  &&//  2(Credito) - CUENTAS POR PAGAR PROVEEDORES-

    SELECT CPDTEMP
    REPLACE CPDTEMP.DETALLE WITH "CxP " + PROPER(ALLTRIM(LEFT(g_nomprov,15))) + " MANT. A: " + aquien
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol
    REPLACE CPDTEMP.codprov WITH g_codprov

    IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH  CONTADOR.CUENTA

        IF CONTADOR.NATURAL = "Debito"   &&// DEBITO o CREDITO *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF

        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)

    ENDIF

    REPLACE CPDTEMP.SUBCTA WITH CONTADOR.MODOCT
    REPLACE CPDTEMP.CODPROVSB WITH Maqmant.codprov

    ***************  CONTRA ...  POSIBLES  COSTOS INDIRECTOS *******************
    ************************ SELECCIONAR CUENTA .... ***************************
    *****************************************************************************

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH valtotal &&&&Maqmant.COSTOMANT

    REPLACE CPDTEMP.DETALLE WITH "Costo Mantenimiento: " + vdetalle
    REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol

    USE IN 248
    USE IN 246

    RETURN

    *****************************************************************************
    *****************************************************************************
    ***** ELABORA COMPROBANTE CONTABLE DE --- USO DE INSUMOS fuera CULTIVOS *****
    *****************************************************************************
PROCEDURE HACECPUFI
    *****************************************************************************

    SELECT 248
    USE CPDTEMP
    SET ORDER TO 1

    IF UPPER(qllama) = "UFC"
        detal = "Usado en: " + LEFT(ufc.MOTUFC,32)
    ELSE
        quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))
        detal = "Usado en: "  + g_nommaq + "/" + ALLTRIM(g_serie) + " " + quecp
    ENDIF


    ** CONTABILIZACION DE USOS POR GRUPO DE INSUMOS  (DISMINUCION INVENTARIOS) **
    *****************************************************************************
    IF UPPER(qllama) = "UFC"
        SELECT Auxufc
        SET FILTER TO Auxufc.CODUFC = g_codufc
        GO TOP
        costins = 0  &&// costo del insumo
        ctipo = ufc.codtipo && // codigo tipo de insumo
        ccomp   = Auxufc.codcomp
        coins   = Auxufc.CODUFC
        coma1 = SELECT("COMPINS")
        inva1 = SELECT("INVENINI")

        DO WHILE !EOF()
            g_actual = RECNO()
            IF Auxufc.INDICA = 0 AND  coma1 <> 0
                SELECT Auxufc.cantusada*((compins.costunit/compins.equivale) ;
                    + compins.sobrecosto) AS VALOR  FROM  sacfadb!compins ;
                    INNER JOIN sacfadb!Auxufc ON  compins.codcomp = Auxufc.codcomp ;
                    WHERE compins.codcomp = ccomp AND Auxufc.CODUFC = coins ;
                    INTO CURSOR Cost22

                costins =  ROUND(Cost22.VALOR,0)
            ENDIF
            IF Auxufc.INDICA = 1 AND inva1 <> 0

                SELECT Auxufc.cantusada * ;
                    ((compins.costunit / compins.equivale) + compins.SOBRECONTA) AS ;
                    VALOR FROM  sacfadb!Auxufc INNER JOIN sacfadb!INVENINI ;
                    ON  Auxufc.codcomp = INVENINI.codinv WHERE compins.codcomp ;
                    = ccomp AND Auxufc.CODUFC = coins INTO CURSOR Cost22

                costins = ROUND(Cost22.VALOR,0)
            ENDIF
            SELECT Auxufc
            GO g_actual
            SKIP
        ENDDO
        ROT = "Uso Fuera del Cultivo-"
    ELSE
        SELECT Auxcomb
        SET FILTER TO Auxcomb.CODUFC = g_codcomb
        GO TOP
        costins = 0  &&// costo del insumo
        ctipo = Combust.codtipo && // codigo tipo de insumo
        ccomp = Auxcomb.codcomp
        coins = Auxcomb.CODUFC
        coma1 = SELECT("COMPINS")
        inva1 = SELECT("INVENINI")


        DO WHILE !EOF()
            g_actual = RECNO()

            IF Auxcomb.INDICA = 0 AND  coma1 <> 0
                SELECT Auxcomb.cantusada*((compins.costunit/compins.equivale) ;
                    + compins.sobrecosto) AS VALOR  FROM  sacfadb!compins ;
                    INNER JOIN sacfadb!Auxcomb ON  compins.codcomp = Auxcomb.codcomp ;
                    WHERE compins.codcomp = ccomp AND Auxcomb.CODUFC = coins ;
                    INTO CURSOR Cost22

                costins =  ROUND(Cost22.VALOR,0)
            ENDIF
            IF Auxcomb.INDICA = 1 AND  inva1 <> 0

                SELECT Auxcomb.cantusada * ;
                    ((compins.costunit / compins.equivale) + compins.SOBRECONTA) AS ;
                    VALOR FROM  sacfadb!Auxcomb INNER JOIN sacfadb!INVENINI ;
                    ON  Auxcomb.codcomp = INVENINI.codinv WHERE compins.codcomp ;
                    = ccomp AND Auxcomb.CODUFC = coins INTO CURSOR Cost22

                costins = ROUND(Cost22.VALOR,0)
            ENDIF
            SELECT Auxcomb
            GO g_actual
            SKIP
        ENDDO
        ROT = "SALIDA INV.-"
    ENDIF

    SELECT tipinsu  &&/// tipinsu....
    SET ORDER TO 1
    SEEK ctipo

    SELECT CPDTEMP
    SEEK ALLTRIM(STR(ctipo))

    IF !FOUND()
        APPEND BLANK
        REPLACE CPDTEMP.CODCONTABL WITH STR(ctipo)
        REPLACE CPDTEMP.DETALLE WITH ROT + " " + LEFT(tipinsu.TIPINSUMO,10)
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol
    ENDIF

    REPLACE CPDTEMP.VALOR WITH CPDTEMP.VALOR + costins

    IF UPPER(qllama) = "UFC"
        SELECT Auxufc
        SET FILTER TO Auxufc.CODUFC = g_codufc
        GO TOP
    ELSE
        SELECT Auxcomb
        SET FILTER TO Auxcomb.CODUFC = g_codcomb
        GO TOP
    ENDIF

    *** COLOCA EL CODIGO CONTABLE, DB o CR, A TIPOS DE INSUMO -SI SE PUEDE... ***
    **************************************************************************
    SELECT 248
    USE CPDTEMP
    GO TOP
    DO WHILE !EOF()

        SELECT tipinsu
        SEEK VAL(CPDTEMP.CODCONTABL)

        IF VAL(tipinsu.CUENTA) <> 0   &&// DEFINIDA LA CUENTA Y DB  o CR
            REPLACE CPDTEMP.CODCONTABL WITH  tipinsu.CUENTA

            IF tipinsu.NAT1 = "Debito" &&//  DESCARGA -  INVERSO - SALIDA
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ELSE
            REPLACE CPDTEMP.CODCONTABL WITH SPAC(10)
        ENDIF

        SELECT CPDTEMP
        SKIP

    ENDDO
    *************** CONTRAPARTIDAS A LAS SALIDAS DE INVENTARIOS ****************
    ************************* SELECCIONAR CUENTA .... **************************

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR   WITH costins
    REPLACE CPDTEMP.DETALLE WITH detal
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol
    REPLACE CPDTEMP.db      WITH costins
    REPLACE CPDTEMP.GUIA    WITH "7301"

    USE IN 248

    RETURN



    *****************************************************************************
    **************** CONTRAPARTIDA EN COSTOS DE PRODUCCION **********************
    *!*/ COSTO DE PROD. GASTO ADMON..
    DO COSTOCONTA WITH Labculti.COSTOTAL,"COSCUL"  &&// COSTO DE PROD. GASTO ADMON..
    *****************************************************************************

    USE IN 248

    RETURN

    *****************************************************************************
    *****************************************************************************
    ***********  ELABORA COMPROBANTE CONTABLE - USO MAQUINARIA PROPIA ***********
PROCEDURE HACECPMQ
    *****************************************************************************
    PARAMETERS totalmaq

    SELECT 248
    USE CPDTEMP

    quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))

    detal = ALLTRIM(g_nomlab) + " " + quecp

    SELECT 246
    USE CONTADOR
    SET ORDER TO  Codigo


    IF depre != 0

        SEEK 24 &&& Depreciacion Acumulada

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH depre
        REPLACE CPDTEMP.DETALLE WITH "DEPRECIACION MQ/PROPIA " + detal
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Credito"  &&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF
    ENDIF

    IF imp != 0

        SELECT CONTADOR
        SEEK 25 &&& Impuestos Pagos por Anticipado

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH imp
        REPLACE CPDTEMP.DETALLE WITH "IMPUESTOS MQ/PROPIA " + detal
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Credito"  &&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF
    ENDIF

    IF inter1 != 0

        SELECT CONTADOR
        SEEK 26 &&& Intereses Pago por Anticipado

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH inter1
        REPLACE CPDTEMP.DETALLE WITH "INTERESES MQ/PROPIA " + detal
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Credito"  &&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF
    ENDIF

    IF repar != 0

        SELECT CONTADOR
        SEEK 27 &&& Provisión Reparación

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH repar
        REPLACE CPDTEMP.DETALLE WITH "PROV. REPA. MQ/PROPIA " + detal
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Credito"  &&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF
    ENDIF

    IF opera != 0

        SELECT CONTADOR
        SEEK 29 &&& Nomina para la parte del uso del empleado

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH opera
        REPLACE CPDTEMP.DETALLE WITH "COSTO NOMINA MQ/PROPIA " + detal
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Dedito"  &&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF

    ENDIF

    ***************** CONTRAPARTIDA EN COSTOS DE PRODUCCION*********************
    *****************************************************************************
    IF totalmaq != 0

        SELECT CONTADOR
        SEEK 28 &&& Costos de Mecanizacion

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH totalmaq
        REPLACE CPDTEMP.DETALLE WITH "COSTOS MECANIZACION MQ/PROPIA " + detal
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Dedito"  &&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
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
PROCEDURE HACECP
    *****************************************************************************
    DECLARE cmp[4,2]
    PRIVATE cmp[4,2]

    SELECT 248
    USE CPDTEMP
    SET ORDER TO 1
    ****** CONTABILIZACION POR GRUPO DE INSUMOS (ENTRADA A INVENTARIOS) *********
    *****************************************************************************

    ******** COLOCA VALORES Y TIPOS DE INSUMO QUE SE VA A CONTABILIZAR **********
    *****************************************************************************
    SELECT tipinsu
    SET ORDER TO 1

    SELECT compins
    SET ORDER TO 6
    SET FILTER TO compins.CODFACT = g_codfact
    GO TOP

    DO WHILE !EOF()
        g_actual = RECNO ()
        g_numfac = ALLTRIM(Facins.numfact)
        cuanto = (compins.CANTUNIT * compins.costunit) + ;
            (compins.CANTINS * compins.SOBRECONTA)

        SELECT LEFT(ALLTRIM(tipinsu.TIPINSUMO),10) AS tipin FROM tipinsu ;
            WHERE tipinsu.codtipo = compins.codtipo INTO CURSOR tipo

        nomti = tipo.tipin

        SELECT COUNT(*)AS cuan FROM CPDTEMP INTO CURSOR vcanti ;
            WHERE compins.codtipo = VAL(ALLTRIM(CPDTEMP.CODCONTABL))

        canti = vcanti.cuan

        IF canti = 0 Then
            INSERT INTO CPDTEMP (CODCONTABL, DETALLE, codcc, numcontrol) ;
                VALUES (STR(compins.codtipo), ;
                "INVENT- " + nomti + " " + g_nomprov + " -FC." + g_numfac, ;
                g_codcc, g_numcontrol )
        ENDIF

        SELECT CPDTEMP
        REPLACE CPDTEMP.VALOR WITH CPDTEMP.VALOR + cuanto

        SELECT compins
        GO g_actual
        SKIP
    ENDDO
    ***** COLOCA EL CODIGO CONTABLE, DB o CR, A TIPOS DE INSUMO -SI SE PUEDE.****
    *****************************************************************************

    SELECT 248
    USE CPDTEMP
    GO TOP

    DO WHILE !EOF()

        SELECT tipinsu
        SEEK VAL(CPDTEMP.CODCONTABL)

        IF VAL(tipinsu.CUENTA) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR
            REPLACE CPDTEMP.CODCONTABL WITH  tipinsu.CUENTA

            IF tipinsu.NAT1 = "Debito"    &&&// DEBITO o CREDITO **************
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ENDIF
            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ELSE
            REPLACE CPDTEMP.CODCONTABL WITH SPAC(10)
        ENDIF

        SELECT CPDTEMP
        SKIP

    ENDDO

    *************** CONTABILIZACION CONTRAPARTIDAS A INVENTARIOS ***************
    ****************************************************************************

    SELECT 246
    USE CONTADOR
    SET ORDER TO  Codigo


    ******* EL TOTAL NETO, CREDITO O CONTADO, DE FACTURA SE CONTABILIZA: *******
    ****************************************************************************

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH Facins.VALORTOT

    IF g_forpago = 2     &&&// FACTURA A CREDITO ***********
        SELECT CONTADOR
        SEEK 7  &&&//  2(Credito) - CUENTAS POR PAGAR PROVEEDORES-

        SELECT CPDTEMP
        REPLACE CPDTEMP.DETALLE WITH "CxP " + PROPER(ALLTRIM(LEFT(g_nomprov,15)));
            + " FACT. " + g_numfac
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH  CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito"   &&&// DEBITO o CREDITO *********
                REPLACE CPDTEMP.db WITH Facins.VALORTOT
            ELSE
                REPLACE CPDTEMP.cr WITH Facins.VALORTOT
            ENDIF
            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ELSE
            REPLACE CPDTEMP.GUIA WITH  CONTADOR.GUIA
            REPLACE CPDTEMP.FT1 WITH  LEFT(CONTADOR.GUIA,2)
        ENDIF

        REPLACE CPDTEMP.SUBCTA WITH CONTADOR.MODOCT
        REPLACE CPDTEMP.CODPROVSB WITH Facins.codprov

    ELSE  &&&&// FACT. DE CONTADO - Seleccionar cja,bancos,obl. financieras (Tarj. credito)
        *************************************************

        REPLACE CPDTEMP.DETALLE WITH "PAGO A " + ;
            PROPER(ALLTRIM(LEFT(g_nomprov,15))) + " FACT. " + Facins.numfact
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol
        SELECT CONTADOR
        SEEK 20  &&&///  CUENTA DE BANCO
        SELECT CPDTEMP
        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR
            REPLACE CPDTEMP.CODCONTABL WITH  CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito" &&&// DEBITO o CREDITO *** //naturaleza inversa .....
                REPLACE CPDTEMP.cr WITH Facins.VALORTOT
            ELSE
                REPLACE CPDTEMP.db WITH Facins.VALORTOT
            ENDIF
        ENDIF

        REPLACE CPDTEMP.FT1 WITH LEFT(CONTADOR.GUIA,2) &&// CUENTAS DE DISPONIBLE - CAJA Y BANCOS
        REPLACE CPDTEMP.FT2 WITH "21" &&// CUENTAS DE OBLIG. FINANCIERAS-T.CREDITO
        REPLACE CPDTEMP.codprov WITH Facins.codprov

    ENDIF
    USE IN 248
    USE IN 246

    RETURN

    *****************************************************************************
    *****************************************************************************
    ****************  ELABORA COMPROBANTE CONTABLE - PAGOS **********************
PROCEDURE HACECPAG
    *****************************************************************************
    SELECT 208
    USE CPD

    SELECT 246
    USE CONTADOR
    SET ORDER TO Codigo
    SEEK 22


    SELECT 248
    USE CPDTEMP

    SELECT DETALLE, codprov, CODCONTABL FROM CPD WHERE CPD.codcp = g_codcp ;
        AND LEFT(CPD.CODCONTABL,2)= "23" INTO CURSOR pdet

    INSERT INTO CPDTEMP (VALOR, DETALLE, codcc, numcontrol, db, codprov, ;
        CODCONTABL) VALUES(vvalor, pdet.DETALLE, g_codcc, g_numcontrol, ;
        vvalor, pdet.codprov, pdet.CODCONTABL)

    ***************  CONTRA ...  POSIBLES  Bancos, cajas ,etc.******************
    ************************ SELECCIONAR CUENTA .... ***************************
    *****************************************************************************

    ldetal = "PAGO " + RIGHT(ALLTRIM(pdet.DETALLE), ;
        LEN(ALLTRIM(pdet.DETALLE))-3)

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH vvalor
    REPLACE CPDTEMP.DETALLE WITH ldetal
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol

    IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

        IF CONTADOR.NATURAL = "Credito"
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ENDIF
        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ENDIF


    USE IN 208
    USE IN 246
    USE IN 248

    RETURN

    ****************************************************************************
    ****************************************************************************
    ************  ELABORA COMPROBANTE CONTABLE - PAGO DE NOMINA ****************
PROCEDURE HACEPLANI
    ****************************************************************************

    SELECT 248
    USE CPDTEMP

    quecp = "  "+g_fechaplani
    detal = "Pago de "

    SELECT 246
    USE CONTADOR
    SET ORDER TO Codigo

    FOR i= 1 TO  2
        SELECT CONTADOR
        IF i = 1
            SEEK 32  &&///  Nomina Administracion Auxilio de transporte
            v_auxtra = "g_nauxtra" + ALLTRIM(STR(i))
        ELSE
            SEEK 41  &&///  Nomina Produccion Auxilio de transporte
            v_auxtra = "g_nauxtra" + ALLTRIM(STR(i))
        ENDIF
        IF &v_auxtra > 0
            SELECT CPDTEMP
            APPEND BLANK

            REPLACE CPDTEMP.VALOR WITH &v_auxtra
            REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
            REPLACE CPDTEMP.codcc WITH g_codcc
            REPLACE CPDTEMP.numcontrol WITH g_numcontrol

            IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
                REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

                IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
                    REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
                ELSE
                    REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
                ENDIF

                REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
            ENDIF
        ENDIF

        ****** Alimentacion

        SELECT CONTADOR

        IF i=1
            SEEK 33  &&///  Nomina Administracion Auxilio de Alimentacion
            v_auxali = "g_nauxalim" + ALLTRIM(STR(i))
        ELSE
            SEEK 42  &&///  Nomina Produccion Auxilio de Alimentacion
            v_auxali = "g_nauxalim" + ALLTRIM(STR(i))
        ENDIF
        IF &v_auxali>0
            SELECT CPDTEMP
            APPEND BLANK

            REPLACE CPDTEMP.VALOR WITH  &v_auxali
            REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
            REPLACE CPDTEMP.codcc WITH g_codcc
            REPLACE CPDTEMP.numcontrol WITH g_numcontrol

            IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
                REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

                IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
                    REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
                ELSE
                    REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
                ENDIF

                REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
            ENDIF
        ENDIF

        ****** Sueldos

        SELECT CONTADOR

        IF i= 1
            SEEK 34  &&///  Nomina Administracion Sueldos
            v_auxsue = "g_nsueldo" + ALLTRIM(STR(i))
        ELSE
            SEEK 43  &&///  Nomina Produccion Sueldos
            v_auxsue = "g_nsueldo" + ALLTRIM(STR(i))
        ENDIF
        IF &v_auxsue>0
            SELECT CPDTEMP
            APPEND BLANK

            REPLACE CPDTEMP.VALOR WITH &v_auxsue
            REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
            REPLACE CPDTEMP.codcc WITH g_codcc
            REPLACE CPDTEMP.numcontrol WITH g_numcontrol

            IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
                REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

                IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
                    REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
                ELSE
                    REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
                ENDIF

                REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
            ENDIF
        ENDIF
        ****** Horas Extras y Recargos

        SELECT CONTADOR

        IF i= 1
            SEEK 35  &&///  Nomina Administracion horas extras y recargos
            v_auxner = "g_nhorrec" + ALLTRIM(STR(i))
        ELSE
            SEEK 44  &&///  Nomina Produccion horas extras y recargos
            v_auxner = "g_nhorrec" + ALLTRIM(STR(i))
        ENDIF
        IF &v_auxner>0
            SELECT CPDTEMP
            APPEND BLANK

            REPLACE CPDTEMP.VALOR WITH &v_auxner
            REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
            REPLACE CPDTEMP.codcc WITH g_codcc
            REPLACE CPDTEMP.numcontrol WITH g_numcontrol

            IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
                REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

                IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
                    REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
                ELSE
                    REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
                ENDIF

                REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
            ENDIF
        ENDIF

        ****** Otros Devengos

        SELECT CONTADOR

        IF i= 1
            SEEK 36  &&///  Nomina Administracion Otros devengos
            v_auxod = "g_notrdev" + ALLTRIM(STR(i))
        ELSE
            SEEK 45  &&///  Nomina Produccion Otros devengos
            v_auxod = "g_notrdev" + ALLTRIM(STR(i))
        ENDIF
        IF &v_auxod>0
            SELECT CPDTEMP
            APPEND BLANK

            REPLACE CPDTEMP.VALOR WITH &v_auxod
            REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
            REPLACE CPDTEMP.codcc WITH g_codcc
            REPLACE CPDTEMP.numcontrol WITH g_numcontrol

            IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
                REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

                IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
                    REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
                ELSE
                    REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
                ENDIF

                REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
            ENDIF
        ENDIF
    ENDFOR

    ****** Otros Descuentos

    SELECT CONTADOR

    SEEK 37  &&///  Nomina Otros Descuentos

    IF g_ndescu>0
        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH g_ndescu
        REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF
    ENDIF

    ****** Salud

    SELECT CONTADOR

    SEEK 38  &&///  Nomina Salud

    IF g_nsalud>0
        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH g_nsalud
        REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF
    ENDIF

    ****** Pension

    SELECT CONTADOR

    SEEK 39  &&///  Nomina Pension

    IF g_npension>0
        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH g_npension
        REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF
    ENDIF
    ****** Fdo de Solidaridad

    SELECT CONTADOR

    SEEK 46  &&///  Fdo de Solidaridad
    IF g_nfsolida>0

        SELECT CPDTEMP
        APPEND BLANK

        REPLACE CPDTEMP.VALOR WITH g_nfsolida
        REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
        REPLACE CPDTEMP.codcc WITH g_codcc
        REPLACE CPDTEMP.numcontrol WITH g_numcontrol

        IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
            REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

            IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
                REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
            ELSE
                REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
            ENDIF

            REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
        ENDIF
    ENDIF
    *********** Contrapartida en Nómina por pagar

    SELECT CONTADOR

    SEEK 47  &&///  Nomina por pagar
    SELECT CPDTEMP
    APPEND BLANK
    v_NomxPag=g_nsueldo1+g_nsueldo2+g_nhorrec1+g_nhorrec2+g_nauxtra1+g_nauxtra2+g_nauxalim1+g_nauxalim2;
        +g_notrdev1+g_notrdev2-(g_ndescu+g_nsalud+g_npension+g_nfsolida)
    REPLACE CPDTEMP.VALOR WITH v_NomxPag
    REPLACE CPDTEMP.DETALLE WITH detal + ALLTRIM(CONTADOR.concepto) + quecp
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol

    IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

        IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF

        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ENDIF



    **********Nomina por pagar
    USE IN 246
    USE IN 248

    RETURN


    ****************************************************************************
    ****************************************************************************
    ************  ELABORA COMPROBANTE CONTABLE - TRASLADO DE VENTA ****************
PROCEDURE HACETRASV
    ****************************************************************************

    SELECT 248
    USE CPDTEMP

    quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))

    IF g_trasinv = 1

        detal = "Traslado a Inv. " + quecp
        ncuen = "1425050005"

        SELECT CODCONTABL, SUM(db-cr) AS cr, codcc, numcontrol, codprov;
            FROM CPD INTO TABLE trscpd ;
            WHERE LEFT(CPD.CODCONTABL,1) = "7" AND numcontrol = g_numcontrol ;
            AND db != 0  ORDER BY CODCONTABL GROUP BY CODCONTABL

        SUM (trscpd.cr) TO s_trs

        SELECT CPDTEMP
        APPEND FROM trscpd
        GO TOP

        REPLACE CPDTEMP.DETALLE WITH detal ALL


    ELSE

        detal = "Traslado a Prod. Term. " + quecp
        ncuen = "1430150005"

        SELECT CODCONTABL, SUM(db) AS cr, codcc, numcontrol, codprov;
            FROM CPD INTO TABLE trscpd ;
            WHERE LEFT(CPD.CODCONTABL,4) = "1425" AND numcontrol = g_numcontrol ;
            AND db != 0 ORDER BY CODCONTABL GROUP BY CODCONTABL

        SUM (trscpd.cr) TO s_trs

        SELECT CPDTEMP
        APPEND FROM trscpd
        GO TOP

        REPLACE CPDTEMP.DETALLE WITH detal ALL

    ENDIF


    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH s_trs
    REPLACE CPDTEMP.DETALLE WITH detal
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol
    REPLACE CPDTEMP.CODCONTABL WITH ncuen
    REPLACE CPDTEMP.db WITH s_trs

    USE IN 246
    USE IN 248

    RETURN











    *****************************************************************************
    *///  DESDE AQUI --  FUNCIONES  GENERICAS , pueden usarse para todo/  -------
    *****************************************************************************






    ****************************************************************************
    *creacion de COMPROBANTES AUTOMATICOS...
    ****************************************************************************
FUNCTION COMPROA
    *****************************************************************************

    PARAMETERS cptipo,cpfecha,numero

    PRIVATE auxcod,aux,pant,ctapuc1,ya18

    ********************  APERTURA DE ARCHIVOS CDP Y CPG ***********************
    *****************************************************************************

    IF !FILE("CPD.DBF")
        SELECT 100
        USE CPD
    ENDIF
    IF !FILE("CPG.DBF")
        SELECT 100
        USE CPG
    ENDIF
    *****************************************************************************
    ya18 = 0
    ya18 = SELECT("IPROV")

    IF ya18 = 0
        SELECT 18
        USE IPROV
        ya18 = SELECT("IPROV")
        SET ORDER TO 0

    ENDIF

    *****************************************************************************
    SELECT 58
    USE PUC
    SET ORDER TO 1

    SELECT 104
    USE CPG

    ****************** CALCULANDO NUMERO DE COMPROBANTE    **********************
    *****************************************************************************

    IF numero = 0
        SELECT COUNT(*) FROM CPG INTO ARRAY vtotal
        IF vtotal = 0 Then
            auxcod = 1
        ELSE
            SELECT MAX(codcp) FROM CPG  ;
                INTO ARRAY VALORcp
            auxcod = VALORcp + 1
        ENDIF
    ELSE
        SELECT MAX(codcp) FROM CPG  ;
            INTO ARRAY VALORcp
        auxcod = VALORcp + 1
    ENDIF

    *****************************************************************************
    ****************** CONFIRMACION DE CUENTAS POR PANTALLA *********************
    *****************************************************************************
    SELECT 248
    USE CPDTEMP
    GO TOP
    DO WHILE !EOF()
        g_actual = RECNO()
        IF  CPDTEMP.CODCONTABL = SPAC(10)
            rotcab = CPDTEMP.DETALLE
            IF CPDTEMP.CODCONTABL <> SPAC(10)
                donde = CPDTEMP.CODCONTABL
            ELSE
                donde = CPDTEMP.GUIA
            ENDIF
            *****************************************************************************
            **** Esta parte es con el formulario "DONCON" que pide las cuentas para *****
            **** Contabilizar
            sig1 = 1
            sig2 = 1
            rotulo = ALLTRIM(CPDTEMP.DETALLE)
            VALOR  = CPDTEMP.VALOR
            DO WHILE sig2 = 1
                DO FORM doncon.scx WITH sig1, rotulo, VALOR  TO sig2
            ENDDO
            ****************************************************************************
        ENDIF
        SELECT CPDTEMP
        GO g_actual
        SKIP
    ENDDO
    USE IN 248
    *************** REGISTRANDO EL DETALLE DEL COMPROBANTE **********************
    IF SELECT("CPD") <> 0
        SELECT CPD

    ELSE
        SELECT 109
        USE CPD
    ENDIF
    ***************** ADICIONANDO REGISTROS DEL TEMPORAL ************************
    APPEND FROM CPDTEMP
    *****************************************************************************
    ******************  COLOCANDO NUMERO DE COMPROBANTE **********************
    *******************  COLOCANDO FECHA DE COMPROBANTE **********************

    REPLACE CPD.fechacp WITH cpfecha FOR CPD.codcp = 0
    REPLACE CPD.codcp WITH auxcod FOR CPD.codcp = 0
    *****************************************************************************

    ******************* SUMADO DEBITOS Y CREDITOS *******************************

    SUM CPD.db,CPD.cr TO sdbs,scrs FOR CPD.codcp = auxcod &&AND CPD.codprov = -20

    REPLACE CPD.codprov WITH 0 FOR CPD.codprov < 0
    *****************************************************************************

    ******************  COLOCANDO CONSECUTIVO DE REGISTROS **********************
    *****************   Y ACTUALIZANDO MOVIMIENTOS A PUC1 ***********************

    SET ORDER TO 3
    GO BOTTOM
    aux = CPD.codcomp
    aux = aux + 1
    CLOSE INDEXES

    GO TOP
    LOCATE FOR CPD.codcomp = 0
    DO WHILE !EOF()

        REPLACE CPD.codcomp WITH aux
        aux = aux + 1
        SKIP
    ENDDO
    ******************************************************************************
    SET ORDER TO 4
    SET ORDER TO 2
    ****************** REGISTRO GENERAL DEL COMPROBANTE *************************
    SELECT CPG
    APPEND BLANK
    REPLACE CPG.codcp WITH auxcod
    REPLACE CPG.codcc WITH g_codcc
    REPLACE CPG.numcontrol WITH g_numcontrol

    REPLACE CPG.FUENTE WITH cptipo
    REPLACE CPG.fecha WITH cpfecha
    REPLACE CPG.dbs WITH sdbs
    REPLACE CPG.crs WITH scrs
    REPLACE CPG.DFs WITH ABS(sdbs-scrs)
    *****************************************************************************
    *********************** BORRANDO DEL TEMPORAL *******************************
    SELECT 250
    USE CPDTEMP EXCLUSIVE
    ZAP
    *****************************************************************************

    IF ya18 = 0
        USE IN 18
    ENDIF

    USE IN 58
    USE IN 114
    USE IN 104
    USE IN 109
    USE IN 250

    RETURN auxcod


    ********** AUMENTA O DISMINUYE  EL COSTO CONTABLE, ACUMULADO ****************
    *****************************************************************************
FUNCTION SUMACOSTO
    *****************************************************************************

    areact = SELECT()
    acucosto = 0

    *!*	    SELECT db FROM CPD WHERE codcc = g_codcc AND ;
    *!*	        LEFT(CPD.CODCONTABL,6) = "143015" INTO CURSOR proter

    *!*	    IF proter.db = 0
    *!*	        MESSAGEBOX("La Cuenta 143015 No Tiene " + CHR(13) + ;
    *!*	            "Movimiento se Recomienda " + CHR(13)+ ;
    *!*	            "Trasladar Saldos a esta Cuenta" + CHR(13)+ CHR(13)+ ;
    *!*	            "  ! Antes de Efectuar la Venta !",48,"¡Alerta!")

    *!*	    ELSE
    SELECT SUM(db)AS tot FROM CPD WHERE codcc = g_codcc AND ;
        LEFT(CPD.CODCONTABL,6) = "143015" AND numcontrol = g_numcontrol ;
        INTO CURSOR CosContbl

    acucosto = (CosContbl.tot*vporc)/100

    *!*	    ENDIF

    SELECT (areact)
    RETURN acucosto
ENDFUNC

*****************************************************************************
****************** ELIMINA COMPROBANTE AUTOMATICO  **************************
*****************************************************************************
FUNCTION ELIMCP
    *****************************************************************************

    PARAMETERS numecp

    *!*	    valcosto = 0

    SELECT 109
    USE CPD
    SET FILTER TO CPD.codcp = numecp
    GO TOP

    *!*	    SELECT SUM(CPD.db) AS TOTA FROM CPD ;
    *!*	        WHERE  CPD.codcp = numecp INTO CURSOR C_dev

    *!*	    valcosto  = C_dev.TOTA

    DELETE FROM CPD WHERE CPD.codcp = numecp

    IF qtabla = "Venta"
        SELECT COUNT(*) FROM CPD WHERE LEFT(ALLTRIM(DETALLE),15) == "Traslado a Inv.";
            AND CPD.codcp = numecp - 2 INTO ARRAY vcantidad

        IF vcantidad != 0

            DELETE FROM CPD WHERE codcc = g_codcc AND ;
                numcontrol = g_numcontrol AND CPD.codcp = numecp - 2

            SELECT COUNT(*) FROM CPD WHERE LEFT(ALLTRIM(DETALLE),16) == "Traslado a Prod.";
                AND CPD.codcp = numecp - 1 INTO ARRAY vcantidad

            IF vcantidad != 0

                DELETE FROM CPD WHERE codcc = g_codcc AND ;
                    numcontrol = g_numcontrol AND CPD.codcp = numecp - 1

            ENDIF
        ENDIF
    ENDIF

    SELECT 104
    USE CPG

    DELETE FROM CPG WHERE codcp = numecp

    IF qtabla = "Venta" AND vcantidad != 0
        DELETE FROM CPG WHERE codcc = g_codcc AND;
            numcontrol = g_numcontrol AND FUENTE = -84 ;
            AND CPD.codcp = numecp - 1 OR CPD.codcp = numecp - 2
    ENDIF


    USE IN 109
    USE IN 104
    USE IN 114

    *!*	    RETURN valcosto
    RETURN

    *****************************************************************************
    *****************************************************************************
    *************************** COSTOS DE PRODUCCION  ***************************
PROCEDURE COSTOCONTA
    *****************************************************************************
    PARAMETERS valct, cualct

    yaesta = SELECT("CULTIVO")

    IF yaesta <> 0
        SELECT (yaesta)
        SET ORDER TO 1
    ELSE
        SELECT 246
        USE Cultivo
        SET ORDER TO 1
    ENDIF

    SEEK g_codcul

    *   //  VARIABLES PARA COSTO
    CTA = "CULTIVO."+cualct
    NAT = "CULTIVO.NAT1"

    SELECT CPDTEMP
    APPEND BLANK

    REPLACE CPDTEMP.VALOR WITH valct
    REPLACE CPDTEMP.DETALLE WITH "COSTO "+ detal
    REPLACE CPDTEMP.codcc WITH g_codcc
    REPLACE CPDTEMP.numcontrol WITH g_numcontrol

    IF VAL(&CTA) <> 0 &&&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
        REPLACE CPDTEMP.CODCONTABL WITH &CTA

        IF &NAT = "Debito"   &&&// DEBITO o CREDITO *********
            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
        ELSE
            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
        ENDIF
        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    ENDIF


    IF yaesta = 0
        USE IN 246
    ENDIF

    RETURN


    ****************************************************************************
    ****************************************************************************
    *******  ELABORA COMPROBANTE CONTABLE DE --- USO DE CABUYA - EMPAQUE *******
    ****************************************************************************
    *PROCEDURE 	CPCE

    *   SELECT 2
    *   USE tipinsu
    *   INDEX on CODTIPO TO TIPINS1   &&//Indice por CODTIPO.
    *   SEEK acodtip

    *   SELECT 248
    *   USE CPDTEMP

    *   detal = quecp

    ****** CONTABILIZACION costo emapque -cabuya ***********

    *  IF SELECT("COMPINS.DBF") <> 0
    *    SELECT 9
    *    or9 = INDEXORD()
    *     or9 = ORDER()
    *    SET ORDER TO 2
    *  endif

    *  IF SELECT("INVENINI.DBF")<>0
    *     SELECT 16
    *     or16=INDEXORD()
    *     or16 = ORDER()
    *     SET ORDER TO 2
    *  ENDIF

    *SELECT 10
    *filtro10 = FILTER()
    *SET FILTER TO 10->CODLOTE = auxcod AND 10->CODRECOL = 8->CODRECOL
    *GO TOP

    *          IF SELECT("COMPINS.DBF")<>0
    *	       SET RELATION TO 10->CODCOMP INTO 9
    *	       IF SELECT("INVENINI.DBF")<>0
    *	          SET RELATION TO 10->CODCOMP INTO 16 ADDITIVE
    *       	       ENDIF
    *    	  ELSE
    *       		IF SELECT("INVENINI.DBF")<>0
    *          	   SET RELATION TO 10->CODCOMP INTO 16
    *                ENDIF
    *          ENDIF

    *          costins=0  &&// costo de cabuy - empaque

    *          DO WHILE !EOF()
    *             IF 10->INDICA=0 AND SELECT("COMPINS.DBF")<>0
    *               costins = costins+10->CANTUSADA*((9->COSTUNIT/9->EQUIVALE)+9->SOBRECONTA)
    *             ENDIF
    *             IF 10->INDICA=1 AND SELECT("INVENINI.DBF")<>0
    *                costins = costins + 10->CANTUSADA * (16->COSTUNIT / 16->EQUIVALE)
    *             ENDIF

    *             SKIP
    *          ENDDO

    *          SET RELATION TO

    *          SET FILTER TO &filtro10
    *          GO TOP

    *  IF SELECT("COMPINS.DBF")<>0
    *    SELECT 9
    *    SET ORDER TO or9
    *  endif

    *  IF SELECT("INVENINI.DBF")<>0
    *     SELECT 16
    *     SET ORDER TO or16
    *  ENDIF

    ****** COLOCA EL CODIGO CONTABLE, DB o CR, A cabuya-empaque -SI SE PUEDE... *******

    *          SELECT CPDTEMP
    *          APPEND BLANK

    *          REPLACE CPDTEMP.DETALLE WITH "INV.-"+LEFT(2->TIPINSUMO,10)+" "+detal
    *          REPLACE CPDTEMP.VALOR WITH costins

    *    IF VAL(2->CUENTA)<>0   &&// DEFINIDA LA CUENTA Y DB  o CR
    *       REPLACE CPDTEMP.CODCONTABL WITH  2->CUENTA

    *       IF 2->NATURAL="D" &&//  DESCARGA -  INVERSO - SALIDA
    *          REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
    *       ELSE
    *          REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
    *       ENDIF
    *       REPLACE CPDTEMP.AUTO WITH automatico
    *       REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
    *    ELSE
    *       REPLACE CPDTEMP.CODCONTABL WITH SPAC(10)
    *       REPLACE CPDTEMP.AUTO WITH .F.
    *       REPLACE CPDTEMP.GUIA WITH  2->GUIA
    *       REPLACE CPDTEMP.FT1 WITH  LEFT(2->GUIA,2)
    *    ENDIF

    *...............
    ******** CONTRAPARTIDAS A LAS SALIDAS DE INVENTARIOS ************

    ********** COSTOS DE PRODUCCION - MATERIA PRIMA ********
    *********************

    *  DO COSTOCONTA WITH costins,"CD"

    ****************

    * Use In 2
    * Use In 248
    ******
    *RETURN



*!*	PROCEDURE HACECPAS
*!*	    ****************************************************************************

*!*	    PARAMETERS queinter,QUELOTE,acodcult

*!*	    SELECT 248
*!*	    USE CPDTEMP

*!*	    quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))
*!*	    detal = " CRED. " + quecp

*!*	    SELECT 246
*!*	    USE CONTADOR
*!*	    SET ORDER TO  Codigo

*!*	    SEEK 21  &&///  Costos indirectos.......

*!*	    *  //  VARIABLE PARA CONTRAPARTIDA AL COSTO (SUPONE CONTRA >COSTOS INDIRECTOS)
*!*	    *  //  POR AQUI LA CTA. DE COSTOS INDIRECTOS SE VA ACREDITANDO - DISMINUYENDO.
*!*	    *  //  SE VA DISMINUYENDO PORQUE ALLI SE HAN IDO COLOCANDO TODOS LOS gastos
*!*	    *  //  QUE SON GENERALES A TODOS LOS LOTES O CULTIVOS Y  QUE EL PROGRAMA
*!*	    *  //  PRORRATEA, ESTIMA, ETC . AQUI SON:HORAS EMPLEADOS O/Y JORNALES.

*!*	    SELECT CPDTEMP
*!*	    APPEND BLANK

*!*	    REPLACE CPDTEMP.VALOR WITH queinter
*!*	    REPLACE CPDTEMP.DETALLE WITH "INT. "+detal
*!*	    REPLACE CPDTEMP.codcc WITH g_codcc
*!*	    REPLACE CPDTEMP.numcontrol WITH g_numcontrol

*!*	    IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
*!*	        REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

*!*	        IF CONTADOR.NATURAL = "Debito"  &&&// DEBITO o CREDITO - INVERSA *********
*!*	            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
*!*	        ELSE
*!*	            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
*!*	        ENDIF

*!*	        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
*!*	    ENDIF

*!*	    USE IN 246

*!*	    *************** CONTRAPARTIDA EN COSTOS DE PRODUCCION  *********************
*!*	    DO COSTOCONTA WITH queinter,"CD"  &&// COSTO DE PROD. intereses
*!*	    *****************************************************************************

*!*	    USE IN 248

*!*	    RETURN

*!*	    *****************************************************************************
*!*	    *****************************************************************************
*!*	    ***************  ELABORA COMPROBANTE CONTABLE - OTROS GASTOS .***************
*!*	PROCEDURE HACECPGA
*!*	    *****************************************************************************

*!*	    SELECT CPDTEMP
*!*	    USE CPDTEMP

*!*	    quecp = ALLTRIM(g_nomlote) + "/" + ALLTRIM(STR(g_ano))+ "-" + ALLTRIM(STR( g_semestre))
*!*	    detal = "GASTOS ADMON." + quecp

*!*	    SELECT 246
*!*	    USE CONTADOR
*!*	    SET ORDER TO  Codigo

*!*	    SEEK 21  &&///  Costos indirectos.......

*!*	    *  //  VARIABLE PARA CONTRAPARTIDA AL COSTO (SUPONE CONTRA >COSTOS INDIRECTOS)
*!*	    *  //  POR AQUI LA CTA. DE COSTOS INDIRECTOS SE VA ACREDITANDO - DISMINUYENDO.
*!*	    *  //  SE VA DISMINUYENDO PORQUE ALLI SE HAN IDO COLOCANDO TODOS LOS gastos
*!*	    *  //  QUE SON GENERALES A TODOS LOS LOTES O CULTIVOS Y  QUE EL PROGRAMA
*!*	    *  //  PRORRATEA, ESTIMA, ETC . AQUI SON:  OTROS GASTOS DE ADMON...

*!*	    SELECT CPDTEMP
*!*	    APPEND BLANK

*!*	    REPLACE CPDTEMP.VALOR WITH Labculti.COSTOTAL
*!*	    REPLACE CPDTEMP.DETALLE WITH detal
*!*	    REPLACE CPDTEMP.codcc WITH g_codcc
*!*	    REPLACE CPDTEMP.numcontrol WITH g_numcontrol

*!*	    IF VAL(CONTADOR.CUENTA) <> 0 &&// DEFINIDA LA CUENTA Y DB  o CR -?? AUTOMATICO ??
*!*	        REPLACE CPDTEMP.CODCONTABL WITH CONTADOR.CUENTA

*!*	        IF CONTADOR.NATURAL = "Debito"  &&// DEBITO o CREDITO - INVERSA *********
*!*	            REPLACE CPDTEMP.cr WITH CPDTEMP.VALOR
*!*	        ELSE
*!*	            REPLACE CPDTEMP.db WITH CPDTEMP.VALOR
*!*	        ENDIF
*!*	        REPLACE CPDTEMP.FT1 WITH  LEFT(CPDTEMP.CODCONTABL,2)
*!*	    ENDIF

*!*	    USE IN 246
