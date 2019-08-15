*****************************************************************************
****************** POR SI ES CONTABLE LA VERSION DE SACFA *******************
*************** C O N T A B I L I D A D   S   A   C   F   A *****************
*****************************************************************************
PARAMETERS qtabla, g_modo, valtotal, auxano, auxsem, auxcod
* parametros en los cuales trae :
* modo en el que esta
* nombre de la tabla
* año
* semestre
* numcontrol para su respectiva elaboracion de comprobante


SET procedure TO librecon.prg

SELECT 250
USE cpdtemp EXCLUSIVE
ZAP
USE IN  250

DO CASE
    CASE qtabla = "Labcu" &&Modelos de actividades por contrato manual total y maquinaria alquilada.

        DO CASE
            CASE g_modo = "A"  &&AND valtotal <> 0 &&&ELABORA COMPROBANTE CONTABLE DE UN CONTRATO
                DO HACECPCT   &&&esta opcion creo que es para saber si el total es diferente de o
                numcp = COMPROA(-97,g_fecinicio,0)   &&// Comprobante automatico
                SELECT Labculti
                REPLACE Labculti.CODCP WITH numcp
            CASE g_modo = "B" &&& ELIMINACION DE Comprobante
                ELIMCP(g_codcp)
            CASE g_modo = "M" AND valtotal<>0
                DO HACECPCT
                ELIMCP(g_codcp) &&// ELIMINACION DE Comprobante
                numcp = COMPROA(-97,g_fecinicio,g_codcp)   &&&// Comprobante automatico
                IF g_codlab <> 804
                    UPDATE Labculti SET CODCP = numcp WHERE num_fecha = g_numfecha AND ;
                        numcontrol = g_numcontrol
                ELSE
                    UPDATE Labculti SET CODCP = numcp WHERE RECNO() = fecadm.RegAct
                ENDIF
        ENDCASE

    CASE qtabla = "Maqcu" &&&Relaci¢n de labores realizadas con maquinaria propia.

        DO CASE
            CASE g_modo = "A"
                DO HACECPMQ WITH valtotal
                numcp = COMPROA(-94,Maqculti.fechamaq,g_codcp)   &&&// Comprobante automatico
                SELECT Maqculti
                REPLACE Maqculti.CODCP WITH numcp
            CASE g_modo = "B"
                ELIMCP(g_codcp) &&&// ELIMINACION DE Comprobante
            CASE g_modo = "M"
                ELIMCP(g_codcp) &&&// ELIMINACION DE Comprobante
                DO HACECPMQ WITH valtotal
                numcp = COMPROA(-94,Maqculti.fechamaq,g_codcp)   &&&// Comprobante automatico
                SELECT Maqculti
                REPLACE Maqculti.CODCP WITH numcp
        ENDCASE

    CASE qtabla = "Emple" &&Creación de Empleados

        DO CASE
            CASE g_modo = "A"
                DO HACECPJOEM WITH "Dis. Costo Nom." &&&Distribucion Costo Nomina
                numcp = COMPROA(-95,Jorculti.FECHALAB,0) && // Comprobante automatico
                SELECT Jorculti
                REPLACE Jorculti.CODCP WITH numcp
            CASE g_modo = "B"
                ELIMCP(g_codcp) &&// ELIMINACION DE Comprobante
            CASE g_modo = "M"
                DO HACECPJOEM WITH "Dis. Costo Nom."
                ELIMCP(g_codcp) &&&// ELIMINACION DE Comprobante
                numcp = COMPROA(-95,Jorculti.FECHALAB,g_codcp)   &&&// Comprobante automatico
                SELECT Jorculti
                REPLACE Jorculti.CODCP WITH numcp

        ENDCASE

    CASE qtabla = "Jorna"    &&&Creacion jornales

        DO CASE
            CASE g_modo = "A"
                DO HACECPJOEM  WITH "CxP JORN. a "+ PROPER(ALLTRIM(LEFT(g_nomprov,15)))
                numcp = COMPROA(-95,Jorculti.FECHALAB,0)   &&&// Comprobante automatico
                UPDATE Jorculti SET CODCP = numcp WHERE num_fecha = g_numfecha AND ;
                    numcontrol = g_numcontrol
            CASE g_modo = "B"
                ELIMCP(g_codcp) &&&// ELIMINACION DE Comprobante
            CASE g_modo = "M"
                DO HACECPJOEM  WITH "CxP JORN. a "+ PROPER(ALLTRIM(LEFT(g_nomprov,15)))
                ELIMCP(g_codcp) &&&// ELIMINACION DE Comprobante
                numcp = COMPROA(-95,Jorculti.FECHALAB,g_codcp)   &&&// Comprobante automatico
                UPDATE Jorculti SET CODCP = numcp WHERE num_fecha = g_numfecha AND ;
                    numcontrol = g_numcontrol
        ENDCASE

    CASE qtabla = "Siemb" OR  qtabla = "Aplic" &&&Registra datos de labores de siembra y/o Aplicaciones en el cultivo.
        SET NEAR OFF
        DO CASE
            CASE g_modo = "A"  &&&ELABORA COMPROBANTE CONTABLE DE --- USO DE SEMILLA EN CULTIVOS ***
                DO HACECPUI   && HACECPSM
                numcp = COMPROA(-96,Aplica.fecaplic,0)   &&&// Comprobante automatico TRASLADO
                SELECT Aplica
                UPDATE Aplica SET CODCP = numcp WHERE Aplica.codaplic = g_aplica
            CASE g_modo = "B"
                qcosto = ELIMCP(g_codcp)   &&&Apcod // ELIMINACION DE Comprobante
            CASE g_modo = "M"
                DO HACECPUI   && HACECPSM
                qcosto = ELIMCP(g_codcp) && // ELIMINACION DE Comprobante
                numcp = COMPROA(-96,g_fecha1,g_codcp)  &&& // Comprobante automatico
                SELECT Aplica
                UPDATE Aplica SET CODCP = numcp WHERE Aplica.codaplic = g_aplica
        ENDCASE
        SET NEAR ON
        SELECT Aplica
        DATapl = GETNEXTMODIFIED(0)
        IF DATapl <> 0
            = TABLEUPDATE(.T.,.T.) &&almacena 5
        ENDIF

    CASE qtabla = "Venta" &&&Procedimiento de creacion de una nueva venta.

        DO CASE
        	
            CASE g_modo = "A"
                cstovta = sumacosto()
                DO HACECPVT
                numcp = COMPROA(-92,g_fecinicio,0)  &&& Comprobante automatico
                SELECT Ventas
                g_actual = recno()
                REPLACE CODCP WITH numcp &&FOR numcontrol = g_numcontrol and  g_actual
            CASE g_modo = "B"
                ELIMCP(g_codcp_aux) && ELIMINACION DE Comprobante
            CASE g_modo = "M" &&Procedimiento de modificacion de una venta.
                cstovta = sumacosto()
                DO HACECPVT
                ELIMCP(g_codcp_aux) &&& ELIMINACION DE Comprobante
                numcp = COMPROA(-92,g_fecinicio,g_codcp) &&& Comprobante automatico
                SELECT Ventas
                REPLACE CODCP WITH numcp &&FOR numcontrol = g_numcontrol
        ENDCASE

    CASE qtabla = "Factu"&&& Procedimiento que permite modificar una factura ya existente.

        DO CASE
            CASE g_modo = "A"  &&&ELABORA COMPROBANTE CONTABLE DE FACTURA
                DO HACECP
                numcp = COMPROA(-98,Facins.FECHAFAC,0)&&&Comprobante automatico
                SELECT Facins
                UPDATE Facins SET  CODCP = numcp  WHERE Facins.codfact = g_codfact
            CASE g_modo = "B"
                ELIMCP(Facins.CODCP)   &&& ELIMINACION DE Comprobante
            CASE g_modo = "M"
                DO HACECP
                ELIMCP(Facins.CODCP) &&// ELIMINACION DE Comprobante
                numcp = COMPROA(-98,Facins.FECHAFAC,Facins.CODCP)   &&&// Comprobante automatico
                SELECT Facins
                UPDATE Facins SET  CODCP = numcp WHERE Facins.codfact = g_codfact
        ENDCASE

    CASE qtabla = "Pagos"

        DO CASE
            CASE g_modo = "A"  &&&ELABORA COMPROBANTE CONTABLE DE PAGOS
                DO HACECPAG
                numcp = COMPROA(-85,g_fecinicio,0) &&&Comprobante automatico
            CASE g_modo = "B"
                ELIMCP(g_codcp)   &&& ELIMINACION DE Comprobante
            CASE g_modo = "M"
                DO HACECPAG
                ELIMCP(g_codcp) &&// ELIMINACION DE Comprobante
                numcp = COMPROA(-85,g_fecinicio,g_codcp)   &&&// Comprobante automatico
        ENDCASE
        IF g_modo <> "B"
            IF !cred AND (g_modo = "A" OR g_modo = "M")
                SELECT Dfechas
                REPLACE CODCP WITH numcp  FOR Dfechas.num_fecha = g_numfecha
            ELSE
                SELECT Dcredito
                REPLACE CODCP WITH numcp  FOR Dcredito.codcred = g_codcred
            ENDIF
        ENDIF

    CASE qtabla = "Mantm" &&&Procedimiento que permite crear un nuevo mantenimiento.

        DO CASE
            CASE g_modo = "A"	&& ELABORA COMPROBANTE CONTABLE DE --- USO DE INSUMOS fuera de CULTIVOS ***
                valtodos = 0
                DO HACECPMM
                numcp = COMPROA(-87,g_fecinicio,g_codcp)  &&Comprobante automatico
                UPDATE Maqmant SET CODCP = numcp WHERE Maqmant.codmant = g_codmant
            CASE g_modo = "B"
                ELIMCP(g_codcp)   && ELIMINACION DE Comprobante
            CASE g_modo = "M"
                valtodos = 0
                DO HACECPMM
                ELIMCP(g_codcp) &&&// ELIMINACION DE Comprobante
                numcp = COMPROA(-87,g_fecinicio,g_codcp)  &&& // Comprobante automatico
                UPDATE Maqmant SET CODCP = numcp WHERE Maqmant.codmant = g_codmant
        ENDCASE

    CASE  qtabla = "Usufu" &&Programa que permite incluir los usos de insumos fuera del cultivo.
        *** y la administracion de los combustibles en maquinaria.
        DO CASE
            CASE g_modo = "A" &&& ELABORA COMPROBANTE CONTABLE DE --- USO DE INSUMOS fuera de CULTIVOS ***
                valtodos = 0
                DO HACECPUFI
                IF UPPER(qllama) = "UFC"
                    numcp = COMPROA(-89,auxano,Ufc.CODCP)   &&& Comprobante automatico
                    SELECT Ufc
                    REPLACE Ufc.CODCP WITH numcp
                ELSE
                    numcp = COMPROA(-88,auxano,Combust.CODCP)   &&& Comprobante automatico
                    SELECT Combust
                    REPLACE Combust.CODCP WITH numcp
                ENDIF
            CASE g_modo = "B"
                ELIMCP(g_codcp) &&& ELIMINACION DE Comprobante
            CASE g_modo = "M"
                valtodos = 0
                DO HACECPUFI
                ELIMCP(g_codcp)&&&ELIMINACION DE Comprobante
                IF UPPER(qllama) = "UFC"
                    numcp = COMPROA(-89,auxano,Ufc.CODCP)  &&&Comprobante automatico
                    SELECT Ufc
                    REPLACE Ufc.CODCP WITH numcp
                ELSE
                    numcp = COMPROA(-88,auxano,Combust.CODCP)  &&&Comprobante automatico
                    SELECT Combust
                    REPLACE Combust.CODCP WITH numcp
                ENDIF
        ENDCASE

    CASE qtabla = "Credi" &&Hace las operaciones correspondientes a los creditos.

        DO CASE
            CASE g_modo = "A" && ELABORA COMPROBANTE CONTABLE DE CREDITOS
                DO HACECPCR
                numcp = COMPROA(-91,Dcredito.fecha,g_codcp)   &&&// Comprobante automatico
                SELECT Dcredito
                REPLACE Dcredito.CODCP WITH numcp
            CASE g_modo = "M" OR g_modo = "B"
                ELIMCP(g_codcp) &&&// ELIMINACION DE Comprobante
                IF g_modo = "M"
                    DO HACECPCR
                    numcp = COMPROA(-91,Dcredito.fecha,g_codcp)   &&&// Comprobante automatico
                    SELECT Dcredito
                    REPLACE Dcredito.CODCP WITH numcp
                ENDIF
        ENDCASE

    CASE qtabla = "Planill"&&& Procedimiento que Permite Manejar la Parte de Nomina.

        DO HACEPLANI
        g_fecinicio = CTOD(g_fecinicio)
        numcp = COMPROA(-83,g_fecinicio,0)&&&Comprobante automatico

    CASE qtabla = "TrasVen"&&& Procedimiento que Permite Manejar la Parte de Traslados .

        DO CASE
            CASE g_modo = "A"  &&&ELABORA COMPROBANTE CONTABLE DE TRASLADOS
                DO HACETRASV
                numcp = COMPROA(-84,g_fecinicio,0)&&&Comprobante automatico
            CASE g_modo = "B"
                ELIMCP(0)   &&& ELIMINACION DE Comprobante
            CASE g_modo = "M"
                DO HACETRASV
                ELIMCP(Facins.CODCP) &&// ELIMINACION DE Comprobante
                numcp = COMPROA(-84,g_fecinicio,0)   &&&// Comprobante automatico
        ENDCASE

ENDCASE
SET procedure TO 




*****************************************************************************
*****************************************************************************
*****************************************************************************
*****************************************************************************
*!*	   CASE qtabla = "Recol" &&&Permite registrar información de los insumos empaque y cabuya.

*!*	      Do Case
*!*	         Case g_modo = "A" &&ELABORA COMPROBANTE CONTABLE DE --- USO DE CABUYA - EMPAQUE
*!*	              IF recolfecha=CTOD("//")
*!*	                 recolfecha=DATE()
*!*	              ENDIF
*!*	              GO regnume
*!*	         Case g_modo = "B"
*!*	              IF 8->CODCP<>0
*!*	                 qcosto=ELIMCP(8->CODCP) &&&// ELIMINACION DE Comprobante
*!*	     	         *!* sumacosto(-qcosto,auxano,auxsem,auxcod)
*!*	              ENDIF
*!*	              IF 8->CANTOTAL<>0
*!*	                 costins=0*
*!*		             DO HACECPCE
*!*	                 numcp=COMPROA(-96,recolfecha,8->CODCP)   &&&// Comprobante automatico
*!*	                 *!* sumacosto(costins,auxano,auxsem,auxcod)
*!*	                 SELECT 8
*!*	                 REPLACE 8->CODCP WITH numcp
*!*	              ENDIF
