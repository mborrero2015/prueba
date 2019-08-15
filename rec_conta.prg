*****************************************************************************
****************** POR SI ES CONTABLE LA VERSION DE SACFA *******************
*************** C O N T A B I L I D A D   S   A   C   F   A *****************
*****************************************************************************
PARAMETERS qtabla


SET PROCEDURE TO librecon.prg

SELECT 250
USE cpdtemp EXCLUSIVE
ZAP
USE IN  250

DO CASE
    CASE qtabla = "Labcu" &&Modelos de actividades por contrato manual total y maquinaria alquilada.

        DO HACECPCT   &&&esta opcion creo que es para saber si el total es diferente de o
        numcp = COMPROA(-97,g_fecinicio,0)   &&// Comprobante automatico
        SELECT Labculti
        REPLACE Labculti.CODCP WITH numcp


    CASE qtabla = "Maqcu" &&&Relaci¢n de labores realizadas con maquinaria propia.

        DO HACECPMQ WITH valtotal
        numcp = COMPROA(-94,Maqculti.fechamaq,g_codcp)   &&&// Comprobante automatico
        SELECT Maqculti
        REPLACE Maqculti.CODCP WITH numcp

    CASE qtabla = "Emple" &&Creación de Empleados

        DO HACECPJOEM WITH "Dis. Costo Nom." &&&Distribucion Costo Nomina
        numcp = COMPROA(-95,Jorculti.FECHALAB,0) && // Comprobante automatico
        SELECT Jorculti
        REPLACE Jorculti.CODCP WITH numcp

    CASE qtabla = "Jorna"    &&&Creacion jornales


        DO HACECPJOEM  WITH "CxP JORN. a "+ PROPER(ALLTRIM(LEFT(g_nomprov,15)))
        numcp = COMPROA(-95,Jorculti.FECHALAB,0)   &&&// Comprobante automatico
        UPDATE Jorculti SET CODCP = numcp WHERE num_fecha = g_numfecha AND ;
            numcontrol = g_numcontrol


    CASE qtabla = "Siemb" OR  qtabla = "Aplic" &&&Registra datos de labores de siembra y/o Aplicaciones en el cultivo.
        SET NEAR OFF

        ***&&&ELABORA COMPROBANTE CONTABLE DE --- USO DE SEMILLA EN CULTIVOS ***
        DO HACECPUI   && HACECPSM
        numcp = COMPROA(-96,Aplica.fecaplic,0)   &&&// Comprobante automatico TRASLADO
        SELECT Aplica
        UPDATE Aplica SET CODCP = numcp WHERE Aplica.codaplic = g_aplica

        SET NEAR ON

    CASE qtabla = "Venta" &&&Procedimiento de creacion de una nueva venta.

        cstovta = sumacosto()
        DO HACECPVT
        numcp = COMPROA(-92,g_fecinicio,0)  &&& Comprobante automatico
        SELECT Ventas
        g_actual = RECNO()
        REPLACE CODCP WITH numcp &&FOR numcontrol = g_numcontrol and  g_actual



        *********************************************   FACTURAS
        *********************************************
        *********************************************

    CASE qtabla = "Factu"&&& Procedimiento que permite modificar una factura ya existente.

        DO HACECP
        numcp = COMPROA(-98,Facins.FECHAFAC,0)&&&Comprobante automatico
        SELECT Facins
        UPDATE Facins SET  CODCP = numcp  WHERE Facins.codfact = g_codfact


    CASE qtabla = "Pagos"


        DO HACECPAG
        numcp = COMPROA(-85,g_fecinicio,0) &&&Comprobante automatico


        IF !cred
            SELECT Dfechas
            REPLACE CODCP WITH numcp  FOR Dfechas.num_fecha = g_numfecha
        ELSE
            SELECT Dcredito
            REPLACE CODCP WITH numcp  FOR Dcredito.codcred = g_codcred AND RECNO() = g_recno
        ENDIF


    CASE qtabla = "Mantm" &&&Procedimiento que permite crear un nuevo mantenimiento.

        valtodos = 0
        DO HACECPMM
        numcp = COMPROA(-87,g_fecinicio,g_codcp)  &&Comprobante automatico
        UPDATE Maqmant SET CODCP = numcp WHERE Maqmant.codmant = g_codmant


    CASE  qtabla = "Usufu" &&Programa que permite incluir los usos de insumos fuera del cultivo.
        *** y la administracion de los combustibles en maquinaria.
        ***&&& ELABORA COMPROBANTE CONTABLE DE --- USO DE INSUMOS fuera de CULTIVOS ***
        valtodos = 0
        DO HACECPUFI
        IF UPPER(qllama) = "UFC"
            numcp = COMPROA(-89,auxano,0)   &&& Comprobante automatico
            SELECT Ufc
            REPLACE Ufc.CODCP WITH numcp
        ELSE
            numcp = COMPROA(-88,auxano,Combust.CODCP)   &&& Comprobante automatico
            SELECT Combust
            REPLACE Combust.CODCP WITH numcp
        ENDIF


    CASE qtabla = "Credi" &&Hace las operaciones correspondientes a los creditos.

        DO HACECPCR
        numcp = COMPROA(-91,Dcredito.fecha,g_codcp)   &&&// Comprobante automatico
        SELECT Dcredito
        REPLACE Dcredito.CODCP WITH numcp


    CASE qtabla = "Planill"&&& Procedimiento que Permite Manejar la Parte de Nomina.

        DO HACEPLANI
        g_fecinicio = CTOD(g_fecinicio)
        numcp = COMPROA(-83,g_fecinicio,0)&&&Comprobante automatico

    CASE qtabla = "TrasVen"&&& Procedimiento que Permite Manejar la Parte de Traslados .

        DO HACETRASV
        numcp = COMPROA(-84,g_fecinicio,0)&&&Comprobante automatico

ENDCASE

SET PROCEDURE TO
