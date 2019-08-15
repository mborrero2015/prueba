PARAMETER g_cerrar
************   **************   ************   **************
************   **************   ************   **************
****** Variables para generar automaticamente los comprobantes contables teniendo como base la parte basica
************   **************   ************   **************
************   **************   ************   **************
g_contabilidad = .F.
g_prototal     = 0

*!*	SET DEFAULT TO CURDIR()
*!*	SET LIBRARY TO Cipher50.fll ADDITIVE
*!*	DECLARE _fpreset IN msvcrt20.DLL

SET AUTOSAVE ON      &&&vaciará o no los búferes de datos al disco cuando salga del READ
SET CENTURY ON       &&AÑO 2000
SET CONSOLE OFF      &&Activa o desactiva la salida a la ventana principal de Visual FoxPro o a la ventana definida por el usuario activa desde los programas.
SET DATE TO BRITISH  &&dd/mm/aa
SET DELETE ON        &&NO MUESTRA REGISTROS BORRADOS
SET EXCLUSIVE OFF    &&abre los archivos de tabla para uso exclusivo o compartido
SET LOCK ON          &&Activa o desactiva el bloqueo automático de archivos con ciertos comandos.
SET MULTILOCKS ON    &&Determina si puede bloquear múltiples registros
SET NOTIFY OFF       &&Activa o desactiva la presentación de algunos mensajes del sistema.
SET SAFETY OFF       &&Muestra o no un cuadro de diálogo antes de sobrescribir un archivo existente
*SET SYSMENU TO       &&Elimina todos los menús de la barra principal de menús de Visual FoxPro
SET REPROCESS TO 1   &&Especifica cuántas veces, o durante cuánto tiempo, intenta Visual FoxPro bloquear un archivo o un registro después de que fracase un intento de bloqueo.
SET TALK OFF         &&muestra o no los resultados de los comandos
SET NEAR  ON         &&Determina dónde se sitúa el puntero de registro después de que FIND o SEEK busquen un registro y no lo encuentren.


nAnswer = MESSAGEBOX("O J O    L E A   primero este mensaje y preste mucha atencion,";
    + CHR(13)+ CHR(13)+ "Asesorese con su CONTADOR o directamente con FEDEARROZ.";
    + CHR(13)+ CHR(13)+ "Este procedimiento permitira generar una contablidad a";
    +CHR(13)+ "partir de los datos que ya se encuentran en su computadora.";
    +CHR(13)+ CHR(13)+ "Si ya tiene una version contable le generara nuevamente toda su contabilidad";
    +CHR(13)+ CHR(13)+ "Primero asegurese de tener la estructura contable (PUC) con las posibles cuentas";
    +CHR(13)+ "a trabajar y luego verifique que las interfaces esten llenas para hacer mas rapido";
    +CHR(13)+ "el procedimiento.";
    +CHR(13)+ CHR(13)+ "                       ¿Esta seguro de realizar este procedimiento?",36, '¡Generador Contable!')

DO CASE
    CASE nAnswer = 6
        g_seguro = MESSAGEBOX("¿Realmente esta seguro de realizar este procedimiento?",36,"¿Seguro?")
        IF g_seguro = 6
            USE VARIABLES IN 1
            SELECT VARIABLES
            REPLACE VARIABLES.contable WITH .T.
            REPLACE  VARIABLES.qdigit WITH 8

            IF VARIABLES.contable
                g_seg = MESSAGEBOX("La Informacion contable que contiene el programa sera eliminada,";
                    +CHR(13)+"los movimientos realizados en el modulo de comprobantes "+CHR(13)+;
                    "y libro diario; y demás operaciones contables se eliminaran.";
                    +CHR(13)+CHR(13)+"Este programa solamente generara, los comprobantes con la";
                    +CHR(13)+"información que tenga almacenada hasta este momento en su";
                    +CHR(13)+"computador.",52,"!Atencion¡")
                IF g_seg = 6
                    COPY FILE cpd.DBF TO cpd_bak_con.bak
                    COPY FILE cpg.DBF TO cpg_bak_con.bak
                    COPY FILE tipinsu.DBF TO tipinsu_bak_con.bak
                    COPY FILE contador.DBF TO contador_bak_con.bak
                    COPY FILE cultivo.DBF TO cultivo_bak_con.bak
                    COPY FILE tipinsu_cam.DBF TO tipinsu.DBF
                    COPY FILE tipinsu_cam.CDX TO tipinsu.CDX
                    COPY FILE contador_cam.DBF TO contador.DBF
                    COPY FILE contador_cam.CDX TO contador.CDX
                    COPY FILE cultivo_cam.DBF TO cultivo.DBF
                    COPY FILE cultivo_cam.CDX TO cultivo.CDX
                    USE cpd IN 0 EXCLU
                    SELECT cpd
                    ZAP
                    USE cpg IN 0 EXCLU
                    SELECT cpg
                    ZAP
                    USE IN cpd
                    USE IN cpg
                    g_contabilidad = .T.
                ENDIF
            ENDIF
        ENDIF
ENDCASE

DO WHILE g_contabilidad

    USE puc1 IN 2

    SELECT COUNT(*) AS cuant FROM puc1 INTO CURSOR vcant WHERE !DELETE()

    IF vcant.cuant > 15

        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   ************** C R E D I T O S ***********   **************   ************

        ******** AQUI SE PIDE QUE EL USUARIO SELECIONE LA CUENTA DE CONTRAPARTIDA COMO SE HACE NORMALMENTE DEBIDO A
        ******** QUE LA CUENTA POR PAGAR LA PUEDEN DEJAR EN DIFERENTES CUENTAS ......
        ************   **************   ************   **************************   **************   ************   **************


        IF  gen_credito

            USE Credito IN 9
            SELECT Credito
            SET ORDER TO TAG Codcred

            USE Dcredito IN 10

            SELECT Credito


            SELECT Dcredito
            SET RELATION TO Dcredito.Codcred INTO Credito
            COUNT ALL TO v_cuan FOR !DELETE() AND Dcredito.tipo != 3
            SET FILTER TO Dcredito.tipo != 3

            g_prototal = g_prototal + 1
            g_probar1.OBJECT.VALUE = g_prototal

            GO TOP

            IF v_cuan != 0
                g_probar2.OBJECT.MAX = v_cuan

                SCAN
                    WAIT "Generando los comprobantes de CREDITOS " + ALLTRIM(Credito.nomprov) NOWAIT WINDOWS AT 25,25
                    g_recno       = RECNO()
                    * para poner el numero contable en dcredito solamente al registro que corresponde

                    g_probar2.OBJECT.VALUE = g_recno
                    * estos dos son los que manejan los progressbar en el formulario principal

                    cred         = .T.
                    * variable para poner el numero contable en dcredito

                    g_fecinicio  = Dcredito.fecha
                    *  fecha en la cual se realizo la accion

                    g_codprov    = Credito.codprov
                    g_nomprov    = ALLTRIM(Credito.nomprov)
                    * codigo y nombre del provedor


                    IF Dcredito.tipo != 3
                        * tipo 3 es la asignacion a lotes

                        g_codcp    = 0
                        * el numero del comprobante

                        DO CASE
                            CASE  Dcredito.tipo = 1
                                nomtab     = "Credi"
                                * para que funcione con librecon

                            CASE  Dcredito.tipo = 2
                                nomtab     = "Pagos"
                                * para que funcione con librecon

                                detal1     = "22"
                                * esto funciona dentro de librecon para saber que hacer

                                g_numcheq = ALLTRIM(Dcredito.numcheq)
                                * es el numero del cheque si le ingreso

                            CASE  Dcredito.tipo = 4

                                nomtab     = "Pagos"
                                * para que funcione con librecon
                                vporcentaje = 0
                                parte       = ALLTRIM(LEFT(g_nomprov,15))+"/" + Credito.NUMCRED
                                detal1      = "53GASTO Intereses " + parte
                                detal2      = "PAGO Intereses " + parte
                                g_numcheq   = ALLTRIM(Dcredito.numcheq)
                                * es el numero del cheque si le ingreso

                            CASE  Dcredito.tipo = 5
                                nomtab     = "Pagos"
                                * para que funcione con librecon

                                parte       = ALLTRIM(LEFT(g_nomprov,15))+"/" + Credito.NUMCRED
                                creotro     = LEFT(ALLTRIM(Dcredito.motivo),15)
                                vporcentaje = 0
                                detal1      = "53GASTO " + creotro + parte
                                detal2      = "PAGO " + creotro + parte

                                g_numcheq = ALLTRIM(Dcredito.numcheq)
                                * es el numero del cheque si le ingreso
                        ENDCASE

                        IF Dcredito.tipo != 1

                            SELECT codcp FROM Dcredito INTO CURSOR cur_cp ;
                                WHERE Codcred = g_codcred AND tipo = 1 AND !DELETE()
                            g_codcp   =  cur_cp.codcp
                            * el numero del comprobante

                        ENDIF

                        g_codcred  = Credito.Codcred
                        * codigo del credito para tener integridad

                        g_valcred  = Dcredito.valor
                        vvalor     = g_valcred
                        * valor credito el cual fue ingresado

                        g_gascred  = Credito.gastoscred
                        * los gastos del credito se ingresan en el momento de generar el registro

                        valtotal   = Credito.gastoscred + Dcredito.valor
                        * sumando valor del credito y los gastos para la contrapartida

                        disponible = Credito.codfin
                        g_loncc    = LEN(ALLTRIM(STR(disponible)))
                        g_codcc    = STUFFC("0000",5-g_loncc,g_loncc,ALLTRIM(STR(disponible)))
                        * para poner el centro de costo que tiene guardado en la tabla credito


                        g_numcontrol = -1
                        * codigo de control para este caso es -1 porque no hemos trabajado con lotes

                        DO rec_conta.prg WITH nomtab

                    ENDIF
                ENDSCAN
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
        ENDIF

        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   ************** F A C T U R A S ************   **************   ************
        IF  gen_factu

            USE facins IN 9
            SELECT facins
            SET ORDER TO TAG Codfact

            USE compins IN 10
            SELECT compins
            SET ORDER TO TAG Codfact

            USE tipinsu IN 11

            USE dfechas IN 12
            SELECT dfechas
            SET ORDER TO TAG num_fecha

            SELECT facins
            SET RELATION TO facins.Codfact INTO compins ADDITIVE
            SET RELATION TO facins.num_fecha INTO dfechas ADDITIVE

            SELECT facins
            COUNT ALL TO v_cuan FOR !DELETE()

            g_prototal   = g_prototal + 1
            g_probar1.OBJECT.VALUE = g_prototal
            GO TOP

            IF v_cuan != 0
                g_probar2.OBJECT.MAX = v_cuan
                SCAN
                    WAIT "Generando los comprobantes de FACTURAS " + ALLTRIM(facins.nomprov)  NOWAIT WINDOWS AT 25,25

                    IF facins.totafac != 0
                        * para no pasara errores

                        g_recno      = RECNO()
                        g_probar2.OBJECT.VALUE = g_recno
                        * estos dos son los que manejan los progressbar en el formulario principal

                        nomtab       = "Factu"
                        * para que funcione con librecon

                        cred         = .F.
                        * para saber si se llama desde creditos o no

                        g_codfact    = facins.Codfact
                        * codigo de la factura

                        g_codprov    = facins.codprov
                        g_nomprov    = ALLTRIM(facins.nomprov)
                        * codigo y nombre del provedor

                        g_codcc      = ALLTRIM(facins.codcc)
                        * codigo del centro de costo

                        g_numcontrol = -1
                        *codigo de control para este caso es -1 porque no hemos trabajado con lotes

                        g_forpago    = facins.formafac
                        *forma de pago de la factura

                        g_numcheq    = ALLTRIM(facins.numcheq)
                        * si la factura se pago con cheque este sera el numero

                        g_numfecha   = facins.num_fecha



                        DO rec_conta.prg WITH nomtab

                        ***********  ***********  ***********  ***********  ***********  *************
                        ***********  procedimiento para generar los pagos de las facturas ************
                        ***********  ***********  ***********  ***********  ***********  *************

                        IF g_numfecha  != 0
                            IF g_forpago = 2
                                IF facins.num_fecha = dfechas.num_fecha
                                    WAIT "Generando los comprobantes de PAGOS " + ALLTRIM(facins.nomprov) NOWAIT WINDOWS AT 25,25
                                    nomtab       = "Pagos"
                                    g_numcheq    = ALLTRIM(dfechas.numcheq)
                                    g_fecinicio  = dfechas.fecha
                                    vvalor       = dfechas.valor
                                    detal1       = "23"
                                    g_codcp      = facins.codcp

                                    DO rec_conta.prg WITH nomtab

                                ENDIF
                            ENDIF
                        ELSE
                            MESSAGEBOX("La Información de la FACTURAS contiene errores, por lo";
                                +CHR(13)+"tanto los comprobantes de PAGOS no se pueden generar";
                                +CHR(13)+CHR(13)+"Solicite asistencia a FEDEARROZ";
                                +CHR(13)+CHR(13)+"Desea Continuar",20,"!ERROR - dfechas ¡")
                            g_contabilidad = .F.
                        ENDIF
                    ENDIF

                ENDSCAN
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
            * estos dos son los que manejan los progressbar en el formulario principal
        ENDIF
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   ********* A P L I C A C I O N E S ************   **************   *********
        IF gen_aplica


            USE aplica IN 9

            USE insaplica IN 10
            SELECT insaplica
            SET ORDER TO TAG codaplic

            USE auxapli IN 11
            SELECT auxapli
            SET ORDER TO TAG codinsap

            USE tipinsu IN 12
            SELECT tipinsu
            SET ORDER TO TAG codtipo

            USE insumo IN 13
            SELECT insumo
            SET ORDER TO TAG codinsum

            USE compins IN 14

            USE infculti IN 15
            SELECT infculti
            SET ORDER TO TAG numcontrol

            USE lotesfin IN 16
            SELECT lotesfin
            SET ORDER TO TAG codlote

            SELECT aplica
            SET RELATION TO aplica.numcontrol INTO infculti ADDITIVE
            SET RELATION TO aplica.codaplic INTO insaplica ADDITIVE

            SELECT infculti
            SET RELATION TO infculti.codlote INTO lotesfin ADDITIVE

            SELECT insaplica
            SET RELATION TO insaplica.codinsap INTO auxapli ADDITIVE

            SELECT auxapli
            SET RELATION TO codtipo INTO tipinsu ADDITIVE
            SET RELATION TO codinsum INTO insumo ADDITIVE

            ****** ******** ****** ******** ****** ******** ****** ******** ****** ********
            *****  select para detectar errores en las compras , en insumos aplicados, y en el auxiliar
            *****  por tipo de insumo y no permitir ejecutar los comprobantes de aplicaciones por error
            *****  en inventarios
            ****** ******** ****** ******** ****** ******** ****** ******** ****** ********

            SELECT compins.codinsum, SUM(compins.cantins) AS comtotins ,  SUM(compins.usototins)AS comtot  ;
                FROM compins  GROUP BY codinsum;
                INTO TABLE tem_inv_comp ORDER BY codinsum

            SELECT insaplica.codinsum, SUM(insaplica.cantotal) AS instot ;
                FROM insaplica GROUP BY codinsum;
                INTO TABLE tem_inv_insa ORDER BY codinsum

            SELECT  auxapli.codinsum, SUM(auxapli.cantusada) AS auxtot;
                FROM auxapli  GROUP BY codinsum;
                INTO TABLE tem_inv_auxa ORDER BY codinsum

            SELECT  auxufc.codinsum, SUM(auxufc.cantusada) AS auxufc;
                FROM auxufc  GROUP BY codinsum;
                INTO TABLE tem_auxufc ORDER BY codinsum


            SELECT  ufc.codinsum, SUM(ufc.cantotal) AS ufc;
                FROM ufc  GROUP BY codinsum;
                INTO TABLE tem_ufc ORDER BY codinsum

            SELECT  combust.codinsum, SUM(combust.cantotal) AS combust;
                FROM combust  GROUP BY codinsum;
                INTO TABLE tem_combust ORDER BY codinsum

            SELECT  auxcomb.codinsum, SUM(auxcomb.cantusada) AS auxcomb;
                FROM auxcomb GROUP BY codinsum;
                INTO TABLE tem_auxcomb ORDER BY codinsum



            SELECT tem_inv_comp.codinsum,;
                SUM(tem_inv_comp.comtotins) AS comtotins,;   	&&&& total insumo
            SUM(tem_inv_comp.comtot) AS comtot,;  			&&&& total compins
            SUM(tem_inv_insa.instot) AS instot,;
                SUM(tem_inv_auxa.auxtot) AS auxtot ; 			&&&& total auxtotal
            FROM tem_inv_insa INNER JOIN tem_inv_comp;
                INNER JOIN tem_inv_auxa ;
                ON  tem_inv_auxa.codinsum = tem_inv_comp.codinsum ;
                ON  tem_inv_insa.codinsum = tem_inv_comp.codinsum ;
                GROUP BY tem_inv_comp.codinsum;
                ORDER BY tem_inv_comp.codinsum;
                INTO TABLE tem_inv_total

            SELECT  SUM (tem_combust.combust) AS combust, SUM (tem_auxcomb.auxcomb) AS auxcomb;
                ,tem_inv_comp.codinsum;
                FROM tem_combust INNER JOIN tem_inv_comp;
                INNER JOIN tem_auxcomb ;
                ON  tem_auxufc.codinsum = tem_inv_comp.codinsum ;
                ON tem_ufc.codinsum = tem_inv_comp.codinsum ;
                GROUP BY tem_inv_comp.codinsum;
                ORDER BY tem_inv_comp.codinsum;
                INTO TABLE tem_comb_total

            SELECT  SUM (tem_ufc.ufc) AS ufc, SUM (tem_auxufc.auxufc) AS auxufc,tem_inv_comp.codinsum;
                FROM tem_ufc INNER JOIN tem_inv_comp;
                INNER JOIN tem_auxufc ;
                ON  tem_auxufc.codinsum = tem_inv_comp.codinsum ;
                ON tem_ufc.codinsum = tem_inv_comp.codinsum ;
                GROUP BY tem_inv_comp.codinsum;
                ORDER BY tem_inv_comp.codinsum;
                INTO TABLE tem_ufc_total

            ****** ******** ****** ******** ****** ******** ****** ******** ****** ********
            **** para detectar las inconsistencias por tipo de insumo
            ****** ******** ****** ******** ****** ******** ****** ******** ****** ********

            SELECT * FROM tem_inv_total WHERE comtotins < comtot OR comtot != instot ;
                OR comtot != auxtot OR instot != auxtot;
                INTO TABLE tem_inv_dif
            *SELECT 	COUNT(*) FROM tem_inv_dif INTO array error_inv
            SELECT * FROM tem_ufc_total WHERE ufc!=auxufc INTO TABLE tem_ufc_dif
            SELECT 	COUNT(*) FROM tem_ufc_dif INTO ARRAY error_ufc
            SELECT * FROM tem_comb_total WHERE combust!=auxcomb INTO TABLE tem_comb_dif
            SELECT 	COUNT(*) FROM tem_comb_dif INTO ARRAY error_comb
            SELECT auxufc+auxtot AS aux, ufc+instot AS ins, tem_ufc_total.codinsum,comtot,comtotins FROM tem_ufc_total ;
                INNER JOIN tem_inv_dif ON tem_inv_dif.codinsum = tem_ufc_total.codinsum INTO TABLE temp_error
            SELECT * FROM temp_error WHERE aux!=ins OR aux!=comtot INTO TABLE errores
            SELECT 	COUNT(*) FROM errores INTO ARRAY error_inv
            ********************

            IF error_inv != 0
                MESSAGEBOX("La Información de las APLICACIONES contienen errores, por";
                    +CHR(13)+" lo tanto los comprobantes no se pueden generar";
                    +CHR(13)+CHR(13)+"Solicite asistencia a FEDEARROZ";
                    +CHR(13)+CHR(13)+"Desea Continuar",20,"!ERROR¡")
                g_contabilidad = .F.
            ELSE
                SELECT aplica
                COUNT ALL TO v_cuan FOR !DELETE()

                g_prototal = g_prototal + 1
                g_probar1.OBJECT.VALUE = g_prototal
                GO TOP

                IF v_cuan != 0
                    g_probar2.OBJECT.MAX = v_cuan
                    SCAN
                        WAIT "Generando los comprobantes de APLICACIONES "+ ALLTRIM(aplica.codaplic) NOWAIT WINDOWS AT 25,25

                        nomtab       = "Aplic"
                        * para que funcione con librecon

                        g_recno = RECNO()
                        g_probar2.OBJECT.VALUE = g_recno
                        * estos dos son los que manejan los progressbar en el formulario principal

                        cred         = .F.

                        g_codlote    = lotesfin.codlote
                        g_nomlote    = ALLTRIM(infculti.nomlote)
                        * codigo y nombre del lote que se esta trabajando

                        g_codcul     = infculti.codcultivo
                        * codigo del cultivo que se esta trabajando

                        g_aplica     = ALLTRIM(aplica.codaplic)
                        * codigo de la aplicacion

                        g_fecinicio  = aplica.fecaplic
                        * fecha en la cual se hizo la labor

                        g_ano        = YEAR(g_fecinicio)
                        mes          = MONTH(g_fecinicio)

                        IF mes > 6
                            g_semestre   = 2
                        ELSE
                            g_semestre   = 1
                        ENDIF

                        g_codcc      = ALLTRIM(infculti.codcc)
                        * codigo del centro de costo

                        g_numcontrol = infculti.numcontrol
                        * codigo de control

                        DO rec_conta.prg WITH nomtab
                    ENDSCAN
                ENDIF
            ENDIF

            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
            * estos dos son los que manejan los progressbar en el formulario principal

        ENDIF

        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   ************** L A B O R E S ************   **************   ************
        IF gen_contra

            USE labculti IN 9

            USE infculti IN 10
            SELECT infculti
            SET ORDER TO TAG numcontrol

            USE labor IN 11
            SELECT labor
            SET ORDER TO 1

            USE iprov IN 12
            SELECT iprov
            SET ORDER TO 1

            USE lotesfin IN 13
            SELECT lotesfin
            SET ORDER TO TAG codlote

            USE dfechas IN 14
            SELECT dfechas
            SET ORDER TO TAG num_fecha

            SELECT labculti
            SET RELATION TO labculti.numcontrol INTO infculti ADDITIVE
            SET RELATION TO labculti.codlabor INTO labor ADDITIVE
            SET RELATION TO labculti.codprov INTO iprov ADDITIVE
            SET RELATION TO labculti.num_fecha INTO dfechas ADDITIVE

            SELECT infculti
            SET RELATION TO infculti.codlote INTO lotesfin ADDITIVE

            SELECT labculti
            COUNT ALL TO v_cuan FOR !DELETE()

            g_prototal = g_prototal + 1
            g_probar1.OBJECT.VALUE = g_prototal
            GO TOP

            IF v_cuan != 0
                g_probar2.OBJECT.MAX = v_cuan
                SCAN
                    WAIT "Generando los comprobantes de CONTRATOS  "+ ALLTRIM(labculti.nomlabor) NOWAIT WINDOWS AT 25,25
                    nomtab       = "Labcu"
                    * para que funcione con librecon

                    cred         = .F.

                    g_recno = RECNO()
                    g_probar2.OBJECT.VALUE = g_recno
                    * estos dos son los que manejan los progressbar en el formulario principal


                    g_codlote    = lotesfin.codlote
                    g_nomlote    = ALLTRIM(infculti.nomlote)
                    * codigo y nombre del lote que se esta trabajando

                    g_arriendo   = lotesfin.arrend

                    * Esta en 1 si el lote figura como en arriendo  o 2 si es
                    * propio. Para lote propio, se pide que confirme si
                    * reporta el valor de arriendo como administracion.

                    g_codcul     = infculti.codcultivo
                    * codigo del cultivo en el cual se trabaja arroz, sorgo, etc

                    g_fecinicio  = labculti.fechalab
                    * fecha en la cual se hizo la labor
                    g_ano        = YEAR(g_fecinicio)
                    mes          = MONTH(g_fecinicio)

                    IF mes > 6
                        g_semestre   = 2
                    ELSE
                        g_semestre   = 1
                    ENDIF

                    g_codlab     = labculti.codlabor
                    g_nomlab     = ALLTRIM(labor.nomlabor)
                    * codigo y nombre de la labor realizada

                    valtotal     = labculti.costotal
                    * valor total de la labor

                    g_codprov    = labculti.codprov
                    g_nomprov    = ALLTRIM(iprov.nombprov)
                    * codigo y nombre del provedor

                    g_codcc      = ALLTRIM(infculti.codcc)
                    * codigo del centro de costo

                    g_numcontrol = infculti.numcontrol
                    * codigo de control para este caso es -1 porque no hemos trabajado con lotes

                    g_numcheq    = ""

                    g_numfecha   = labculti.num_fecha

                    DO rec_conta.prg WITH nomtab

                    ***********  ***********  ***********  ***********  ***********  *************
                    ***********  procedimiento para generar los pagos de los contratos ***********
                    ***********  ***********  ***********  ***********  ***********  *************
                    IF g_numfecha != 0
                        IF labculti.num_fecha = dfechas.num_fecha
                            WAIT "Generando los comprobantes de PAGOS " + ALLTRIM(labculti.nomlabor) NOWAIT WINDOWS AT 25,25
                            nomtab       = "Pagos"
                            g_numcheq    = ALLTRIM(dfechas.numcheq)
                            g_fecinicio  = dfechas.fecha
                            vvalor       = dfechas.valor
                            detal1       = "21"
                            g_codcp      = labculti.codcp

                            DO rec_conta.prg WITH nomtab

                        ENDIF
                    ELSE
                        MESSAGEBOX("La Información del CONTRATO contiene errores, por lo";
                            +CHR(13)+"tanto los comprobantes de PAGOS no se pueden generar";
                            +CHR(13)+CHR(13)+"Solicite asistencia a FEDEARROZ";
                            +CHR(13)+CHR(13)+"Desea Continuar",20,"!ERROR - dfechas ¡")
                        g_contabilidad = .F.
                    ENDIF
                ENDSCAN
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
            * estos dos son los que manejan los progressbar en el formulario principal


        ENDIF
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   ************** J O R N A L E S ************   **************   ************
        IF gen_jorna

            USE jorculti IN 9

            USE infculti IN 10
            SELECT infculti
            SET ORDER TO TAG numcontrol

            USE labor IN 11
            SELECT labor
            SET ORDER TO 1

            USE iprov IN 12
            SELECT iprov
            SET ORDER TO 1

            USE lotesfin IN 13
            SELECT lotesfin
            SET ORDER TO TAG codlote

            USE dfechas IN 14
            SELECT dfechas
            SET ORDER TO TAG num_fecha


            SELECT jorculti
            SET RELATION TO jorculti.numcontrol INTO infculti ADDITIVE
            SET RELATION TO jorculti.codlabor INTO labor ADDITIVE
            SET RELATION TO jorculti.codprov INTO iprov ADDITIVE
            SET RELATION TO jorculti.num_fecha INTO dfechas ADDITIVE

            SELECT infculti
            SET RELATION TO infculti.codlote INTO lotesfin ADDITIVE

            SELECT jorculti
            COUNT ALL TO v_cuan FOR !DELETE()

            g_prototal = g_prototal + 1
            g_probar1.OBJECT.VALUE = g_prototal
            GO TOP

            IF v_cuan != 0
                g_probar2.OBJECT.MAX = v_cuan
                SCAN
                    WAIT "Generando los comprobantes de JORNALES "+ ALLTRIM(labor.nomlabor)  NOWAIT WINDOWS AT 25,25
                    nomtab       = "Jorna"
                    * para que funcione con librecon

                    cred         = .F.

                    g_recno = RECNO()
                    g_probar2.OBJECT.VALUE = g_recno
                    * estos dos son los que manejan los progressbar en el formulario principal


                    g_codlote    = lotesfin.codlote
                    g_nomlote    = ALLTRIM(infculti.nomlote)
                    * codigo y nombre del lote que se esta trabajando

                    g_arriendo   = lotesfin.arrend

                    * Esta en 1 si el lote figura como en arriendo  o 2 si es
                    * propio. Para lote propio, se pide que confirme si
                    * reporta el valor de arriendo como administracion.

                    g_codcul     = infculti.codcultivo

                    g_fecinicio  = jorculti.fechalab
                    * fecha en la cual se hizo la labor
                    g_ano        = YEAR(g_fecinicio)
                    mes          = MONTH(g_fecinicio)

                    IF mes > 6
                        g_semestre   = 2
                    ELSE
                        g_semestre   = 1
                    ENDIF

                    g_codlab     = jorculti.codlabor
                    g_nomlab     = ALLTRIM(labor.nomlabor)
                    * codigo y nombre de la labor realizada

                    valtotal     = jorculti.costotal
                    * valor total de la labor

                    g_codprov    = jorculti.codprov
                    g_nomprov    = ALLTRIM(iprov.nombprov)
                    * codigo y nombre del provedor

                    g_codcc      = ALLTRIM(infculti.codcc)
                    * codigo del centro de costo

                    g_numcontrol = infculti.numcontrol
                    * codigo de control para este caso es -1 porque no hemos trabajado con lotes

                    g_numfecha    = jorculti.num_fecha

                    DO rec_conta.prg WITH nomtab

                    ***********  ***********  ***********  ***********  ***********  *************
                    ***********  procedimiento para generar los pagos de los JORNALES ***********
                    ***********  ***********  ***********  ***********  ***********  *************
                    IF g_numfecha != 0
                        IF jorculti.num_fecha = dfechas.num_fecha
                            WAIT "Generando los comprobantes de PAGOS "+ ALLTRIM(labor.nomlabor) NOWAIT WINDOWS AT 25,25
                            nomtab       = "Pagos"
                            g_numcheq    = ALLTRIM(dfechas.numcheq)
                            g_fecinicio  = dfechas.fecha
                            vvalor       = dfechas.valor
                            detal1       = "21"
                            g_codcp      = jorculti.codcp

                            DO rec_conta.prg WITH nomtab

                        ENDIF
                    ELSE
                        MESSAGEBOX("La Información del JORNAL contiene errores, por lo";
                            +CHR(13)+"tanto los comprobantes de PAGOS no se pueden generar";
                            +CHR(13)+CHR(13)+"Solicite asistencia a FEDEARROZ";
                            +CHR(13)+CHR(13)+"Desea Continuar",20,"!ERROR - dfechas ¡")
                        g_contabilidad = .F.
                    ENDIF


                ENDSCAN
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
            * estos dos son los que manejan los progressbar en el formulario principal


        ENDIF
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   ************** M A Q U I N A R I A ************   **************   ************
        IF gen_utimaq

            USE Maqculti IN 9

            USE infculti IN 10
            SELECT infculti
            SET ORDER TO TAG numcontrol

            USE labor IN 11
            SELECT labor
            SET ORDER TO 1

            USE lotesfin IN 13
            SELECT lotesfin
            SET ORDER TO TAG codlote

            SELECT Maqculti
            SET RELATION TO Maqculti.numcontrol INTO infculti ADDITIVE
            SET RELATION TO Maqculti.codlabor INTO labor ADDITIVE


            SELECT infculti
            SET RELATION TO infculti.codlote INTO lotesfin ADDITIVE

            SELECT Maqculti
            COUNT ALL TO v_cuan FOR !DELETE()

            g_prototal = g_prototal + 1
            g_probar1.OBJECT.VALUE = g_prototal
            GO TOP

            IF v_cuan != 0
                g_probar2.OBJECT.MAX = v_cuan
                SCAN
                    WAIT "Generando los comprobantes de MAQUINARIA " + ALLTRIM(labor.nomlabor) NOWAIT WINDOWS AT 25,25
                    nomtab       = "Maqcu"
                    * para que funcione con librecon

                    g_recno = RECNO()
                    g_probar2.OBJECT.VALUE = g_recno
                    * estos dos son los que manejan los progressbar en el formulario principal

                    g_codlote    = lotesfin.codlote
                    g_nomlote    = ALLTRIM(infculti.nomlote)
                    * codigo y nombre del lote que se esta trabajando

                    g_arriendo   = lotesfin.arrend

                    * Esta en 1 si el lote figura como en arriendo  o 2 si es
                    * propio. Para lote propio, se pide que confirme si
                    * reporta el valor de arriendo como administracion.

                    g_codcul     = infculti.codcultivo

                    g_fecinicio  = Maqculti.fechamaq
                    * fecha en la cual se hizo la labor

                    g_ano        = YEAR(g_fecinicio)
                    mes          = MONTH(g_fecinicio)

                    IF mes > 6
                        g_semestre   = 2
                    ELSE
                        g_semestre   = 1
                    ENDIF

                    g_codlab     = Maqculti.codlabor
                    g_nomlab     = ALLTRIM(labor.nomlabor)
                    * codigo y nombre de la labor realizada

                    valtotal     = Maqculti.costotal
                    * valor total de la labor

                    g_codcc      = ALLTRIM(infculti.codcc)
                    * codigo del centro de costo

                    g_numcontrol = infculti.numcontrol
                    * codigo de control para este caso es -1 porque no hemos trabajado con lotes

                    *g_codcp      = Maqculti.codcp
                    g_codcp      = 0

                    depre        = Maqculti.depreci
                    * depreciacion maquinaria

                    imp          = Maqculti.impuesto
                    * Impuestos Pagos por Anticipado

                    inter1       = Maqculti.interes
                    * Intereses Pago por Anticipado

                    repar        = Maqculti.reparacion
                    * Provisión Reparación

                    opera        = Maqculti.operario
                    * Nomina para la parte del uso del empleado

                    totalmaq     = Maqculti.costotal
                    * Costos de Mecanizacion

                    DO rec_conta.prg WITH nomtab
                ENDSCAN
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
            * estos dos son los que manejan los progressbar en el formulario principal

        ENDIF

        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   U S O S    F U E R A   D E   C U L T I V O    ************   **************
        IF  gen_usocul

            USE ufc IN 9

            USE auxufc IN 10
            SELECT auxufc
            SET ORDER TO TAG codufc

            USE tipinsu IN 11
            SELECT tipinsu
            SET ORDER TO TAG codtipo

            USE insumo IN 12
            SELECT insumo
            SET ORDER TO TAG codinsum

            USE compins IN 13

            USE ubica IN 14
            SELECT ubica
            SET ORDER TO TAG codfin

            SELECT ufc
            SET RELATION TO ufc.codufc INTO auxufc ADDITIVE
            SET RELATION TO ufc.codfin INTO ubica ADDITIVE
            SET RELATION TO ufc.codtipo INTO tipinsu ADDITIVE

            SELECT auxufc
            SET RELATION TO codinsum INTO insumo ADDITIVE

            SELECT ufc
            COUNT ALL TO v_cuan FOR !DELETE()

            g_prototal = g_prototal + 1
            g_probar1.OBJECT.VALUE = g_prototal
            GO TOP

            IF v_cuan != 0
                g_probar2.OBJECT.MAX = v_cuan
                SCAN
                    g_codufc     = ufc.codufc
                    WAIT "Generando los comprobantes de USOS FUERA DE CULTIVO  "+ ALLTRIM(STR(g_codufc)) NOWAIT WINDOWS AT 25,25
                    qllama       = "UFC"
                    nomtab       = "Usufu"
                    * para que funcione con librecon

                    g_recno = RECNO()
                    g_probar2.OBJECT.VALUE = g_recno
                    * estos dos son los que manejan los progressbar en el formulario principal

                    g_codufc     = ufc.codufc
                    * codigo de la aplicacion

                    g_fecinicio  = ufc.fechaufc
                    * fecha en la cual se hizo la labor


                    g_ano        = YEAR(g_fecinicio)
                    auxano       = g_fecinicio
                    mes          = MONTH(g_fecinicio)

                    IF mes > 6
                        g_semestre   = 2
                    ELSE
                        g_semestre   = 1
                    ENDIF

                    g_codcc      = ALLTRIM(ubica.codcc)
                    * codigo del centro de costo

                    g_numcontrol = -1
                    * codigo de control

                    DO rec_conta.prg WITH nomtab
                ENDSCAN
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
            * estos dos son los que manejan los progressbar en el formulario principal
        ENDIF
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   M A N E J O   D E   C O M B U S T I B L E S   ************   **************
        IF gen_mancom

            USE combust IN 9

            USE auxcomb IN 10
            SELECT auxcomb
            SET ORDER TO TAG codufc

            USE tipinsu IN 11
            SELECT tipinsu
            SET ORDER TO TAG codtipo

            USE insumo IN 12
            SELECT insumo
            SET ORDER TO TAG codinsum

            USE compins IN 13

            USE infculti IN 14
            SELECT infculti
            SET ORDER TO TAG numcontrol

            USE lotesfin IN 15
            SELECT lotesfin
            SET ORDER TO TAG codlote

            USE maquinas IN 16
            SELECT maquinas
            SET ORDER TO TAG codmaq

            SELECT combust
            SET RELATION TO combust.codufc INTO auxcomb ADDITIVE
            SET RELATION TO combust.numcontrol INTO infculti ADDITIVE
            SET RELATION TO combust.codmaq INTO maquinas ADDITIVE
            SET RELATION TO combust.codtipo INTO tipinsu ADDITIVE

            SELECT infculti
            SET RELATION TO infculti.codlote INTO lotesfin ADDITIVE

            SELECT auxcomb
            SET RELATION TO codinsum INTO insumo ADDITIVE

            SELECT combust
            COUNT ALL TO v_cuan FOR !DELETE()

            g_prototal = g_prototal + 1
            g_probar1.OBJECT.VALUE = g_prototal
            GO TOP

            IF v_cuan != 0
                g_probar2.OBJECT.MAX = v_cuan
                SCAN
                    WAIT "Generando los comprobantes de MANEJO DE COMBUSTIBLES  "+ ALLTRIM(STR(combust.codufc)) NOWAIT WINDOWS AT 25,25

                    qllama       = "COMBUST"
                    nomtab       = "Usufu"
                    * para que funcione con librecon

                    g_recno = RECNO()
                    g_probar2.OBJECT.VALUE = g_recno
                    * estos dos son los que manejan los progressbar en el formulario principal

                    g_codcomb     = combust.codufc
                    * codigo de la aplicacion

                    g_codlote    = lotesfin.codlote
                    g_nomlote    = ALLTRIM(infculti.nomlote)
                    * codigo y nombre del lote que se esta trabajando

                    g_fecinicio  = combust.fechaufc
                    * fecha en la cual se hizo la labor

                    auxano       = g_fecinicio
                    g_ano        = YEAR(g_fecinicio)
                    mes          = MONTH(g_fecinicio)

                    IF mes > 6
                        g_semestre   = 2
                    ELSE
                        g_semestre   = 1
                    ENDIF

                    g_nommaq     = ALLTRIM(maquinas.nombmaq)
                    g_serie      = ALLTRIM(maquinas.seriemaq)
                    * nombre y serie de la maquina

                    g_codcc      = ALLTRIM(infculti.codcc)
                    * codigo del centro de costo

                    g_numcontrol = combust.numcontrol
                    * codigo de control

                    DO rec_conta.prg WITH nomtab
                ENDSCAN
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
            * estos dos son los que manejan los progressbar en el formulario principal

        ENDIF
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   M A N T E N I M I E N T O S  **************   ************   **************

        IF gen_manmaq

            USE maqmant IN 9

            USE maquinas IN 10
            SELECT maquinas
            SET ORDER TO TAG codmaq

            USE ubica IN 11
            SELECT ubica
            SET ORDER TO TAG codfin

            USE dfechas IN 12
            SELECT dfechas
            SET ORDER TO TAG num_fecha

            USE iprov IN 13
            SELECT iprov
            SET ORDER TO TAG  codprov


            SELECT maqmant
            SET RELATION TO maqmant.codmaq INTO maquinas ADDITIVE
            SET RELATION TO maqmant.codfin INTO ubica ADDITIVE
            SET RELATION TO maqmant.num_fecha INTO dfechas ADDITIVE
            SET RELATION TO maqmant.codprov INTO iprov ADDITIVE

            SELECT maqmant
            COUNT ALL TO v_cuan FOR !DELETE()

            g_prototal = g_prototal + 1
            g_probar1.OBJECT.VALUE = g_prototal
            GO TOP

            IF v_cuan != 0
                g_probar2.OBJECT.MAX = v_cuan
                SCAN
                    WAIT "Generando los comprobantes de MANTENIMIENTOS " + ALLTRIM(maquinas.nombmaq) NOWAIT WINDOWS AT 25,25

                    nomtab       = "Mantm"
                    * para que funcione con librecon

                    cred         = .F.

                    g_recno = RECNO()
                    g_probar2.OBJECT.VALUE = g_recno
                    * estos dos son los que manejan los progressbar en el formulario principal


                    g_codmant    = maqmant.codmant
                    * para grabar los cambios en maqmant

                    g_fecinicio  = maqmant.fechamant
                    * fecha en la cual se hizo la labor

                    g_ano        = YEAR(g_fecinicio)
                    mes          = MONTH(g_fecinicio)

                    IF mes > 6
                        g_semestre   = 2
                    ELSE
                        g_semestre   = 1
                    ENDIF

                    g_nommaq     = ALLTRIM(maquinas.nombmaq)
                    g_serie      = ALLTRIM(maquinas.seriemaq)
                    * nombre y serie de la maquina

                    g_codcc      = ALLTRIM(ubica.codcc)
                    * codigo del centro de costo

                    g_numcontrol = -1
                    * codigo de control

                    g_codprov    = maqmant.codprov
                    g_nomprov    = ALLTRIM(iprov.nombprov)
                    * codigo y nombre del provedor

                    vdetalle     = ALLTRIM(maqmant.motivomant)
                    * motivo por el cual se reparo la maquina o implemento

                    *g_codcp      = maqmant.codcp
                    g_codcp      = 0
                    * codigo del comprobante

                    g_numfecha   = maqmant.num_fecha

                    valtotal     = maqmant.costomant

                    DO rec_conta.prg WITH nomtab

                    ***********  ***********  ***********  ***********  ***********  *************
                    ***********  procedimiento para generar los pagos de los mantenimientos ******
                    ***********  ***********  ***********  ***********  ***********  *************
                    IF g_numfecha != 0
                        IF maqmant.num_fecha = dfechas.num_fecha
                            WAIT "Generando los comprobantes de PAGOS " + ALLTRIM(maquinas.nombmaq) NOWAIT WINDOWS AT 25,25
                            nomtab       = "Pagos"
                            g_numcheq    = ALLTRIM(dfechas.numcheq)
                            g_fecinicio  = dfechas.fecha
                            vvalor       = dfechas.valor
                            detal1       = "23"

                            DO rec_conta.prg WITH nomtab

                        ENDIF
                    ELSE
                        MESSAGEBOX("La Información del MANTENIMIENTO contiene errores, por lo";
                            +CHR(13)+"tanto los comprobantes de PAGOS no se pueden generar";
                            +CHR(13)+CHR(13)+"Solicite asistencia a FEDEARROZ";
                            +CHR(13)+CHR(13)+"Desea Continuar",20,"!ERROR - dfechas ¡")
                        g_contabilidad = .F.
                    ENDIF
                ENDSCAN
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
            * estos dos son los que manejan los progressbar en el formulario principal
        ENDIF
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   *** P L A N I L L A S ****   **************   ************   **************

        IF gen_planil

            USE planicab IN 9

            USE nomina IN 10
            SELECT nomina
            SET ORDER TO TAG codempl

            USE planilla IN 11
            SELECT planilla
            SET ORDER TO TAG codigopla

            USE novedades IN 12

            USE emplefin IN 13
            SELECT emplefin
            INDEX ON emplefin.codigopla TO codi

            USE asig_nom IN 14
            SELECT asig_nom
            SET ORDER TO TAG coempl

            USE lotesfin IN 15
            SELECT lotesfin
            SET ORDER TO TAG codlote

            USE ubica IN 16
            SELECT ubica
            SET ORDER TO TAG codfin

            SELECT planilla
            SET RELATION TO planilla.codempl INTO nomina ADDITIVE


            SELECT nomina
            SET RELATION TO nomina.codempl INTO asig_nom ADDITIVE

            SELECT asig_nom
            SET RELATION TO asig_nom.codlote INTO lotesfin ADDITIVE
            SET RELATION TO asig_nom.codfin INTO ubica ADDITIVE

            SELECT planicab
            SET RELATION TO planicab.codigopla INTO planilla ADDITIVE
            SET RELATION TO planicab.codigopla INTO emplefin ADDITIVE

            SELECT planicab
            COUNT ALL TO v_cuan FOR !DELETE()

            g_prototal = g_prototal + 1
            g_probar1.OBJECT.VALUE = g_prototal
            GO TOP

            IF v_cuan != 0
                g_probar2.OBJECT.MAX = v_cuan
                SCAN
                    WAIT "Generando los comprobantes de PLANILLAS "+ ALLTRIM(planicab.codigopla) NOWAIT WINDOWS AT 25,25

                    nomtab       = "Planill"
                    * para que funcione con librecon

                    g_recno = RECNO()
                    g_probar2.OBJECT.VALUE = g_recno
                    * estos dos son los que manejan los progressbar en el formulario principal

                    IF asig_nom.numcontrol != -1
                        g_numcontrol = asig_nom.numcontrol
                        g_codcc      = lotesfin.codcc
                    ELSE
                        g_numcontrol = -1
                        g_codcc      = ubica.codcc
                    ENDIF
                    * control general

                    vc_fecha     = planicab.codigopla
                    v_fecha      = SUBSTR(vc_fecha ,9,2) + "/" + SUBSTR(vc_fecha ,6,2) + "/" +LEFT(vc_fecha,4)
                    g_fechaplani = SUBSTR(vc_fecha ,9,2) + "/" + SUBSTR(vc_fecha ,6,2) + "/" +LEFT(vc_fecha,4);
                        + " al "+  SUBSTR(vc_fecha ,19,2) + "/" + SUBSTR(vc_fecha ,16,2) + "/" +SUBSTR(vc_fecha,11,4)
                    g_fecinicio  = v_fecha
                    * manejo de fechas en la cual se hizo la planilla

                    ************   **************   ************   **************************   **************   ************   **************
                    ************   ****    procedimiento que realiza sumas varias ************
                    ************   **************   ************   **************************   **************   ************   **************
                    SELECT emplefin
                    SUM ALL emplefin.Stranspo FOR emplefin.area="Administrativa" TO g_nauxtra1
                    g_nauxtra1 = ROUND(g_nauxtra1,0)
                    SUM ALL emplefin.Stranspo FOR emplefin.area="Produccion" TO g_nauxtra2
                    g_nauxtra2 = ROUND(g_nauxtra2,0)
                    SUM ALL emplefin.Saliment FOR emplefin.area="Administrativa" TO g_nauxalim1
                    g_nauxalim1 = ROUND(g_nauxalim1 ,0)
                    SUM ALL emplefin.Saliment FOR emplefin.area="Produccion" TO g_nauxalim2
                    g_nauxalim2 = ROUND(g_nauxalim2,0)
                    SUM ALL emplefin.Diastraba*emplefin.Sueldodia FOR emplefin.area="Administrativa" TO g_nsueldo1
                    g_nsueldo1 = ROUND(g_nsueldo1,0)
                    SUM ALL emplefin.Diastraba*emplefin.Sueldodia FOR emplefin.area="Produccion" TO g_nsueldo2
                    g_nsueldo2 = ROUND(g_nsueldo2,0)

                    SUM ALL emplefin.hediurna * emplefin.Sueldodia/8+ emplefin.henoctur * emplefin.Sueldodia/8+;
                        emplefin.hfdiurna * emplefin.Sueldodia/8+ emplefin.hfnoctur*  emplefin.Sueldodia/8 ;
                        + emplefin.rnoctur* emplefin.Sueldodia+ emplefin.rfestivo* emplefin.Sueldodia/8 ;
                        FOR emplefin.area="Administrativa" TO g_nhorrec1
                    g_nhorrec1 = ROUND(g_nhorrec1,0)
                    SUM ALL emplefin.hediurna * emplefin.Sueldodia/8+ emplefin.henoctur * emplefin.Sueldodia/8+;
                        emplefin.hfdiurna * emplefin.Sueldodia/8+ emplefin.hfnoctur*  emplefin.Sueldodia/8 ;
                        + emplefin.rnoctur* emplefin.Sueldodia+ emplefin.rfestivo* emplefin.Sueldodia/8 ;
                        FOR emplefin.area="Administrativa" TO g_nhorrec2
                    g_nhorrec2 = ROUND(g_nhorrec2,0)
                    SUM ALL emplefin.otrosdev TO g_notrdev1
                    g_notrdev1 = ROUND(g_notrdev1,0)
                    SUM ALL emplefin.otrosdev TO g_notrdev2
                    g_notrdev2 = ROUND(g_notrdev2,0)

                    SUM ALL emplefin.otrosdesc TO g_ndescu
                    g_ndescu = ROUND(g_ndescu,0)
                    SUM ALL emplefin.salud TO g_nsalud
                    g_nsalud = ROUND(g_nsalud,0)
                    SUM ALL emplefin.pension TO g_npension
                    g_npension = ROUND(g_npension,0)
                    SUM ALL emplefin.solida TO g_nfsolida
                    g_nfsolida = ROUND(g_nfsolida,0)
                    SUM ALL emplefin.retencion TO g_nretefuen
                    g_nretefuen = ROUND(g_nretefuen ,0)
                    ************   **************   ************   **************************   **************   ************   **************
                    ************   **************   ************   **************************   **************   ************   **************

                    SELECT planicab
                    DO rec_conta.prg WITH nomtab

                ENDSCAN
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
            g_probar2.OBJECT.VALUE = 0
            * estos dos son los que manejan los progressbar en el formulario principal
        ENDIF
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************************   **************   ************   **************
        ************   **************   ************   **************  V E N T A S  **************   ************   **************
        IF  gen_ventas

            respu = MESSAGEBOX("Antes de Realizar los comprobantes de Venta" + CHR(13) + ;
                "Revise que Todos los Costos" + CHR(13) + ;
                "Esten Ya Causados" + CHR(13) + CHR(13) + ;
                "- Nominas" + CHR(13) +;
                "- Arriendos" + CHR(13) +;
                "- Depreciaciones" + CHR(13) +;
                "- Otros Gastos" + CHR(13) +;
                "- Demás Ajustes Contables Necesarios" + CHR(13) + CHR(13)+;
                "¿Esta Seguro de Realizar la Venta?",36,"¡Alerta!")

            IF respu = 6

                USE Ventas IN 9

                USE infculti IN 10
                SELECT infculti
                SET ORDER TO TAG numcontrol

                USE iprov IN 12
                SELECT iprov
                SET ORDER TO 1

                USE lotesfin IN 13
                SELECT lotesfin
                SET ORDER TO TAG codlote

                USE cultivo IN 14


                SELECT Ventas
                SET RELATION TO Ventas.numcontrol INTO infculti ADDITIVE
                SET RELATION TO Ventas.codprov INTO iprov ADDITIVE


                SELECT Ventas
                COUNT ALL TO v_cuan FOR !DELETE()

                g_prototal = g_prototal + 1
                g_probar1.OBJECT.VALUE = g_prototal
                GO TOP

                IF v_cuan  != 0
                    g_probar2.OBJECT.MAX = v_cuan
                    SCAN
                        WAIT "Generando los comprobantes de TRASLADO A CULTIVOS EN DESARROLLO " + ALLTRIM(iprov.nombprov) NOWAIT WINDOWS AT 25,25
                        ***********  ***********  ***********  ***********  ***********  *************
                        ***********  procedimiento para generar los traslados para luego vender ******
                        ***********  ***********  ***********  ***********  ***********  *************
                        nomtab       = "TrasVen"
                        * para que funcione con librecon

                        g_recno = RECNO()
                        g_probar2.OBJECT.VALUE = g_recno
                        * estos dos son los que manejan los progressbar en el formulario principal


                        g_codlote    = lotesfin.codlote
                        g_nomlote    = ALLTRIM(infculti.nomlote)
                        * codigo y nombre del lote que se esta trabajando

                        g_arriendo   = lotesfin.arrend

                        * Esta en 1 si el lote figura como en arriendo  o 2 si es
                        * propio. Para lote propio, se pide que confirme si
                        * reporta el valor de arriendo como administracion.

                        g_codcul     = infculti.codcultivo

                        g_fecinicio  = Ventas.fecha
                        * fecha en la cual se hizo la venta

                        g_ano        = YEAR(g_fecinicio)
                        mes          = MONTH(g_fecinicio)

                        IF mes > 6
                            g_semestre   = 2
                        ELSE
                            g_semestre   = 1
                        ENDIF

                        g_codprov    = Ventas.codprov
                        g_nomprov    = ALLTRIM(iprov.nombprov)
                        g_codemp     = g_codprov
                        g_nomemp     = g_nomprov
                        * codigo y nombre del provedor

                        g_codcc      = ALLTRIM(infculti.codcc)
                        * codigo del centro de costo

                        g_numcontrol = infculti.numcontrol
                        * codigo de control para este caso es -1 porque no hemos trabajado con lotes

                        g_trasinv = 1
                        * hace el traslado a inventario cuenta "14250101"

                        DO rec_conta.prg WITH nomtab

                        g_trasinv = 2
                        * hace el traslado a producto terminado cuenta "14301501"

                        WAIT "Generando los comprobantes de TRASLADO A PRODUCTO TERMINADO " + ALLTRIM(iprov.nombprov) NOWAIT WINDOWS AT 25,25

                        DO rec_conta.prg WITH nomtab
                        ***********  ***********  ***********  ***********  ***********  *************
                        ***********  procedimiento para generar la venta    ***********  *************
                        ***********  ***********  ***********  ***********  ***********  *************
                        WAIT "Generando los comprobantes de VENTA "+ ALLTRIM(Ventas.numfactu)  NOWAIT WINDOWS AT 25,25

                        nomtab       = "Venta"
                        vpreve       = Ventas.valventa
                        vtove        = Ventas.tonels
                        vporc        = Ventas.porcentaje

                        vrete		 = Ventas.reten
                        votdes       = Ventas.otros
                        vcuofo       = Ventas.cuota
                        vfactu       = Ventas.numfactu

                        vtavta       = ROUND((vpreve * vtove),3)
                        costo 		 = ROUND((vtavta)*((vcuofo+votdes)/100),3)
                        rterte 		 = ROUND((vtavta)*(vrete/100),3)
                        cuota  		 = ROUND((vtavta)*(vcuofo/100),3)
                        otodes 		 = ROUND((vtavta)*(votdes/100),3)
                        cxc    		 = ROUND((vtavta - rterte - costo),3)

                        DO rec_conta.prg WITH nomtab

                    ENDSCAN
                ENDIF
            ENDIF
            FOR i = 2 TO 300
                fesd = "Use in " + ALLTRIM(STR(i))
                &fesd
            ENDFOR
        ENDIF
    ELSE
        MESSAGEBOX("No Hay Cuentas Registradas"+CHR(13)+"  Por Favor Ingrese Cuentas",64,"¡Ojo falta!")
    ENDIF
    g_contabilidad = .F.
    MESSAGEBOX("Ha terminado exitosamente La ";
        +CHR(13)+"Generación de Comprobantes Contables",48,"¡Exitosa!")
    g_probar1.OBJECT.VALUE = 0
ENDDO

g_cerrar = .T.
RETURN g_cerrar
* retorna al formulario para cerrarlo
