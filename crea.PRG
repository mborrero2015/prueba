* *********************************************************
* *
* * 15/01/2003             SACFADB.DBC             17:15:07
* *
* *********************************************************
* *
* * Descripción:
* * Este programa ha sido generado automáticamente por GENDBC
* * Versión 2.26.67
* *
* *********************************************************

CLOSE DATA ALL
CREATE DATABASE 'SACFADB.DBC'


********* Volver a crear el procedimiento *********
IF !FILE([CREA.krt])
    ? [¡Advertencia! No se ha encontrado ningún archivo de procedimientos.]
ELSE
	CLOSE DATABASE
	USE 'SACFADB.DBC'
	g_SetSafety = SET('SAFETY')
	SET SAFETY OFF
	LOCATE FOR Objectname = 'StoredProceduresSource'
	IF FOUND()
        APPEND MEMO Code FROM [CREA.krt] OVERWRITE
	    REPLACE Code WITH SUBSTR(Code, 87, 221456)
	ENDIF
	LOCATE FOR Objectname = 'StoredProceduresObject'
	IF FOUND()
        APPEND MEMO Code FROM [CREA.krt] OVERWRITE
        REPLACE Code WITH SUBSTR(Code, 221543)
	ENDIF
    SET SAFETY &g_SetSafety
	USE
	OPEN DATABASE [SACFADB.DBC]
ENDIF

***** Configuración de tabla para UBICA *****
CREATE TABLE 'UBICA.DBF' NAME 'UBICA' (CODFIN N(4, 0) NOT NULL, ;
                    NOMFIN C(20) NOT NULL, ;
                    PAIS C(10) NOT NULL, ;
                    DEPARTAM C(14) NOT NULL, ;
                    MUNICIPIO C(17) NOT NULL, ;
                    PROPIETA C(30) NOT NULL, ;
                    ESTACIONM C(30) NOT NULL, ;
                    MARCAFIN L NULL, ;
                    CODCC C(8) NOT NULL)

***** Crear cada índice para UBICA *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'UBICA' ADD PRIMARY KEY CODFIN TAG CODFIN
INDEX ON CODCC TAG CODCC

***** Cambiar propiedades para UBICA *****
CREATE TRIGGER ON 'UBICA' FOR DELETE AS __ri_delete_ubica()
CREATE TRIGGER ON 'UBICA' FOR UPDATE AS __ri_update_ubica()

***** Configuración de tabla para LOTESFIN *****
CREATE TABLE 'LOTESFIN.DBF' NAME 'LOTESFIN' (CODFIN N(4, 0) NOT NULL, ;
                       CODLOTE N(4, 0) NOT NULL, ;
                       NOMLOTE C(20) NOT NULL, ;
                       AREA N(8, 2) NOT NULL, ;
                       ARREND N(1, 0) NOT NULL, ;
                       COSARRTOT N(10, 0) NULL, ;
                       COSARRHEC N(10, 0) NULL, ;
                       VALCOMTOT N(10, 0) NULL, ;
                       VALCOMHEC N(10, 0) NULL, ;
                       CONTLOTE N(4, 0) NULL, ;
                       MARCALOTE L NULL, ;
                       CULTI C(7) NULL, ;
                       CODCC C(8) NOT NULL)

***** Crear cada índice para LOTESFIN *****
SET COLLATE TO 'MACHINE'
INDEX ON CODFIN TAG CODFIN
ALTER TABLE 'LOTESFIN' ADD PRIMARY KEY CODLOTE TAG CODLOTE
INDEX ON CODCC TAG CODCC

***** Cambiar propiedades para LOTESFIN *****
CREATE TRIGGER ON 'LOTESFIN' FOR DELETE AS __ri_delete_lotesfin()
CREATE TRIGGER ON 'LOTESFIN' FOR INSERT AS __ri_insert_lotesfin()
CREATE TRIGGER ON 'LOTESFIN' FOR UPDATE AS __ri_update_lotesfin()

***** Configuración de tabla para MAQMANT *****
CREATE TABLE 'MAQMANT.DBF' NAME 'MAQMANT' (CODFIN N(4, 0) NOT NULL, ;
                      CODMANT N(4, 0) NOT NULL, ;
                      CODMAQ N(4, 0) NOT NULL, ;
                      CODTMAQ N(1, 0) NOT NULL, ;
                      FECHAMANT D NOT NULL, ;
                      MOTIVOMANT C(50) NOT NULL, ;
                      COSTOMANT N(8, 0) NOT NULL, ;
                      SERIEMAQ C(10) NOT NULL, ;
                      TALLER C(25) NOT NULL, ;
                      MARCAMANT L NOT NULL, ;
                      CODCP N(10, 0) NOT NULL, ;
                      CODPROV N(4, 0) NOT NULL, ;
                      NUM_FECHA N(8, 0) NOT NULL)

***** Crear cada índice para MAQMANT *****
SET COLLATE TO 'MACHINE'
INDEX ON CODMAQ TAG CODMAQ
INDEX ON CODPROV TAG CODPROV

***** Cambiar propiedades para MAQMANT *****
CREATE TRIGGER ON 'MAQMANT' FOR INSERT AS __ri_insert_maqmant()
CREATE TRIGGER ON 'MAQMANT' FOR UPDATE AS __ri_update_maqmant()

***** Configuración de tabla para AYUDA *****
CREATE TABLE 'AYUDA.DBF' NAME 'AYUDA' (CONSECUTIVO N(4, 0) NOT NULL, ;
                    TEXTO M NOT NULL, ;
                    CLAVE C(20) NOT NULL)

***** Crear cada índice para AYUDA *****

***** Cambiar propiedades para AYUDA *****

***** Configuración de tabla para GENPARA *****
CREATE TABLE 'GENPARA.DBF' NAME 'GENPARA' (COSTOADM N(5, 4) NOT NULL, ;
                      TASAOPORT N(5, 4) NOT NULL, ;
                      ARRIENDO N(5, 4) NOT NULL, ;
                      RETENCION N(5, 4) NOT NULL, ;
                      AYUDA_INICIO L NOT NULL)

***** Crear cada índice para GENPARA *****

***** Cambiar propiedades para GENPARA *****

***** Configuración de tabla para MAQPARA *****
CREATE TABLE 'MAQPARA.DBF' NAME 'MAQPARA' (SALVAMENTO N(5, 4) NOT NULL, ;
                      INTERES N(5, 4) NOT NULL, ;
                      IMPUSEGU N(5, 4) NOT NULL, ;
                      CABALLAJE N(5, 4) NOT NULL, ;
                      REPARMANT N(5, 4) NOT NULL, ;
                      SALARIO N(5, 4) NOT NULL)

***** Crear cada índice para MAQPARA *****

***** Cambiar propiedades para MAQPARA *****

***** Configuración de tabla para CULTIVO *****
CREATE TABLE 'CULTIVO.DBF' NAME 'CULTIVO' (CODCULTIVO N(3, 0) NOT NULL, ;
                      NOMCULTIVO C(7) NOT NULL, ;
                      MARCACUL L NOT NULL, ;
                      COSCUL C(10) NOT NULL, ;
                      NAT1 C(9) NOT NULL, ;
                      COSVEN C(10) NOT NULL, ;
                      NAT2 C(9) NOT NULL, ;
                      INGVEN C(10) NOT NULL, ;
                      NAT3 C(9) NOT NULL, ;
                      GUIA1 C(4) NOT NULL, ;
                      GUIA2 C(4) NOT NULL, ;
                      GUIA3 C(4) NOT NULL, ;
                      SALICULT C(10) NOT NULL, ;
                      NAT4 C(9) NOT NULL)

***** Crear cada índice para CULTIVO *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'CULTIVO' ADD PRIMARY KEY CODCULTIVO TAG CODCULTIVO

***** Cambiar propiedades para CULTIVO *****
CREATE TRIGGER ON 'CULTIVO' FOR DELETE AS __ri_delete_cultivo()
CREATE TRIGGER ON 'CULTIVO' FOR UPDATE AS __ri_update_cultivo()

***** Configuración de tabla para CREDITO *****
CREATE TABLE 'CREDITO.DBF' NAME 'CREDITO' (CODFIN N(4, 0) NOT NULL, ;
                      ANO N(4, 0) NOT NULL, ;
                      SEMESTRE N(1, 0) NOT NULL, ;
                      CODCRED N(4, 0) NOT NULL, ;
                      NUMCRED C(10) NOT NULL, ;
                      FINAGRO N(1, 0) NOT NULL, ;
                      NOMPROV C(30) NOT NULL, ;
                      CODPROV N(4, 0) NOT NULL, ;
                      FINANHEC N(8, 0) NOT NULL, ;
                      HASFINAN N(8, 2) NOT NULL, ;
                      VALCRED N(11, 0) NOT NULL, ;
                      INTERES N(3, 2) NOT NULL, ;
                      FORMINT N(1, 0) NOT NULL, ;
                      PERPAGO N(2, 0) NOT NULL, ;
                      GASTOSCRED N(10, 0) NOT NULL, ;
                      USOCRED N(3, 0) NOT NULL, ;
                      MARCACRED L NOT NULL, ;
                      CONDTF L NOT NULL, ;
                      CODCP N(10, 0) NOT NULL)

***** Crear cada índice para CREDITO *****
SET COLLATE TO 'MACHINE'
INDEX ON NUMCRED TAG NUMCRED
INDEX ON CODFIN TAG CODFIN
ALTER TABLE 'CREDITO' ADD PRIMARY KEY CODCRED TAG CODCRED
INDEX ON CODPROV TAG CODPROV
INDEX ON NOMPROV TAG NOMPROV

***** Cambiar propiedades para CREDITO *****
CREATE TRIGGER ON 'CREDITO' FOR DELETE AS __ri_delete_credito()
CREATE TRIGGER ON 'CREDITO' FOR INSERT AS __ri_insert_credito()
CREATE TRIGGER ON 'CREDITO' FOR UPDATE AS __ri_update_credito()

***** Configuración de tabla para DCREDITO *****
CREATE TABLE 'DCREDITO.DBF' NAME 'DCREDITO' (CODCRED N(4, 0) NOT NULL, ;
                       TIPO N(1, 0) NOT NULL, ;
                       SECUENCIA N(2, 0) NULL, ;
                       FECHA D NULL, ;
                       PORCENTAJE N(9, 6) NOT NULL, ;
                       VALOR N(9, 0) NOT NULL, ;
                       CODLOTE N(4, 0) NULL, ;
                       NOMLOTE C(20) NULL, ;
                       MARCA L NULL, ;
                       CODCP N(10, 0) NOT NULL, ;
                       NUMCHEQ C(12) NOT NULL, ;
                       MOTIVO C(30) NOT NULL)

***** Crear cada índice para DCREDITO *****
SET COLLATE TO 'MACHINE'
INDEX ON CODCRED TAG CODCRED

***** Cambiar propiedades para DCREDITO *****
CREATE TRIGGER ON 'DCREDITO' FOR INSERT AS __ri_insert_dcredito()
CREATE TRIGGER ON 'DCREDITO' FOR UPDATE AS __ri_update_dcredito()

***** Configuración de tabla para INFCULTI *****
CREATE TABLE 'INFCULTI.DBF' NAME 'INFCULTI' (MARCAINF L NOT NULL, ;
                       CODFIN N(4, 0) NOT NULL, ;
                       CODLOTE N(4, 0) NOT NULL, ;
                       NOMLOTE C(20) NOT NULL, ;
                       ANO N(4, 0) NOT NULL, ;
                       SEMESTRE N(1, 0) NOT NULL, ;
                       CODCULTIVO N(2, 0) NOT NULL, ;
                       CULTIVO C(7) NOT NULL, ;
                       SISTEMA N(1, 0) NOT NULL, ;
                       TONELADAS N(12, 5) NOT NULL, ;
                       ASISTEC N(7, 0) NOT NULL, ;
                       ARRHEC N(10, 0) NOT NULL, ;
                       ARREND N(1, 0) NOT NULL, ;
                       COSTOTSC N(16, 5) NOT NULL, ;
                       INGTSC N(12, 0) NOT NULL, ;
                       TIRSC N(16, 5) NOT NULL, ;
                       TIRCC N(16, 5) NOT NULL, ;
                       VPNSC N(16, 5) NOT NULL, ;
                       RBCSC N(16, 5) NOT NULL, ;
                       COSTOTCC N(16, 5) NOT NULL, ;
                       INGTCC N(12, 0) NOT NULL, ;
                       VPNCC N(16, 5) NOT NULL, ;
                       RBCCC N(16, 5) NOT NULL, ;
                       ASISTENCIA N(6, 5) NOT NULL, ;
                       ARRIENDO N(6, 5) NOT NULL, ;
                       PREPAR N(6, 5) NOT NULL, ;
                       SIEMBRA N(6, 5) NOT NULL, ;
                       SEMILLA N(6, 5) NOT NULL, ;
                       LABSIEM N(6, 5) NOT NULL, ;
                       RIEGO N(6, 5) NOT NULL, ;
                       AGUA N(6, 5) NOT NULL, ;
                       MMTO N(6, 5) NOT NULL, ;
                       FERTIL N(6, 5) NOT NULL, ;
                       INSFERT N(6, 5) NOT NULL, ;
                       APFERT N(6, 5) NOT NULL, ;
                       MALEZAS N(6, 5) NOT NULL, ;
                       HERBIC N(6, 5) NOT NULL, ;
                       APHERB N(6, 5) NOT NULL, ;
                       MANHERB N(6, 5) NOT NULL, ;
                       PLAGAS N(6, 5) NOT NULL, ;
                       INSECT N(6, 5) NOT NULL, ;
                       APINSECT N(6, 5) NOT NULL, ;
                       MANINSECT N(6, 5) NOT NULL, ;
                       ENFERM N(6, 5) NOT NULL, ;
                       FUNGIC N(6, 5) NOT NULL, ;
                       APFUNGIC N(6, 5) NOT NULL, ;
                       TOTINS N(6, 5) NOT NULL, ;
                       INSUMOS N(6, 5) NOT NULL, ;
                       APINSUM N(6, 5) NOT NULL, ;
                       RECOLEC N(6, 5) NOT NULL, ;
                       TRANSPORTE N(6, 5) NOT NULL, ;
                       VARIOS N(6, 5) NOT NULL, ;
                       SUBTOTAL N(6, 5) NOT NULL, ;
                       ADMON N(6, 5) NOT NULL, ;
                       INTERESES N(6, 5) NOT NULL, ;
                       IMPUESTOS N(6, 5) NOT NULL, ;
                       TOTALES N(6, 5) NOT NULL, ;
                       COSTOCONTA N(12, 0) NOT NULL, ;
                       NUMCONTROL N(8, 0) NOT NULL, ;
                       CODCC C(8) NOT NULL)

***** Crear cada índice para INFCULTI *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'INFCULTI' ADD PRIMARY KEY NUMCONTROL TAG NUMCONTROL
INDEX ON CODCULTIVO TAG CODCULT
INDEX ON CODLOTE TAG CODLOTE

***** Cambiar propiedades para INFCULTI *****
CREATE TRIGGER ON 'INFCULTI' FOR DELETE AS __ri_delete_infculti()
CREATE TRIGGER ON 'INFCULTI' FOR INSERT AS __ri_insert_infculti()
CREATE TRIGGER ON 'INFCULTI' FOR UPDATE AS __ri_update_infculti()

***** Configuración de tabla para LABOR *****
CREATE TABLE 'LABOR.DBF' NAME 'LABOR' (CODLABOR N(4, 0) NOT NULL, ;
                    NOMLABOR C(30) NOT NULL, ;
                    ARROZ L NOT NULL, ;
                    SORGO L NOT NULL, ;
                    SOYA L NOT NULL, ;
                    MAIZ L NOT NULL, ;
                    ALGODON L NOT NULL, ;
                    FORMLABOR N(1, 0) NOT NULL, ;
                    MARCALAB L NOT NULL, ;
                    CODCOSTO C(40) NOT NULL)

***** Crear cada índice para LABOR *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'LABOR' ADD PRIMARY KEY CODLABOR TAG CODLABOR

***** Cambiar propiedades para LABOR *****
CREATE TRIGGER ON 'LABOR' FOR DELETE AS __ri_delete_labor()
CREATE TRIGGER ON 'LABOR' FOR UPDATE AS __ri_update_labor()

***** Configuración de tabla para LABCULTI *****
CREATE TABLE 'LABCULTI.DBF' NAME 'LABCULTI' (NUMCONTROL N(8, 0) NOT NULL, ;
                       CODLABOR N(4, 0) NOT NULL, ;
                       NOMLABOR C(30) NOT NULL, ;
                       VALUNICO N(14, 5) NOT NULL, ;
                       CANTPASES N(11, 5) NOT NULL, ;
                       FECHALAB D NOT NULL, ;
                       FORMALAB N(1, 0) NOT NULL, ;
                       CODPROV N(3, 0) NOT NULL, ;
                       PESOBULTO N(11, 5) NOT NULL, ;
                       TARIFA N(2, 0) NOT NULL, ;
                       OPCION N(1, 0) NOT NULL, ;
                       OPCMAN N(1, 0) NOT NULL, ;
                       HECTAREA N(8, 5) NOT NULL, ;
                       COSTOTAL N(10, 0) NOT NULL, ;
                       VAPAGO N(10, 0) NOT NULL, ;
                       NUM_FECHA N(8, 0) NOT NULL, ;
                       MARCA L NOT NULL, ;
                       NOMEMPL C(30) NOT NULL, ;
                       CODAPLICA C(8) NOT NULL, ;
                       OBSERVA C(30) NOT NULL, ;
                       FIJO N(1, 0) NOT NULL, ;
                       CODCP N(4, 0) NOT NULL)

***** Crear cada índice para LABCULTI *****
SET COLLATE TO 'MACHINE'
INDEX ON CODLABOR TAG CODLABOR
ALTER TABLE 'LABCULTI' ADD PRIMARY KEY NUM_FECHA TAG NUMFECHA
INDEX ON CODPROV TAG CODPROV
INDEX ON NUMCONTROL TAG NUMCONTROL

***** Cambiar propiedades para LABCULTI *****
CREATE TRIGGER ON 'LABCULTI' FOR INSERT AS __ri_insert_labculti()
CREATE TRIGGER ON 'LABCULTI' FOR UPDATE AS __ri_update_labculti()

***** Configuración de tabla para DFECHAS *****
CREATE TABLE 'DFECHAS.DBF' NAME 'DFECHAS' (NUM_FECHA N(8, 0) NOT NULL, ;
                      SECUENCIA N(8, 0) NOT NULL, ;
                      FECHA D NOT NULL, ;
                      PORCENTAJE N(6, 2) NOT NULL, ;
                      VALOR N(10, 0) NOT NULL, ;
                      CODCP N(4, 0) NOT NULL, ;
                      NUMCHEQ C(12) NOT NULL)

***** Crear cada índice para DFECHAS *****
SET COLLATE TO 'MACHINE'
INDEX ON NUM_FECHA TAG NUM_FECHA

***** Cambiar propiedades para DFECHAS *****

***** Configuración de tabla para JORCULTI *****
CREATE TABLE 'JORCULTI.DBF' NAME 'JORCULTI' (NUMCONTROL N(8, 0) NOT NULL, ;
                       CODLABOR N(4, 0) NOT NULL, ;
                       FECHALAB D NOT NULL, ;
                       CANTJORN N(8, 5) NOT NULL, ;
                       CUNITJORN N(8, 5) NOT NULL, ;
                       CODEMPL N(4, 0) NOT NULL, ;
                       NOMEMIP C(30) NOT NULL, ;
                       CODPROV N(4, 0) NOT NULL, ;
                       HORASEMP C(6) NOT NULL, ;
                       COSTOTAL N(12, 5) NOT NULL, ;
                       FORMAJOR N(1, 0) NOT NULL, ;
                       ACCION N(1, 0) NOT NULL, ;
                       MARCAJORN L NOT NULL, ;
                       NOMLABOR C(30) NOT NULL, ;
                       CODAPLICA C(8) NOT NULL, ;
                       OBSERVA C(30) NOT NULL, ;
                       VAPAGO N(10, 0) NOT NULL, ;
                       NUM_FECHA N(8, 0) NOT NULL, ;
                       CODCP N(4, 0) NOT NULL)

***** Crear cada índice para JORCULTI *****
SET COLLATE TO 'MACHINE'
INDEX ON CODLABOR TAG CODLABOR
INDEX ON CODEMPL TAG CODEMPLE
INDEX ON CODPROV TAG CODPROV
INDEX ON NUM_FECHA TAG NUM_FECHA
INDEX ON NUMCONTROL TAG NUMCONTROL

***** Cambiar propiedades para JORCULTI *****
CREATE TRIGGER ON 'JORCULTI' FOR INSERT AS __ri_insert_jorculti()
CREATE TRIGGER ON 'JORCULTI' FOR UPDATE AS __ri_update_jorculti()

***** Configuración de tabla para UNDCOMP *****
CREATE TABLE 'UNDCOMP.DBF' NAME 'UNDCOMP' (CODUNIDAD N(2, 0) NOT NULL, ;
                      CODIGO N(2, 0) NOT NULL, ;
                      NOMBREUND C(10) NOT NULL, ;
                      EQUIVALE N(8, 5) NOT NULL)

***** Crear cada índice para UNDCOMP *****
SET COLLATE TO 'MACHINE'
INDEX ON CODUNIDAD TAG CODUNIDAD
ALTER TABLE 'UNDCOMP' ADD PRIMARY KEY CODIGO TAG CODIGO

***** Cambiar propiedades para UNDCOMP *****
CREATE TRIGGER ON 'UNDCOMP' FOR DELETE AS __ri_delete_undcomp()
CREATE TRIGGER ON 'UNDCOMP' FOR INSERT AS __ri_insert_undcomp()
CREATE TRIGGER ON 'UNDCOMP' FOR UPDATE AS __ri_update_undcomp()

***** Configuración de tabla para UNDMEDIS *****
CREATE TABLE 'UNDMEDIS.DBF' NAME 'UNDMEDIS' (CODUNIDAD N(1, 0) NOT NULL, ;
                       NOMMEDINS C(10) NOT NULL, ;
                       ABRENOM C(10) NOT NULL)

***** Crear cada índice para UNDMEDIS *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'UNDMEDIS' ADD PRIMARY KEY CODUNIDAD TAG CODUNIDAD

***** Cambiar propiedades para UNDMEDIS *****
CREATE TRIGGER ON 'UNDMEDIS' FOR DELETE AS __ri_delete_undmedis()
CREATE TRIGGER ON 'UNDMEDIS' FOR UPDATE AS __ri_update_undmedis()

***** Configuración de tabla para PUC1 *****
CREATE TABLE 'PUC1.DBF' NAME 'PUC1' (CODCONTABL C(10) NOT NULL, ;
                   NOMBRECTA C(100) NOT NULL, ;
                   CODPROV N(4, 0) NOT NULL, ;
                   NIVELCTA C(1) NOT NULL, ;
                   ORIGINAL C(1) NOT NULL, ;
                   CTACIERRE C(10) NOT NULL, ;
                   PYG5 N(13, 0) NOT NULL, ;
                   PYG4 N(13, 0) NOT NULL, ;
                   PYG3 N(13, 0) NOT NULL, ;
                   PYG2 N(13, 0) NOT NULL, ;
                   PYG1 N(13, 0) NOT NULL, ;
                   SALDO5 N(13, 0) NOT NULL, ;
                   SALDO4 N(13, 0) NOT NULL, ;
                   SALDO3 N(13, 0) NOT NULL, ;
                   SALDO2 N(13, 0) NOT NULL, ;
                   SALDO1 N(13, 0) NOT NULL, ;
                   SALDOINIC N(13, 0) NOT NULL, ;
                   MDEBITO N(13, 0) NOT NULL, ;
                   MCREDITO N(13, 0) NOT NULL, ;
                   SALDOTR1 N(15, 0) NOT NULL, ;
                   CDB N(13, 0) NOT NULL, ;
                   CCR N(13, 0) NOT NULL, ;
                   SALDOTR2 N(13, 0) NOT NULL, ;
                   TIPOSALDO C(7) NOT NULL)

***** Crear cada índice para PUC1 *****
SET COLLATE TO 'MACHINE'
INDEX ON CODCONTABL TAG CODCONTABL

***** Cambiar propiedades para PUC1 *****

***** Configuración de tabla para IPROV *****
CREATE TABLE 'IPROV.DBF' NAME 'IPROV' (CODPROV N(4, 0) NOT NULL, ;
                    NOMBPROV C(30) NOT NULL, ;
                    DIRECPROV C(30) NOT NULL, ;
                    TELPROV N(10, 0) NOT NULL, ;
                    FAXPROV N(10, 0) NOT NULL, ;
                    CIUDPROV C(23) NOT NULL, ;
                    CONTPROV N(5, 0) NOT NULL, ;
                    NITPROV C(11) NOT NULL, ;
                    MARCAPROV C(1) NOT NULL, ;
                    TIPOENTI C(2) NOT NULL, ;
                    PAISPROV C(15) NOT NULL, ;
                    DINTER C(25) NOT NULL, ;
                    COINTER C(25) NOT NULL)

***** Crear cada índice para IPROV *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'IPROV' ADD PRIMARY KEY CODPROV TAG CODPROV
INDEX ON NOMBPROV TAG NOMBPROV

***** Cambiar propiedades para IPROV *****
CREATE TRIGGER ON 'IPROV' FOR DELETE AS __ri_delete_iprov()
CREATE TRIGGER ON 'IPROV' FOR UPDATE AS __ri_update_iprov()

***** Configuración de tabla para COMPINS *****
CREATE TABLE 'COMPINS.DBF' NAME 'COMPINS' (CODFIN N(4, 0) NOT NULL, ;
                      CODCOMP N(7, 0) NOT NULL, ;
                      CODFACT N(7, 0) NOT NULL, ;
                      NOMINSUM C(15) NOT NULL, ;
                      CODINSUM N(4, 0) NOT NULL, ;
                      NOMTIPO C(15) NOT NULL, ;
                      CODTIPO N(4, 0) NOT NULL, ;
                      CANTUNIT N(11, 5) NOT NULL, ;
                      EQUIVALE N(8, 5) NOT NULL, ;
                      CANTINS N(15, 5) NOT NULL, ;
                      COSTUNIT N(18, 5) NOT NULL, ;
                      NOMUNID C(12) NOT NULL, ;
                      CODUNID N(4, 0) NOT NULL, ;
                      USOTOTINS N(15, 5) NOT NULL, ;
                      SOBRECOSTO N(20, 5) NOT NULL, ;
                      SOBRECONTA N(20, 5) NOT NULL, ;
                      MARCACOMP C(1) NOT NULL, ;
                      TOTAL N(18, 5) NOT NULL, ;
                      FECHACOMP D NOT NULL)

***** Crear cada índice para COMPINS *****
SET COLLATE TO 'MACHINE'
INDEX ON NOMINSUM TAG NOMINSUM
INDEX ON CODINSUM TAG CODINSUM
INDEX ON CODUNID TAG CODUNID
INDEX ON CODFIN TAG CODFIN
INDEX ON FECHACOMP TAG FECHACOMP
INDEX ON CODTIPO TAG CODTIPO
ALTER TABLE 'COMPINS' ADD PRIMARY KEY CODCOMP TAG CODCOMP
INDEX ON CODFACT TAG CODFACT

***** Cambiar propiedades para COMPINS *****
CREATE TRIGGER ON 'COMPINS' FOR INSERT AS __ri_insert_compins()
CREATE TRIGGER ON 'COMPINS' FOR UPDATE AS __ri_update_compins()

***** Configuración de tabla para TIPINSU *****
CREATE TABLE 'TIPINSU.DBF' NAME 'TIPINSU' (CODTIPO N(4, 0) NOT NULL, ;
                      TIPINSUMO C(14) NOT NULL, ;
                      SINGULAR C(20) NOT NULL, ;
                      GRUPO C(4) NOT NULL, ;
                      MARCATIP C(1) NOT NULL, ;
                      CUENTA C(10) NOT NULL, ;
                      NAT1 C(9) NOT NULL, ;
                      FINANCOMPRA C(10) NOT NULL, ;
                      NAT2 C(9) NOT NULL, ;
                      COSTOINSU C(10) NOT NULL, ;
                      NAT3 C(9) NOT NULL, ;
                      MODOCT C(1) NOT NULL, ;
                      IMPRIME L NOT NULL)

***** Crear cada índice para TIPINSU *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'TIPINSU' ADD PRIMARY KEY CODTIPO TAG CODTIPO
INDEX ON TIPINSUMO TAG TIPINSUMO

***** Cambiar propiedades para TIPINSU *****
CREATE TRIGGER ON 'TIPINSU' FOR DELETE AS __ri_delete_tipinsu()
CREATE TRIGGER ON 'TIPINSU' FOR UPDATE AS __ri_update_tipinsu()

***** Configuración de tabla para FACINS *****
CREATE TABLE 'FACINS.DBF' NAME 'FACINS' (CODFIN N(4, 0) NOT NULL, ;
                     CODFACT N(7, 0) NOT NULL, ;
                     NOMPROV C(30) NOT NULL, ;
                     CODPROV N(4, 0) NOT NULL, ;
                     NUMFACT C(9) NOT NULL, ;
                     FECHAFAC D NOT NULL, ;
                     FECVEN1 D NOT NULL, ;
                     FORMAFAC N(1, 0) NOT NULL, ;
                     VALDESC N(11, 0) NOT NULL, ;
                     VALFIN N(11, 0) NOT NULL, ;
                     VALCOM N(11, 0) NOT NULL, ;
                     VALFLETE N(11, 0) NOT NULL, ;
                     VALORIVA N(11, 0) NOT NULL, ;
                     VALORTOT N(20, 5) NOT NULL, ;
                     MARCAFAC L NOT NULL, ;
                     CODCP N(10, 0) NOT NULL, ;
                     PAGADA L NOT NULL, ;
                     TOTAFAC N(20, 0) NOT NULL, ;
                     IMPRIMIR L NOT NULL, ;
                     NUM_FECHA N(8, 0) NOT NULL, ;
                     CODCC C(8) NOT NULL, ;
                     NUMCHEQ C(12) NOT NULL)

***** Crear cada índice para FACINS *****
SET COLLATE TO 'MACHINE'
INDEX ON NUMFACT TAG NUMFACT
INDEX ON CODFIN TAG CODFIN
INDEX ON CODPROV TAG CODPROV
INDEX ON FECHAFAC TAG FECHAFAC
INDEX ON NOMPROV TAG NOMPROV
ALTER TABLE 'FACINS' ADD PRIMARY KEY CODFACT TAG CODFACT

***** Cambiar propiedades para FACINS *****
CREATE TRIGGER ON 'FACINS' FOR INSERT AS __ri_insert_facins()
CREATE TRIGGER ON 'FACINS' FOR UPDATE AS __ri_update_facins()

***** Configuración de tabla para CANVAL *****
CREATE TABLE 'CANVAL.DBF' NAME 'CANVAL' (CODFIN N(4, 0) NOT NULL, ;
                     NOMPROV C(20) NOT NULL, ;
                     Q1 N(15, 0) NOT NULL, ;
                     VAL1 N(15, 0) NOT NULL, ;
                     Q2 N(15, 0) NOT NULL, ;
                     VAL2 N(15, 0) NOT NULL)

***** Crear cada índice para CANVAL *****
SET COLLATE TO 'MACHINE'
INDEX ON NOMPROV TAG NOMPROV

***** Cambiar propiedades para CANVAL *****

***** Configuración de tabla para COMBUST *****
CREATE TABLE 'COMBUST.DBF' NAME 'COMBUST' (NUMCONTROL N(8, 0) NOT NULL, ;
                      CODLABOR N(4, 0) NOT NULL, ;
                      CODAPLIC C(8) NOT NULL, ;
                      CODUFC N(4, 0) NOT NULL, ;
                      CODMAQ N(4, 0) NOT NULL, ;
                      CODINSUM N(4, 0) NOT NULL, ;
                      NOMINSUMO C(15) NOT NULL, ;
                      CODTIPO N(4, 0) NOT NULL, ;
                      FECHAUFC D NOT NULL, ;
                      KILOMETRO N(7, 0) NOT NULL, ;
                      CANTUFC N(11, 5) NOT NULL, ;
                      CODUNID N(4, 0) NOT NULL, ;
                      NOMUNID C(15) NOT NULL, ;
                      EQUIVALE N(8, 5) NOT NULL, ;
                      CANTOTAL N(14, 5) NOT NULL, ;
                      MARCAUFC C(1) NOT NULL, ;
                      CODCP N(10, 0) NOT NULL)

***** Crear cada índice para COMBUST *****
SET COLLATE TO 'MACHINE'
INDEX ON CODMAQ TAG CODMAQ
INDEX ON CODMAQ TAG WIZARD_2
INDEX ON NOMINSUMO TAG NOMINSUMO
INDEX ON CODLABOR TAG CODLABOR
INDEX ON NUMCONTROL TAG NUMCONTROL
INDEX ON CODAPLIC TAG CODAPLIC
ALTER TABLE 'COMBUST' ADD PRIMARY KEY CODUFC TAG CODUFC

***** Cambiar propiedades para COMBUST *****
CREATE TRIGGER ON 'COMBUST' FOR DELETE AS __ri_delete_combust()
CREATE TRIGGER ON 'COMBUST' FOR INSERT AS __ri_insert_combust()
CREATE TRIGGER ON 'COMBUST' FOR UPDATE AS __ri_update_combust()

***** Configuración de tabla para MAQUINAS *****
CREATE TABLE 'MAQUINAS.DBF' NAME 'MAQUINAS' (CODFIN N(4, 0) NOT NULL, ;
                       CODMAQ N(4, 0) NOT NULL, ;
                       CODTMAQ N(1, 0) NOT NULL, ;
                       NOMBMAQ C(15) NOT NULL, ;
                       MARCAMOD C(15) NOT NULL, ;
                       SERIEMAQ C(10) NOT NULL, ;
                       FECHACOMP D NOT NULL, ;
                       VALORCOMP N(11, 0) NOT NULL, ;
                       FECHAVENTA D NOT NULL, ;
                       VALORVENTA N(11, 0) NOT NULL, ;
                       CABALLOS N(6, 5) NOT NULL, ;
                       ANOSUTIL N(6, 5) NOT NULL, ;
                       HORASANO N(4, 0) NOT NULL, ;
                       CODCOMB N(1, 0) NOT NULL, ;
                       CONSHORA N(7, 5) NOT NULL, ;
                       LUBRICANTE N(5, 4) NOT NULL, ;
                       DIASTRAB N(2, 0) NOT NULL, ;
                       HORASTRAB N(2, 0) NOT NULL, ;
                       CONSUMOHP N(5, 4) NOT NULL, ;
                       BOMBEO N(1, 0) NOT NULL, ;
                       CONTMAQ N(4, 0) NOT NULL, ;
                       MARCAMAQ L NOT NULL)

***** Crear cada índice para MAQUINAS *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'MAQUINAS' ADD PRIMARY KEY CODMAQ TAG CODMAQ
INDEX ON CODTMAQ TAG CODTMAQ
INDEX ON CODFIN TAG CODFIN

***** Cambiar propiedades para MAQUINAS *****
CREATE TRIGGER ON 'MAQUINAS' FOR DELETE AS __ri_delete_maquinas()
CREATE TRIGGER ON 'MAQUINAS' FOR INSERT AS __ri_insert_maquinas()
CREATE TRIGGER ON 'MAQUINAS' FOR UPDATE AS __ri_update_maquinas()

***** Configuración de tabla para TIPC *****
CREATE TABLE 'TIPC.DBF' NAME 'TIPC' (FUENTE N(4, 0) NOT NULL, ;
                   NOMBFTE C(25) NOT NULL, ;
                   CONTFTE N(5, 0) NOT NULL, ;
                   MARCAFTE C(1) NOT NULL)

***** Crear cada índice para TIPC *****
SET COLLATE TO 'MACHINE'
INDEX ON FUENTE TAG FUENTE
INDEX ON NOMBFTE TAG NOMBFTE

***** Cambiar propiedades para TIPC *****

***** Configuración de tabla para CPD *****
CREATE TABLE 'CPD.DBF' NAME 'CPD' (CODCOMP N(10, 0) NOT NULL, ;
                  CODCP N(10, 0) NOT NULL, ;
                  CODCONTABL C(10) NOT NULL, ;
                  CODPROV N(4, 0) NOT NULL, ;
                  DETALLE C(60) NOT NULL, ;
                  DB N(18, 5) NOT NULL, ;
                  CR N(18, 5) NOT NULL, ;
                  CIERRE L NOT NULL, ;
                  CODCC C(8) NOT NULL, ;
                  NUMCONTROL N(8, 0) NOT NULL, ;
                  FECHACP D NOT NULL)

***** Crear cada índice para CPD *****
SET COLLATE TO 'MACHINE'
INDEX ON CODCP TAG CODCP
INDEX ON CODPROV TAG CODPROV
INDEX ON CODCOMP TAG CODCOMP
INDEX ON CODCONTABL TAG CODCONTABL
INDEX ON STR(CODCP)+STR(CODCOMP)+CODCONTABL TAG COMPUESTO

***** Cambiar propiedades para CPD *****

***** Configuración de tabla para INVENINI *****
CREATE TABLE 'INVENINI.DBF' NAME 'INVENINI' (CODFIN N(4, 0) NOT NULL, ;
                       CODINV N(4, 0) NOT NULL, ;
                       CODINSUM N(4, 0) NOT NULL, ;
                       CODTIPO N(4, 0) NOT NULL, ;
                       CANTUNIT N(15, 5) NOT NULL, ;
                       CANTINS N(15, 5) NOT NULL, ;
                       COSTUNIT N(14, 5) NOT NULL, ;
                       CODUNID N(4, 0) NOT NULL, ;
                       EQUIVALE N(8, 5) NOT NULL, ;
                       USOTOTINS N(15, 5) NOT NULL, ;
                       MARCAINV L NOT NULL)

***** Crear cada índice para INVENINI *****
SET COLLATE TO 'MACHINE'
INDEX ON CODTIPO TAG CODTIPO
INDEX ON CODINSUM TAG CODINSUM
INDEX ON CODFIN TAG CODFIN

***** Cambiar propiedades para INVENINI *****
CREATE TRIGGER ON 'INVENINI' FOR INSERT AS __ri_insert_invenini()
CREATE TRIGGER ON 'INVENINI' FOR UPDATE AS __ri_update_invenini()

***** Configuración de tabla para INSUMO *****
CREATE TABLE 'INSUMO.DBF' NAME 'INSUMO' (CODINSUM N(4, 0) NOT NULL, ;
                     CODTIPO N(4, 0) NOT NULL, ;
                     CODUNIDAD N(1, 0) NOT NULL, ;
                     NOMINSUM C(20) NOT NULL, ;
                     CONTINSUM N(4, 0) NOT NULL, ;
                     MARCAINS C(1) NOT NULL)

***** Crear cada índice para INSUMO *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'INSUMO' ADD PRIMARY KEY CODINSUM TAG CODINSUM
INDEX ON CODTIPO TAG CODTIPO
INDEX ON NOMINSUM TAG NOMINSUM

***** Cambiar propiedades para INSUMO *****
CREATE TRIGGER ON 'INSUMO' FOR DELETE AS __ri_delete_insumo()
CREATE TRIGGER ON 'INSUMO' FOR INSERT AS __ri_insert_insumo()
CREATE TRIGGER ON 'INSUMO' FOR UPDATE AS __ri_update_insumo()

***** Configuración de tabla para INVENT *****
CREATE TABLE 'INVENT.DBF' NAME 'INVENT' (CODINSUM N(4, 0) NOT NULL, ;
                     CODTIPO N(4, 0) NOT NULL, ;
                     CANTINI N(15, 5) NOT NULL, ;
                     CANTCOMP N(15, 5) NOT NULL, ;
                     USOTOTINS N(15, 5) NOT NULL, ;
                     VALORACION N(15, 0) NOT NULL, ;
                     MARCAINV C(1) NOT NULL)

***** Crear cada índice para INVENT *****

***** Cambiar propiedades para INVENT *****

***** Configuración de tabla para CPG *****
CREATE TABLE 'CPG.DBF' NAME 'CPG' (CODCP N(10, 0) NOT NULL, ;
                  FECHA D NOT NULL, ;
                  FUENTE N(4, 0) NOT NULL, ;
                  DBS N(18, 5) NOT NULL, ;
                  CRS N(18, 5) NOT NULL, ;
                  DFS N(18, 5) NOT NULL, ;
                  MARCACP C(1) NOT NULL, ;
                  CIERRE L NOT NULL, ;
                  CODCC C(8) NOT NULL, ;
                  NUMCONTROL N(8, 0) NOT NULL)

***** Crear cada índice para CPG *****
SET COLLATE TO 'MACHINE'
INDEX ON FECHA TAG FECHA
INDEX ON FUENTE TAG FUENTE
ALTER TABLE 'CPG' ADD PRIMARY KEY CODCP TAG CODCP
INDEX ON DTOS(FECHA)+STR(FUENTE,4,0) TAG WIZARD_1
INDEX ON DTOS(FECHA) TAG WIZARD_2
INDEX ON STR(CODCP,10,0)+DTOS(FECHA) TAG WIZARD_3
INDEX ON STR(FUENTE,4,0)+DTOS(FECHA) TAG WIZARD_4
INDEX ON CODCC TAG CODCC

***** Cambiar propiedades para CPG *****
CREATE TRIGGER ON 'CPG' FOR DELETE AS __ri_delete_cpg()
CREATE TRIGGER ON 'CPG' FOR UPDATE AS __ri_update_cpg()

***** Configuración de tabla para METEREO *****
CREATE TABLE 'METEREO.DBF' NAME 'METEREO' (CODFIN N(4, 0) NOT NULL, ;
                      FECHA D NOT NULL, ;
                      MES C(3) NOT NULL, ;
                      DIA N(2, 0) NOT NULL, ;
                      PRECIPITA N(5, 2) NOT NULL, ;
                      BRISOL N(5, 2) NOT NULL, ;
                      HUMERELA N(5, 2) NOT NULL, ;
                      TXEMPERAMX N(5, 2) NOT NULL, ;
                      TNEMPERAMN N(5, 2) NOT NULL, ;
                      IMPR L NOT NULL)

***** Crear cada índice para METEREO *****
SET COLLATE TO 'MACHINE'
INDEX ON CODFIN TAG CODFIN

***** Cambiar propiedades para METEREO *****
CREATE TRIGGER ON 'METEREO' FOR INSERT AS __ri_insert_metereo()
CREATE TRIGGER ON 'METEREO' FOR UPDATE AS __ri_update_metereo()

***** Configuración de tabla para PLUB *****
CREATE TABLE 'PLUB.DBF' NAME 'PLUB' (DIA N(2, 0) NOT NULL, ;
                   ENE N(3, 0) NOT NULL, ;
                   FEB N(3, 0) NOT NULL, ;
                   MAR N(3, 0) NOT NULL, ;
                   ABR N(3, 0) NOT NULL, ;
                   MAY N(3, 0) NOT NULL, ;
                   JUN N(3, 0) NOT NULL, ;
                   JUL N(3, 0) NOT NULL, ;
                   AGO N(3, 0) NOT NULL, ;
                   SEP N(3, 0) NOT NULL, ;
                   OCT N(3, 0) NOT NULL, ;
                   NOV N(3, 0) NOT NULL, ;
                   DIC N(3, 0) NOT NULL)

***** Crear cada índice para PLUB *****

***** Cambiar propiedades para PLUB *****

***** Configuración de tabla para VARAÑOS *****
CREATE TABLE 'VARAÑOS.DBF' NAME 'VARAÑOS' (MES C(3) NOT NULL, ;
                      FES2 N(8, 0) NOT NULL, ;
                      FES3 N(8, 0) NOT NULL, ;
                      FES4 N(8, 0) NOT NULL, ;
                      FES5 N(8, 0) NOT NULL, ;
                      FES6 N(8, 0) NOT NULL, ;
                      FES7 N(8, 0) NOT NULL, ;
                      FES8 N(8, 0) NOT NULL, ;
                      FES9 N(8, 0) NOT NULL, ;
                      FES10 N(8, 0) NOT NULL, ;
                      FES11 N(8, 0) NOT NULL, ;
                      FES12 N(8, 0) NOT NULL, ;
                      FES13 N(8, 0) NOT NULL, ;
                      FES14 N(8, 0) NOT NULL, ;
                      FES15 N(8, 0) NOT NULL, ;
                      FES16 N(8, 0) NOT NULL, ;
                      FES17 N(8, 0) NOT NULL, ;
                      FES18 N(8, 0) NOT NULL, ;
                      FES19 N(8, 0) NOT NULL, ;
                      FES20 N(8, 0) NOT NULL, ;
                      FES21 N(8, 0) NOT NULL, ;
                      FES22 N(8, 0) NOT NULL, ;
                      FES23 N(8, 0) NOT NULL, ;
                      FES24 N(8, 0) NOT NULL, ;
                      FES25 N(8, 0) NOT NULL, ;
                      FES26 N(8, 0) NOT NULL, ;
                      NUMES N(2, 0) NOT NULL)

***** Crear cada índice para VARAÑOS *****

***** Cambiar propiedades para VARAÑOS *****

***** Configuración de tabla para NMMESES *****
CREATE TABLE 'NMMESES.DBF' NAME 'NMMESES' (MES C(10) NOT NULL, ;
                      IMPRI L NOT NULL)

***** Crear cada índice para NMMESES *****

***** Cambiar propiedades para NMMESES *****

***** Configuración de tabla para PORCOSTO *****
CREATE TABLE 'PORCOSTO.DBF' NAME 'PORCOSTO' (CODCOSTO N(4, 0) NOT NULL, ;
                       NOMCOSTO C(20) NOT NULL)

***** Crear cada índice para PORCOSTO *****

***** Cambiar propiedades para PORCOSTO *****

***** Configuración de tabla para MAQTIPO *****
CREATE TABLE 'MAQTIPO.DBF' NAME 'MAQTIPO' (CODTMAQ N(10, 0) NOT NULL, ;
                      TIPOMAQ C(10) NOT NULL)

***** Crear cada índice para MAQTIPO *****
SET COLLATE TO 'MACHINE'
ALTER TABLE 'MAQTIPO' ADD PRIMARY KEY CODTMAQ TAG CODTMAQ

***** Cambiar propiedades para MAQTIPO *****
CREATE TRIGGER ON 'MAQTIPO' FOR DELETE AS __ri_delete_maqtipo()
CREATE TRIGGER ON 'MAQTIPO' FOR UPDATE AS __ri_update_maqtipo()

***** Configuración de tabla para INDIMAQ *****
CREATE TABLE 'INDIMAQ.DBF' NAME 'INDIMAQ' (ANO N(4, 0) NOT NULL, ;
                      INDICE N(12, 5) NOT NULL, ;
                      MARCAIND L NOT NULL)

***** Crear cada índice para INDIMAQ *****

***** Cambiar propiedades para INDIMAQ *****

***** Configuración de tabla para PROCION *****
CREATE TABLE 'PROCION.DBF' NAME 'PROCION' (NUMCONTROL N(4, 0) NOT NULL, ;
                      CODUNIDAD N(10, 0) NOT NULL, ;
                      CANTIDAD N(10, 5) NOT NULL, ;
                      PESO N(10, 5) NOT NULL, ;
                      TOTALPROD N(10, 5) NOT NULL)

***** Crear cada índice para PROCION *****
SET COLLATE TO 'MACHINE'
INDEX ON NUMCONTROL TAG NUMCONTROL

***** Cambiar propiedades para PROCION *****
CREATE TRIGGER ON 'PROCION' FOR INSERT AS __ri_insert_procion()
CREATE TRIGGER ON 'PROCION' FOR UPDATE AS __ri_update_procion()

***** Configuración de tabla para AUXAPLI *****
CREATE TABLE 'AUXAPLI.DBF' NAME 'AUXAPLI' (CODINSAP N(4, 0) NOT NULL, ;
                      CODINSUM N(4, 0) NOT NULL, ;
                      CODTIPO N(4, 0) NOT NULL, ;
                      CODCOMP N(4, 0) NOT NULL, ;
                      INDICA N(1, 0) NOT NULL, ;
                      CANTUSADA N(14, 5) NOT NULL, ;
                      CODAPLIC C(8) NOT NULL)

***** Crear cada índice para AUXAPLI *****
SET COLLATE TO 'MACHINE'
INDEX ON CODCOMP TAG CODCOMP
INDEX ON CODAPLIC TAG CODAPLIC
INDEX ON CODINSAP TAG CODINSAP

***** Cambiar propiedades para AUXAPLI *****

***** Configuración de tabla para APLICA *****
CREATE TABLE 'APLICA.DBF' NAME 'APLICA' (NUMCONTROL N(8, 0) NOT NULL, ;
                     CODAPLIC C(8) NOT NULL, ;
                     FECAPLIC D NOT NULL, ;
                     HECTAPLIC N(8, 2) NOT NULL, ;
                     SECUENCIA N(8, 0) NOT NULL, ;
                     MARCAPLIC C(1) NOT NULL, ;
                     CODCP N(10, 0) NOT NULL, ;
                     CODCPT N(10, 0) NOT NULL, ;
                     CODCPR N(10, 0) NOT NULL)

***** Crear cada índice para APLICA *****
SET COLLATE TO 'MACHINE'
INDEX ON FECAPLIC TAG FECAPLIC
ALTER TABLE 'APLICA' ADD PRIMARY KEY CODAPLIC TAG CODAPLIC
INDEX ON NUMCONTROL TAG NUMCONTROL

***** Cambiar propiedades para APLICA *****

***** Configuración de tabla para VENTAS *****
CREATE TABLE 'VENTAS.DBF' NAME 'VENTAS' (NUMCONTROL N(8, 0) NOT NULL, ;
                     VALVENTA N(14, 5) NOT NULL, ;
                     TONELS N(10, 5) NOT NULL, ;
                     NOMBCOMP C(25) NOT NULL, ;
                     NIT C(15) NOT NULL, ;
                     CODPROV N(4, 0) NOT NULL, ;
                     RETEN N(5, 4) NOT NULL, ;
                     CUOTA N(5, 4) NOT NULL, ;
                     OTROS N(5, 4) NOT NULL, ;
                     CODLOTE N(4, 0) NOT NULL, ;
                     CODCP N(10, 0) NOT NULL, ;
                     FECHA D NOT NULL, ;
                     NUMFACTU C(10) NOT NULL, ;
                     PORCENTAJE N(5, 2) NOT NULL)

***** Crear cada índice para VENTAS *****
SET COLLATE TO 'MACHINE'
INDEX ON FECHA TAG FECHA
INDEX ON CODPROV TAG CODPROV
INDEX ON NUMCONTROL TAG NUMCONTROL

***** Cambiar propiedades para VENTAS *****
CREATE TRIGGER ON 'VENTAS' FOR INSERT AS __ri_insert_ventas()
CREATE TRIGGER ON 'VENTAS' FOR UPDATE AS __ri_update_ventas()

***** Configuración de tabla para RESUCONTA *****
CREATE TABLE 'RESUCONTA.DBF' NAME 'RESUCONTA' (CODACT N(2, 0) NOT NULL, ;
                        ACTIVIDAD C(20) NOT NULL, ;
                        COSTOTOTAL N(12, 0) NOT NULL, ;
                        COSTOHA N(11, 0) NOT NULL, ;
                        COSTOTON N(11, 0) NOT NULL, ;
                        PORCENTAJE N(6, 3) NOT NULL)

***** Crear cada índice para RESUCONTA *****
SET COLLATE TO 'MACHINE'
INDEX ON CODACT TAG CODACT

***** Cambiar propiedades para RESUCONTA *****

***** Configuración de tabla para AUPLX *****
CREATE TABLE 'AUPLX.DBF' NAME 'AUPLX' (CODPLI C(8) NOT NULL, ;
                    CTOTA N(10, 0) NOT NULL)

***** Crear cada índice para AUPLX *****

***** Cambiar propiedades para AUPLX *****

***** Configuración de tabla para AUXRECO *****
CREATE TABLE 'AUXRECO.DBF' NAME 'AUXRECO' (COSTOTAL N(10, 0) NOT NULL, ;
                      CODLABOR N(4, 0) NOT NULL)

***** Crear cada índice para AUXRECO *****

***** Cambiar propiedades para AUXRECO *****

***** Configuración de tabla para FLUFIN *****
CREATE TABLE 'FLUFIN.DBF' NAME 'FLUFIN' (CUENTA C(17) NOT NULL, ;
                     M1 C(14) NOT NULL, ;
                     M2 C(14) NOT NULL, ;
                     M3 C(14) NOT NULL, ;
                     M4 C(14) NOT NULL, ;
                     M5 C(14) NOT NULL, ;
                     M6 C(14) NOT NULL, ;
                     M7 C(14) NOT NULL, ;
                     M8 C(14) NOT NULL, ;
                     M9 C(14) NOT NULL, ;
                     M10 C(14) NOT NULL, ;
                     M11 C(14) NOT NULL, ;
                     M12 C(14) NOT NULL)

***** Crear cada índice para FLUFIN *****

***** Cambiar propiedades para FLUFIN *****

***** Configuración de tabla para FLUCAJA *****
CREATE TABLE 'FLUCAJA.DBF' NAME 'FLUCAJA' (CUENTA C(17) NOT NULL, ;
                      M1 C(14) NOT NULL, ;
                      M2 C(14) NOT NULL, ;
                      M3 C(14) NOT NULL, ;
                      M4 C(14) NOT NULL, ;
                      M5 C(14) NOT NULL, ;
                      M6 C(14) NOT NULL, ;
                      M7 C(14) NOT NULL, ;
                      M8 C(14) NOT NULL, ;
                      M9 C(14) NOT NULL, ;
                      M10 C(14) NOT NULL, ;
                      M11 C(14) NOT NULL, ;
                      M12 C(14) NOT NULL)

***** Crear cada índice para FLUCAJA *****

***** Cambiar propiedades para FLUCAJA *****

***** Configuración de tabla para RESUFIN *****
CREATE TABLE 'RESUFIN.DBF' NAME 'RESUFIN' (CUENTA C(17) NOT NULL, ;
                      TOTAL N(14, 0) NOT NULL, ;
                      HA N(14, 0) NOT NULL, ;
                      TON N(14, 0) NOT NULL)

***** Crear cada índice para RESUFIN *****

***** Cambiar propiedades para RESUFIN *****

***** Configuración de tabla para INDF *****
CREATE TABLE 'INDF.DBF' NAME 'INDF' (CUENTA C(35) NOT NULL, ;
                   COSTOCONC N(16, 5) NOT NULL, ;
                   COSTOSINC N(16, 5) NOT NULL, ;
                   PORC N(4, 2) NOT NULL)

***** Crear cada índice para INDF *****

***** Cambiar propiedades para INDF *****

***** Configuración de tabla para VPNS *****
CREATE TABLE 'VPNS.DBF' NAME 'VPNS' (TOTAL N(14, 0) NOT NULL, ;
                   TOTAL1 N(14, 0) NOT NULL)

***** Crear cada índice para VPNS *****

***** Cambiar propiedades para VPNS *****

***** Configuración de tabla para COSTOT *****
CREATE TABLE 'COSTOT.DBF' NAME 'COSTOT' (CODLOTE N(4, 0) NOT NULL, ;
                     LOTES C(20) NOT NULL, ;
                     COSHA1 N(14, 5) NOT NULL, ;
                     COSTON1 N(14, 5) NOT NULL, ;
                     COSHA2 N(14, 5) NOT NULL, ;
                     COSTON2 N(14, 5) NOT NULL, ;
                     COSHA3 N(14, 5) NOT NULL, ;
                     COSTON3 N(14, 5) NOT NULL, ;
                     COSHA4 N(14, 5) NOT NULL, ;
                     COSTON4 N(14, 5) NOT NULL, ;
                     COSHA5 N(14, 5) NOT NULL, ;
                     COSTON5 N(14, 5) NOT NULL, ;
                     COSHA6 N(14, 5) NOT NULL, ;
                     COSTON6 N(14, 5) NOT NULL, ;
                     COSHA7 N(14, 5) NOT NULL, ;
                     COSTON7 N(14, 5) NOT NULL, ;
                     COSHA8 N(14, 5) NOT NULL, ;
                     COSTON8 N(14, 5) NOT NULL, ;
                     COSHA9 N(14, 5) NOT NULL, ;
                     COSTON9 N(14, 5) NOT NULL, ;
                     COSHA10 N(14, 5) NOT NULL, ;
                     COSTON10 N(14, 5) NOT NULL)

***** Crear cada índice para COSTOT *****

***** Cambiar propiedades para COSTOT *****

***** Configuración de tabla para COSPOR *****
CREATE TABLE 'COSPOR.DBF' NAME 'COSPOR' (ACTIVIDAD C(30) NOT NULL, ;
                     PORC1 N(6, 5) NOT NULL, ;
                     PORC2 N(6, 5) NOT NULL, ;
                     PORC3 N(6, 5) NOT NULL, ;
                     PORC4 N(6, 5) NOT NULL, ;
                     PORC5 N(6, 5) NOT NULL, ;
                     PORC6 N(6, 5) NOT NULL, ;
                     PORC7 N(6, 5) NOT NULL, ;
                     PORC8 N(6, 5) NOT NULL, ;
                     PORC9 N(6, 5) NOT NULL, ;
                     PORC10 N(6, 5) NOT NULL)

***** Crear cada índice para COSPOR *****

***** Cambiar propiedades para COSPOR *****

***** Configuración de tabla para PROCUL *****
CREATE TABLE 'PROCUL.DBF' NAME 'PROCUL' (CODLOTE N(4, 0) NOT NULL, ;
                     LOTES C(20) NOT NULL, ;
                     HAS N(6, 0) NOT NULL, ;
                     TON1 N(8, 5) NOT NULL, ;
                     TON2 N(11, 5) NOT NULL, ;
                     TON3 N(11, 5) NOT NULL, ;
                     TON4 N(11, 5) NOT NULL, ;
                     TON5 N(11, 5) NOT NULL, ;
                     TON6 N(11, 5) NOT NULL, ;
                     TON7 N(11, 5) NOT NULL, ;
                     TON8 N(11, 5) NOT NULL, ;
                     TON9 N(11, 5) NOT NULL, ;
                     TON10 N(11, 5) NOT NULL)

***** Crear cada índice para PROCUL *****

***** Cambiar propiedades para PROCUL *****

***** Configuración de tabla para AUXUFC *****
CREATE TABLE 'AUXUFC.DBF' NAME 'AUXUFC' (CODUFC N(4, 0) NOT NULL, ;
                     CODCOMP N(4, 0) NOT NULL, ;
                     INDICA N(1, 0) NOT NULL, ;
                     CANTUSADA N(11, 5) NOT NULL, ;
                     CODINSUM N(4, 0) NOT NULL)

***** Crear cada índice para AUXUFC *****
SET COLLATE TO 'MACHINE'
INDEX ON CODCOMP TAG CODCOMP
INDEX ON CODUFC TAG CODUFC

***** Cambiar propiedades para AUXUFC *****

***** Configuración de tabla para MAYORR *****
CREATE TABLE 'MAYORR.DBF' NAME 'MAYORR' (AA1 C(50) NOT NULL, ;
                     CC1 N(12, 0) NOT NULL, ;
                     CC2 N(12, 0) NOT NULL, ;
                     CC3 N(12, 0) NOT NULL, ;
                     CC4 N(12, 0) NOT NULL, ;
                     CC5 N(12, 0) NOT NULL, ;
                     CC6 N(12, 0) NOT NULL)

***** Crear cada índice para MAYORR *****

***** Cambiar propiedades para MAYORR *****

***** Configuración de tabla para MAYORD *****
CREATE TABLE 'MAYORD.DBF' NAME 'MAYORD' (AA1 C(30) NOT NULL, ;
                     AA2 C(10) NOT NULL, ;
                     AA3 C(20) NOT NULL, ;
                     AA4 C(50) NOT NULL, ;
                     CC1 N(12, 5) NOT NULL, ;
                     CC2 N(12, 5) NOT NULL)

***** Crear cada índice para MAYORD *****

***** Cambiar propiedades para MAYORD *****

***** Configuración de tabla para CPDTEMP *****
CREATE TABLE 'CPDTEMP.DBF' NAME 'CPDTEMP' (CODCONTABL C(10) NOT NULL, ;
                      CODPROV N(4, 0) NOT NULL, ;
                      DETALLE C(60) NOT NULL, ;
                      DB N(18, 5) NOT NULL, ;
                      CR N(18, 5) NOT NULL, ;
                      VALOR N(18, 5) NOT NULL, ;
                      AUTO L NOT NULL, ;
                      GUIA C(4) NOT NULL, ;
                      SUBCTA C(1) NOT NULL, ;
                      CODPROVSB N(4, 0) NOT NULL, ;
                      FT1 C(2) NOT NULL, ;
                      FT2 C(2) NOT NULL, ;
                      FT3 C(2) NOT NULL, ;
                      FT4 C(2) NOT NULL, ;
                      CODCC C(8) NOT NULL, ;
                      NUMCONTROL N(8, 0) NOT NULL)

***** Crear cada índice para CPDTEMP *****
SET COLLATE TO 'MACHINE'
INDEX ON CODCONTABL TAG CODCONTABL

***** Cambiar propiedades para CPDTEMP *****

***** Configuración de tabla para BGI1 *****
CREATE TABLE 'BGI1.DBF' NAME 'BGI1' (AA1 C(10) NOT NULL, ;
                   AA2 C(60) NOT NULL, ;
                   CC1 N(12, 0) NOT NULL, ;
                   CC2 N(12, 0) NOT NULL)

***** Crear cada índice para BGI1 *****

***** Cambiar propiedades para BGI1 *****

***** Configuración de tabla para PGI1 *****
CREATE TABLE 'PGI1.DBF' NAME 'PGI1' (AA1 C(10) NOT NULL, ;
                   AA2 C(60) NOT NULL, ;
                   CC1 N(12, 0) NOT NULL, ;
                   CC2 N(12, 0) NOT NULL)

***** Crear cada índice para PGI1 *****

***** Cambiar propiedades para PGI1 *****

***** Configuración de tabla para CICLOS *****
CREATE TABLE 'CICLOS.DBF' NAME 'CICLOS' (FECHAI D NOT NULL, ;
                     FECHA1 D NOT NULL, ;
                     FECHA2 D NOT NULL, ;
                     FECHA3 D NOT NULL, ;
                     FECHA4 D NOT NULL, ;
                     FECHA5 D NOT NULL, ;
                     CIERR N(10, 0) NOT NULL)

***** Crear cada índice para CICLOS *****

***** Cambiar propiedades para CICLOS *****

***** Configuración de tabla para UFC *****
CREATE TABLE 'UFC.DBF' NAME 'UFC' (CODFIN N(4, 0) NOT NULL, ;
                  CODUFC N(4, 0) NOT NULL, ;
                  CODINSUM N(4, 0) NOT NULL, ;
                  NOMINSUM C(13) NOT NULL, ;
                  CODTIPO N(4, 0) NOT NULL, ;
                  FECHAUFC D NOT NULL, ;
                  MOTUFC C(50) NOT NULL, ;
                  CANTUFC N(11, 5) NOT NULL, ;
                  CODUNID N(4, 0) NOT NULL, ;
                  EQUIVALE N(8, 5) NOT NULL, ;
                  CANTOTAL N(14, 5) NOT NULL, ;
                  MARCAUFC C(1) NOT NULL, ;
                  CODCP N(10, 0) NOT NULL)

***** Crear cada índice para UFC *****
SET COLLATE TO 'MACHINE'
INDEX ON CODUNID TAG CODUNID
ALTER TABLE 'UFC' ADD PRIMARY KEY CODUFC TAG CODUFC
INDEX ON CODINSUM TAG CODINSUM
INDEX ON NOMINSUM TAG NOMINSUM
INDEX ON CODFIN TAG CODFIN

***** Cambiar propiedades para UFC *****
CREATE TRIGGER ON 'UFC' FOR DELETE AS __ri_delete_ufc()
CREATE TRIGGER ON 'UFC' FOR INSERT AS __ri_insert_ufc()
CREATE TRIGGER ON 'UFC' FOR UPDATE AS __ri_update_ufc()

***** Configuración de tabla para AUXCOMB *****
CREATE TABLE 'AUXCOMB.DBF' NAME 'AUXCOMB' (CODUFC N(4, 0) NOT NULL, ;
                      CODCOMP N(4, 0) NOT NULL, ;
                      CODINSUM N(4, 0) NOT NULL, ;
                      INDICA N(1, 0) NOT NULL, ;
                      CANTUSADA N(11, 5) NOT NULL)

***** Crear cada índice para AUXCOMB *****
SET COLLATE TO 'MACHINE'
INDEX ON CODUFC TAG CODUFC

***** Cambiar propiedades para AUXCOMB *****

***** Configuración de tabla para AUXCRED *****
CREATE TABLE 'AUXCRED.DBF' NAME 'AUXCRED' (CODCRED N(4, 0) NOT NULL, ;
                      CODLOTE N(3, 0) NOT NULL, ;
                      MONTOP N(6, 5) NOT NULL, ;
                      CODCP N(10, 0) NOT NULL)

***** Crear cada índice para AUXCRED *****

***** Cambiar propiedades para AUXCRED *****

***** Configuración de tabla para TIPO_NOVEDAD *****
CREATE TABLE 'TIPO_NOVEDAD.DBF' NAME 'TIPO_NOVEDAD' (TIPO N(1, 0) NOT NULL, ;
                           NOVEDAD C(28) NOT NULL)

***** Crear cada índice para TIPO_NOVEDAD *****
SET COLLATE TO 'MACHINE'
INDEX ON TIPO TAG TIPO

***** Cambiar propiedades para TIPO_NOVEDAD *****

***** Configuración de tabla para GRAFICOS *****
CREATE TABLE 'GRAFICOS.DBF' NAME 'GRAFICOS' (GRAPRE G NOT NULL, ;
                       GRABRI G NOT NULL, ;
                       GRAHUM G NOT NULL, ;
                       GRATEM G NOT NULL, ;
                       GRATEM1 G NOT NULL, ;
                       GCOMESP G NOT NULL, ;
                       GCOMESB G NOT NULL, ;
                       GCOMESH G NOT NULL, ;
                       GCOMEST G NOT NULL, ;
                       GCOMEST1 G NOT NULL, ;
                       GAGREANU G NOT NULL, ;
                       GRAMEN G NOT NULL, ;
                       GRAMEN1 G NOT NULL, ;
                       GRARECO1 G NOT NULL, ;
                       GRARECO2 G NOT NULL, ;
                       GRARECO3 G NOT NULL, ;
                       GRARECO4 G NOT NULL, ;
                       GRARECO5 G NOT NULL)

***** Crear cada índice para GRAFICOS *****

***** Cambiar propiedades para GRAFICOS *****

***** Configuración de tabla para PUC *****
CREATE TABLE 'PUC.DBF' NAME 'PUC' (CODIGO C(10) NOT NULL, ;
                  DENOMINA C(150) NOT NULL, ;
                  CLASE C(1) NOT NULL, ;
                  GRP C(1) NOT NULL, ;
                  CUENTA C(2) NOT NULL, ;
                  SUBCUENTA C(2) NOT NULL, ;
                  NATURALEZA C(7) NOT NULL)

***** Crear cada índice para PUC *****
SET COLLATE TO 'MACHINE'
INDEX ON CLASE+GRP+CUENTA+SUBCUENTA TAG PUC

***** Cambiar propiedades para PUC *****

***** Configuración de tabla para RETEFUEN *****
CREATE TABLE 'RETEFUEN.DBF' NAME 'RETEFUEN' (RETE N(5, 2) NOT NULL, ;
                       VALOR N(15, 4) NOT NULL, ;
                       VALRET N(15, 4) NOT NULL)

***** Crear cada índice para RETEFUEN *****

***** Cambiar propiedades para RETEFUEN *****

***** Configuración de tabla para INSAPLICA *****
CREATE TABLE 'INSAPLICA.DBF' NAME 'INSAPLICA' (CODINSAP N(4, 0) NOT NULL, ;
                        NUMCONTROL N(4, 0) NOT NULL, ;
                        CODAPLIC C(8) NOT NULL, ;
                        CODTIPO N(4, 0) NOT NULL, ;
                        NOMTIPO C(14) NOT NULL, ;
                        CODINSUM N(4, 0) NOT NULL, ;
                        NOMINSUMO C(15) NOT NULL, ;
                        CODUNID N(4, 0) NOT NULL, ;
                        NOMUNID C(11) NOT NULL, ;
                        CANTAPLIC N(14, 5) NOT NULL, ;
                        EQUIVALE N(8, 5) NOT NULL, ;
                        CANTOTAL N(14, 5) NOT NULL, ;
                        FORMA1 N(1, 0) NOT NULL, ;
                        MARCAINAP C(1) NOT NULL, ;
                        PESO N(8, 5) NOT NULL, ;
                        FECHAPLI D NOT NULL, ;
                        FECHAGER D NOT NULL, ;
                        FECHA1 D NOT NULL, ;
                        CODCP N(4, 0) NOT NULL)

***** Crear cada índice para INSAPLICA *****
SET COLLATE TO 'MACHINE'
INDEX ON FECHA1 TAG FECHA1
INDEX ON NUMCONTROL TAG NUMCONTROL
INDEX ON NOMINSUMO TAG NOMINSUMO
INDEX ON CODAPLIC TAG CODAPLIC
INDEX ON CODINSAP TAG CODINSAP CANDIDATE

***** Cambiar propiedades para INSAPLICA *****
CREATE TRIGGER ON 'INSAPLICA' FOR DELETE AS __ri_delete_insaplica()
CREATE TRIGGER ON 'INSAPLICA' FOR UPDATE AS __ri_update_insaplica()

***** Configuración de tabla para MAQCULTI *****
CREATE TABLE 'MAQCULTI.DBF' NAME 'MAQCULTI' (NUMCONTROL N(8, 0) NOT NULL, ;
                       CODLABOR N(4, 0) NOT NULL, ;
                       NOMLABOR C(30) NOT NULL, ;
                       CODMAQ N(4, 0) NOT NULL, ;
                       CODTMAQ N(1, 0) NOT NULL, ;
                       FECHAMAQ D NOT NULL, ;
                       HASCUB N(10, 5) NOT NULL, ;
                       CANTPASES N(10, 0) NOT NULL, ;
                       TIEMPO C(5) NOT NULL, ;
                       SALARIO N(10, 3) NOT NULL, ;
                       DEPRECI N(10, 3) NOT NULL, ;
                       IMPUESTO N(10, 3) NOT NULL, ;
                       INTERES N(10, 3) NOT NULL, ;
                       REPARACION N(10, 3) NOT NULL, ;
                       OPERARIO N(10, 3) NOT NULL, ;
                       COSTOTAL N(12, 0) NOT NULL, ;
                       MARCAMAQ L NOT NULL, ;
                       NOMMAQ C(15) NOT NULL, ;
                       CODAPLICA C(8) NOT NULL, ;
                       CODCP N(4, 0) NOT NULL)

***** Crear cada índice para MAQCULTI *****
SET COLLATE TO 'MACHINE'
INDEX ON COSTOTAL TAG COSTOTAL
INDEX ON NUMCONTROL TAG NUMCONTROL
INDEX ON CODMAQ TAG CODMAQ
INDEX ON CODLABOR TAG CODLABOR

***** Cambiar propiedades para MAQCULTI *****
CREATE TRIGGER ON 'MAQCULTI' FOR INSERT AS __ri_insert_maqculti()
CREATE TRIGGER ON 'MAQCULTI' FOR UPDATE AS __ri_update_maqculti()

***** Configuración de tabla para CONTADOR *****
CREATE TABLE 'CONTADOR.DBF' NAME 'CONTADOR' (TIPO C(2) NOT NULL, ;
                       CODIGO N(4, 0) NOT NULL, ;
                       CONCEPTO C(32) NOT NULL, ;
                       CUENTA C(10) NOT NULL, ;
                       NATURAL C(9) NOT NULL, ;
                       MODOCT C(2) NOT NULL, ;
                       GUIA C(4) NOT NULL)

***** Crear cada índice para CONTADOR *****
SET COLLATE TO 'MACHINE'
INDEX ON CODIGO TAG CODIGO

***** Cambiar propiedades para CONTADOR *****

***** Configuración de tabla para BPI1 *****
CREATE TABLE 'BPI1.DBF' NAME 'BPI1' (AA1 C(10) NOT NULL, ;
                   AA2 C(60) NOT NULL, ;
                   CC1 N(12, 0) NOT NULL, ;
                   CC2 N(12, 0) NOT NULL, ;
                   CC3 N(12, 0) NOT NULL, ;
                   CC4 N(12, 0) NOT NULL, ;
                   CC5 N(12, 0) NOT NULL, ;
                   CC6 N(12, 0) NOT NULL)

***** Crear cada índice para BPI1 *****

***** Cambiar propiedades para BPI1 *****

***** Configuración de tabla para NOMIPARA *****
CREATE TABLE 'NOMIPARA.DBF' NAME 'NOMIPARA' (SALUD N(6, 3) NOT NULL, ;
                       PENSION N(6, 3) NOT NULL, ;
                       SOLIDA N(6, 3) NOT NULL, ;
                       SOBREQUE C(8) NOT NULL, ;
                       EDIURNA N(6, 3) NOT NULL, ;
                       ENOCTUR N(6, 3) NOT NULL, ;
                       FDIURNA N(6, 3) NOT NULL, ;
                       FNOCTUR N(6, 3) NOT NULL, ;
                       RNOCTUR N(6, 3) NOT NULL, ;
                       RFESTIVO N(6, 3) NOT NULL, ;
                       STRANSPO N(10, 0) NOT NULL, ;
                       SALIMENT N(10, 0) NOT NULL, ;
                       MINIMO N(10, 0) NOT NULL)

***** Crear cada índice para NOMIPARA *****

***** Cambiar propiedades para NOMIPARA *****

***** Configuración de tabla para NOVEDADES *****
CREATE TABLE 'NOVEDADES.DBF' NAME 'NOVEDADES' (CODIGOPLA C(20) NOT NULL, ;
                        CODNOV N(1, 0) NOT NULL, ;
                        DESCRIPCIO C(50) NOT NULL, ;
                        NOMBEMPL C(30) NOT NULL, ;
                        CODEMPL N(3, 0) NOT NULL, ;
                        VALOR N(10, 5) NOT NULL, ;
                        FECHA_NOV D NOT NULL, ;
                        CERRADO C(1) NOT NULL, ;
                        VALOR_REAL N(10, 5) NOT NULL)

***** Crear cada índice para NOVEDADES *****
SET COLLATE TO 'MACHINE'
INDEX ON FECHA_NOV TAG I

***** Cambiar propiedades para NOVEDADES *****

***** Configuración de tabla para PLANILLA *****
CREATE TABLE 'PLANILLA.DBF' NAME 'PLANILLA' (CODIGOPLA C(20) NOT NULL, ;
                       CODEMPL N(3, 0) NOT NULL, ;
                       NOMEMPL C(20) NOT NULL, ;
                       SUELDODIA N(18, 5) NOT NULL, ;
                       DIASTRABA N(2, 0) NOT NULL, ;
                       STRANSPO N(10, 0) NOT NULL, ;
                       SALIMENT N(10, 0) NOT NULL, ;
                       HEDIURNA N(18, 5) NOT NULL, ;
                       RNOCTUR N(18, 5) NOT NULL, ;
                       RFESTIVO N(18, 5) NOT NULL, ;
                       HENOCTUR N(18, 5) NOT NULL, ;
                       HFDIURNA N(18, 5) NOT NULL, ;
                       HFNOCTUR N(18, 5) NOT NULL, ;
                       OTROSDEV N(18, 5) NOT NULL, ;
                       SALUD N(18, 5) NOT NULL, ;
                       PENSION N(18, 5) NOT NULL, ;
                       SOLIDA N(18, 5) NOT NULL, ;
                       OTROSDESC N(18, 5) NOT NULL, ;
                       RETENCION N(18, 5) NOT NULL)

***** Crear cada índice para PLANILLA *****
SET COLLATE TO 'MACHINE'
INDEX ON CODEMPL TAG CODEMPL
INDEX ON CODIGOPLA TAG CODIGOPLA

***** Cambiar propiedades para PLANILLA *****

***** Configuración de tabla para NOMINA *****
CREATE TABLE 'NOMINA.DBF' NAME 'NOMINA' (CODEMPL N(4, 0) NOT NULL, ;
                     NOMBEMPL C(30) NOT NULL, ;
                     CEDULA N(10, 0) NOT NULL, ;
                     CIUDAD C(20) NOT NULL, ;
                     DIREMPL C(20) NOT NULL, ;
                     TELEMPL N(10, 0) NOT NULL, ;
                     FECING D NOT NULL, ;
                     FORMCONT N(1, 0) NOT NULL, ;
                     FECFIN D NOT NULL, ;
                     CARGOEMP C(25) NOT NULL, ;
                     SALARIO N(12, 5) NOT NULL, ;
                     FORMPAGO N(1, 0) NOT NULL, ;
                     MARCANOM L NOT NULL, ;
                     AREA C(14) NOT NULL, ;
                     SALUD N(12, 5) NOT NULL, ;
                     PENSION N(12, 5) NOT NULL, ;
                     SOLIDA N(12, 5) NOT NULL, ;
                     TRANSPORTE N(10, 2) NOT NULL, ;
                     ALIMENTA N(10, 2) NOT NULL, ;
                     RETEFUEN N(2, 0) NOT NULL, ;
                     ACTIVO N(1, 0) NOT NULL)

***** Crear cada índice para NOMINA *****
SET COLLATE TO 'MACHINE'
INDEX ON CODEMPL TAG CODEMPL CANDIDATE

***** Cambiar propiedades para NOMINA *****

***** Configuración de tabla para PLANICAB *****
CREATE TABLE 'PLANICAB.DBF' NAME 'PLANICAB' (CODIGOPLA C(20) NOT NULL, ;
                       TIPO C(1) NOT NULL, ;
                       FEDE D NOT NULL, ;
                       FEA D NOT NULL, ;
                       DEVENGOS N(18, 5) NOT NULL, ;
                       DEDUCCION N(18, 5) NOT NULL, ;
                       DESCUENTOS C(1) NOT NULL, ;
                       ESTADO C(1) NOT NULL, ;
                       CODCP N(4, 0) NOT NULL)

***** Crear cada índice para PLANICAB *****
SET COLLATE TO 'MACHINE'
INDEX ON CODIGOPLA TAG CODIGOPLA

***** Cambiar propiedades para PLANICAB *****

***** Configuración de tabla para TIPROV *****
CREATE TABLE 'TIPROV.DBF' NAME 'TIPROV' (CODTPRO N(2, 0) NOT NULL, ;
                     NOMTPRO C(25) NOT NULL, ;
                     ABRNOM C(2) NOT NULL)

***** Crear cada índice para TIPROV *****
SET COLLATE TO 'MACHINE'
INDEX ON CODTPRO TAG CODTPRO CANDIDATE
INDEX ON ABRNOM TAG ABRNOM

***** Cambiar propiedades para TIPROV *****

***** Configuración de tabla para GRACOM *****
CREATE TABLE 'GRACOM.DBF' NAME 'GRACOM' (GRARECO1 G NOT NULL, ;
                     GRARECO2 G NOT NULL, ;
                     GRARECO6 G NOT NULL)

***** Crear cada índice para GRACOM *****

***** Cambiar propiedades para GRACOM *****

***** Configuración de tabla para COSTOS_NOM *****
CREATE TABLE 'COSTOS_NOM.DBF' NAME 'COSTOS_NOM' (CODFIN N(4, 0) NOT NULL, ;
                         COEMPL N(4, 0) NOT NULL, ;
                         ASIGNA N(1, 0) NOT NULL, ;
                         PORCENTAJE N(5, 1) NOT NULL, ;
                         CODFINLOTE C(30) NOT NULL, ;
                         CODLOTE N(4, 0) NOT NULL, ;
                         NUMCONTROL N(8, 0) NOT NULL, ;
                         VALOR N(18, 5) NOT NULL, ;
                         AREA C(15) NOT NULL)

***** Crear cada índice para COSTOS_NOM *****

***** Cambiar propiedades para COSTOS_NOM *****

***** Configuración de tabla para ASIG_NOM *****
CREATE TABLE 'ASIG_NOM.DBF' NAME 'ASIG_NOM' (CODFIN N(4, 0) NOT NULL, ;
                       COEMPL N(4, 0) NOT NULL, ;
                       ASIGNA N(1, 0) NOT NULL, ;
                       PORCENTAJE N(5, 1) NOT NULL, ;
                       CODFINLOTE C(20) NOT NULL, ;
                       CODLOTE N(4, 0) NOT NULL, ;
                       NUMCONTROL N(8, 0) NOT NULL)

***** Crear cada índice para ASIG_NOM *****
SET COLLATE TO 'MACHINE'
INDEX ON COEMPL TAG COEMPL

***** Cambiar propiedades para ASIG_NOM *****

***** Configuración de tabla para AÑOS *****
CREATE TABLE 'AÑOS.DBF' NAME 'AÑOS' (MES C(3) NOT NULL, ;
                   CB1 N(8, 0) NOT NULL, ;
                   CB2 N(8, 0) NOT NULL, ;
                   CB3 N(8, 0) NOT NULL, ;
                   CB4 N(8, 0) NOT NULL, ;
                   CB5 N(8, 0) NOT NULL, ;
                   CB6 N(8, 0) NOT NULL, ;
                   CB7 N(8, 0) NOT NULL, ;
                   CB8 N(8, 0) NOT NULL, ;
                   CB9 N(8, 0) NOT NULL, ;
                   CB10 N(8, 0) NOT NULL, ;
                   CB11 N(8, 0) NOT NULL, ;
                   NUMES N(2, 0) NOT NULL)

***** Crear cada índice para AÑOS *****
SET COLLATE TO 'MACHINE'
INDEX ON MES TAG MES

***** Cambiar propiedades para AÑOS *****

***************** Ver la configuración para VLISFACDET ***************

CREATE SQL VIEW "VLISFACDET" ; 
   AS SELECT Facins.codfact, Facins.numfact, Facins.fechafac, Facins.fecven1,  Facins.valdesc, Facins.valfin, Facins.valcom, Facins.valflete,  Facins.valoriva, Facins.valortot, Facins.codcp, Facins.pagada,  Facins.totafac, Facins.codfin, Iprov.nombprov, Iprov.direcprov,  Iprov.telprov, Iprov.faxprov, Iprov.ciudprov, Iprov.contprov,  Iprov.nitprov, Iprov.paisprov, Iprov.dinter, Iprov.cointer,  Compins.nominsum, Compins.nomtipo, Compins.cantunit, Compins.equivale,  Compins.cantins, Compins.costunit, Compins.nomunid, Compins.total,  Facins.marcafac FROM sacfadb!facins, sacfadb!iprov, sacfadb!compins WHERE Facins.codprov = Iprov.codprov   AND Facins.codfact = Compins.codfact ORDER BY Facins.numfact, Facins.fechafac

DBSetProp('VLISFACDET', 'View', 'UpdateType', 1)
DBSetProp('VLISFACDET', 'View', 'WhereType', 3)
DBSetProp('VLISFACDET', 'View', 'FetchMemo', .T.)
DBSetProp('VLISFACDET', 'View', 'SendUpdates', .F.)
DBSetProp('VLISFACDET', 'View', 'UseMemoSize', 255)
DBSetProp('VLISFACDET', 'View', 'FetchSize', 100)
DBSetProp('VLISFACDET', 'View', 'MaxRecords', -1)
DBSetProp('VLISFACDET', 'View', 'Tables', 'sacfadb!facins')
DBSetProp('VLISFACDET', 'View', 'Prepared', .F.)
DBSetProp('VLISFACDET', 'View', 'CompareMemo', .T.)
DBSetProp('VLISFACDET', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VLISFACDET', 'View', 'FetchSize', 100)
DBSetProp('VLISFACDET', 'View', 'Comment', "")
DBSetProp('VLISFACDET', 'View', 'BatchUpdateCount', 1)
DBSetProp('VLISFACDET', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VLISFACDET
* Propiedades para VLISFACDET.codfact field.
DBSetProp('VLISFACDET.codfact', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.codfact', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.codfact', 'Field', 'UpdateName', 'sacfadb!facins.codfact')
DBSetProp('VLISFACDET.codfact', 'Field', 'DataType', "N(7)")
* Propiedades para VLISFACDET.numfact field.
DBSetProp('VLISFACDET.numfact', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.numfact', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.numfact', 'Field', 'UpdateName', 'sacfadb!facins.numfact')
DBSetProp('VLISFACDET.numfact', 'Field', 'DataType', "C(9)")
* Propiedades para VLISFACDET.fechafac field.
DBSetProp('VLISFACDET.fechafac', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.fechafac', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.fechafac', 'Field', 'UpdateName', 'sacfadb!facins.fechafac')
DBSetProp('VLISFACDET.fechafac', 'Field', 'DataType', "D")
* Propiedades para VLISFACDET.fecven1 field.
DBSetProp('VLISFACDET.fecven1', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.fecven1', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.fecven1', 'Field', 'UpdateName', 'sacfadb!facins.fecven1')
DBSetProp('VLISFACDET.fecven1', 'Field', 'DataType', "D")
* Propiedades para VLISFACDET.valdesc field.
DBSetProp('VLISFACDET.valdesc', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.valdesc', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.valdesc', 'Field', 'UpdateName', 'sacfadb!facins.valdesc')
DBSetProp('VLISFACDET.valdesc', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFACDET.valfin field.
DBSetProp('VLISFACDET.valfin', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.valfin', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.valfin', 'Field', 'UpdateName', 'sacfadb!facins.valfin')
DBSetProp('VLISFACDET.valfin', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFACDET.valcom field.
DBSetProp('VLISFACDET.valcom', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.valcom', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.valcom', 'Field', 'UpdateName', 'sacfadb!facins.valcom')
DBSetProp('VLISFACDET.valcom', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFACDET.valflete field.
DBSetProp('VLISFACDET.valflete', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.valflete', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.valflete', 'Field', 'UpdateName', 'sacfadb!facins.valflete')
DBSetProp('VLISFACDET.valflete', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFACDET.valoriva field.
DBSetProp('VLISFACDET.valoriva', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.valoriva', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.valoriva', 'Field', 'UpdateName', 'sacfadb!facins.valoriva')
DBSetProp('VLISFACDET.valoriva', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFACDET.valortot field.
DBSetProp('VLISFACDET.valortot', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.valortot', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.valortot', 'Field', 'UpdateName', 'sacfadb!facins.valortot')
DBSetProp('VLISFACDET.valortot', 'Field', 'DataType', "N(20,1)")
* Propiedades para VLISFACDET.codcp field.
DBSetProp('VLISFACDET.codcp', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.codcp', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.codcp', 'Field', 'UpdateName', 'sacfadb!facins.codcp')
DBSetProp('VLISFACDET.codcp', 'Field', 'DataType', "N(10)")
* Propiedades para VLISFACDET.pagada field.
DBSetProp('VLISFACDET.pagada', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.pagada', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.pagada', 'Field', 'UpdateName', 'sacfadb!facins.pagada')
DBSetProp('VLISFACDET.pagada', 'Field', 'DataType', "L")
* Propiedades para VLISFACDET.totafac field.
DBSetProp('VLISFACDET.totafac', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.totafac', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.totafac', 'Field', 'UpdateName', 'sacfadb!facins.totafac')
DBSetProp('VLISFACDET.totafac', 'Field', 'DataType', "N(20)")
* Propiedades para VLISFACDET.codfin field.
DBSetProp('VLISFACDET.codfin', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.codfin', 'Field', 'Updatable', .T.)
DBSetProp('VLISFACDET.codfin', 'Field', 'UpdateName', 'sacfadb!facins.codfin')
DBSetProp('VLISFACDET.codfin', 'Field', 'DataType', "N(4)")
* Propiedades para VLISFACDET.nombprov field.
DBSetProp('VLISFACDET.nombprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.nombprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.nombprov', 'Field', 'UpdateName', 'sacfadb!iprov.nombprov')
DBSetProp('VLISFACDET.nombprov', 'Field', 'DataType', "C(30)")
* Propiedades para VLISFACDET.direcprov field.
DBSetProp('VLISFACDET.direcprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.direcprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.direcprov', 'Field', 'UpdateName', 'sacfadb!iprov.direcprov')
DBSetProp('VLISFACDET.direcprov', 'Field', 'DataType', "C(30)")
* Propiedades para VLISFACDET.telprov field.
DBSetProp('VLISFACDET.telprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.telprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.telprov', 'Field', 'UpdateName', 'sacfadb!iprov.telprov')
DBSetProp('VLISFACDET.telprov', 'Field', 'DataType', "N(10)")
* Propiedades para VLISFACDET.faxprov field.
DBSetProp('VLISFACDET.faxprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.faxprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.faxprov', 'Field', 'UpdateName', 'sacfadb!iprov.faxprov')
DBSetProp('VLISFACDET.faxprov', 'Field', 'DataType', "N(10)")
* Propiedades para VLISFACDET.ciudprov field.
DBSetProp('VLISFACDET.ciudprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.ciudprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.ciudprov', 'Field', 'UpdateName', 'sacfadb!iprov.ciudprov')
DBSetProp('VLISFACDET.ciudprov', 'Field', 'DataType', "C(23)")
* Propiedades para VLISFACDET.contprov field.
DBSetProp('VLISFACDET.contprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.contprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.contprov', 'Field', 'UpdateName', 'sacfadb!iprov.contprov')
DBSetProp('VLISFACDET.contprov', 'Field', 'DataType', "N(5)")
* Propiedades para VLISFACDET.nitprov field.
DBSetProp('VLISFACDET.nitprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.nitprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.nitprov', 'Field', 'UpdateName', 'sacfadb!iprov.nitprov')
DBSetProp('VLISFACDET.nitprov', 'Field', 'DataType', "C(11)")
* Propiedades para VLISFACDET.paisprov field.
DBSetProp('VLISFACDET.paisprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.paisprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.paisprov', 'Field', 'UpdateName', 'sacfadb!iprov.paisprov')
DBSetProp('VLISFACDET.paisprov', 'Field', 'DataType', "C(15)")
* Propiedades para VLISFACDET.dinter field.
DBSetProp('VLISFACDET.dinter', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.dinter', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.dinter', 'Field', 'UpdateName', 'sacfadb!iprov.dinter')
DBSetProp('VLISFACDET.dinter', 'Field', 'DataType', "C(25)")
* Propiedades para VLISFACDET.cointer field.
DBSetProp('VLISFACDET.cointer', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.cointer', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.cointer', 'Field', 'UpdateName', 'sacfadb!iprov.cointer')
DBSetProp('VLISFACDET.cointer', 'Field', 'DataType', "C(25)")
* Propiedades para VLISFACDET.nominsum field.
DBSetProp('VLISFACDET.nominsum', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.nominsum', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.nominsum', 'Field', 'UpdateName', 'sacfadb!compins.nominsum')
DBSetProp('VLISFACDET.nominsum', 'Field', 'DataType', "C(15)")
* Propiedades para VLISFACDET.nomtipo field.
DBSetProp('VLISFACDET.nomtipo', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.nomtipo', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.nomtipo', 'Field', 'UpdateName', 'sacfadb!compins.nomtipo')
DBSetProp('VLISFACDET.nomtipo', 'Field', 'DataType', "C(15)")
* Propiedades para VLISFACDET.cantunit field.
DBSetProp('VLISFACDET.cantunit', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.cantunit', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.cantunit', 'Field', 'UpdateName', 'sacfadb!compins.cantunit')
DBSetProp('VLISFACDET.cantunit', 'Field', 'DataType', "N(11,2)")
* Propiedades para VLISFACDET.equivale field.
DBSetProp('VLISFACDET.equivale', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.equivale', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.equivale', 'Field', 'UpdateName', 'sacfadb!compins.equivale')
DBSetProp('VLISFACDET.equivale', 'Field', 'DataType', "N(8,3)")
* Propiedades para VLISFACDET.cantins field.
DBSetProp('VLISFACDET.cantins', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.cantins', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.cantins', 'Field', 'UpdateName', 'sacfadb!compins.cantins')
DBSetProp('VLISFACDET.cantins', 'Field', 'DataType', "N(15,3)")
* Propiedades para VLISFACDET.costunit field.
DBSetProp('VLISFACDET.costunit', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.costunit', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.costunit', 'Field', 'UpdateName', 'sacfadb!compins.costunit')
DBSetProp('VLISFACDET.costunit', 'Field', 'DataType', "N(10,2)")
* Propiedades para VLISFACDET.nomunid field.
DBSetProp('VLISFACDET.nomunid', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.nomunid', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.nomunid', 'Field', 'UpdateName', 'sacfadb!compins.nomunid')
DBSetProp('VLISFACDET.nomunid', 'Field', 'DataType', "C(12)")
* Propiedades para VLISFACDET.total field.
DBSetProp('VLISFACDET.total', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.total', 'Field', 'Updatable', .F.)
DBSetProp('VLISFACDET.total', 'Field', 'UpdateName', 'sacfadb!compins.total')
DBSetProp('VLISFACDET.total', 'Field', 'DataType', "N(16,1)")
* Propiedades para VLISFACDET.marcafac field.
DBSetProp('VLISFACDET.marcafac', 'Field', 'KeyField', .F.)
DBSetProp('VLISFACDET.marcafac', 'Field', 'Updatable', .T.)
DBSetProp('VLISFACDET.marcafac', 'Field', 'UpdateName', 'sacfadb!facins.marcafac')
DBSetProp('VLISFACDET.marcafac', 'Field', 'DataType', "L")
***************** Ver la configuración para VLISFAC ***************

CREATE SQL VIEW "VLISFAC" ; 
   AS SELECT Facins.codfin, Facins.codfact, Facins.numfact, Facins.fechafac,  Facins.fecven1, Facins.valdesc, Facins.valfin, Facins.valcom,  Facins.valflete, Facins.valoriva, Facins.valortot, Facins.codcp,  Facins.pagada, Facins.totafac, Iprov.nombprov, Iprov.direcprov,  Iprov.telprov, Iprov.faxprov, Iprov.ciudprov, Iprov.contprov,  Iprov.nitprov, Iprov.paisprov, Iprov.dinter, Iprov.cointer,  Compins.nominsum, Compins.nomtipo, Compins.cantunit, Compins.equivale,  Compins.cantins, Compins.costunit, Compins.nomunid, Compins.total,  Facins.marcafac FROM sacfadb!facins, sacfadb!iprov, sacfadb!compins WHERE Facins.codprov = Iprov.codprov   AND Facins.codfact = Compins.codfact GROUP BY Facins.codfact ORDER BY Facins.numfact, Facins.fechafac

DBSetProp('VLISFAC', 'View', 'UpdateType', 1)
DBSetProp('VLISFAC', 'View', 'WhereType', 3)
DBSetProp('VLISFAC', 'View', 'FetchMemo', .T.)
DBSetProp('VLISFAC', 'View', 'SendUpdates', .F.)
DBSetProp('VLISFAC', 'View', 'UseMemoSize', 255)
DBSetProp('VLISFAC', 'View', 'FetchSize', 100)
DBSetProp('VLISFAC', 'View', 'MaxRecords', -1)
DBSetProp('VLISFAC', 'View', 'Tables', 'sacfadb!facins')
DBSetProp('VLISFAC', 'View', 'Prepared', .F.)
DBSetProp('VLISFAC', 'View', 'CompareMemo', .T.)
DBSetProp('VLISFAC', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VLISFAC', 'View', 'FetchSize', 100)
DBSetProp('VLISFAC', 'View', 'Comment', "")
DBSetProp('VLISFAC', 'View', 'BatchUpdateCount', 1)
DBSetProp('VLISFAC', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VLISFAC
* Propiedades para VLISFAC.codfin field.
DBSetProp('VLISFAC.codfin', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.codfin', 'Field', 'Updatable', .T.)
DBSetProp('VLISFAC.codfin', 'Field', 'UpdateName', 'sacfadb!facins.codfin')
DBSetProp('VLISFAC.codfin', 'Field', 'DataType', "N(4)")
* Propiedades para VLISFAC.codfact field.
DBSetProp('VLISFAC.codfact', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.codfact', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.codfact', 'Field', 'UpdateName', 'sacfadb!facins.codfact')
DBSetProp('VLISFAC.codfact', 'Field', 'DataType', "N(7)")
* Propiedades para VLISFAC.numfact field.
DBSetProp('VLISFAC.numfact', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.numfact', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.numfact', 'Field', 'UpdateName', 'sacfadb!facins.numfact')
DBSetProp('VLISFAC.numfact', 'Field', 'DataType', "C(9)")
* Propiedades para VLISFAC.fechafac field.
DBSetProp('VLISFAC.fechafac', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.fechafac', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.fechafac', 'Field', 'UpdateName', 'sacfadb!facins.fechafac')
DBSetProp('VLISFAC.fechafac', 'Field', 'DataType', "D")
* Propiedades para VLISFAC.fecven1 field.
DBSetProp('VLISFAC.fecven1', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.fecven1', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.fecven1', 'Field', 'UpdateName', 'sacfadb!facins.fecven1')
DBSetProp('VLISFAC.fecven1', 'Field', 'DataType', "D")
* Propiedades para VLISFAC.valdesc field.
DBSetProp('VLISFAC.valdesc', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.valdesc', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.valdesc', 'Field', 'UpdateName', 'sacfadb!facins.valdesc')
DBSetProp('VLISFAC.valdesc', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFAC.valfin field.
DBSetProp('VLISFAC.valfin', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.valfin', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.valfin', 'Field', 'UpdateName', 'sacfadb!facins.valfin')
DBSetProp('VLISFAC.valfin', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFAC.valcom field.
DBSetProp('VLISFAC.valcom', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.valcom', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.valcom', 'Field', 'UpdateName', 'sacfadb!facins.valcom')
DBSetProp('VLISFAC.valcom', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFAC.valflete field.
DBSetProp('VLISFAC.valflete', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.valflete', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.valflete', 'Field', 'UpdateName', 'sacfadb!facins.valflete')
DBSetProp('VLISFAC.valflete', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFAC.valoriva field.
DBSetProp('VLISFAC.valoriva', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.valoriva', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.valoriva', 'Field', 'UpdateName', 'sacfadb!facins.valoriva')
DBSetProp('VLISFAC.valoriva', 'Field', 'DataType', "N(11)")
* Propiedades para VLISFAC.valortot field.
DBSetProp('VLISFAC.valortot', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.valortot', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.valortot', 'Field', 'UpdateName', 'sacfadb!facins.valortot')
DBSetProp('VLISFAC.valortot', 'Field', 'DataType', "N(20,1)")
* Propiedades para VLISFAC.codcp field.
DBSetProp('VLISFAC.codcp', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.codcp', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.codcp', 'Field', 'UpdateName', 'sacfadb!facins.codcp')
DBSetProp('VLISFAC.codcp', 'Field', 'DataType', "N(10)")
* Propiedades para VLISFAC.pagada field.
DBSetProp('VLISFAC.pagada', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.pagada', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.pagada', 'Field', 'UpdateName', 'sacfadb!facins.pagada')
DBSetProp('VLISFAC.pagada', 'Field', 'DataType', "L")
* Propiedades para VLISFAC.totafac field.
DBSetProp('VLISFAC.totafac', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.totafac', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.totafac', 'Field', 'UpdateName', 'sacfadb!facins.totafac')
DBSetProp('VLISFAC.totafac', 'Field', 'DataType', "N(20)")
* Propiedades para VLISFAC.nombprov field.
DBSetProp('VLISFAC.nombprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.nombprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.nombprov', 'Field', 'UpdateName', 'sacfadb!iprov.nombprov')
DBSetProp('VLISFAC.nombprov', 'Field', 'DataType', "C(30)")
* Propiedades para VLISFAC.direcprov field.
DBSetProp('VLISFAC.direcprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.direcprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.direcprov', 'Field', 'UpdateName', 'sacfadb!iprov.direcprov')
DBSetProp('VLISFAC.direcprov', 'Field', 'DataType', "C(30)")
* Propiedades para VLISFAC.telprov field.
DBSetProp('VLISFAC.telprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.telprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.telprov', 'Field', 'UpdateName', 'sacfadb!iprov.telprov')
DBSetProp('VLISFAC.telprov', 'Field', 'DataType', "N(10)")
* Propiedades para VLISFAC.faxprov field.
DBSetProp('VLISFAC.faxprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.faxprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.faxprov', 'Field', 'UpdateName', 'sacfadb!iprov.faxprov')
DBSetProp('VLISFAC.faxprov', 'Field', 'DataType', "N(10)")
* Propiedades para VLISFAC.ciudprov field.
DBSetProp('VLISFAC.ciudprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.ciudprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.ciudprov', 'Field', 'UpdateName', 'sacfadb!iprov.ciudprov')
DBSetProp('VLISFAC.ciudprov', 'Field', 'DataType', "C(23)")
* Propiedades para VLISFAC.contprov field.
DBSetProp('VLISFAC.contprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.contprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.contprov', 'Field', 'UpdateName', 'sacfadb!iprov.contprov')
DBSetProp('VLISFAC.contprov', 'Field', 'DataType', "N(5)")
* Propiedades para VLISFAC.nitprov field.
DBSetProp('VLISFAC.nitprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.nitprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.nitprov', 'Field', 'UpdateName', 'sacfadb!iprov.nitprov')
DBSetProp('VLISFAC.nitprov', 'Field', 'DataType', "C(11)")
* Propiedades para VLISFAC.paisprov field.
DBSetProp('VLISFAC.paisprov', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.paisprov', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.paisprov', 'Field', 'UpdateName', 'sacfadb!iprov.paisprov')
DBSetProp('VLISFAC.paisprov', 'Field', 'DataType', "C(15)")
* Propiedades para VLISFAC.dinter field.
DBSetProp('VLISFAC.dinter', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.dinter', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.dinter', 'Field', 'UpdateName', 'sacfadb!iprov.dinter')
DBSetProp('VLISFAC.dinter', 'Field', 'DataType', "C(25)")
* Propiedades para VLISFAC.cointer field.
DBSetProp('VLISFAC.cointer', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.cointer', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.cointer', 'Field', 'UpdateName', 'sacfadb!iprov.cointer')
DBSetProp('VLISFAC.cointer', 'Field', 'DataType', "C(25)")
* Propiedades para VLISFAC.nominsum field.
DBSetProp('VLISFAC.nominsum', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.nominsum', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.nominsum', 'Field', 'UpdateName', 'sacfadb!compins.nominsum')
DBSetProp('VLISFAC.nominsum', 'Field', 'DataType', "C(15)")
* Propiedades para VLISFAC.nomtipo field.
DBSetProp('VLISFAC.nomtipo', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.nomtipo', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.nomtipo', 'Field', 'UpdateName', 'sacfadb!compins.nomtipo')
DBSetProp('VLISFAC.nomtipo', 'Field', 'DataType', "C(15)")
* Propiedades para VLISFAC.cantunit field.
DBSetProp('VLISFAC.cantunit', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.cantunit', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.cantunit', 'Field', 'UpdateName', 'sacfadb!compins.cantunit')
DBSetProp('VLISFAC.cantunit', 'Field', 'DataType', "N(11,2)")
* Propiedades para VLISFAC.equivale field.
DBSetProp('VLISFAC.equivale', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.equivale', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.equivale', 'Field', 'UpdateName', 'sacfadb!compins.equivale')
DBSetProp('VLISFAC.equivale', 'Field', 'DataType', "N(8,3)")
* Propiedades para VLISFAC.cantins field.
DBSetProp('VLISFAC.cantins', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.cantins', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.cantins', 'Field', 'UpdateName', 'sacfadb!compins.cantins')
DBSetProp('VLISFAC.cantins', 'Field', 'DataType', "N(15,3)")
* Propiedades para VLISFAC.costunit field.
DBSetProp('VLISFAC.costunit', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.costunit', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.costunit', 'Field', 'UpdateName', 'sacfadb!compins.costunit')
DBSetProp('VLISFAC.costunit', 'Field', 'DataType', "N(10,2)")
* Propiedades para VLISFAC.nomunid field.
DBSetProp('VLISFAC.nomunid', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.nomunid', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.nomunid', 'Field', 'UpdateName', 'sacfadb!compins.nomunid')
DBSetProp('VLISFAC.nomunid', 'Field', 'DataType', "C(12)")
* Propiedades para VLISFAC.total field.
DBSetProp('VLISFAC.total', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.total', 'Field', 'Updatable', .F.)
DBSetProp('VLISFAC.total', 'Field', 'UpdateName', 'sacfadb!compins.total')
DBSetProp('VLISFAC.total', 'Field', 'DataType', "N(16,1)")
* Propiedades para VLISFAC.marcafac field.
DBSetProp('VLISFAC.marcafac', 'Field', 'KeyField', .F.)
DBSetProp('VLISFAC.marcafac', 'Field', 'Updatable', .T.)
DBSetProp('VLISFAC.marcafac', 'Field', 'UpdateName', 'sacfadb!facins.marcafac')
DBSetProp('VLISFAC.marcafac', 'Field', 'DataType', "L")
***************** Ver la configuración para VISAPL ***************

CREATE SQL VIEW "VISAPL" ; 
   AS SELECT Regfin2.codaplic, Regfin2.fecaplic, Regfin2.hectaplic,  Regfin2.dias, Regfin2.tipins, Regfin2.cantusada, Regfin2.abrenom,  Regfin2.vtot, Regfin2.valplic, Regfin2.valtaplic FROM regfin2

DBSetProp('VISAPL', 'View', 'UpdateType', 1)
DBSetProp('VISAPL', 'View', 'WhereType', 3)
DBSetProp('VISAPL', 'View', 'FetchMemo', .T.)
DBSetProp('VISAPL', 'View', 'SendUpdates', .F.)
DBSetProp('VISAPL', 'View', 'UseMemoSize', 255)
DBSetProp('VISAPL', 'View', 'FetchSize', 100)
DBSetProp('VISAPL', 'View', 'MaxRecords', -1)
DBSetProp('VISAPL', 'View', 'Tables', 'regfin2')
DBSetProp('VISAPL', 'View', 'Prepared', .F.)
DBSetProp('VISAPL', 'View', 'CompareMemo', .T.)
DBSetProp('VISAPL', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VISAPL', 'View', 'FetchSize', 100)
DBSetProp('VISAPL', 'View', 'Comment', "")
DBSetProp('VISAPL', 'View', 'BatchUpdateCount', 1)
DBSetProp('VISAPL', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VISAPL
* Propiedades para VISAPL.codaplic field.
DBSetProp('VISAPL.codaplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.codaplic', 'Field', 'Updatable', .T.)
DBSetProp('VISAPL.codaplic', 'Field', 'UpdateName', 'regfin2.codaplic')
DBSetProp('VISAPL.codaplic', 'Field', 'DataType', "C(8)")
* Propiedades para VISAPL.fecaplic field.
DBSetProp('VISAPL.fecaplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.fecaplic', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL.fecaplic', 'Field', 'UpdateName', 'regfin2.fecaplic')
DBSetProp('VISAPL.fecaplic', 'Field', 'DataType', "D")
* Propiedades para VISAPL.hectaplic field.
DBSetProp('VISAPL.hectaplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.hectaplic', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL.hectaplic', 'Field', 'UpdateName', 'regfin2.hectaplic')
DBSetProp('VISAPL.hectaplic', 'Field', 'DataType', "N(8,2)")
* Propiedades para VISAPL.dias field.
DBSetProp('VISAPL.dias', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.dias', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL.dias', 'Field', 'UpdateName', 'regfin2.dias')
DBSetProp('VISAPL.dias', 'Field', 'DataType', "N(10)")
* Propiedades para VISAPL.tipins field.
DBSetProp('VISAPL.tipins', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.tipins', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL.tipins', 'Field', 'UpdateName', 'regfin2.tipins')
DBSetProp('VISAPL.tipins', 'Field', 'DataType', "C(25)")
* Propiedades para VISAPL.cantusada field.
DBSetProp('VISAPL.cantusada', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.cantusada', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL.cantusada', 'Field', 'UpdateName', 'regfin2.cantusada')
DBSetProp('VISAPL.cantusada', 'Field', 'DataType', "N(14,3)")
* Propiedades para VISAPL.abrenom field.
DBSetProp('VISAPL.abrenom', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.abrenom', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL.abrenom', 'Field', 'UpdateName', 'regfin2.abrenom')
DBSetProp('VISAPL.abrenom', 'Field', 'DataType', "C(10)")
* Propiedades para VISAPL.vtot field.
DBSetProp('VISAPL.vtot', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.vtot', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL.vtot', 'Field', 'UpdateName', 'regfin2.vtot')
DBSetProp('VISAPL.vtot', 'Field', 'DataType', "N(20)")
* Propiedades para VISAPL.valplic field.
DBSetProp('VISAPL.valplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.valplic', 'Field', 'Updatable', .T.)
DBSetProp('VISAPL.valplic', 'Field', 'UpdateName', 'regfin2.valplic')
DBSetProp('VISAPL.valplic', 'Field', 'DataType', "N(10)")
* Propiedades para VISAPL.valtaplic field.
DBSetProp('VISAPL.valtaplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL.valtaplic', 'Field', 'Updatable', .T.)
DBSetProp('VISAPL.valtaplic', 'Field', 'UpdateName', 'regfin2.valtaplic')
DBSetProp('VISAPL.valtaplic', 'Field', 'DataType', "N(10)")
***************** Ver la configuración para VLUBRI ***************

CREATE SQL VIEW "VLUBRI" ; 
   AS SELECT Maquinas.codfin, Maquinas.nombmaq, Maquinas.seriemaq,  Maquinas.marcamod, Combust.fechaufc, Combust.cantufc, Combust.equivale,  Insumo.nominsum, Undcomp.nombreund, Combust.kilometro, Maquinas.codmaq,  Combust.numcontrol, Combust.codufc FROM sacfadb!maquinas, sacfadb!combust, sacfadb!insumo,  sacfadb!undcomp WHERE Maquinas.codmaq = Combust.codmaq   AND Combust.codinsum = Insumo.codinsum   AND Combust.codunid = Undcomp.codigo ORDER BY Combust.fechaufc

DBSetProp('VLUBRI', 'View', 'UpdateType', 1)
DBSetProp('VLUBRI', 'View', 'WhereType', 3)
DBSetProp('VLUBRI', 'View', 'FetchMemo', .T.)
DBSetProp('VLUBRI', 'View', 'SendUpdates', .F.)
DBSetProp('VLUBRI', 'View', 'UseMemoSize', 255)
DBSetProp('VLUBRI', 'View', 'FetchSize', 100)
DBSetProp('VLUBRI', 'View', 'MaxRecords', -1)
DBSetProp('VLUBRI', 'View', 'Tables', 'sacfadb!combust,sacfadb!maquinas')
DBSetProp('VLUBRI', 'View', 'Prepared', .F.)
DBSetProp('VLUBRI', 'View', 'CompareMemo', .T.)
DBSetProp('VLUBRI', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VLUBRI', 'View', 'FetchSize', 100)
DBSetProp('VLUBRI', 'View', 'Comment', "")
DBSetProp('VLUBRI', 'View', 'BatchUpdateCount', 1)
DBSetProp('VLUBRI', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VLUBRI
* Propiedades para VLUBRI.codfin field.
DBSetProp('VLUBRI.codfin', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.codfin', 'Field', 'Updatable', .F.)
DBSetProp('VLUBRI.codfin', 'Field', 'UpdateName', 'sacfadb!maquinas.codfin')
DBSetProp('VLUBRI.codfin', 'Field', 'DataType', "N(4)")
* Propiedades para VLUBRI.nombmaq field.
DBSetProp('VLUBRI.nombmaq', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.nombmaq', 'Field', 'Updatable', .F.)
DBSetProp('VLUBRI.nombmaq', 'Field', 'UpdateName', 'sacfadb!maquinas.nombmaq')
DBSetProp('VLUBRI.nombmaq', 'Field', 'DataType', "C(15)")
* Propiedades para VLUBRI.seriemaq field.
DBSetProp('VLUBRI.seriemaq', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.seriemaq', 'Field', 'Updatable', .F.)
DBSetProp('VLUBRI.seriemaq', 'Field', 'UpdateName', 'sacfadb!maquinas.seriemaq')
DBSetProp('VLUBRI.seriemaq', 'Field', 'DataType', "C(10)")
* Propiedades para VLUBRI.marcamod field.
DBSetProp('VLUBRI.marcamod', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.marcamod', 'Field', 'Updatable', .F.)
DBSetProp('VLUBRI.marcamod', 'Field', 'UpdateName', 'sacfadb!maquinas.marcamod')
DBSetProp('VLUBRI.marcamod', 'Field', 'DataType', "C(15)")
* Propiedades para VLUBRI.fechaufc field.
DBSetProp('VLUBRI.fechaufc', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.fechaufc', 'Field', 'Updatable', .F.)
DBSetProp('VLUBRI.fechaufc', 'Field', 'UpdateName', 'sacfadb!combust.fechaufc')
DBSetProp('VLUBRI.fechaufc', 'Field', 'DataType', "D")
* Propiedades para VLUBRI.cantufc field.
DBSetProp('VLUBRI.cantufc', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.cantufc', 'Field', 'Updatable', .F.)
DBSetProp('VLUBRI.cantufc', 'Field', 'UpdateName', 'sacfadb!combust.cantufc')
DBSetProp('VLUBRI.cantufc', 'Field', 'DataType', "N(11,5)")
* Propiedades para VLUBRI.equivale field.
DBSetProp('VLUBRI.equivale', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.equivale', 'Field', 'Updatable', .F.)
DBSetProp('VLUBRI.equivale', 'Field', 'UpdateName', 'sacfadb!combust.equivale')
DBSetProp('VLUBRI.equivale', 'Field', 'DataType', "N(8,5)")
* Propiedades para VLUBRI.nominsum field.
DBSetProp('VLUBRI.nominsum', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.nominsum', 'Field', 'Updatable', .F.)
DBSetProp('VLUBRI.nominsum', 'Field', 'UpdateName', 'sacfadb!insumo.nominsum')
DBSetProp('VLUBRI.nominsum', 'Field', 'DataType', "C(20)")
* Propiedades para VLUBRI.nombreund field.
DBSetProp('VLUBRI.nombreund', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.nombreund', 'Field', 'Updatable', .F.)
DBSetProp('VLUBRI.nombreund', 'Field', 'UpdateName', 'sacfadb!undcomp.nombreund')
DBSetProp('VLUBRI.nombreund', 'Field', 'DataType', "C(10)")
* Propiedades para VLUBRI.kilometro field.
DBSetProp('VLUBRI.kilometro', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.kilometro', 'Field', 'Updatable', .T.)
DBSetProp('VLUBRI.kilometro', 'Field', 'UpdateName', 'sacfadb!combust.kilometro')
DBSetProp('VLUBRI.kilometro', 'Field', 'DataType', "N(7)")
* Propiedades para VLUBRI.codmaq field.
DBSetProp('VLUBRI.codmaq', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.codmaq', 'Field', 'Updatable', .T.)
DBSetProp('VLUBRI.codmaq', 'Field', 'UpdateName', 'sacfadb!maquinas.codmaq')
DBSetProp('VLUBRI.codmaq', 'Field', 'DataType', "N(4)")
* Propiedades para VLUBRI.numcontrol field.
DBSetProp('VLUBRI.numcontrol', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.numcontrol', 'Field', 'Updatable', .T.)
DBSetProp('VLUBRI.numcontrol', 'Field', 'UpdateName', 'sacfadb!combust.numcontrol')
DBSetProp('VLUBRI.numcontrol', 'Field', 'DataType', "N(8)")
* Propiedades para VLUBRI.codufc field.
DBSetProp('VLUBRI.codufc', 'Field', 'KeyField', .F.)
DBSetProp('VLUBRI.codufc', 'Field', 'Updatable', .T.)
DBSetProp('VLUBRI.codufc', 'Field', 'UpdateName', 'sacfadb!combust.codufc')
DBSetProp('VLUBRI.codufc', 'Field', 'DataType', "N(4)")
***************** Ver la configuración para VCREDI ***************

CREATE SQL VIEW "VCREDI" ; 
   AS SELECT Credito.codfin, Credito.numcred, Credito.nomprov, Credito.finagro,  Credito.interes, Dcredito.fecha AS fec1, Dcredito.valor AS val,  Dcredito.tipo, Credito.marcacred, Credito.ano, Credito.semestre,  Dcredito.codcred, Credito.valcred FROM  sacfadb!credito LEFT OUTER JOIN sacfadb!dcredito    ON  Credito.codcred = Dcredito.codcred GROUP BY Dcredito.codcred ORDER BY Dcredito.fecha

DBSetProp('VCREDI', 'View', 'UpdateType', 1)
DBSetProp('VCREDI', 'View', 'WhereType', 3)
DBSetProp('VCREDI', 'View', 'FetchMemo', .T.)
DBSetProp('VCREDI', 'View', 'SendUpdates', .F.)
DBSetProp('VCREDI', 'View', 'UseMemoSize', 255)
DBSetProp('VCREDI', 'View', 'FetchSize', 100)
DBSetProp('VCREDI', 'View', 'MaxRecords', -1)
DBSetProp('VCREDI', 'View', 'Tables', 'sacfadb!dcredito')
DBSetProp('VCREDI', 'View', 'Prepared', .F.)
DBSetProp('VCREDI', 'View', 'CompareMemo', .T.)
DBSetProp('VCREDI', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VCREDI', 'View', 'FetchSize', 100)
DBSetProp('VCREDI', 'View', 'Comment', "")
DBSetProp('VCREDI', 'View', 'BatchUpdateCount', 1)
DBSetProp('VCREDI', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VCREDI
* Propiedades para VCREDI.codfin field.
DBSetProp('VCREDI.codfin', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.codfin', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.codfin', 'Field', 'UpdateName', 'sacfadb!credito.codfin')
DBSetProp('VCREDI.codfin', 'Field', 'DataType', "N(4)")
* Propiedades para VCREDI.numcred field.
DBSetProp('VCREDI.numcred', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.numcred', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.numcred', 'Field', 'UpdateName', 'sacfadb!credito.numcred')
DBSetProp('VCREDI.numcred', 'Field', 'DataType', "C(10)")
* Propiedades para VCREDI.nomprov field.
DBSetProp('VCREDI.nomprov', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.nomprov', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.nomprov', 'Field', 'UpdateName', 'sacfadb!credito.nomprov')
DBSetProp('VCREDI.nomprov', 'Field', 'DataType', "C(30)")
* Propiedades para VCREDI.finagro field.
DBSetProp('VCREDI.finagro', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.finagro', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.finagro', 'Field', 'UpdateName', 'sacfadb!credito.finagro')
DBSetProp('VCREDI.finagro', 'Field', 'DataType', "N(1)")
* Propiedades para VCREDI.interes field.
DBSetProp('VCREDI.interes', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.interes', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.interes', 'Field', 'UpdateName', 'sacfadb!credito.interes')
DBSetProp('VCREDI.interes', 'Field', 'DataType', "N(3,2)")
* Propiedades para VCREDI.fec1 field.
DBSetProp('VCREDI.fec1', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.fec1', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.fec1', 'Field', 'UpdateName', 'sacfadb!dcredito.fecha')
DBSetProp('VCREDI.fec1', 'Field', 'DataType', "D")
* Propiedades para VCREDI.val field.
DBSetProp('VCREDI.val', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.val', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.val', 'Field', 'UpdateName', 'sacfadb!dcredito.valor')
DBSetProp('VCREDI.val', 'Field', 'DataType', "N(9)")
* Propiedades para VCREDI.tipo field.
DBSetProp('VCREDI.tipo', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.tipo', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.tipo', 'Field', 'UpdateName', 'sacfadb!dcredito.tipo')
DBSetProp('VCREDI.tipo', 'Field', 'DataType', "N(1)")
* Propiedades para VCREDI.marcacred field.
DBSetProp('VCREDI.marcacred', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.marcacred', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.marcacred', 'Field', 'UpdateName', 'sacfadb!credito.marcacred')
DBSetProp('VCREDI.marcacred', 'Field', 'DataType', "L")
* Propiedades para VCREDI.ano field.
DBSetProp('VCREDI.ano', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.ano', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.ano', 'Field', 'UpdateName', 'sacfadb!credito.ano')
DBSetProp('VCREDI.ano', 'Field', 'DataType', "N(4)")
* Propiedades para VCREDI.semestre field.
DBSetProp('VCREDI.semestre', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.semestre', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.semestre', 'Field', 'UpdateName', 'sacfadb!credito.semestre')
DBSetProp('VCREDI.semestre', 'Field', 'DataType', "N(1)")
* Propiedades para VCREDI.codcred field.
DBSetProp('VCREDI.codcred', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.codcred', 'Field', 'Updatable', .T.)
DBSetProp('VCREDI.codcred', 'Field', 'UpdateName', 'sacfadb!dcredito.codcred')
DBSetProp('VCREDI.codcred', 'Field', 'DataType', "N(4)")
* Propiedades para VCREDI.valcred field.
DBSetProp('VCREDI.valcred', 'Field', 'KeyField', .F.)
DBSetProp('VCREDI.valcred', 'Field', 'Updatable', .F.)
DBSetProp('VCREDI.valcred', 'Field', 'UpdateName', 'sacfadb!credito.valcred')
DBSetProp('VCREDI.valcred', 'Field', 'DataType', "N(11)")
***************** Ver la configuración para RENTRENDIMI ***************

CREATE SQL VIEW "RENTRENDIMI" ; 
   AS SELECT Infide.codlote FROM infide WHERE Infide.codcult = 1   AND Infide.ano > 90 GROUP BY Infide.codlote ORDER BY Infide.codlote

DBSetProp('RENTRENDIMI', 'View', 'UpdateType', 1)
DBSetProp('RENTRENDIMI', 'View', 'WhereType', 3)
DBSetProp('RENTRENDIMI', 'View', 'FetchMemo', .T.)
DBSetProp('RENTRENDIMI', 'View', 'SendUpdates', .F.)
DBSetProp('RENTRENDIMI', 'View', 'UseMemoSize', 255)
DBSetProp('RENTRENDIMI', 'View', 'FetchSize', 100)
DBSetProp('RENTRENDIMI', 'View', 'MaxRecords', -1)
DBSetProp('RENTRENDIMI', 'View', 'Tables', '')
DBSetProp('RENTRENDIMI', 'View', 'Prepared', .F.)
DBSetProp('RENTRENDIMI', 'View', 'CompareMemo', .T.)
DBSetProp('RENTRENDIMI', 'View', 'FetchAsNeeded', .F.)
DBSetProp('RENTRENDIMI', 'View', 'FetchSize', 100)
DBSetProp('RENTRENDIMI', 'View', 'Comment', "")
DBSetProp('RENTRENDIMI', 'View', 'BatchUpdateCount', 1)
DBSetProp('RENTRENDIMI', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para RENTRENDIMI
* Propiedades para RENTRENDIMI.codlote field.
DBSetProp('RENTRENDIMI.codlote', 'Field', 'KeyField', .F.)
DBSetProp('RENTRENDIMI.codlote', 'Field', 'Updatable', .F.)
DBSetProp('RENTRENDIMI.codlote', 'Field', 'UpdateName', 'infide.codlote')
DBSetProp('RENTRENDIMI.codlote', 'Field', 'DataType', "N(4)")
***************** Ver la configuración para VLOTES ***************

CREATE SQL VIEW "VLOTES" ; 
   AS SELECT Infide.codlote, Lotesfin.nomlote, Lotesfin.area FROM  Infide INNER JOIN sacfadb!Lotesfin  ON  Infide.codlote = Lotesfin.codlote WHERE Infide.codcult = 1 GROUP BY Infide.codlote ORDER BY Infide.codlote

DBSetProp('VLOTES', 'View', 'UpdateType', 1)
DBSetProp('VLOTES', 'View', 'WhereType', 3)
DBSetProp('VLOTES', 'View', 'FetchMemo', .T.)
DBSetProp('VLOTES', 'View', 'SendUpdates', .F.)
DBSetProp('VLOTES', 'View', 'UseMemoSize', 255)
DBSetProp('VLOTES', 'View', 'FetchSize', 100)
DBSetProp('VLOTES', 'View', 'MaxRecords', -1)
DBSetProp('VLOTES', 'View', 'Tables', 'Infide,sacfadb!Lotesfin')
DBSetProp('VLOTES', 'View', 'Prepared', .F.)
DBSetProp('VLOTES', 'View', 'CompareMemo', .T.)
DBSetProp('VLOTES', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VLOTES', 'View', 'FetchSize', 100)
DBSetProp('VLOTES', 'View', 'Comment', "")
DBSetProp('VLOTES', 'View', 'BatchUpdateCount', 1)
DBSetProp('VLOTES', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VLOTES
* Propiedades para VLOTES.codlote field.
DBSetProp('VLOTES.codlote', 'Field', 'KeyField', .F.)
DBSetProp('VLOTES.codlote', 'Field', 'Updatable', .T.)
DBSetProp('VLOTES.codlote', 'Field', 'UpdateName', 'Infide.codlote')
DBSetProp('VLOTES.codlote', 'Field', 'DataType', "N(4)")
* Propiedades para VLOTES.nomlote field.
DBSetProp('VLOTES.nomlote', 'Field', 'KeyField', .F.)
DBSetProp('VLOTES.nomlote', 'Field', 'Updatable', .T.)
DBSetProp('VLOTES.nomlote', 'Field', 'UpdateName', 'sacfadb!Lotesfin.nomlote')
DBSetProp('VLOTES.nomlote', 'Field', 'DataType', "C(20)")
* Propiedades para VLOTES.area field.
DBSetProp('VLOTES.area', 'Field', 'KeyField', .F.)
DBSetProp('VLOTES.area', 'Field', 'Updatable', .T.)
DBSetProp('VLOTES.area', 'Field', 'UpdateName', 'sacfadb!Lotesfin.area')
DBSetProp('VLOTES.area', 'Field', 'DataType', "N(8,2)")
***************** Ver la configuración para VINFCULTI ***************

CREATE SQL VIEW "VINFCULTI" ; 
   AS SELECT Infide.codlote, Infide.ano, Infide.semestre, Infide.tirsc,  Infide.toneladas/ Lotesfin.area As Ton FROM  infide INNER JOIN sacfadb!lotesfin    ON  Infide.codlote = Lotesfin.codlote WHERE Infide.codcult = 1   AND Infide.ano > 90 ORDER BY Infide.codlote, Infide.ano, Infide.semestre

DBSetProp('VINFCULTI', 'View', 'UpdateType', 1)
DBSetProp('VINFCULTI', 'View', 'WhereType', 3)
DBSetProp('VINFCULTI', 'View', 'FetchMemo', .T.)
DBSetProp('VINFCULTI', 'View', 'SendUpdates', .F.)
DBSetProp('VINFCULTI', 'View', 'UseMemoSize', 255)
DBSetProp('VINFCULTI', 'View', 'FetchSize', 100)
DBSetProp('VINFCULTI', 'View', 'MaxRecords', -1)
DBSetProp('VINFCULTI', 'View', 'Tables', '')
DBSetProp('VINFCULTI', 'View', 'Prepared', .F.)
DBSetProp('VINFCULTI', 'View', 'CompareMemo', .T.)
DBSetProp('VINFCULTI', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VINFCULTI', 'View', 'FetchSize', 100)
DBSetProp('VINFCULTI', 'View', 'Comment', "")
DBSetProp('VINFCULTI', 'View', 'BatchUpdateCount', 1)
DBSetProp('VINFCULTI', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VINFCULTI
* Propiedades para VINFCULTI.codlote field.
DBSetProp('VINFCULTI.codlote', 'Field', 'KeyField', .F.)
DBSetProp('VINFCULTI.codlote', 'Field', 'Updatable', .F.)
DBSetProp('VINFCULTI.codlote', 'Field', 'UpdateName', 'infide.codlote')
DBSetProp('VINFCULTI.codlote', 'Field', 'DataType', "N(4)")
* Propiedades para VINFCULTI.ano field.
DBSetProp('VINFCULTI.ano', 'Field', 'KeyField', .F.)
DBSetProp('VINFCULTI.ano', 'Field', 'Updatable', .F.)
DBSetProp('VINFCULTI.ano', 'Field', 'UpdateName', 'infide.ano')
DBSetProp('VINFCULTI.ano', 'Field', 'DataType', "N(2)")
* Propiedades para VINFCULTI.semestre field.
DBSetProp('VINFCULTI.semestre', 'Field', 'KeyField', .F.)
DBSetProp('VINFCULTI.semestre', 'Field', 'Updatable', .F.)
DBSetProp('VINFCULTI.semestre', 'Field', 'UpdateName', 'infide.semestre')
DBSetProp('VINFCULTI.semestre', 'Field', 'DataType', "N(1)")
* Propiedades para VINFCULTI.tirsc field.
DBSetProp('VINFCULTI.tirsc', 'Field', 'KeyField', .F.)
DBSetProp('VINFCULTI.tirsc', 'Field', 'Updatable', .F.)
DBSetProp('VINFCULTI.tirsc', 'Field', 'UpdateName', 'infide.tirsc')
DBSetProp('VINFCULTI.tirsc', 'Field', 'DataType', "N(16,4)")
* Propiedades para VINFCULTI.ton field.
DBSetProp('VINFCULTI.ton', 'Field', 'KeyField', .F.)
DBSetProp('VINFCULTI.ton', 'Field', 'Updatable', .F.)
DBSetProp('VINFCULTI.ton', 'Field', 'UpdateName', '')
DBSetProp('VINFCULTI.ton', 'Field', 'DataType', "N(19,4)")
***************** Ver la configuración para VRENTREND ***************

CREATE SQL VIEW "VRENTREND" ; 
   AS SELECT Infide.codlote, Lotesfin.nomlote, Lotesfin.area, Infide.ano,  Infide.semestre, Infide.tirsc, Infide.toneladas/ Lotesfin.area As Ton FROM  infide INNER JOIN sacfadb!lotesfin    ON  Infide.codlote = Lotesfin.codlote WHERE Infide.codcult = 1   AND Infide.ano > 90 ORDER BY Infide.codlote, Infide.ano, Infide.semestre

DBSetProp('VRENTREND', 'View', 'UpdateType', 1)
DBSetProp('VRENTREND', 'View', 'WhereType', 3)
DBSetProp('VRENTREND', 'View', 'FetchMemo', .T.)
DBSetProp('VRENTREND', 'View', 'SendUpdates', .F.)
DBSetProp('VRENTREND', 'View', 'UseMemoSize', 255)
DBSetProp('VRENTREND', 'View', 'FetchSize', 100)
DBSetProp('VRENTREND', 'View', 'MaxRecords', -1)
DBSetProp('VRENTREND', 'View', 'Tables', 'sacfadb!lotesfin')
DBSetProp('VRENTREND', 'View', 'Prepared', .F.)
DBSetProp('VRENTREND', 'View', 'CompareMemo', .T.)
DBSetProp('VRENTREND', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VRENTREND', 'View', 'FetchSize', 100)
DBSetProp('VRENTREND', 'View', 'Comment', "")
DBSetProp('VRENTREND', 'View', 'BatchUpdateCount', 1)
DBSetProp('VRENTREND', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VRENTREND
* Propiedades para VRENTREND.codlote field.
DBSetProp('VRENTREND.codlote', 'Field', 'KeyField', .F.)
DBSetProp('VRENTREND.codlote', 'Field', 'Updatable', .F.)
DBSetProp('VRENTREND.codlote', 'Field', 'UpdateName', 'infide.codlote')
DBSetProp('VRENTREND.codlote', 'Field', 'DataType', "N(4)")
* Propiedades para VRENTREND.nomlote field.
DBSetProp('VRENTREND.nomlote', 'Field', 'KeyField', .F.)
DBSetProp('VRENTREND.nomlote', 'Field', 'Updatable', .T.)
DBSetProp('VRENTREND.nomlote', 'Field', 'UpdateName', 'sacfadb!lotesfin.nomlote')
DBSetProp('VRENTREND.nomlote', 'Field', 'DataType', "C(20)")
* Propiedades para VRENTREND.area field.
DBSetProp('VRENTREND.area', 'Field', 'KeyField', .F.)
DBSetProp('VRENTREND.area', 'Field', 'Updatable', .T.)
DBSetProp('VRENTREND.area', 'Field', 'UpdateName', 'sacfadb!lotesfin.area')
DBSetProp('VRENTREND.area', 'Field', 'DataType', "N(8,2)")
* Propiedades para VRENTREND.ano field.
DBSetProp('VRENTREND.ano', 'Field', 'KeyField', .F.)
DBSetProp('VRENTREND.ano', 'Field', 'Updatable', .F.)
DBSetProp('VRENTREND.ano', 'Field', 'UpdateName', 'infide.ano')
DBSetProp('VRENTREND.ano', 'Field', 'DataType', "N(2)")
* Propiedades para VRENTREND.semestre field.
DBSetProp('VRENTREND.semestre', 'Field', 'KeyField', .F.)
DBSetProp('VRENTREND.semestre', 'Field', 'Updatable', .F.)
DBSetProp('VRENTREND.semestre', 'Field', 'UpdateName', 'infide.semestre')
DBSetProp('VRENTREND.semestre', 'Field', 'DataType', "N(1)")
* Propiedades para VRENTREND.tirsc field.
DBSetProp('VRENTREND.tirsc', 'Field', 'KeyField', .F.)
DBSetProp('VRENTREND.tirsc', 'Field', 'Updatable', .F.)
DBSetProp('VRENTREND.tirsc', 'Field', 'UpdateName', 'infide.tirsc')
DBSetProp('VRENTREND.tirsc', 'Field', 'DataType', "N(16,4)")
* Propiedades para VRENTREND.ton field.
DBSetProp('VRENTREND.ton', 'Field', 'KeyField', .F.)
DBSetProp('VRENTREND.ton', 'Field', 'Updatable', .F.)
DBSetProp('VRENTREND.ton', 'Field', 'UpdateName', '')
DBSetProp('VRENTREND.ton', 'Field', 'DataType', "N(19,4)")
***************** Ver la configuración para VISVENTAS ***************

CREATE SQL VIEW "VISVENTAS" ; 
   AS SELECT * FROM sacfadb!ventas

DBSetProp('VISVENTAS', 'View', 'UpdateType', 1)
DBSetProp('VISVENTAS', 'View', 'WhereType', 3)
DBSetProp('VISVENTAS', 'View', 'FetchMemo', .T.)
DBSetProp('VISVENTAS', 'View', 'SendUpdates', .F.)
DBSetProp('VISVENTAS', 'View', 'UseMemoSize', 255)
DBSetProp('VISVENTAS', 'View', 'FetchSize', 100)
DBSetProp('VISVENTAS', 'View', 'MaxRecords', -1)
DBSetProp('VISVENTAS', 'View', 'Tables', 'sacfadb!ventas')
DBSetProp('VISVENTAS', 'View', 'Prepared', .F.)
DBSetProp('VISVENTAS', 'View', 'CompareMemo', .T.)
DBSetProp('VISVENTAS', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VISVENTAS', 'View', 'FetchSize', 100)
DBSetProp('VISVENTAS', 'View', 'Comment', "")
DBSetProp('VISVENTAS', 'View', 'BatchUpdateCount', 1)
DBSetProp('VISVENTAS', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VISVENTAS
* Propiedades para VISVENTAS.numcontrol field.
DBSetProp('VISVENTAS.numcontrol', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.numcontrol', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.numcontrol', 'Field', 'UpdateName', 'sacfadb!ventas.numcontrol')
DBSetProp('VISVENTAS.numcontrol', 'Field', 'DataType', "N(4)")
* Propiedades para VISVENTAS.valventa field.
DBSetProp('VISVENTAS.valventa', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.valventa', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.valventa', 'Field', 'UpdateName', 'sacfadb!ventas.valventa')
DBSetProp('VISVENTAS.valventa', 'Field', 'DataType', "N(14,3)")
* Propiedades para VISVENTAS.tonels field.
DBSetProp('VISVENTAS.tonels', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.tonels', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.tonels', 'Field', 'UpdateName', 'sacfadb!ventas.tonels')
DBSetProp('VISVENTAS.tonels', 'Field', 'DataType', "N(7,3)")
* Propiedades para VISVENTAS.nombcomp field.
DBSetProp('VISVENTAS.nombcomp', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.nombcomp', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.nombcomp', 'Field', 'UpdateName', 'sacfadb!ventas.nombcomp')
DBSetProp('VISVENTAS.nombcomp', 'Field', 'DataType', "C(25)")
* Propiedades para VISVENTAS.nit field.
DBSetProp('VISVENTAS.nit', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.nit', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.nit', 'Field', 'UpdateName', 'sacfadb!ventas.nit')
DBSetProp('VISVENTAS.nit', 'Field', 'DataType', "C(15)")
* Propiedades para VISVENTAS.codprov field.
DBSetProp('VISVENTAS.codprov', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.codprov', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.codprov', 'Field', 'UpdateName', 'sacfadb!ventas.codprov')
DBSetProp('VISVENTAS.codprov', 'Field', 'DataType', "N(4)")
* Propiedades para VISVENTAS.reten field.
DBSetProp('VISVENTAS.reten', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.reten', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.reten', 'Field', 'UpdateName', 'sacfadb!ventas.reten')
DBSetProp('VISVENTAS.reten', 'Field', 'DataType', "N(5,3)")
* Propiedades para VISVENTAS.cuota field.
DBSetProp('VISVENTAS.cuota', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.cuota', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.cuota', 'Field', 'UpdateName', 'sacfadb!ventas.cuota')
DBSetProp('VISVENTAS.cuota', 'Field', 'DataType', "N(5,3)")
* Propiedades para VISVENTAS.otros field.
DBSetProp('VISVENTAS.otros', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.otros', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.otros', 'Field', 'UpdateName', 'sacfadb!ventas.otros')
DBSetProp('VISVENTAS.otros', 'Field', 'DataType', "N(5,3)")
* Propiedades para VISVENTAS.codlote field.
DBSetProp('VISVENTAS.codlote', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.codlote', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.codlote', 'Field', 'UpdateName', 'sacfadb!ventas.codlote')
DBSetProp('VISVENTAS.codlote', 'Field', 'DataType', "N(4)")
* Propiedades para VISVENTAS.codcp field.
DBSetProp('VISVENTAS.codcp', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.codcp', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.codcp', 'Field', 'UpdateName', 'sacfadb!ventas.codcp')
DBSetProp('VISVENTAS.codcp', 'Field', 'DataType', "N(10)")
* Propiedades para VISVENTAS.fecha field.
DBSetProp('VISVENTAS.fecha', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.fecha', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.fecha', 'Field', 'UpdateName', 'sacfadb!ventas.fecha')
DBSetProp('VISVENTAS.fecha', 'Field', 'DataType', "D")
* Propiedades para VISVENTAS.numfactu field.
DBSetProp('VISVENTAS.numfactu', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.numfactu', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.numfactu', 'Field', 'UpdateName', 'sacfadb!ventas.numfactu')
DBSetProp('VISVENTAS.numfactu', 'Field', 'DataType', "C(10)")
* Propiedades para VISVENTAS.porcentaje field.
DBSetProp('VISVENTAS.porcentaje', 'Field', 'KeyField', .F.)
DBSetProp('VISVENTAS.porcentaje', 'Field', 'Updatable', .T.)
DBSetProp('VISVENTAS.porcentaje', 'Field', 'UpdateName', 'sacfadb!ventas.porcentaje')
DBSetProp('VISVENTAS.porcentaje', 'Field', 'DataType', "N(3)")
***************** Ver la configuración para VISAPL1 ***************

CREATE SQL VIEW "VISAPL1" ; 
   AS SELECT Regfin2.codaplic, Regfin2.fecaplic, Regfin2.dias,  Regfin2.hectaplic, Regfin2.codinsum, Regfin2.codtipo, Regfin2.cantusada,  Regfin2.abrenom, Regfin2.tipins, Regfin2.codunidad,  sum(Regfin2.vtot) as  vtot, sum(Regfin2.valplic) as valplic,  sum(Regfin2.valtaplic) as valtaplic FROM regfin2 GROUP BY Regfin2.fecaplic, Regfin2.codinsum ORDER BY Regfin2.fecaplic

DBSetProp('VISAPL1', 'View', 'UpdateType', 1)
DBSetProp('VISAPL1', 'View', 'WhereType', 3)
DBSetProp('VISAPL1', 'View', 'FetchMemo', .T.)
DBSetProp('VISAPL1', 'View', 'SendUpdates', .F.)
DBSetProp('VISAPL1', 'View', 'UseMemoSize', 255)
DBSetProp('VISAPL1', 'View', 'FetchSize', 100)
DBSetProp('VISAPL1', 'View', 'MaxRecords', -1)
DBSetProp('VISAPL1', 'View', 'Tables', '')
DBSetProp('VISAPL1', 'View', 'Prepared', .F.)
DBSetProp('VISAPL1', 'View', 'CompareMemo', .T.)
DBSetProp('VISAPL1', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VISAPL1', 'View', 'FetchSize', 100)
DBSetProp('VISAPL1', 'View', 'Comment', "")
DBSetProp('VISAPL1', 'View', 'BatchUpdateCount', 1)
DBSetProp('VISAPL1', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VISAPL1
* Propiedades para VISAPL1.codaplic field.
DBSetProp('VISAPL1.codaplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.codaplic', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.codaplic', 'Field', 'UpdateName', 'regfin2.codaplic')
DBSetProp('VISAPL1.codaplic', 'Field', 'DataType', "C(8)")
* Propiedades para VISAPL1.fecaplic field.
DBSetProp('VISAPL1.fecaplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.fecaplic', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.fecaplic', 'Field', 'UpdateName', 'regfin2.fecaplic')
DBSetProp('VISAPL1.fecaplic', 'Field', 'DataType', "D")
* Propiedades para VISAPL1.dias field.
DBSetProp('VISAPL1.dias', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.dias', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.dias', 'Field', 'UpdateName', 'regfin2.dias')
DBSetProp('VISAPL1.dias', 'Field', 'DataType', "N(10)")
* Propiedades para VISAPL1.hectaplic field.
DBSetProp('VISAPL1.hectaplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.hectaplic', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.hectaplic', 'Field', 'UpdateName', 'regfin2.hectaplic')
DBSetProp('VISAPL1.hectaplic', 'Field', 'DataType', "N(8,2)")
* Propiedades para VISAPL1.codinsum field.
DBSetProp('VISAPL1.codinsum', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.codinsum', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.codinsum', 'Field', 'UpdateName', 'regfin2.codinsum')
DBSetProp('VISAPL1.codinsum', 'Field', 'DataType', "N(4)")
* Propiedades para VISAPL1.codtipo field.
DBSetProp('VISAPL1.codtipo', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.codtipo', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.codtipo', 'Field', 'UpdateName', 'regfin2.codtipo')
DBSetProp('VISAPL1.codtipo', 'Field', 'DataType', "N(4)")
* Propiedades para VISAPL1.cantusada field.
DBSetProp('VISAPL1.cantusada', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.cantusada', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.cantusada', 'Field', 'UpdateName', 'regfin2.cantusada')
DBSetProp('VISAPL1.cantusada', 'Field', 'DataType', "N(14,5)")
* Propiedades para VISAPL1.abrenom field.
DBSetProp('VISAPL1.abrenom', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.abrenom', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.abrenom', 'Field', 'UpdateName', 'regfin2.abrenom')
DBSetProp('VISAPL1.abrenom', 'Field', 'DataType', "C(10)")
* Propiedades para VISAPL1.tipins field.
DBSetProp('VISAPL1.tipins', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.tipins', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.tipins', 'Field', 'UpdateName', 'regfin2.tipins')
DBSetProp('VISAPL1.tipins', 'Field', 'DataType', "C(25)")
* Propiedades para VISAPL1.codunidad field.
DBSetProp('VISAPL1.codunidad', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.codunidad', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.codunidad', 'Field', 'UpdateName', 'regfin2.codunidad')
DBSetProp('VISAPL1.codunidad', 'Field', 'DataType', "N(1)")
* Propiedades para VISAPL1.vtot field.
DBSetProp('VISAPL1.vtot', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.vtot', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.vtot', 'Field', 'UpdateName', '')
DBSetProp('VISAPL1.vtot', 'Field', 'DataType', "N(20)")
* Propiedades para VISAPL1.valplic field.
DBSetProp('VISAPL1.valplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.valplic', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.valplic', 'Field', 'UpdateName', '')
DBSetProp('VISAPL1.valplic', 'Field', 'DataType', "N(16)")
* Propiedades para VISAPL1.valtaplic field.
DBSetProp('VISAPL1.valtaplic', 'Field', 'KeyField', .F.)
DBSetProp('VISAPL1.valtaplic', 'Field', 'Updatable', .F.)
DBSetProp('VISAPL1.valtaplic', 'Field', 'UpdateName', '')
DBSetProp('VISAPL1.valtaplic', 'Field', 'DataType', "N(16)")
***************** Ver la configuración para VIS_AREA ***************

CREATE SQL VIEW "VIS_AREA" ; 
   AS SELECT Planilla.*, Nomina.formpago, Nomina.area FROM  sacfadb!planilla INNER JOIN sacfadb!nomina    ON  Planilla.codempl = Nomina.codempl ORDER BY Nomina.area

DBSetProp('VIS_AREA', 'View', 'UpdateType', 1)
DBSetProp('VIS_AREA', 'View', 'WhereType', 3)
DBSetProp('VIS_AREA', 'View', 'FetchMemo', .T.)
DBSetProp('VIS_AREA', 'View', 'SendUpdates', .F.)
DBSetProp('VIS_AREA', 'View', 'UseMemoSize', 255)
DBSetProp('VIS_AREA', 'View', 'FetchSize', 100)
DBSetProp('VIS_AREA', 'View', 'MaxRecords', -1)
DBSetProp('VIS_AREA', 'View', 'Tables', 'sacfadb!planilla,sacfadb!nomina')
DBSetProp('VIS_AREA', 'View', 'Prepared', .F.)
DBSetProp('VIS_AREA', 'View', 'CompareMemo', .T.)
DBSetProp('VIS_AREA', 'View', 'FetchAsNeeded', .F.)
DBSetProp('VIS_AREA', 'View', 'FetchSize', 100)
DBSetProp('VIS_AREA', 'View', 'Comment', "")
DBSetProp('VIS_AREA', 'View', 'BatchUpdateCount', 1)
DBSetProp('VIS_AREA', 'View', 'ShareConnection', .F.)

*!* Propiedades a nivel de campo para VIS_AREA
* Propiedades para VIS_AREA.codigopla field.
DBSetProp('VIS_AREA.codigopla', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.codigopla', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.codigopla', 'Field', 'UpdateName', 'sacfadb!planilla.codigopla')
DBSetProp('VIS_AREA.codigopla', 'Field', 'DataType', "C(20)")
* Propiedades para VIS_AREA.codempl field.
DBSetProp('VIS_AREA.codempl', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.codempl', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.codempl', 'Field', 'UpdateName', 'sacfadb!planilla.codempl')
DBSetProp('VIS_AREA.codempl', 'Field', 'DataType', "N(3)")
* Propiedades para VIS_AREA.nomempl field.
DBSetProp('VIS_AREA.nomempl', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.nomempl', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.nomempl', 'Field', 'UpdateName', 'sacfadb!planilla.nomempl')
DBSetProp('VIS_AREA.nomempl', 'Field', 'DataType', "C(20)")
* Propiedades para VIS_AREA.sueldodia field.
DBSetProp('VIS_AREA.sueldodia', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.sueldodia', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.sueldodia', 'Field', 'UpdateName', 'sacfadb!planilla.sueldodia')
DBSetProp('VIS_AREA.sueldodia', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.diastraba field.
DBSetProp('VIS_AREA.diastraba', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.diastraba', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.diastraba', 'Field', 'UpdateName', 'sacfadb!planilla.diastraba')
DBSetProp('VIS_AREA.diastraba', 'Field', 'DataType', "N(2)")
* Propiedades para VIS_AREA.stranspo field.
DBSetProp('VIS_AREA.stranspo', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.stranspo', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.stranspo', 'Field', 'UpdateName', 'sacfadb!planilla.stranspo')
DBSetProp('VIS_AREA.stranspo', 'Field', 'DataType', "N(10)")
* Propiedades para VIS_AREA.saliment field.
DBSetProp('VIS_AREA.saliment', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.saliment', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.saliment', 'Field', 'UpdateName', 'sacfadb!planilla.saliment')
DBSetProp('VIS_AREA.saliment', 'Field', 'DataType', "N(10)")
* Propiedades para VIS_AREA.hediurna field.
DBSetProp('VIS_AREA.hediurna', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.hediurna', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.hediurna', 'Field', 'UpdateName', 'sacfadb!planilla.hediurna')
DBSetProp('VIS_AREA.hediurna', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.rnoctur field.
DBSetProp('VIS_AREA.rnoctur', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.rnoctur', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.rnoctur', 'Field', 'UpdateName', 'sacfadb!planilla.rnoctur')
DBSetProp('VIS_AREA.rnoctur', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.rfestivo field.
DBSetProp('VIS_AREA.rfestivo', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.rfestivo', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.rfestivo', 'Field', 'UpdateName', 'sacfadb!planilla.rfestivo')
DBSetProp('VIS_AREA.rfestivo', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.henoctur field.
DBSetProp('VIS_AREA.henoctur', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.henoctur', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.henoctur', 'Field', 'UpdateName', 'sacfadb!planilla.henoctur')
DBSetProp('VIS_AREA.henoctur', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.hfdiurna field.
DBSetProp('VIS_AREA.hfdiurna', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.hfdiurna', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.hfdiurna', 'Field', 'UpdateName', 'sacfadb!planilla.hfdiurna')
DBSetProp('VIS_AREA.hfdiurna', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.hfnoctur field.
DBSetProp('VIS_AREA.hfnoctur', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.hfnoctur', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.hfnoctur', 'Field', 'UpdateName', 'sacfadb!planilla.hfnoctur')
DBSetProp('VIS_AREA.hfnoctur', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.otrosdev field.
DBSetProp('VIS_AREA.otrosdev', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.otrosdev', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.otrosdev', 'Field', 'UpdateName', 'sacfadb!planilla.otrosdev')
DBSetProp('VIS_AREA.otrosdev', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.salud field.
DBSetProp('VIS_AREA.salud', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.salud', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.salud', 'Field', 'UpdateName', 'sacfadb!planilla.salud')
DBSetProp('VIS_AREA.salud', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.pension field.
DBSetProp('VIS_AREA.pension', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.pension', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.pension', 'Field', 'UpdateName', 'sacfadb!planilla.pension')
DBSetProp('VIS_AREA.pension', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.solida field.
DBSetProp('VIS_AREA.solida', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.solida', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.solida', 'Field', 'UpdateName', 'sacfadb!planilla.solida')
DBSetProp('VIS_AREA.solida', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.otrosdesc field.
DBSetProp('VIS_AREA.otrosdesc', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.otrosdesc', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.otrosdesc', 'Field', 'UpdateName', 'sacfadb!planilla.otrosdesc')
DBSetProp('VIS_AREA.otrosdesc', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.retencion field.
DBSetProp('VIS_AREA.retencion', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.retencion', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.retencion', 'Field', 'UpdateName', 'sacfadb!planilla.retencion')
DBSetProp('VIS_AREA.retencion', 'Field', 'DataType', "N(18,5)")
* Propiedades para VIS_AREA.formpago field.
DBSetProp('VIS_AREA.formpago', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.formpago', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.formpago', 'Field', 'UpdateName', 'sacfadb!nomina.formpago')
DBSetProp('VIS_AREA.formpago', 'Field', 'DataType', "N(1)")
* Propiedades para VIS_AREA.area field.
DBSetProp('VIS_AREA.area', 'Field', 'KeyField', .F.)
DBSetProp('VIS_AREA.area', 'Field', 'Updatable', .T.)
DBSetProp('VIS_AREA.area', 'Field', 'UpdateName', 'sacfadb!nomina.area')
DBSetProp('VIS_AREA.area', 'Field', 'DataType', "C(14)")
*************** Inicio del establecimiento de relaciones **************
ALTER TABLE 'LABCULTI' ADD FOREIGN KEY TAG CODLABOR REFERENCES LABOR TAG CODLABOR
ALTER TABLE 'INFCULTI' ADD FOREIGN KEY TAG CODCULT REFERENCES CULTIVO TAG CODCULTIVO
ALTER TABLE 'JORCULTI' ADD FOREIGN KEY TAG CODPROV REFERENCES IPROV TAG CODPROV
ALTER TABLE 'INSUMO' ADD FOREIGN KEY TAG CODTIPO REFERENCES TIPINSU TAG CODTIPO
ALTER TABLE 'MAQUINAS' ADD FOREIGN KEY TAG CODTMAQ REFERENCES MAQTIPO TAG CODTMAQ
ALTER TABLE 'COMPINS' ADD FOREIGN KEY TAG CODINSUM REFERENCES INSUMO TAG CODINSUM
ALTER TABLE 'COMPINS' ADD FOREIGN KEY TAG CODUNID REFERENCES UNDCOMP TAG CODIGO
ALTER TABLE 'UFC' ADD FOREIGN KEY TAG CODINSUM REFERENCES INSUMO TAG CODINSUM
ALTER TABLE 'UFC' ADD FOREIGN KEY TAG CODUNID REFERENCES UNDCOMP TAG CODIGO
ALTER TABLE 'MAQMANT' ADD FOREIGN KEY TAG CODMAQ REFERENCES MAQUINAS TAG CODMAQ
ALTER TABLE 'DCREDITO' ADD FOREIGN KEY TAG CODCRED REFERENCES CREDITO TAG CODCRED
ALTER TABLE 'UNDCOMP' ADD FOREIGN KEY TAG CODUNIDAD REFERENCES UNDMEDIS TAG CODUNIDAD
ALTER TABLE 'COMBUST' ADD FOREIGN KEY TAG CODMAQ REFERENCES MAQUINAS TAG CODMAQ
ALTER TABLE 'CPD' ADD FOREIGN KEY TAG CODCP REFERENCES CPG TAG CODCP
ALTER TABLE 'PROCION' ADD FOREIGN KEY TAG NUMCONTROL REFERENCES INFCULTI TAG NUMCONTROL
ALTER TABLE 'INFCULTI' ADD FOREIGN KEY TAG CODLOTE REFERENCES LOTESFIN TAG CODLOTE
ALTER TABLE 'JORCULTI' ADD FOREIGN KEY TAG CODLABOR REFERENCES LABOR TAG CODLABOR
ALTER TABLE 'VENTAS' ADD FOREIGN KEY TAG CODPROV REFERENCES IPROV TAG CODPROV
ALTER TABLE 'MAQMANT' ADD FOREIGN KEY TAG CODPROV REFERENCES IPROV TAG CODPROV
ALTER TABLE 'LABCULTI' ADD FOREIGN KEY TAG CODPROV REFERENCES IPROV TAG CODPROV
ALTER TABLE 'LOTESFIN' ADD FOREIGN KEY TAG CODFIN REFERENCES UBICA TAG CODFIN
ALTER TABLE 'CREDITO' ADD FOREIGN KEY TAG CODFIN REFERENCES UBICA TAG CODFIN
ALTER TABLE 'FACINS' ADD FOREIGN KEY TAG CODFIN REFERENCES UBICA TAG CODFIN
ALTER TABLE 'INVENINI' ADD FOREIGN KEY TAG CODFIN REFERENCES UBICA TAG CODFIN
ALTER TABLE 'MAQUINAS' ADD FOREIGN KEY TAG CODFIN REFERENCES UBICA TAG CODFIN
ALTER TABLE 'UFC' ADD FOREIGN KEY TAG CODFIN REFERENCES UBICA TAG CODFIN
ALTER TABLE 'METEREO' ADD FOREIGN KEY TAG CODFIN REFERENCES UBICA TAG CODFIN
ALTER TABLE 'MAQCULTI' ADD FOREIGN KEY TAG CODLABOR REFERENCES LABOR TAG CODLABOR
ALTER TABLE 'MAQCULTI' ADD FOREIGN KEY TAG CODMAQ REFERENCES MAQUINAS TAG CODMAQ
ALTER TABLE 'AUXCOMB' ADD FOREIGN KEY TAG CODUFC REFERENCES COMBUST TAG CODUFC
ALTER TABLE 'AUXUFC' ADD FOREIGN KEY TAG CODUFC REFERENCES UFC TAG CODUFC
ALTER TABLE 'AUXAPLI' ADD FOREIGN KEY TAG CODINSAP REFERENCES INSAPLICA TAG CODINSAP
***** Configuración de integridad referencial *****
CLOSE DATABASE
USE 'SACFADB.DBC'
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'labculti'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codlabor'$Property AND 'labor'$Property AND 'codlabor'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'infculti'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codcult'$Property AND 'cultivo'$Property AND 'codcultivo'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'jorculti'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codprov'$Property AND 'iprov'$Property AND 'codprov'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'insumo'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codtipo'$Property AND 'tipinsu'$Property AND 'codtipo'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'maquinas'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codtmaq'$Property AND 'maqtipo'$Property AND 'codtmaq'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'compins'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codinsum'$Property AND 'insumo'$Property AND 'codinsum'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'compins'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codunid'$Property AND 'undcomp'$Property AND 'codigo'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'ufc'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codinsum'$Property AND 'insumo'$Property AND 'codinsum'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'ufc'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codunid'$Property AND 'undcomp'$Property AND 'codigo'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'maqmant'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codmaq'$Property AND 'maquinas'$Property AND 'codmaq'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'dcredito'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codcred'$Property AND 'credito'$Property AND 'codcred'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'undcomp'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codunidad'$Property AND 'undmedis'$Property AND 'codunidad'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'combust'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codmaq'$Property AND 'maquinas'$Property AND 'codmaq'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'cpd'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codcp'$Property AND 'cpg'$Property AND 'codcp'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCI   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'procion'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'numcontrol'$Property AND 'infculti'$Property AND 'numcontrol'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'infculti'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codlote'$Property AND 'lotesfin'$Property AND 'codlote'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'jorculti'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codlabor'$Property AND 'labor'$Property AND 'codlabor'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'ventas'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codprov'$Property AND 'iprov'$Property AND 'codprov'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'maqmant'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codprov'$Property AND 'iprov'$Property AND 'codprov'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'labculti'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codprov'$Property AND 'iprov'$Property AND 'codprov'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'lotesfin'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codfin'$Property AND 'ubica'$Property AND 'codfin'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'credito'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codfin'$Property AND 'ubica'$Property AND 'codfin'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'facins'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codfin'$Property AND 'ubica'$Property AND 'codfin'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'invenini'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codfin'$Property AND 'ubica'$Property AND 'codfin'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'maquinas'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codfin'$Property AND 'ubica'$Property AND 'codfin'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'ufc'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codfin'$Property AND 'ubica'$Property AND 'codfin'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'metereo'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codfin'$Property AND 'ubica'$Property AND 'codfin'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'maqculti'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codlabor'$Property AND 'labor'$Property AND 'codlabor'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'maqculti'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codmaq'$Property AND 'maquinas'$Property AND 'codmaq'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCR   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'auxcomb'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codufc'$Property AND 'combust'$Property AND 'codufc'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'ICI   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'auxufc'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codufc'$Property AND 'ufc'$Property AND 'codufc'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'ICI   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
LOCATE FOR ObjectType = 'Table' AND ObjectName = 'auxapli'
IF FOUND()
    nObjectID = ObjectID
    LOCATE FOR ObjectType = 'Relation' AND 'codinsap'$Property AND 'insaplica'$Property AND 'codinsap'$Property AND ParentID = nObjectID
    IF FOUND()
	      REPLACE RiInfo WITH 'CCI   '
    ELSE
       ? "No se pudo establecer información de IR."
    ENDIF
ENDIF
USE

