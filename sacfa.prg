PUBLIC g_salir
vcont=.F.
v_adicionar = .F.
_SCREEN.ICON="sacfalite32a.ICO"
IF NOT F_ActivaWin("SACFA L I T E   - Sistema de Administracion de Fincas Arroceras")
	vcont= .T.
ELSE
	CLEAR DLLS
ENDIF
*!*	IF vcont
*!*		IF  NOT F_ActivaWin("SACFA Versión L I T E   - Sistema de Administración de Fincas Arroceras     CONTABLE")
*!*			vcont= .T.
*!*		ELSE
*!*			vcont= .F.
*!*			Clear DLLS
*!*		ENDIF
*!*	ENDIF
DELETE FROM JORCULTI WHERE CODAPLICA NOT IN (SELECT CODAPLIC FROM INSAPLICA) AND (CODLABOR=301 OR CODLABOR=705)
DELETE FROM LABCULTI WHERE CODAPLICA NOT IN (SELECT CODAPLIC FROM INSAPLICA) AND (CODLABOR=301 OR CODLABOR=705)
UPDATE INSAPLICA SET codtipo=VAL(SUBSTR(ALLTRIM(STR(codinsum)),1,1)) where codinsum>1000 AND SUBSTR(ALLTRIM(STR(codinsum)),1,1)!=ALLTRIM(STR(codtipo))
USE IN INSAPLICA
USE IN JORCULTI
USE IN LABCULTI

*!*	IF !FILE("cantprecio.ost")
*!*		IF !USED("insaplica.dbf")
*!*			SELECT 0
*!*			USE INSAPLICA EXCLU IN 0
*!*			ALTER TABLE INSAPLICA ALTER COLUMN precio N(14,5)
*!*		ENDIF
*!*		IF !USED("aplica.dbf")
*!*			USE aplica IN 0
*!*		ENDIF
*!*		SELECT INSAPLICA
*!*		SCAN
*!*			v_aplica=INSAPLICA.CODAPLIC
*!*			IF INSAPLICA.equivale!=1	AND INSAPLICA.equivale!=0
*!*				SELECT hectaplic FROM aplica,INSAPLICA WHERE aplica.CODAPLIC=v_aplica INTO CURSOR c_area
*!*				SELECT INSAPLICA
*!*				REPLACE INSAPLICA.cantotal WITH INSAPLICA.cantaplic*INSAPLICA.equivale*c_area.hectaplic
*!*				REPLACE INSAPLICA.precio WITH INSAPLICA.precio/INSAPLICA.equivale
*!*			ENDIF

*!*		ENDSCAN
*!*		mistake = FCREATE("cantprecio.ost")
*!*		IF mistake < 0
*!*			MESSAGEBOX("Error de Escritura en Disco")
*!*		ELSE
*!*			=FWRITE(mistake,"SE corrigio cantotal en insaplica")
*!*		ENDIF

*!*		USE IN aplica
*!*		USE IN INSAPLICA

*!*	ENDIF
*!*	IF !FILE("precio.ost")
*!*		UPDATE INSAPLICA SET precio=precio*equivale WHERE equivale>1
*!*		mistake = FCREATE("precio.ost")
*!*		IF mistake < 0
*!*			MESSAGEBOX("Error de Escritura en Disco")
*!*		ELSE
*!*			=FWRITE(mistake,"Se corrigio precio para productos que no estan en u. basica")
*!*		ENDIF

*!*	ENDIF
*****Actualizacion 2015******************

IF !FILE("act2015.ost")
	IF !USED("ubica")
		SELECT 0
		USE ubica EXCLU IN 0
*	ALTER table ubica rename COLUMN precio n(14,5)
		ALTER TABLE ubica ADD COLUMN cofinca c(5)
		ALTER TABLE ubica ADD COLUMN codmuni c(5)
		ALTER TABLE ubica ADD COLUMN agronomo c(30)
		ALTER TABLE ubica ADD COLUMN arrendatario c(30)
		ALTER TABLE ubica ADD COLUMN tenencia N(1)
		ALTER TABLE ubica ADD COLUMN vereda c(25)

	ENDIF
	IF !USED("lotesfin")
		SELECT 0
		USE lotesfin EXCLU IN 0
*	ALTER table ubica rename COLUMN precio n(14,5)
		ALTER TABLE lotesfin ADD COLUMN cofinca c(5)
		ALTER TABLE lotesfin ADD COLUMN finca N(1)

	ENDIF
	IF !USED("insumo")
		SELECT 0
		USE insumo EXCLU IN 0
		ALTER TABLE insumo ADD COLUMN codins N(4)
	ENDIF

*!*		OPEN DATABASE sacfadb
*!*		ADD TABLE sistema
	mistake = FCREATE("act2015.ost")
	IF mistake < 0
		MESSAGEBOX("Error de Escritura en Disco")
	ELSE
		=FWRITE(mistake,"Se corrigio la estructura de insumos y fincas")
	ENDIF

ENDIF

*****Actualizacion 2015a******************
IF !FILE("act2015a.ost")

	IF !USED("infculti")
		SELECT 0
		USE infculti EXCLU IN 0
		ALTER TABLE infculti ADD COLUMN cofinca c(5)
	ENDIF
	IF !USED("insaplica")
		SELECT 0
		USE INSAPLICA EXCLU IN 0
		ALTER TABLE INSAPLICA ADD COLUMN cofinca c(5)
	ENDIF
	IF !USED("ventas")
		SELECT 0
		USE ventas EXCLU IN 0
		ALTER TABLE ventas ADD COLUMN cofinca c(5)
	ENDIF
	IF !USED("jorculti")
		SELECT 0
		USE JORCULTI EXCLU IN 0
		ALTER TABLE JORCULTI ADD COLUMN cofinca c(5)
	ENDIF
	IF !USED("labculti")
		SELECT 0
		USE LABCULTI EXCLU IN 0
		ALTER TABLE LABCULTI ADD COLUMN cofinca c(5)
	ENDIF
	IF !USED("procion")
		SELECT 0
		USE procion EXCLU IN 0
		ALTER TABLE procion ADD COLUMN cofinca c(5)
	ENDIF
	IF !USED("aplica")
		SELECT 0
		USE aplica EXCLU IN 0
		ALTER TABLE aplica ADD COLUMN cofinca c(5)
	ENDIF
	mistake = FCREATE("act2015a.ost")
	IF mistake < 0
		MESSAGEBOX("Error de Escritura en Disco")
	ELSE
		=FWRITE(mistake,"Se corrigio la estructura de insumos y fincas")
	ENDIF

ENDIF
******Actualiza 3 gastos generales y credito
IF !FILE("act2015b.ost")

	IF !USED("gast_gene")
		SELECT 0
		USE gast_gene EXCLU IN 0
		ALTER TABLE gast_gene ADD COLUMN cofinca c(5)
	ENDIF
	IF !USED("dcredito")
		SELECT 0
		USE dcredito EXCLU IN 0
		ALTER TABLE dcredito ADD COLUMN cofinca c(5)
	ENDIF


	mistake = FCREATE("act2015b.ost")
	IF mistake < 0
		MESSAGEBOX("Error de Escritura en Disco")
	ELSE
		=FWRITE(mistake,"Se corrigio la estructura de insumos y fincas para gastos generales y credito")
	ENDIF

ENDIF
**************************

******Actualiza 3 gastos generales y credito
IF !FILE("act2015c.ost")

	IF !USED("infculti")
		SELECT 0
		USE infculti EXCLU IN 0

		ALTER TABLE infculti alter COLUMN asistencia n(10,5)
		ALTER TABLE infculti alter COLUMN ARRIENDO n(10,5)
		ALTER TABLE infculti alter COLUMN PREPAR n(10,5)
		ALTER TABLE infculti alter COLUMN SIEMBRA n(10,5)
		ALTER TABLE infculti alter COLUMN SEMILLA n(10,5)
		ALTER TABLE infculti alter COLUMN LABSIEM n(10,5)
		ALTER TABLE infculti alter COLUMN RIEGO  n(10,5)
		ALTER TABLE infculti alter COLUMN AGUA n(10,5)
		ALTER TABLE infculti alter COLUMN MMTO n(10,5)
		ALTER TABLE infculti alter COLUMN FERTIL n(10,5)
		ALTER TABLE infculti alter COLUMN INSFERT n(10,5)
		ALTER TABLE infculti alter COLUMN APFERT n(10,5)
		ALTER TABLE infculti alter COLUMN MALEZAS n(10,5)
		ALTER TABLE infculti alter COLUMN HERBIC n(10,5)
		ALTER TABLE infculti alter COLUMN APHERB n(10,5)
		ALTER TABLE infculti alter COLUMN MANHERB n(10,5)
		ALTER TABLE infculti alter COLUMN PLAGAS n(10,5)
		ALTER TABLE infculti alter COLUMN INSECT n(10,5)
		ALTER TABLE infculti alter COLUMN APINSECT n(10,5)
		ALTER TABLE infculti alter COLUMN MANINSECT n(10,5)
		ALTER TABLE infculti alter COLUMN ENFERM n(10,5)
		ALTER TABLE infculti alter COLUMN FUNGIC n(10,5)
		ALTER TABLE infculti alter COLUMN APFUNGIC n(10,5)
		ALTER TABLE infculti alter COLUMN TOTINS n(10,5)
		ALTER TABLE infculti alter COLUMN INSUMOS n(10,5)
		ALTER TABLE infculti alter COLUMN APINSUM n(10,5)
		ALTER TABLE infculti alter COLUMN RECOLEC n(10,5)
		ALTER TABLE infculti alter COLUMN TRANSPORTE n(10,5)
		ALTER TABLE infculti alter COLUMN VARIOS n(10,5)
		ALTER TABLE infculti alter COLUMN SUBTOTAL n(10,5)
		ALTER TABLE infculti alter COLUMN ADMON n(10,5)
		ALTER TABLE infculti alter COLUMN INTERESES n(10,5)
		ALTER TABLE infculti alter COLUMN IMPUESTOS n(10,5)
		ALTER TABLE infculti alter COLUMN TOTALES n(10,5)

	ENDIF
	IF !USED("jorculti")
		SELECT 0
		USE JORCULTI EXCLU IN 0
		ALTER TABLE JORCULTI alter COLUMN cantjorn n(10,5)
		ALTER TABLE JORCULTI alter COLUMN cunitjorn n(10,5)
	ENDIF

	IF !USED("labculti")
		SELECT 0
		USE LABCULTI EXCLU IN 0
		ALTER TABLE LABCULTI  alter COLUMN hectarea n(10,5)

	ENDIF






	mistake = FCREATE("act2015c.ost")
	IF mistake < 0
		MESSAGEBOX("Error de Escritura en Disco")
	ELSE
		=FWRITE(mistake,"Se corrigio la estructura de infculti")
	ENDIF

ENDIF

********************
IF !FILE("act2015d.ost")
	IF !USED("insaplica.dbf")
		SELECT 0
		USE INSAPLICA IN 0
		
	ENDIF
	IF !USED("aplica.dbf")
		USE aplica IN 0
	ENDIF
	SELECT INSAPLICA
	SCAN FOR INSAPLICA.equivale!=1	
		v_aplica=INSAPLICA.CODAPLIC
		IF INSAPLICA.equivale!=1	AND INSAPLICA.equivale!=0
			SELECT hectaplic FROM aplica,INSAPLICA WHERE aplica.CODAPLIC=v_aplica INTO CURSOR c_area
			SELECT INSAPLICA
			REPLACE INSAPLICA.cantotal WITH INSAPLICA.cantaplic*INSAPLICA.equivale*c_area.hectaplic
		ENDIF
	ENDSCAN
	mistake = FCREATE("act2015d.ost")
	IF mistake < 0
		MESSAGEBOX("Error de Escritura en Disco")
	ELSE
		=FWRITE(mistake,"Se corrigio insaplica para aplicaiones con equivale !=1 en informe de productos")
	ENDIF
endif
**************************

IF FILE("act2015a.ost") AND FILE("act2015.ost") AND FILE("act2015b.ost")
	SELECT * FROM lotesfin WHERE cofinca!=" " INTO CURSOR c_actualizado
	IF _tally>0
		IF !USED("lotesfin")
			SELECT 0
			USE lotesfin EXCLU IN 0
		ENDIF
		Select c_actualizado
		SCAN
			vcofinca=  c_actualizado.cofinca
			vcodlote = c_actualizado.codlote
			vcodfin	= c_actualizado.codfin
*Update LOTESFIN Set nomlote = vnombre, area = varea,finca=vfinca,cofinca=vcofinca ;
WHERE codfin = vcodfin And codlote = vcodlote
			SELECT numcontrol FROM 	infculti WHERE codfin = vcodfin And codlote = vcodlote INTO CURSOR c_infculti
			vnumcontrol=c_infculti.numcontrol
			scan
				UPDATE infculti SET cofinca=vcofinca where 	numcontrol=vnumcontrol
				UPDATE INSAPLICA SET cofinca=vcofinca where numcontrol=vnumcontrol
				UPDATE ventas SET cofinca=vcofinca where 	numcontrol=vnumcontrol
				UPDATE JORCULTI SET cofinca=vcofinca where 	numcontrol=vnumcontrol
				UPDATE LABCULTI SET cofinca=vcofinca where 	numcontrol=vnumcontrol
				UPDATE procion SET cofinca=vcofinca where 	numcontrol=vnumcontrol
				UPDATE aplica SET cofinca=vcofinca where 	numcontrol=vnumcontrol
			ENDSCAN
			UPDATE gast_gene SET cofinca=vcofinca where codfin=vcodfin
			UPDATE dcredito SET cofinca=vcofinca where 	codlote=vcodlote
			Select c_actualizado
		ENDSCAN
		IF USED("infculti")
			USE IN infculti
		ENDIF
		IF USED("insaplica")
			USE IN INSAPLICA
		endif
		IF USED("ventas")
			USE IN ventas
		endif
		IF USED("jorculti")
			USE IN JORCULTI
		endif
		IF USED("labculti")
			USE IN LABCULTI
		endif
		IF USED("procion")
			USE IN procion
		endif
		IF USED("aplica")
			USE IN aplica
		endif

		IF USED("lotesfin")
			USE IN lotesfin
		endif

		IF USED("gast_gene.dbf")
			USE IN gast_gene
		endif
		IF USED("dcredito")
			USE IN dcredito
		endif

	ENDIF

ENDIF


*!*	If !Used("insaplica.dbf")
*!*			SELECT 0
*!*			Use insaplica EXCLU In 0
*!*			DELETE TAG codinsap OF insaplica
*!*	ENDIF



*************************************

*!*			v_asa =sys(5) + sys(2003) +"\comctl32.ocx /s"
*!*			!regsvr32 &v_asa
*!*			mistake = FCREATE("regis.ost")
*!*			IF mistake < 0
*!*				MESSAGEBOX("Error de Escritura en Disco")
*!*			ELSE
*!*				=FWRITE(mistake,"SE REALIZO el registro de comctl32.ocx")
*!*			ENDIF
*!*			=FCLOSE(mistake)
*!*		ENDIF

*!*	ENDIF


*ON ERROR QUIT
IF vcont
	IF !USED("variables.dbf")

* correr aplicación
		SET DEFAULT TO CURDIR()
*    SET LIBRARY TO foxtools.fll additive
		SET LIBRARY TO Cipher50.fll ADDITIVE
		DECLARE _fpreset IN msvcrt20.DLL


***_SCREEN.VISIBLE = .F.


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
		SET ENGINEBEHAVIOR 70
*!*			USE dcredito  EXCLU
*!*			FOR i=1 TO FCOUNT()
*!*				IF UPPER(FIELD(i))="MOTIVO"
*!*					v_adicionar = .T.
*!*				ENDIF
*!*			ENDFOR
*!*			IF !v_adicionar
*!*				ALTER TABLE dcredito ADD COLUMN motivo c(30)
*!*			ENDIF
*!*			USE IN dcredito
*!*			CLOSE DATA
***ACTUALIZAR JORCULTI MAQCULTI LABCULTI

*!*			IF  !file("fixcodcp.ost")
*!*
*!*				USE LABCULTI EXCLU
*!*				IF FSIZE('codcp')=4
*!*					ALTER TABLE LABCULTI ALTER COLUMN codcp n(8)
*!*				ENDIF
*!*				USE in LABCULTI

*!*				USE jorCULTI EXCLU
*!*				IF FSIZE('codcp')=4
*!*					ALTER TABLE jorCULTI ALTER COLUMN codcp n(8)
*!*				ENDIF
*!*				USE in jorCULTI
*!*
*!*				mistake = FCREATE("fixcodcp.ost")
*!*				IF mistake < 0
*!*					MESSAGEBOX("Error de Escritura en Disco")
*!*				ELSE
*!*					=FWRITE(mistake,"SE ACTUALIZA JORCULTI LABCULTI MAQCULTIO A 8 DIGITOS")
*!*				ENDIF
*!*				=FCLOSE(mistake)
*!*				CLOSE data
*!*			ENDIF
**************************
		PUBLIC g_pagi

* Puntero general para ubicar registro cuando
* se pasa de una ventana a otra sin enlace directo
		PUBLIC g_actual

* Codigo de la finca, nombre de la finca, nombre del lote y codigo
		PUBLIC g_codfin, g_nomfin, g_codlote, g_nomlote,gcofinca

* Codigo del insumo, nombre del insumo
		PUBLIC g_codinsum, g_nominsum

* Codigo del tipo de insumo, nombre del tipo de insumo
		PUBLIC  g_codtipo, g_nomtipo

* Codigo unidad de medida, nombre unidad de medida
		PUBLIC g_codundmed, g_nomundmed

* Codigo unidad comparadada, nombre unidad comparada, equivalencia
		PUBLIC g_codundcom, g_nomundcom, g_equivale

* Flag para saber si hay una o varias fincas
* para engranar la parte contable con la basica y saber porque año
* semestre y lote se deb filtra la informacion
		PUBLIC g_numcontrol

* Codigo de la maquina, nombre de la maquina, tipo de maquina
		PUBLIC g_codmaq, g_nommaq, g_codtmaq

* Numero de digitos con que se va a trabajar en la contabilidad
		PUBLIC qdigit,  pomenu, qllama

* Lleva la fecha del computador Año y Semestre
		PUBLIC g_fecactual, g_ano, g_semestre

* para saber si esta creando o modificando
		PUBLIC g_modo, cred

		PUBLIC adela, imagen, vca123, g_maqui11

		PUBLIC g_imple11, g_lote11, g_mant11, g_emple11, g_epuc11
* para itilizar cuando abrimos algo desde fincas

		PUBLIC g_donde, g_codcp, g_codcc, g_nomcc, g_fcodcc, g_numcheq
* g_codcc para manejar los centros de costos y nombres
* g_fcodcc para manejar los centro de costo finca en las aplicaciones
* g_cocp para manejar los comprobantes contables
* g_numcheq par amanejar los numeros de los chueques en los pagos

		PUBLIC salir ,vporc
*vporc    maneja el porcentaje de producion para la venta

		PUBLIC g_nomprov, g_codprov
* Maneja el nombre y codigo de los proveedores

		salir    = .F.
		g_donde  = 1
		pomenu   = ""
		qllama   = ""
		g_codcp  = 0
		vporc    = 0
		g_nomfin = ""
		g_actual = 1
		g_numcontrol = -1
		g_numcheq = " "
************   **************   ************   **************
		g_fecactual = DATE()
		v_adicionar = .F.
		DO FORM primero.scx

		READ EVENTS

	ENDIF
ENDIF
*-----------------------------
FUNCTION F_ActivaWin(cCaption)
*-----------------------------
LOCAL nHWD
DECLARE INTEGER FindWindow IN WIN32API ;
STRING cNULL, ;
STRING cWinName

DECLARE SetForegroundWindow IN WIN32API ;
INTEGER nHandle

DECLARE SetActiveWindow IN WIN32API ;
INTEGER nHandle

DECLARE ShowWindow IN WIN32API ;
INTEGER nHandle, ;
INTEGER nState

nHWD = FindWindow(0, cCaption)
IF nHWD > 0
* VENTANA YA ACTIVA
* LA "LLAMAMOS":
	ShowWindow (nHWD,9)

* LA PONEMOS ENCIMA
	SetForegroundWindow(nHWD)

* LA ACTIVAMOS
	SetActiveWindow(nHWD)
	RETURN .T.
ELSE
* VENTANA NO ACTIVA
	RETURN .F.
ENDIF
