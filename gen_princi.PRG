SET DEFAULT TO CURDIR()
SET LIBRARY TO Cipher50.fll ADDITIVE
DECLARE _fpreset IN msvcrt20.DLL



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

*DO FORM for_ge_conta_1.scx
DO FORM for_ge_conta.scx
*DO FORM for_ge.scx
READ EVENTS