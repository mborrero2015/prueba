******actualizacion tabla fincas


v_ruta1=SYS(5)+CURDIR()
v_ruta=v_ruta1+"update\"
v_archivo=v_ruta+"actualiza.zip"

IF !directory(v_ruta)
	mkdir(v_ruta)
endif
v_sentence= "actualiza.vbs"+" " +v_archivo

oShell = CreateObject("WScript.Shell")
oShell.Run(v_sentence,10,.t.)

IF FILE(v_archivo)

	v_si = aunzip(.t.,v_archivo,v_ruta1,"*.*","DOA")

	IF v_si >0
		MESSAGEBOX("Actualizacion Tabla insumo exitosa."+ chr(13)+ "Descompresos: "+ alltrim(str(v_si)), 48, "Exito")
	ELSE
		MESSAGEBOX("Error en tabla insumo nacional, ejecute nuevamente la actualización", 48, "Exito")
	ENDIF
ENDIF

****actualizacion ejecutable
v_archivo2=v_ruta+"update1.zip"
v_sentence2="update_exe.vbs"+" " +v_archivo2
oShell1 = CreateObject("WScript.Shell")
oShell1.Run(v_sentence2,10,.t.)
IF FILE(v_archivo)

	v_si1 = aunzip(.t.,v_archivo2,v_ruta,"*.*","DOA")

	IF v_si1 >0
		MESSAGEBOX("Actualizacion Ejecutable Descargada. "+ chr(13)+ "Salga del programa y SOBREESCRIBA el archivo "+ v_ruta +"sacfalite.exe" + chr(13)+  "en el directorio: " + v_ruta1+chr(13)+;
			" Descompresos: "+ alltrim(str(v_si1)), 48, "Exito")
	ELSE
		MESSAGEBOX("Error en archivo exe, ejecute nuevamente la actualización", 48, "Exito")
	ENDIF
ENDIF
*! update.vbs
****






