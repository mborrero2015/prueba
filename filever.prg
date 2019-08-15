if file("version.exe")
	! VERSION.EXE
	v_path = sys(5)+sys(2003)+"\"
	gnFileHandle = FOPEN(v_path+"version.txt")
	*gnFileHandle = FOPEN("version.txt")
	nSize =  FSEEK(gnFileHandle, 0, 2)           && Lleva el puntero a EOF
	IF nSize <= 0
* Si el archivo está vacío, mostrar un mensaje de error
		messagebox("No se pudo encontrar la version")
	ELSE
* en memoria, después muestra el texto en la ventana principal de Visual FoxPro
		= FSEEK(gnFileHandle, 4, 0)              && Mover puntero a BOF
		cString = FREAD(gnFileHandle, nSize-4)
		
		if len(cString)=7
			Dia = substr(cString,1,2)
			Mes = substr(cString,3,2)
			Ano=  substr(cString,6,2)
		else
			Dia = "0"+substr(cString,1,1)
			Mes = substr(cString,2,2)
			Ano=  substr(cString,5,2)
		endif
		Messagebox("Version: " + Dia+"-"+Mes+"-20"+Ano,64,"version")
	
	ENDIF
	= FCLOSE(gnFileHandle)
else
	Messagebox("No se actualizo la version falta archivo version.exe",64,"version")
endif
