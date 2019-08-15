v_file = sys(5)+sys(2003)+"\copias\base.zip"
v_destino = sys(5)+sys(2003)
IF file(v_file) and file(sys(5)+sys(2003)+"\descom32.dll")
	v_si = aunzip(.t.,v_file,v_destino,"g*.*","DOA")
	
	USar ("lotesfin",.t.)
	USar ("infculti",.t.)
	USar ("insaplica",.t.)
*********corrige aplica
	SELECT cantotal/area as cantaplic1,codinsap from insaplica inner join infculti inner join lotesfin on ;
		insaplica.numcontrol=infculti.numcontrol on infculti.codlote=lotesfin.codlote ;
		where cantaplic>9999 order by codinsap into cursor c_insaplic
	SELECT c_insaplic
	GO top
	SCAN
		UPDATE insaplica set cantaplic=c_insaplic.cantaplic1 where cantaplic>9999 and insaplica.codinsap =c_insaplic.codinsap
	ENDSCAN
	BEGIN transaction
	select infculti
	SCAN
		vnumcontrol =infculti.numcontrol
		SELECT COUNT (*) FROM Labculti WHERE numcontrol = vnumcontrol  ;
			INTO ARRAY vcantidad1
		SELECT COUNT (*) FROM jorculti WHERE numcontrol = vnumcontrol  ;
			INTO ARRAY vcantidad2
		
		
		SELECT COUNT (*) FROM Aplica  WHERE numcontrol = vnumcontrol ;
			INTO ARRAY vcantidad4
		SELECT COUNT (*) FROM ventas WHERE numcontrol =vnumcontrol ;
			INTO ARRAY vcantidad5
		SELECT COUNT (*) FROM procion WHERE numcontrol =vnumcontrol  ;
			INTO ARRAY vcantidad6

		IF vcantidad1 = 0 AND vcantidad2 = 0 AND vcantidad4 = 0 ;
				AND vcantidad5 = 0 AND vcantidad6 = 0
			
			Delete FROM infculti WHERE numcontrol = vnumcontrol
		ENDIF
	ENDSCAN
	END transaction
	USE in lotesfin
	USE in infculti
	USE in insaplica
	IF v_si>0
		v_si = Messagebox("Se corrigio el archivo de graficos."+chr(13)+"Adicionalmente Desea corregir la base de datos?",36,"Corregir?")
		IF v_si =6
			IF file("sacfa.exe")
*!*					Clear events
*!*					close database
				si = aunzip(.t.,v_file,v_destino,"s*.*","DOA")
				IF si>0
					MESSAGEBOX("Se corrigio la base de datos.",64,"Corregido")
				ENDIF
*!*					QUIT
			ENDIF

		ENDIF

	ENDIF
ELSE
	MESSAGEBOX("No se encontro el archivo",64,"Falta Archivo")
ENDIF


