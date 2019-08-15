v_sii=messagebox("Esta opcion borrará los comprobantes que presenten inconsistencias"+chr(13)+"Por favor realice un backup antes de proceder"+chr(13)+"Desea continuar?",36,"Continuar...")
IF v_sii=6
	Delete from cpd where codcp in (select codcp from cpg or empty(codcp);
		where dbs!=crs or (dbs=0 and crs=0))
	v_cambio =  _tally
	Delete from cpg where dbs!=crs or (dbs=0 and crs=0) or empty(fecha) or empty(codcp)
	v_cambio= v_cambio+_tally
	if v_cambio >0
		messagebox("Actualización realizada",48,"Exito")
	else
		messagebox("No existen comprobantes por actualizar",16,"No existen comprobantes")
	endif
ENDIF
