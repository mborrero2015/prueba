***esta funcionando con planilla pilas no borre el codigo para ver si se puede utilizar

parameter g_actual,qllama,cuareg

do case
case qllama="compins" &&compra de insumos
     ruta  ="compins.usototins"
     eti   ="Comprar Insumo"
     campo ="codfact"
     valor ="facins.codfact"
   	 nuite =1
   	 
case qllama  ="invenini"  &&inventario de apertura
     ruta    ="invenini.usototins"
     eti     ="Agregar Insumo"
     campo   ="codtipo"
	 valor   ="tipinsu.codtipo"
	 nuite   =1
	    
case qllama ="combust"  &&manejo de combustibles y lubricantes
	 conqc  =0
	 ruta   ="conqc"
 	 eti    ="Usar Com/Lub/Rep"
 	 campo  ="Codmaq"
	 valor  ="Maquinas.codmaq"
	 nuite  =1
	    
case qllama ="ufc" &&uso fuera del cultivo
	 conqc  =0
	 ruta   ="conqc"
 	 eti    ="Usar Insumo"    
 	 campo  ="codtipo"
	 valor  ="tipinsu.codtipo"
	 nuite  =1

case qllama ="cpd"  && comprabantes de contabilidad
	 conqc  =0
	 ruta   ="conqc"
 	 eti    ="Nueva Cuenta"    
 	 campo  ="codcp"
	 valor  ="Cpg.codcp"
	 nuite  =1
	 
case qllama ="planicab"  && planilla de nomina
	 conqc  =0
	 ruta   ="planicab.estado"
 	 nuite  =0

endcase
***********************************


SET SHADOW ON
DEFINE POPUP menuizqi ;
  FROM MROW(), MCOL() ;
  MARGIN ;
  SHORTCUT		&& agregar sombra (jd 11/20/99)

DEFINE BAR 1 OF menuizqi PROMPT "Eliminar"
DEFINE BAR 2 OF menuizqi PROMPT "\-"
DEFINE BAR 3 OF menuizqi PROMPT eti

IF CUAREG =0
    SET SKIP OF BAR 1 OF menuizqi .T.
ELSE
    ON SELECTION BAR 1 OF menuizqi DO elimina2
ENDIF
IF NUITE =0
    SET SKIP OF BAR 3 OF menuizqi .T.
ELSE
    ON SELECTION BAR 3 OF menuizqi DO nuevo 
ENDIF

ACTIVATE POPUP menuizqi

RELEASE POPUP menuizqi

************************************************************
PROCEDURE elimina2
  
 IF &ruta=0 or &ruta<>"C" THEN  

	nAnswer = MESSAGEBOX("   Esta Seguro de"+chr(13)+"Eliminar Este Registro",36,"!Elimina¡")

    IF nAnswer = 6
   
       DELETE FROM &qllama WHERE g_actual =recno()
	 
    ENDIF

 ELSE
  
	 MESSAGEBOX("      Este Registro"+chr(13)+"No se puede Eliminar ",64,"!Elimina¡")
 	 go g_actual
 ENDIF
  
 RETURN
 ENDPROC

***********************************************************
PROCEDURE nuevo

ta =(qllama+".dbf")

insert into &ta  (&campo)values (&valor)

go g_actual
RETURN
ENDPROC