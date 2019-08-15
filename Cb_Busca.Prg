LOCAL cDisplayValue,cSqlStr,cCountryName
cDisplayValue = ALLTRIM(THIS.DisplayValue)
THIS.SelLength = 0
IF EMPTY(m.cDisplayValue)
	RETURN
ENDIF

THISFORM.LockScreen = .T.

IF THIS.Value =" "

ELSE
	SELECT iprov.nombprov AS encontre,* FROM iprov ;
		 WHERE UPPER(ALLTRIM(iprov.nombprov)) = UPPER(m.cDisplayValue);
		 INTO ARRAY escoja
	thisform.text2.text = "escoja[1,1]"
	thisform.text3.text = "escoja[1,2]"
	thisform.text4.text = "escoja[1,3]"
	IF _TALLY #0
		Nproveedor = ALLTRIM(iprov.nombprov)
		IF ATC(m.Nproveedor,THIS.RowSource) = 0 AND !EMPTY(m.Nproveedor)
			THIS.RowSource=THIS.RowSource+","+m.Nproveedor 
		ENDIF
		THIS.Value = m.Nproveedor
	ENDIF	
ENDIF
THISFORM.ResetCombos(THIS)
THISFORM.LockScreen = .F.
******************************
LOCAL unico,tec,duro
tec	=lastkey()
unico=" "
duro=ALLTRIM(THIS.value)


IF tec = 7 or tec =127
	unico = duro
	IF LEN(unico)=1
		unico = ""
	ELSE
		unico = LEFT(unico,LEN(unico)-1)
	ENDIF
ELSE
	unico = ALLTRIM(THIS.value)+CHR(tec)
ENDIF

THISFORM.LockScreen = .T.
DO CASE
CASE EMPTY(unico)
	thisform.paginas.page1.contiene_todo.Text2.value = " "
	thisform.paginas.page1.contiene_todo.Text3.value = " "
	thisform.paginas.page1.contiene_todo.Text4.value = " "
	
	
OTHERWISE

  LOCATE FOR LEFT(NOMBPROV,LEN(UNICO))=UNICO
  this.requery
  this.refresh
   
*	SELECT COUNT(*)FROM iprov;
*	 WHERE unico = Iprov.nombprov;
*	  INTO ARRAY los 
	
*	IF los = 0 THEN
*		do form locfile ("provee.scx") with unico  
		
*	ELSE
	
*		SELECT Iprov.nombprov,Iprov.direcprov,Iprov.telprov FROM iprov;
*		 WHERE UPPER(ALLTRIM(iprov.nombprov)) = UPPER(unico);
*	 	  INTO ARRAY escoja
*		   thisform.paginas.page1.contiene_todo.Text2.value = escoja[1,1]
*		   thisform.paginas.page1.contiene_todo.Text3.value = escoja[1,2]
*		   thisform.paginas.page1.contiene_todo.Text4.value = escoja[1,3]
*	ENDIF
ENDCASE
THISFORM.LockScreen = .F.




*****************************************
