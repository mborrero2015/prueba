************************************************************************
*Procedimiento que permite calcular LIBRO MAYOR.
*PROCEDURE LMAYOR

SET DEFA TO
SET DATE FORMAT "dd/mm/yyyy"
SET SOFTSEEK ON

******VARIABLES SIMULADAS /// ELIMINAR
tipoconta=1  /// POR FINCA  
*******

 PRIVATE a,b,m,pant,cactual,auxcod,prov,auxnum1,qreg,qrfin,lini,lifin,finca

 PRIVATE campos[3],titulos[3]

 SELECT 18
 BASECO=DIRACTU+"IPROV"
 USE &BASECO
 BASECO2=DIRACTU+"iprov2"
 SET INDEX TO &baseco2 //Indices por NOMBPROV,CODPROV,NITPROV

 SELECT 78
 BASECO=DIRACTU+"TIPC"
 USE &BASECO
 BASECO1=DIRACTU+"TIPC1"
 BASECO2=DIRACTU+"TIPC2"
 INDEX ON FUENTE TO &baseco2

 SELECT 58
 BASECO=DIRACTU+"PUC"
 USE &BASECO
 SET FILTER TO LEN(ALLTRIM(58->CODIGO))=2
 SET INDEX TO &BASECO

 GO TOP
 STORE ALLTRIM(58->CODIGO) TO desde,desde1

 GO BOTTOM
 STORE ALLTRIM(58->CODIGO) TO hasta,hasta1

 SET FILTER TO
--------------------------

   IF tipoconta=1
      ante=1
      DO TRABFIN WITH 9,35 
      IF LASTKEY()=27
         RETURN
      ENDIF
   ENDIF   

------------------------------
 pant=SAVESCREEN(0,0,MAXROW(),MAXCOL())

 SETCOLOR(fondo)
 @1,0 CLEAR TO MAXROW()-2,MAXCOL()

 SETCURSOR(1)

 @3,3 SAY  " RANGO DE GRUPOS A PROCESAR " COLOR(tide)

 @5,5 SAY " Desde el Grupo: "+desde1 
 @8,5 SAY " Hasta el Grupo: "+hasta1

 ///// PONERCLASE(cuenta,1negrilla,linea,columna,longitud)

PONERCLASE(left(desde1,1),0,5,25,50)
PONERCLASE(left(desde1,2),0,6,25,50)

PONERCLASE(left(hasta1,1),0,8,25,50)
PONERCLASE(left(hasta1,2),0,9,25,50)

 @5,22 get desde1 PICT '99' valid desde1>=desde .and. desde1<=hasta .and. PONERCLASE(left(desde1,1),0,5,25,50)<>0 .and. PONERCLASE(left(desde1,2),0,6,25,50)<>0
 @8,22 get hasta1 PICT '99' VALID hasta1>=desde1 .and. hasta1<=hasta .and. PONERCLASE(left(hasta1,1),0,8,25,50)<>0 .and. PONERCLASE(left(hasta1,2),0,9,25,50)<>0
 READ

 RESTSCREEN(0,0,MAXROW(),MAXCOL(),pant)
-----------------------


if lastkey()=13 //-----

 SELECT 104
 USE CPG
 INDEX ON CODcp TO cp2
 SET RELATION TO 104->FUENTE INTO 78

 SELECT 109
 USE CPD
 SET RELATION TO 109->CODCP INTO 104
 SET RELATION TO 109->CODPROV INTO 18 ADDITIVE
 INDEX ON 104->FECHA TO CPd9
***** INDEX ON 109->CODcontabl+DTOC(104->FECHA) TO CPd9

 SELECT 115
 USE LMAYOR 
 ZAP

 SELECT 114
 USE PUC1 
 go top

//// CALCULANDO SALDO FINAL SIN CIERRES ////////
     REPLACE 114->SALDOTR1 WITH 114->SALDOINIC+114->MDEBITO-114->MCREDITO ALL
     SUM 114->SALDOTR1 TO cero
/////////////
  GO TOP

 SELECT 114
 USE PUC1 
 SET RELATION TO 114->CODPROV INTO 18
 INDEX ON 114->CODCONTABL TO PUC2  //Indices por CODCONTABL
 SET FILTER TO LEFT(114->CODCONTABL,4)>=desde1 .and.  LEFT(114->CODCONTABL,4)<=hasta1  
 GO TOP

/////////  PASANDO A lmayor.DBF ///////////

DO WHILE .NOT. EOF()
   IF 114->SALDOTR1<>0 .or. 114->SALDOTR2<>0
     SELECT 115
     APPEND BLANK
     REPLACE 115->CODCONTABL WITH 114->CODCONTABL
     REPLACE 115->NOMBRECTA WITH  LEFT(SPAC(1-LEN(ALLTRIM(114->ORIGINAL)))+ALLTRIM(ALLTRIM(18->NOMBPROV)+' '+ALLTRIM(114->NOMBRECTA))+spac(100),100)

     IF 114->SALDOTR1>0
        REPLACE 115->SFDB WITH 114->SALDOTR1
     ELSE
        REPLACE 115->SFCR WITH ABS(114->SALDOTR1)
     ENDIF

        REPLACE 115->MVDB WITH 114->MDEBITO
        REPLACE 115->MVCR WITH 114->MCREDITO

     IF 114->SALDOINIC>0
        REPLACE 115->SADB WITH 114->SALDOINIC
     ELSE
        REPLACE 115->SACR WITH ABS(114->SALDOINIC)
     ENDIF

     SELECT 114
  ENDIF 
     SKIP  
ENDDO

SELECT 115
SUM 115->SFDB,115->SFCR,115->SADB,115->SACR,115->MVDB,115->MVCR TO SFDB1,SFCR1,SADB1,SACR1,MVDB1,MVCR1

APPEND BLANK
APPEND BLANK

REPLACE 115->CODCONTABL WITH ">>>>>>>>>>"
REPLACE 115->NOMBRECTA WITH  ">>>>>>>>>>>>  T o t a l e s  --> $$"

   REPLACE 115->SFDB WITH SFDB1
   REPLACE 115->SFCR WITH SFCR1
   REPLACE 115->SADB WITH SADB1
   REPLACE 115->SACR WITH SACR1
   REPLACE 115->MVDB WITH MVDB1
   REPLACE 115->MVCR WITH MVCR1

/////////////////////////////////////////

 SELECT 115

 SET FILTER TO  115->SFDB+115->SFCR+115->SADB+115->SACR+115->MVDB+115->MVCR <> 0 
 prov=SPACE(30)
-----------------

 pant=SAVESCREEN(0,0,MAXROW(),MAXCOL())

 SETCOLOR(fondo)
 @1,0 CLEAR TO MAXROW()-2,MAXCOL()
 SETCOLOR(letayu)
 @23,0 SAY REPLICATE (" ",80)
 @24,0 SAY REPLICATE (" ",80)

 @23,0  SAY "[F1]-Ayuda          "
 @23,40 SAY "[ESC]-Regresar      "
 @24,40 SAY "["+CHR(24)+CHR(25)+"]-Desplazar      "

 @23,18 SAY "[F8]-Imp. Resumen."
 @24,18 SAY "[F9]-Imp. Detallado."

 @24,60 SAY REPLICATE (" ",20)

 SETCOLOR(menu)
 a=9
 b=2

 DO CAJA WITH a-7,b,a+13,b+77
 
 titucg="LIBRO MAYOR"

 @a-7,b SAY space(round((78-len(alltrim(titucg)))/2,0))+alltrim(titucg)+space(int((78-len(alltrim(titucg)))/2)) COLOR(tide)

 @a-1,b to a-1,b+77

////////////////////////////////////////////////////////////////////////
 GO BOTTOM
 KEYBOARD CHR(5)+CHR(5)+CHR(5)+CHR(5)+CHR(5)+CHR(5)+CHR(5)+CHR(5)+CHR(5)

*** variables para marcas de Pag. Arriba - Abajo
 qrfin=0
 COUNT TO qreg
 lini=a+3  // +3 cuando el titulo esta en (2) lineas
 lifin=a+13 
********
 qcol=1
 
 campos[1]="115->CODCONTABL+CHR(179)+LEFT(115->NOMBRECTA,35)+CHR(179)+TRANSFORM(115->SADB,'@Z 999,999,999,999')+CHR(179)+TRANSFORM(115->SACR,'@Z 999,999,999,999')"
 titulos[1]="  Codigo                                            S a l d o  Inicial... -->; Contable         Nombre de la Cuenta              Debito          Credito "

 campos[2]="115->CODCONTABL+CHR(179)+LEFT(115->NOMBRECTA,35)+CHR(179)+TRANSFORM(115->MVDB,'@Z 999,999,999,999')+CHR(179)+TRANSFORM(115->MVCR,'@Z 999,999,999,999')"
 titulos[2]="  Codigo                                            Movimientos ......... ==>; Contable         Nombre de la Cuenta              Debito          Credito "

 campos[3]="115->CODCONTABL+CHR(179)+LEFT(115->NOMBRECTA,35)+CHR(179)+TRANSFORM(115->SFDB,'@Z 999,999,999,999')+CHR(179)+TRANSFORM(115->SFCR,'@Z 999,999,999,999')"
 titulos[3]="  Codigo                                            S a l d o   F i n a l.   ; Contable         Nombre de la Cuenta              Debito          Credito "

  DBEDIT(a,b,a+12,b+77,campos,"gener5","",titulos,"Ä","³") 


endif   // ---------


    IF tipoconta=1
       DIRCHANGE("..") 
    ENDIF   


  close data

 SETCOLOR(cactual) 
 RESTSCREEN(0,0,MAXROW(),MAXCOL(),pant)
RETURN

******************************************************************************
*Funci¢n de evaluaci¢n de la tecla presionada.
******************************************************************************

FUNCTION gener5
 PARAMETERS modo,cont
 PRIVATE ulttecla,cactual

lrw=ROW()
FLE1(a-1,b+1,qrfin,qreg,lini,lifin)  // Flechas arrriba - abajo

 IF qcol<>1
    IF qcol=cont
        @a-1,b+72 SAY chr(17)+spac(1) COLOR(tide)
    ELSEIF cont=1
         @a-1,b+72 SAY chr(16)+spac(1) COLOR(tide)
    else
         @a-1,b+72 SAY chr(17)+chr(16) COLOR(tide)
    ENDIF
 ENDIF

 SET ESCAPE ON
 SET SCOREBOARD OFF
 SET CONFIRM ON 
 SETCURSOR(0)
 ulttecla=LASTKEY()


    SELECT 58

    SEEK LEFT(115->CODCONTABL,1)
    @ a-6,b+2 say SPAC(72) COLOR(mesa1)
    if found()
       @ a-6,b+2 say  LEFT(115->CODCONTABL,1)+" "+LEFT(DENOMINA,66) COLOR(mesa1)
    endif

    SEEK LEFT(115->CODCONTABL,2)
    @ a-5,b+2 say SPAC(72) COLOR(mesa1)
    if found()
       @ a-5,b+2 say LEFT(115->CODCONTABL,2)+" "+LEFT(DENOMINA,66) COLOR(mesa1)
    endif

    SEEK LEFT(115->CODCONTABL,4)
    @ a-4,b+2 say SPAC(72) COLOR(mesa1)

    IF FOUND() .AND. LEN(ALLTRIM(LEFT(115->CODCONTABL,4)))=4
       @ a-4,b+2 say LEFT(115->CODCONTABL,4)+" "+LEFT(UPPER(DENOMINA),66) COLOR(mesa1)
    ELSE
           SELEC 114
           SEEK LEFT(115->CODCONTABL,4)
           if found()
              @ a-4,b+2 say LEFT(115->CODCONTABL,4)+" "+LEFT(ALLTRIM(ALLTRIM(UPPER(18->NOMBPROV))+' '+ALLTRIM(UPPER(114->NOMBRECTA)))+spac(66),66) COLOR(mesa1)
           endif
           SELECT 58                      
    ENDIF

    SEEK LEFT(115->CODCONTABL,6)
    @ a-3,b+2 say SPAC(72) COLOR(mesa1)

    IF FOUND() .AND. LEN(ALLTRIM(LEFT(115->CODCONTABL,6)))=6
        @ a-3,b+2 say LEFT(115->CODCONTABL,6)+" "+LEFT(DENOMINA,66) COLOR(mesa1)
    ELSE
           SELEC 114
           SEEK LEFT(115->CODCONTABL,6)
           if found()
              @ a-3,b+2 say LEFT(115->CODCONTABL,6)+" "+LEFT(ALLTRIM(ALLTRIM(18->NOMBPROV)+' '+ALLTRIM(114->NOMBRECTA))+spac(66),66) COLOR(mesa1)
           endif
           SELECT 58                      
    ENDIF

    @ a-2,b+2 say SPAC(72) COLOR(mesa1)
    IF LEN(ALLTRIM(115->CODCONTABL))=10
       @ a-2,b+2 say 115->CODCONTABL+" "+LEFT(115->NOMBRECTA,62) COLOR(mesa1)
    ENDIF

///////////////




    SELECT 115

 DO CASE
    CASE modo = 0
         RETURN 1
    CASE modo = 1 .OR. modo = 2
            TONE(100,3)
          RETURN 2
    CASE modo = 3
         DO UNO WITH a+4,b+18,"NO HAY REGISTROS..."
         RETURN 1
    CASE modo = 4
         DO CASE 
            CASE ulttecla = 27
                 RETURN 0

            CASE ulttecla = 13
                    @lrw,b+3 SAY CHR(16) COLOR(flec)
                    auxcod=115->CODcontabl
   		    DO detmayor
                 RETURN 1

            CASE ulttecla = -7
        	 SET ORDER TO 1  	 
                 impre20("Resumido","CONDENSADO",ulttecla)
                 RETURN 1
            CASE ulttecla = -8
        	 SET ORDER TO 2  	 
                 impre20("Detallado","CONDENSADO",ulttecla)
                 RETURN 1

            OTHERWISE
                 RETURN 1
         ENDCASE
 ENDCASE    
*************

****************************************************************************
*Procedimiento que permite MOSTRAR LA CUENTA EN DETALLES.....
****************************************************************************

PROCEDURE detmayor

 PRIVATE a,b,pant,var,var1,p,p1,d,qreg,qrfin,lini,lifin,az,bz,by,qcol,ctapuc1,nompuc1,ternom
 PRIVATE campos[1],titulos[1],cactual,prov,ayuda1,niprov
 cactual=SETCOLOR()
 a=10
 b=1
 qcol=1
 campos[1]="DTOC(104->fecha)+CHR(179)+right('000000000'+alltrim(str(104->CODcp)),10)+CHR(179)+18->NITPROV+CHR(179)+left(109->DETALLE,13)+CHR(179)+TRANSFORM(109->DB,'@Z 999,999,999')+CHR(179)+TRANSFORM(109->CR,'@Z 999,999,999')"

 titulos[1]="              No.        NIT o C.C.                                                     ;  Fecha     Comprob.      Tercero     Detalle Trans    Debito     Credito"

 pant=SAVESCREEN(0,0,MAXROW(),MAXCOL())
 SETCOLOR(letayu)

    @23,0 SAY "[F1]-Ayuda          "
    @24,0 SAY REPLICATE (" ",20)
    @23,20 SAY "[ESC]-Regresar      "
    @24,20 SAY "["+CHR(24)+CHR(25)+"]-Seleccionar    "
    @23,40 SAY REPLICATE (" ",40)
    @24,40 SAY REPLICATE (" ",40)

 SELECT 109
 SET FILTER TO 109->CODcontabl=auxcod
 GO TOP

 SETCOLOR(menu)
 DO CAJA WITH a,b+1,a+11,b+77

 rotulo="Movimiento Detallado Cuenta: "+auxcod
 rotulo=spac(int((77-LEN(rotulo))/2))+rotulo
 rotulo=rotulo+SPAC(77-len(rotulo))

 @a,b+1 SAY  rotulo COLOR(tide)

  @ a+11,B+24 say "** Total Movimientos --> $  "+CHR(179)+"           "+CHR(179)
 @a+12,b+1 TO a+12,b+76
 
  aregins=RECNO()
  SUM 109->DB,109->CR TO sdbs,scrs

    @ a+11,B+53 say sdbs pict "@Z 999,999,999" COLOR(mesa1)
    @ a+11,B+65 say scrs pict "@Z 999,999,999" COLOR(mesa1)
  go aregins

*** variables para marcas de Pag. Arriba - Abajo
 qrfin=0
 COUNT TO qreg
 lini=a+1+2  // +2 cuando el titulo esta en (1) sola linea
 lifin=a+18
 az=a
 bz=b+2
 by=b+74 
********

 GO TOP
    DBEDIT(a+1,b+1,a+10,b+76,campos,"f11ff",,titulos,"Ä","³")

 SELECT 109
 SET FILTER TO 

 SELECT 114
 SETCOLOR(cactual)
 RESTSCREEN(0,0,MAXROW(),MAXCOL(),pant)
RETURN

*************************************************************


*************
FUNCTION impre20
 PARAMETERS mens1,mens2,cual

 PRIVATE opc,a,b
 opc=SPACE(1)
 CAJA1(10,20,19,65,"")
 @12,21 TO 12,64
 @11,22 SAY "         IMPRIMIENDO LIBRO MAYOR  " COLOR(tit)
 @13,22 SAY "Esta opci¢n permite imprimir el movimiento"
 @14,22 SAY "por cuenta, de forma "+mens1
 @16,22 SAY "Aliste impresora en caracter "+mens2
 WAIT ""

 narch=diractu+"SALIDAS\LMAYOR"
 opc=PUERTO(narch)

IF LASTKEY()=13 .AND. opc <> 0

    SET DEVICE TO PRINTER
    GO TOP
    linea=1001
    pagina=0

    SELECT 109
    filtro=DBFILTER()

    select 115
    GO TOP

      iclase=""
      igrp=""
      icta=""
      isbcta=""
      swl=0

     DO WHILE .NOT. EOF()

           DO CASE
              CASE cual=-7  &&  resumido

		DO TITUCTA
     
		@linea,49 SAY 115->SADB pict "@Z 999,999,999,999"
		@linea,65 SAY 115->SACR pict "@Z 999,999,999,999"

		@linea,81 SAY 115->MVDB pict "@Z 99,999,999,999"
		@linea,96 SAY 115->MVCR pict "@Z 99,999,999,999"

		@linea,111 SAY 115->SFDB pict "@Z 999,999,999,999"
		@linea,111 SAY 115->SFDB pict "@Z 999,999,999,999"

		@linea,127 SAY 115->SFCR pict "@Z 999,999,999,999"
		@linea,127 SAY 115->SFCR pict "@Z 999,999,999,999"

     	        linea=linea+1
		skip

               CASE cual=-8   &&  detallado

		 IF VAL(115->CODCONTABL)<>0

			DO TITUCTA
			@linea,91 SAY "Saldo Inicial ..."
			@linea,110 SAY 115->SADB pict "@Z 999,999,999,999"
			@linea,127 SAY 115->SACR pict "@Z 999,999,999,999"
                        linea=linea+1
			sdbs=115->SADB
			scrs=115->SACR

			SELECT 109
	     	        SET FILTER TO 109->CODcontabl=115->CODcontabl
  	 		GO TOP

		     DO WHILE .NOT. EOF()
		        if linea>55
		      	      do cabimp20
		        endif
			@linea,2 SAY CMONTH(104->fecha)+" "+str(day(104->fecha),2)+" de "+str(YEAR(104->fecha),4)
			@linea,25 SAY right('000000000'+alltrim(str(104->CODcp)),10)
	  	        @linea,39 SAY 18->NOMBPROV
		        @linea,71 SAY 109->DETALLE
 		        @linea,114 SAY 109->DB pict "@Z 999,999,999"
		        @linea,131 SAY 109->CR pict "@Z 999,999,999"
                        linea=linea+1
			sdbs=sdbs+109->db
			scrs=scrs+109->cr
			SKIP
		     ENDDO
				salfinal=SDBS-SCRS
				store 0 to SDBS,SCRS
                               
				IF salfinal>0
                                   SDBS=salfinal
                                ELSE
                                   SCRS=abs(salfinal)
 				ENDIF

				@linea,91 SAY "  Saldo Final ..."
				@linea,91 SAY "  Saldo Final ..."
                           
				@linea,110 SAY SDBS pict "@Z 999,999,999,999"
				@linea,110 SAY SDBS pict "@Z 999,999,999,999"
				@linea,110 SAY "__________________"
				@linea,110 SAY "__________________"

				@linea,127 SAY SCRS pict "@Z 999,999,999,999"
				@linea,127 SAY SCRS pict "@Z 999,999,999,999"
				@linea,127 SAY "__________________"
				@linea,127 SAY "__________________"

	                        linea=linea+1

                          	select 115
                 ENDIF
		 skip

	   ENDCASE

     ENDDO

    EJECT
    DONDESTA(opc,narch)

    SELECT 109
    SET FILTER TO &filtro
    GO TOP

 ENDIF

RETURN


************
procedure cabimp20

    if linea<1000
       eject
    endif

    pagina=pagina+1

    if len(mens2)=6
       colum=25
    else
       colum=50
    endif

    @0,2 SAY "P gina No. "+str(pagina,3)
    @1,2 SAY DATE()
    @2,colum SAY " >>  SACFA VER. 3.0.  - CONTABLE  <<"

    @3,colum SAY "      LIBRO MAYOR - "+mens1

    @5,colum*2+10 SAY "FINCA: "+finca

    DO CASE
       	CASE cual=-7 && resumido

   	@7,1 SAY "                                               |  S a l d o   A n t e r i o r  |      M o v i m i e n t o    | **  S a l d o   F i n a l **  |"
   	@8,1 SAY "          Cuenta Contable                      |    Debito     |    Credito    |    Debito    |    Credito   |     Debito    |     Credito   |"
    	@8,1 SAY "_____________________________________________________________________________________________________________________________________________"

	CASE cual=-8  &&& detallado

    	@6,1 SAY " * Cuenta Contable * "
   	@7,1 SAY "       Fecha          |# Comprobante|           Tercero            |           Detalle Transaccion           |     Debito    |     Credito   |"
    	@7,1 SAY "_____________________________________________________________________________________________________________________________________________"

    ENDCASE

    linea=8
return




FUNCTION PONERCLASE
  PARAMETERS cta,vez,nlin,ncol,largo

    area=SELECT()

    @ nlin,ncol say SPAC(52)

    SELECT 58
    SEEK cta

    IF FOUND() .AND. LEN(cta)<=6
        @ nlin,ncol say cta+" "+LEFT(DENOMINA,largo)
	if vez<>0
           @ nlin,ncol-1 say "*"+cta+" "+LEFT(DENOMINA,largo)
        endif
    ELSEIF ALIAS(114)<>""
           SELEC 114
           SEEK cta
           if found()
              @ nlin,ncol say cta+" "+LEFT(ALLTRIM(ALLTRIM(18->NOMBPROV)+' '+ALLTRIM(114->NOMBRECTA))+spac(largo),largo) 
		if vez<>0
                    @ nlin,ncol-1 say "*"+cta+" "+LEFT(ALLTRIM(ALLTRIM(18->NOMBPROV)+' '+ALLTRIM(114->NOMBRECTA))+spac(largo),largo) 
	        endif
           endif
           SELECT 58                      
    ENDIF
    SELECT (area)
	IF vez <> 0
           nlin = nlin +1
        ENDIF

RETURN nlin


PROCEDURE TITUCTA

           if linea>55
      	      do cabimp20
           endif

	   swl=0

           IF iclase<>LEFT(115->codcontabl,1)
		linea=linea+2
              	linea=PONERCLASE(left(115->codcontabl,1),1,linea,1,100)
		iclase=LEFT(115->codcontabl,1)
                swl=1
           ENDIF

           IF igrp<>LEFT(115->codcontabl,2)
                IF swl=0
		   linea=linea+1
                ENDIF
              	linea=PONERCLASE(left(115->codcontabl,2),1,linea,1,100)
		igrp=LEFT(115->codcontabl,2)
                swl=1
           ENDIF

           IF icta<>LEFT(115->codcontabl,4)
                IF swl=0
		   linea=linea+1
                ENDIF
              	linea=PONERCLASE(left(115->codcontabl,4),1,linea,1,100)
		icta=LEFT(115->codcontabl,4)
                swl=1
           ENDIF

           IF isbcta<>LEFT(115->codcontabl,6) 
	     	IF len(alltrim(115->codcontabl))-6 = 0
		   lola=41
                ELSE
                   IF swl=0
  		      linea=linea+1
                   ENDIF
 		   lola=100
                ENDIF	
              	linea=PONERCLASE(left(115->codcontabl,6),len(alltrim(115->codcontabl))-6,linea,1,lola)
		isbcta=LEFT(115->codcontabl,6) 
		swl=1	
           ENDIF

           IF len(alltrim(115->codcontabl))=10 
	     	IF cual=-8  && detallado
		   lola=100
                ELSE
		   lola=41
                ENDIF	
              	linea=PONERCLASE(115->codcontabl,0,linea,1,lola)
           ENDIF

	   swl=0

RETURN
* É  »  ¼  È  º  Í  Ë  Ê  Î  Ä  ¿ Ú À Ù ª © ³ Â Á Å ¹ Ì

* °  ±  ²  Û  Ü  Ý  Þ  ß  þ     ‚  ¡  ¢  £  ¤  ¥