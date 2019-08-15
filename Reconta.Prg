
Select Count (*) From Infculti Where codfin =  g_codfin AND; 
  ano = g_ano AND semestre = g_semestre AND marcainf Into Array cuantos


IF cuantos = 0
	MESSAGEBOX("No Hay Lotes Seleccionados"+Chr(13)+;
	 "     Por Favor Seleccione",32,"Captura de Datos")
	Thisform.gr_lotes.column1.Check1.Setfocus()
ELSE
 cualot  =""
 cualot1 =""
 cualot2 =""
 cualot3 =""  
 cuapl   =""
   
 Select Infculti.numcontrol, Infculti.codlote, Infculti.nomlote, Lotesfin.area From Infculti,;
   Lotesfin Where Infculti.codfin = g_codfin AND Infculti.ano = g_ano AND ;
    Infculti.semestre = g_semestre AND Infculti.marcainf AND ;
      Lotesfin.codlote = Infculti.codlote Into Cursor clot
  
  if cuantos = 1 
   cualot  = " Labculti.numcontrol = " + alltrim(str(clot.numcontrol))
   cualot1 = " Jorculti.numcontrol = " + alltrim(str(clot.numcontrol))
   cualot2 = " Maqculti.numcontrol = " + alltrim(str(clot.numcontrol))
   cualot3 = " Aplica.numcontrol = " + alltrim(str(clot.numcontrol))
   SELECT Aplica.codaplic FROM sacfadb!aplica;
    WHERE (&cualot3) AND LEFT(Aplica.codaplic,1) = "A";
     ORDER BY Aplica.codaplic Into Cursor Ap123

  Select ap123   
   Go top
  
   Do while !eof() 
     cuapl = cuapl + " Auxapli.codaplic = ";
       + Chr(34)+Alltrim(Ap123.codaplic)+Chr(34) +" OR" 
     Skip
   Enddo
   
  cuapl = left(cuapl,(Len(cuapl)-3))
   
   
  else
    Select clot
    Go top
   
   Do while !eof() 
     cualot = cualot + " Labculti.numcontrol = ";
       + Alltrim(str(clot.numcontrol)) +" OR" 
     cualot1 = cualot1  + " Jorculti.numcontrol = ";
      + Alltrim(str(clot.numcontrol)) +" OR" 
     cualot2 = cualot2  + " Maqculti.numcontrol = ";
      + Alltrim(str(clot.numcontrol)) +" OR" 
     cualot3 = cualot3  + " Aplica.numcontrol = ";
      + Alltrim(str(clot.numcontrol)) +" OR"       
     Skip
   Enddo
  endif

   cualot  = left(cualot,(Len(cualot)-3))
   cualot1 = left(cualot1,(Len(cualot1)-3))
   cualot2 = left(cualot2,(Len(cualot2)-3))
   cualot3 = left(cualot3,(Len(cualot3)-3))
   
   SELECT Aplica.codaplic FROM sacfadb!aplica;
    WHERE (&cualot3) AND LEFT(Aplica.codaplic,1) = "A";
     ORDER BY Aplica.codaplic Into Cursor Ap123

  Select ap123   
   Go top

   Do while !eof() 
     cuapl = cuapl + " Auxapli.codaplic = ";
       + Chr(34)+Alltrim(Ap123.codaplic)+Chr(34) +" OR" 
     Skip
   Enddo
        
  cuapl = left(cuapl,(Len(cuapl)-3))
  plan = Thisform.Op_Informe.value
  
Do case
 Case plan = 1

   Select Sum(Labculti.costotal) as costotal , codlabor From Labculti ;
    Into Table Reco1 Where  &cualot  Group By codlabor 
    
    Select auxreco
     Append From Reco1 Fields costotal, codlabor
  
   Select Sum(Jorculti.costotal) as costotal , codlabor From Jorculti Into Table Reco1;
     Where  &cualot1  Group By codlabor     

    Select auxreco
     Append From Reco1 Fields costotal, codlabor     

   Select Sum(Maqculti.costotal) as costotal , codlabor From Maqculti Into Table Reco1;
     Where  &cualot2  Group By codlabor     
 
    Select auxreco
     Append From Reco1 Fields costotal, codlabor

   Select Sum(costotal) As Total , codlabor From Auxreco ;
    Into Cursor sureco Group By codlabor

 
 SELECT Labculti.codaplica As codpli, SUM(Labculti.costotal) AS ctota;
    FROM sacfadb!labculti WHERE (&cualot) AND Labculti.codlabor = 705;
     GROUP BY Labculti.codaplica ORDER BY Labculti.codaplica ;
      Into Table Reco2

   Select Auplx
    Append From Reco2 Fields ctota, codpli

   SELECT Jorculti.codaplica As codpli, SUM(Jorculti.costotal) As ctota;
    FROM sacfadb!jorculti WHERE (&cualot1) AND Jorculti.codlabor = 705;
     GROUP BY Jorculti.codaplica ORDER BY Jorculti.codaplica ;
      Into Table Reco2

    Select Auplx
     Append From Reco2 Fields ctota, codpli

   SELECT Maqculti.codaplica As codpli, SUM(Maqculti.costotal) As ctota;
    FROM sacfadb!maqculti  WHERE (&cualot2) AND Maqculti.codlabor = 705;
     GROUP BY Maqculti.codaplica ORDER BY Maqculti.codaplica ;
      Into Table Reco2

       Select Auplx
        Append From Reco2 Fields ctota, codpli


   
******** ***********   *************  ************  ************  **********

   Select total As vtot From Sureco Where codlabor = 900 ;
     into cursor lin1 &&&ASISTENCIA TECNICA
     
   Select total As vtot From Sureco Where codlabor = 901 ;
     into cursor lin2 &&&ARRIENDO
     
   Select sum(total)As vtot From Sureco Where Between(codlabor, 100, 199) ;
     into cursor lin3 && PREPARACION  DE TIERRA
     
   Select sum(total) As vtot From Sureco Where Between(codlabor, 300, 399) ;
     into cursor lin6 && SEMILLA
     
   Select Sum((Auxapli.cantusada*(Compins.costunit/Compins.equivale)) ;
              +(Auxapli.cantusada*Compins.sobrecosto)) As valort ; 
     From  sacfadb!aplica Inner Join sacfadb!auxapli ;
      Inner Join sacfadb!compins ;
       On  Compins.codcomp = Auxapli.codcomp ;
       On  Aplica.codaplic = Auxapli.codaplic ;
        Where &cualot3 And Left(Auxapli.codaplic,1) = "S" ;
         Into Cursor lin5 &&VALOR DE LA SEMILLA
         
    Select sum(lin5.valort + lin6.vtot) As vtot From lin5, lin6 ;
     Into Cursor Lin4 &&SIEMBRA
     
    Select sum(total) As agtot  From Sureco Where codlabor = 506 OR ;
     codlabor = 507 OR codlabor = 501 into cursor lin8 && AGUA

    Select sum(total) As Mantot From Sureco Where codlabor = 500 OR;
       Between(codlabor, 502, 504) OR codlabor = 508 ;
          into cursor lin9 && MANTENIMIENTO
                    
    Select Sum(lin8.agtot+lin9.mantot) From lin8, lin9 ;
     Into Cursor Lin7 &&RIEGO
 

******** Costo Aplicacion por Tipo de Insumo*******************************

    SELECT Auxapli.codaplic, SUM(Auxapli.cantusada)As Cusa ;
     FROM sacfadb!auxapli WHERE &cuapl GROUP BY Auxapli.codaplic ;
      ORDER BY Auxapli.codaplic Into Cursor Pusado

    Select Auplx.codpli, Pusado.Cusa, Sum(Auplx.ctota) As Total ;
      From Auplx, Pusado  Group By codpli Into Cursor AAppll;
       Where Auplx.codpli = Pusado.codaplic
    
    SELECT Auxapli.codtipo, ;
     Round(SUM((Auxapli.cantusada/aappll.cusa)*aappll.total),-1) As totapl ;
      FROM  sacfadb!Auxapli GROUP BY Auxapli.codtipo Into cursor tApli ;
       Where &cuapl
       
****************************************************************************
    Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
               +(Auxapli.cantusada * Compins.sobrecosto)) As valort ; 
      From  sacfadb!aplica Inner Join sacfadb!auxapli ;
       Inner Join sacfadb!compins ;
        On  Compins.codcomp = Auxapli.codcomp ;
        On  Aplica.codaplic = Auxapli.codaplic ;
         Where &cualot3 And Left(Auxapli.codaplic,1) = "A" ;
                And Auxapli.codtipo = 2 Into Cursor lin11 &&FERTILIZANTES
 
    Select Sum(totapl)As fert From tapli where tapli.codptipo = 2 ;
     Into Cursor lin12 && Aplicacion de Fertilizantes


    Select Sum(Lin11.valort+Lin12.fert) As vtot From lin11, lin12 ;
     Into Cursor Lin10 &&Fertilizacion

    Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
                +(Auxapli.cantusada * Compins.sobrecosto)) As valort ; 
      From  sacfadb!aplica Inner Join sacfadb!auxapli ;
       Inner Join sacfadb!compins ;
        On  Compins.codcomp = Auxapli.codcomp ;
        On  Aplica.codaplic = Auxapli.codaplic ;
         Where &cualot3 And Left(Auxapli.codaplic,1) = "A" ;
               And Auxapli.codtipo = 5 Into Cursor lin14 &&HERBICIDAS
               
    Select Sum(totapl)As herb From tapli where tapli.codptipo = 5 ;
     Into Cursor lin15 && Aplicacion de Herbicidas

***************************************************************************

    Select sum(total) As tot From Sureco Where Between(codlabor, 203, 299);
     OR codlabor = 200 Or codlabor = 201 Into cursor lin16 
      && CONTROL malezas desroje etc......
     
    Select Sum(Lin14.valort+Lin15.herb+Lin16.tot) As vtot ;
      From lin14, lin15, Lin16 Into Cursor Lin13 &&Control Malezas

   Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
                +(Auxapli.cantusada * Compins.sobrecosto)) As valort ; 
       From  sacfadb!aplica Inner Join sacfadb!auxapli ;
        Inner Join sacfadb!compins ;
         On  Compins.codcomp = Auxapli.codcomp ;
         On  Aplica.codaplic = Auxapli.codaplic ;
          Where &cualot3 And Left(Auxapli.codaplic,1) = "A" ;
                And (Auxapli.codtipo = 4 or Auxapli.codtipo = 6 or ;
                  Auxapli.codtipo = 7 ) Into Cursor lin18 &&INSECTICIDAS, 
                  *RODENTICIDAS Y NEMATICIDAS APLICADOS
 
   Select Sum(totapl)As Insec From tapli where tapli.codptipo = 4 ;
     Into Cursor lin19 && Aplicacion de Insecticidas,rodenticidas y  
     *                    nematicidas 
 
   Select sum(total) As tot From Sureco Where codlabor = 20 ;
    Into cursor lin20 && CONTROL plaglas pajareo
 
   Select Sum(Lin18.valort+Lin19.insec+Lin20.tot) As vtot 
    From lin18, lin19, Lin20 Into Cursor Lin17 &&Control Plagas

****************************************************************************
   Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
               +(Auxapli.cantusada * Compins.sobrecosto)) As valort ; 
      From  sacfadb!aplica Inner Join sacfadb!auxapli ;
       Inner Join sacfadb!compins ;
        On  Compins.codcomp = Auxapli.codcomp ;
        On  Aplica.codaplic = Auxapli.codaplic ;
         Where &cualot3 And Left(Auxapli.codaplic,1) = "A" ;
                And Auxapli.codtipo = 3 Into Cursor lin22 &&FUNGICIDAS

   Select Sum(totapl)As Fung From tapli where tapli.codptipo = 3 ;
     Into Cursor lin23 && Aplicacion de fungicidas
  
   Select Sum(lin22.valort + lin23.fung) As vtot From lin22, lin23 ;
     Into Cursor lin21 &&CONTROL DE ENFERMEDADES
  
  Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
              +(Auxapli.cantusada * Compins.sobrecosto)) As valort ; 
     From  sacfadb!aplica Inner Join sacfadb!auxapli ;
      Inner Join sacfadb!compins ;
       On  Compins.codcomp = Auxapli.codcomp ;
       On  Aplica.codaplic = Auxapli.codaplic ;
        Where &cualot3 And Left(Auxapli.codaplic,1) = "A" ;
               And Auxapli.codtipo = 9 Into Cursor lin25 &&INSUMOS

   Select Sum(totapl)As insum From tapli where tapli.codptipo = 9 ;
     Into Cursor lin26 && Aplicacion de fungicidas
  
   Select Sum(lin25.valort + lin26.insum) As vtot From lin25, lin26 ;
     Into Cursor lin24 &&OTROS INSUMOS
               
   Select sum(total) As recol From Sureco Where Between(codlabor, 400, 499);
         into cursor lin27 && RECOLECCION

   Select sum(total) As ttrans From Sureco Where Between(codlabor, 600, 699);
         into cursor lin28 && TRANSPORTE

   Select sum(total) As vvario From Sureco Where codlabor = 801 Or ;
    codlabor = 802 into cursor lin29 && VARIOS Alimentacion y Celaduria

   Select Sum(Lin1.vtot+Lin2.vtot+Lin3.vtot+Lin4.vtot+Lin7.vtot+Lin10.vtot;
    +Lin13.vtot+Lin17.vtot+Lin21.vtot+Lin24.vtot+Lin27.recol+Lin28.ttrans;
     +Lin29.vvario) From Lin1, Lin2, Lin3, Lin4, Lin7, Lin10, Lin13, Lin17, ;
      Lin21, Lin24, Lin27, Lin28, Lin29 Into Cursor Lin30 
 
 
   Select sum(total) As Admon From Sureco Where codlabor = 803  ;
    Into cursor lin31 && ADMINISTRACION
 

***************************************************************************
 *************<<  RESUMEN DE COSTOS DE INTERESES  >>*************

 IF FILE(auxcred)

    SELECT 12
    USE &auxcred           //Proviene de CREDXXXX.DBF
     IF .NOT. FILE(a3cred)
        INDEX ON ENTIDAD TO &a1cred
        INDEX ON NUMCRED TO &a2cred
        INDEX ON CODCRED TO &a3cred
     ENDIF
    SET INDEX TO &a3cred   //Indice por CODCRED.

    SELECT 20
    USE &auxcred1           //Proviene de CRAAXXXX.DBF
    SET FILTER TO 20->CODLOTE=xcodl
    GO TOP
    SET RELATION TO CODCRED INTO 12
 
    DO WHILE .NOT. EOF()
       idia=(12->INTERES/365)/100
       IF 12->PVALDES<>0
          saldo=(12->PVALDES * (MONTOP/100))
       ELSE
          saldo=(12->VALCRED*(12->PDESEM/100)) * (MONTOP/100)
       ENDIF
       fec1=12->PFECHD
       DO WHILE ROUND(saldo,5)<>0
          fec1=fec1+1
          aux = aux + (saldo * idia)
          DO CASE
             CASE fec1=12->SFECHD
                  IF 12->SVALDES<>0
                     saldo=saldo+(12->SVALDES * (MONTOP/100))
                  ELSE
                     saldo=saldo+((12->VALCRED*(12->SDESEM/100)) * (MONTOP/100))
                  ENDIF
             CASE fec1=12->TFECHD
                  IF 12->TVALDES<>0 
                     saldo=saldo+(12->TVALDES * (MONTOP/100))
                  ELSE
                     saldo=saldo+((12->VALCRED*(12->TDESEM/100)) * (MONTOP/100))
                  ENDIF 
             CASE fec1=12->CFECHD
                  IF 12->CVALDES<>0 
                     saldo=saldo+(12->CVALDES * (MONTOP/100))
                  ELSE
                     saldo=saldo+((12->VALCRED*(12->CDESEM/100)) * (MONTOP/100))
                  ENDIF
             CASE fec1=12->QFECHD
                  IF 12->QVALDES<>0 
                     saldo=saldo+(12->QVALDES* (MONTOP/100))
                  ELSE
                     saldo=saldo+((12->VALCRED*(12->QDESEM/100))* (MONTOP/100))
                  ENDIF
             CASE fec1=12->PFECHR
                  IF 12->PVALREE<>0
                     saldo=saldo-(12->PVALREE* (MONTOP/100))
                  ELSE
                     saldo=saldo-((12->VALCRED*(12->PREEM/100))* (MONTOP/100))
                  ENDIF
             CASE fec1=12->SFECHR
                  IF 12->SVALREE<>0
                     saldo=saldo-(12->SVALREE* (MONTOP/100))
                  ELSE
                     saldo=saldo-((12->VALCRED*(12->SREEM/100))* (MONTOP/100))
                  ENDIF
             CASE fec1=12->TFECHR
                  IF 12->TVALREE<>0
                     saldo=saldo-(12->TVALREE* (MONTOP/100))
                  ELSE
                     saldo=saldo-((12->VALCRED*(12->TREEM/100))*(MONTOP/100))
                  ENDIF
             CASE fec1=12->CFECHR
                  IF 12->CVALREE<>0
                     saldo=saldo-(12->CVALREE* (MONTOP/100))
                  ELSE
                     saldo=saldo-((12->VALCRED*(12->CREEM/100))* (MONTOP/100))
                  ENDIF
             CASE fec1=12->QFECHR
                  IF 12->QVALREE<>0
                     saldo=saldo-(12->QVALREE* (MONTOP/100))
                  ELSE
                     saldo=saldo-((12->VALCRED*(12->QREEM/100))* (MONTOP/100))
                  ENDIF
          ENDCASE
       ENDDO
       SKIP
    ENDDO
    SET RELATION TO
    SET FILTER TO
    CLOSE 12
    CLOSE 20
 ENDIF

 SELECT 11
 SEEK 34
 uso = 11->COSTOTOTAL
 uso = uso + aux
 REPLACE 11->COSTOTOTAL WITH uso
 aux:=uso:=0



***************************************************************************

 Endcase




    
ENDIF



*repor form c:\censo99\finca.frx preview