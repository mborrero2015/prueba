Declare  pctj[34]

   Select qcult
    Go top

  Do while !eof()
     codcu = qcult.codcultivo
     Select * From clot Where Clot.codcultivo = codcu Into Cursor &clot    
     suljj= clot+".Area"
     Select Sum(&suljj) as toarea from &clot into cursor garea
     Select Count(*) As Qtidad From &clot Into Cursor cctos  
     cutos = cctos.Qtidad
     
      if cutos = 1 

          noljj = clot+".nomlote"
          gc_lotes1=   &noljj + "    "+"("+Alltrim(str(garea.toarea));
                    +" Ha)" 
          gc_lotes  = " "
      else
         Select &clot
	      Go top
         Do while !eof() 
           noljj = clot+".nomlote"
	       suljj = clot+".area"
	       gc_lotes2 = gc_lotes2+ &noljj + "   -   "+"(" ;
	       + Alltrim(str(&suljj))+" Ha)  "  
	        Skip
	     Enddo
	     gc_lotes1="CONSOLIDADOS " + "( "+ alltrim(str(garea.toarea))+"   Ha. )"
	  endif
	  
	    Select &clot
	    Go top
   
	   Do while !eof() 
	     ljjos = clot+".numcontrol"
	     afded = clot+".codlote"  	     
	     
    	 cualot = cualot + " Labculti.numcontrol = ";
	       + Alltrim(str(&ljjos)) +" OR" 
	     cualot1 = cualot1  + " Jorculti.numcontrol = ";
	      + Alltrim(str(&ljjos)) +" OR" 
	     cualot2 = cualot2  + " Maqculti.numcontrol = ";
	      + Alltrim(str(&ljjos)) +" OR" 
	     cualot3 = cualot3  + " Aplica.numcontrol = ";
	      + Alltrim(str(&ljjos)) +" OR"      
	      cualot4 = cualot4  + " Ventas.numcontrol = ";
	      + Alltrim(str(&ljjos)) +" OR"      
	     cuapro = cuapro  + " procion.numcontrol = ";
	      + Alltrim(str(&ljjos)) +" OR"        
	     Lotes = Lotes  +  " codlote = ";
	      + Alltrim(str(&afded)) +" OR"        
	     Skip
	   Enddo
         
         cualot  = left(cualot,(Len(cualot)-3))
         cualot1 = left(cualot1,(Len(cualot1)-3))
 	     cualot2 = left(cualot2,(Len(cualot2)-3))
	     cualot3 = left(cualot3,(Len(cualot3)-3))
	     cualot4 = left(cualot4,(Len(cualot4)-3))
	     cuapro  = left(cuapro,(Len(cuapro)-3))
         Lotes   = left(Lotes,(Len(Lotes)-3))
         
  	   SELECT Aplica.codaplic FROM sacfadb!aplica;
	    WHERE (&cualot3) AND LEFT(Aplica.codaplic,1) = "A";
	     ORDER BY Aplica.codaplic Into Cursor Ap123
	
	  Select ap123   
	   Go top
	
	   Do while !eof() 
	     cuapl = cuapl + " insaplica.codaplic = ";
	       + Chr(34)+Alltrim(Ap123.codaplic)+Chr(34) +" OR" 
	     Skip
	   Enddo
	        
      cuapl = left(cuapl,(Len(cuapl)-3))
      g_nomcul = qcult.cultivo
      
**********       *************         **************         *************
******	   pasar a rutina si se puede ? para  acoplarlo con administracion 
*****     % costo directo
**********       *************         **************         *************

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
     Into Cursor sureco Group By codlabor Where !delete()
 
  
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
      into cursor lin6 && LABOR DE LA SEMILLA
      
    Select Sum((Auxapli.cantusada*(Compins.costunit/Compins.equivale)) ;
               +(Auxapli.cantusada*Compins.sobrecosto)) As vtot ; 
      From  sacfadb!aplica Inner Join sacfadb!auxapli ;
       Inner Join sacfadb!compins ;
        On  Compins.codcomp = Auxapli.codcomp ;
        On  Aplica.codaplic = Auxapli.codaplic ;
         Where (&cualot3) And Left(Auxapli.codaplic,1) = "S" ;
          Into Cursor lin5 &&VALOR DE LA SEMILLA
          
     Lin4 =  Round((lin5.vtot + lin6.vtot),0)&&SIEMBRA
     
      
     Select sum(total) As vtot  From Sureco Where codlabor = 506 OR ;
      codlabor = 507 OR codlabor = 501 into cursor lin8 && AGUA
 
     Select sum(total) As vtot From Sureco Where codlabor = 500 OR;
        Between(codlabor, 502, 504) OR codlabor = 508 ;
            into cursor lin9 && MANTENIMIENTO
    
    
     Lin7 = Round((lin8.vtot+lin9.vtot),0)&&RIEGO
     
 ******** Costo Aplicacion por Tipo de Insumo*******************************

 ****   por   INSAPLICA ****
 
  if  !empty(cuapl)
     SELECT insaplica.codaplic, SUM(insaplica.cantotal)As Cusa ;
      FROM sacfadb!insaplica WHERE &cuapl GROUP BY insaplica.codaplic ;
       ORDER BY insaplica.codaplic Into Cursor Pusado
 
     Select Auplx.codpli, Pusado.Cusa, Sum(Auplx.ctota) As Total ;
       From Auplx, Pusado  Group By codpli Into cursor AAppll;
        Where Auplx.codpli = Pusado.codaplic and !delete()

******** problemaa ???     

     SELECT insaplica.codtipo,aappll.cusa,;
      Round(SUM((insaplica.cantotal/aappll.cusa)*aappll.total),0) As totapl ;
       FROM  sacfadb!insaplica,aappll;
        GROUP BY insaplica.codtipo Into cursor tApli;
         Where (&cuapl) and insaplica.codaplic = aappll.codpli

   endif     

 ****************************************************************************

     Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
                +(Auxapli.cantusada * Compins.sobrecosto)) As vtot ; 
       From  sacfadb!aplica Inner Join sacfadb!auxapli ;
        Inner Join sacfadb!compins ;
         On  Compins.codcomp = Auxapli.codcomp ;
         On  Aplica.codaplic = Auxapli.codaplic ;
          Where (&cualot3) And Left(Auxapli.codaplic,1) = "A" ;
                 And Auxapli.codtipo = 2 Into Cursor lin11 &&FERTILIZANTES


  if  !empty(cuapl)
     Select Sum(totapl)As vtot From tapli where tapli.codtipo = 2 ;
      Into Cursor lin12 && Aplicacion de Fertilizantes

      Lin10 = Round((Lin11.vtot+Lin12.vtot),0)&&Fertilizacion
  else
      Lin10 = Round((Lin11.vtot),0)&&Fertilizacion
      Lin12 = 0
  endif
       
     Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
                 +(Auxapli.cantusada * Compins.sobrecosto)) As vtot ; 
       From  sacfadb!aplica Inner Join sacfadb!auxapli ;
        Inner Join sacfadb!compins ;
         On  Compins.codcomp = Auxapli.codcomp ;
         On  Aplica.codaplic = Auxapli.codaplic ;
          Where (&cualot3) And Left(Auxapli.codaplic,1) = "A" ;
                And Auxapli.codtipo = 5 Into Cursor lin14 &&HERBICIDAS

 
      Select sum(total) As vtot From Sureco Where Between(codlabor, 203, 299);
       OR codlabor = 200 Or codlabor = 201 Into cursor lin16 
        && CONTROL malezas desroje etc......
                
  if  !empty(cuapl)
     Select Sum(totapl)As vtot From tapli where tapli.codtipo = 5 ;
      Into Cursor lin15 && Aplicacion de Herbicidas

     Lin13 =  Round((Lin14.vtot+Lin15.vtot+Lin16.vtot),0)&&Control Malezas
  else
     Lin13 =  Round((Lin14.vtot+Lin16.vtot),0)&&Control Malezas
     Lin15 = 0
  endif  
 ***************************************************************************
 
    Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
                 +(Auxapli.cantusada * Compins.sobrecosto)) As vtot ; 
        From  sacfadb!aplica Inner Join sacfadb!auxapli ;
         Inner Join sacfadb!compins ;
          On  Compins.codcomp = Auxapli.codcomp ;
          On  Aplica.codaplic = Auxapli.codaplic ;
           Where (&cualot3) And Left(Auxapli.codaplic,1) = "A" ;
                 And (Auxapli.codtipo = 4 or Auxapli.codtipo = 6 or ;
                   Auxapli.codtipo = 7 ) Into Cursor lin18 &&INSECTICIDAS, 
                   *RODENTICIDAS Y NEMATICIDAS APLICADOS
  
    Select sum(total) As vtot From Sureco Where codlabor = 20 ;
     Into cursor lin20 && CONTROL plaglas pajareo
  
   if  !empty(cuapl)
    Select Sum(totapl)As vtot From tapli where (tapli.codtipo = 4 or;
                                   tapli.codtipo = 6 or tapli.codtipo = 7) ;
     Into Cursor lin19 && Aplicacion de Insecticidas,rodenticidas y  
       *                    nematicidas 
    
    Lin17 =  Round((Lin18.vtot+Lin19.vtot+Lin20.vtot),0)&&Control Plagas
   else
    Lin17 =  Round((Lin18.vtot+Lin20.vtot),0)&&Control Plagas 
    Lin19 = 0
   endif 
****************************************************************************
    Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
               +(Auxapli.cantusada * Compins.sobrecosto)) As vtot ; 
       From  sacfadb!aplica Inner Join sacfadb!auxapli ;
        Inner Join sacfadb!compins ;
         On  Compins.codcomp = Auxapli.codcomp ;
         On  Aplica.codaplic = Auxapli.codaplic ;
          Where (&cualot3) And Left(Auxapli.codaplic,1) = "A" ;
                 And Auxapli.codtipo = 3 Into Cursor lin22 &&FUNGICIDAS

   if  !empty(cuapl)
    Select Sum(totapl)As vtot From tapli where tapli.codtipo = 3 ;
      Into Cursor lin23 && Aplicacion de fungicidas
  
    Lin21 = Round((lin22.vtot + lin23.vtot),0)&&CONTROL DE ENFERMEDADES
   else
    Lin21 = Round((lin22.vtot),0)&&CONTROL DE ENFERMEDADES
    Lin23 = 0
   endif
   
   Select Sum((Auxapli.cantusada * (Compins.costunit/Compins.equivale)) ;
               +(Auxapli.cantusada * Compins.sobrecosto)) As vtot ; 
      From  sacfadb!aplica Inner Join sacfadb!auxapli ;
       Inner Join sacfadb!compins ;
        On  Compins.codcomp = Auxapli.codcomp ;
        On  Aplica.codaplic = Auxapli.codaplic ;
         Where (&cualot3) And Left(Auxapli.codaplic,1) = "A" ;
                And (Auxapli.codtipo = 9 or Auxapli.codtipo = 8) Into Cursor lin25 &&INSUMOS

   if  !empty(cuapl)
    Select Sum(totapl)As vtot From tapli where (tapli.codtipo = 9 or tapli.codtipo = 8) ;
      Into Cursor lin26 && Aplicacion de fungicidas
  
    Lin24 = Round((lin25.vtot + lin26.vtot),0)&&OTROS INSUMOS
   else
    Lin24 = Round((lin25.vtot),0)&&OTROS INSUMOS
    Lin26 = 0
   endif            
   
    Select sum(total) As vtot From Sureco Where Between(codlabor, 400, 499);
          into cursor lin27 && RECOLECCION

    Select sum(total) As vtot From Sureco Where Between(codlabor, 600, 699);
          into cursor lin28 && TRANSPORTE

    Select sum(total) As vtot From Sureco Where codlabor = 801 Or ;
     codlabor = 802 into cursor lin29 && VARIOS Alimentacion y Celaduria

    Lin30 = Round((Lin1.vtot+Lin2.vtot+Lin3.vtot+Lin4+Lin7+Lin10+Lin13+Lin17+;
             Lin21+Lin24+Lin27.vtot+Lin28.vtot+Lin29.vtot),0)

**************SUBTOTALES

     Select sum(total) As vtot From Sureco Where codlabor = 803  ;
      Into cursor lin31A && ADMINISTRACION
 
     Select total As vtot From Sureco Where codlabor = 804  ;
      Into cursor lin31B && ADMINISTRACION
 
    Lin31 = lin31A.vtot + ((lin30*lin31B.vtot)/100)


***************************************************************************
 *************<<  RESUMEN DE COSTOS DE INTERESES  >>*************
****************ESTO SE ENGRANA CON LA PARTE DE EFRA***********

Do placred With g_ano, g_semestre, Lotes
lin32 = interes()

***************
***************************************************************************



 * IF FILE(auxvent)

   Select Sum(((valventa*tonels)*((reten+cuota+otros)/100))) As vtot;
    From Ventas Where  &cualot4 Into Cursor Lin33 &&COSTOS DE IMPUESTOS  

*   ENDif

   Lin34 = Round((lin30+lin31+lin33.vtot),0)&&T O T A L E S ****lin32,
   
    Select Sum(totalprod) As tpro From Procion Where &cuapro;
    Into Cursor toprod


**********       *************         **************         *************
**********       *************         **************         *************
**********       *************         **************         *************




*******************COSTO POR HECTAREA Y POR TONELADA***********************
  Select Resuconta
   Set Order To 1
  
   For i = 1 to 34
      ci = i
        parlin = "lin"+Alltrim(str(i))+".vtot"
  
      If parlin = "lin4.vtot" or parlin = "lin7.vtot"  ;
        or parlin = "lin10.vtot" or parlin = "lin13.vtot" ;
        or parlin = "lin17.vtot" or parlin = "lin21.vtot" ;
        or parlin = "lin24.vtot" or parlin = "lin30.vtot" ;
        or parlin = "lin32.vtot" or parlin = "lin34.vtot" ;
        or parlin = "lin31.vtot" Then       

        Parlin = left(parlin,(len(parlin)-5))

      Endif
        
      If empty(cuapl) and (parlin = "lin12.vtot" ;
      or parlin = "lin15.vtot" or parlin = "lin19.vtot" ;
      or parlin = "lin23.vtot" or parlin = "lin26.vtot" ) Then

               Parlin = left(parlin,(len(parlin)-5))
      Endif
      
     if garea.toarea <> 0 then
      coshec = Round((&parlin/garea.toarea),0)
     endif
     if toprod.tpro <> 0   then
      coston = Round((&parlin/toprod.tpro),0)
     endif
     if Lin34 <> 0  
      cospor = Round(((&parlin/Lin34)*100),2)
      pctj[i]= cospor
     endif     
      Do case
         case ci = 30 
            ci = ci + 1 
             seek ci
              Replace costototal With &parlin, costoha With coshec, ;
               costoton with coston, porcentaje with cospor
   
         Case ci = 31 or ci = 32 or ci = 33 
             ci = ci + 2
             seek ci
               Replace costototal With &parlin, costoha With coshec, ;
                costoton with coston, porcentaje with cospor
         Case ci = 34 
             ci = ci + 3 
             seek ci
               Replace costototal With &parlin, costoha With coshec, ;
                 costoton with coston, porcentaje with cospor    
       
         Otherwise 

             seek ci
               Replace costototal With &parlin, costoha With coshec, ;
                 costoton with coston, porcentaje with cospor
     
        EndCase
     Endfor
       if plan = 1 
         *********
        If cutos = 1 Then
          Select Infculti
          g_actual = RECNO()  
          UPDATE Infculti SET toneladas = toprod.tpro, ;
                            asistencia = pctj[1],    arriendo = pctj[2], ;
                                prepar = pctj[3],     siembra = pctj[4], ;  
                              semilla  = pctj[5],     labsiem = pctj[6], ;
                                 riego = pctj[7],        agua = pctj[8], ;
                                  mmto = pctj[9],      fertil = pctj[10], ;
                               insfert = pctj[11],     apfert = pctj[12], ;
                               malezas = pctj[13],     herbic = pctj[14], ;
                                apherb = pctj[15],    manherb = pctj[16], ;
                                plagas = pctj[17],     insect = pctj[18], ;
                              apinsect = pctj[19],  maninsect = pctj[20], ;
                                enferm = pctj[21],     fungic = pctj[22], ;
                              apfungic = pctj[23],     totins = pctj[24], ;
                               insumos = pctj[25],    apinsum = pctj[26], ;
                               recolec = pctj[27], transporte = pctj[28], ;
                                varios = pctj[29],   subtotal = pctj[30], ;
                                 admon = pctj[31],  intereses = pctj[32], ;
                             impuestos = pctj[33],    totales = pctj[34], ;
                              costotcc = Lin34 ;
             WHERE  recno() = g_actual 
        Endif            
         **********
         
         Report form infresuco.frx preview
         
            
       else
         &&Resumen General de Costos Grafica  
         Select actividad, porcentaje From Resuconta;
          Into cursor graftor where  !Between(codact,5,6)   and !Between(codact,8,9)   and ;
          !Between(codact,11,12) and !Between(codact,14,16) and !Between(codact,18,20) and ;
          !Between(codact,22,23) and !Between(codact,25,26) and !Between(codact,30,32) and ;
          !Between(codact,36,38) and porcentaje <> 0

			
          encg = "Costo/Tn"
          opc1 = field (2)
          Do case
             case  alltrim(g_nomcul) = "Arroz"
                camg = "grareco1"
                Thisform.con_grafi.gra_reco1.Controlsource =  "Graficos.grareco1"
             case  alltrim(g_nomcul) = "Sorgo"
                camg = "grareco2"        
                Thisform.con_grafi.gra_reco1.Controlsource =  "Graficos.grareco2"
             case  alltrim(g_nomcul) = "Soya"
                camg = "grareco3"
                Thisform.con_grafi.gra_reco1.Controlsource =  "Graficos.grareco3"
             case  alltrim(g_nomcul) = "Maiz"
                camg = "grareco4"
                Thisform.con_grafi.gra_reco1.Controlsource =  "Graficos.grareco4"
             case  alltrim(g_nomcul) = "Algodon"
                camg = "grareco5"
                Thisform.con_grafi.gra_reco1.Controlsource =  "Graficos.grareco5"
          Endcase
       
           LcData= " " + TAB + encg + CRLF
           scan
             LcData = LcData + Graftor.actividad
             LcData = LcData + TAB
             LcData = LcData + alltrim(STR(&opc1)) + TAB +CRLF
           endscan
             
             
           select Graficos
           
             append general &camg Data LcData

           Thisform.con_menu.visible = .f.
           Thisform.con_grafi.visible = .t.
           Thisform.con_grafi.con_cult.txt_cultivo.value = g_nomcul
           
            
       endif  
  
   cualot    = ""
   cualot1   = ""
   cualot2   = ""
   cualot3   = ""  
   cualot4   = ""  
   cuapl     = ""
   cuapro    = ""
   Lotes     = ""
   gc_lotes  = "LISTA DE LOTES CONSOLIDADOS :"
   gc_lotes1 = ""  
   gc_lotes2 = ""  
   
     Select qcult
      Skip 
      Clot = "Clot"+alltrim(str(val(Right(Clot,1))+1))
   Enddo
    
RETURN