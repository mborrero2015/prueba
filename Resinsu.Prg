 Select Sum(area) as toarea from clot into cursor garea
  Select Count(*) As Qtidad From clot Into Cursor cctos  
     cutos = cctos.Qtidad
     
      if cutos = 1 

          noljj = "clot.nomlote"
          gc_lotes1=   &noljj + "    "+"("+Alltrim(str(garea.toarea));
                    +" Ha)" 
                  
      else
         Select clot
	      Go top
         Do while !eof() 
           noljj = "clot.nomlote"
	       suljj = "clot.area"
	       gc_lotes2 = gc_lotes2+ &noljj + "   -   "+"(" ;
	       + Alltrim(str(&suljj))+" Ha)  "  
	        Skip
	     Enddo
	     gc_lotes1="CONSOLIDADOS " + "( "+ alltrim(str(garea.toarea))+"   Ha. )"
	  endif
        Select clot
	     Go top
	   Do while !eof() 
	     ljjos = "clot.numcontrol"
	     	     
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
	     Skip
	   Enddo
         
         cualot  = left(cualot,(Len(cualot)-3))      
         cualot1 = left(cualot1,(Len(cualot1)-3))
 	     cualot2 = left(cualot2,(Len(cualot2)-3))
	     cualot3 = left(cualot3,(Len(cualot3)-3))
	     cualot4 = left(cualot4,(Len(cualot4)-3))
	     cuapro  = left(cuapro,(Len(cuapro)-3))
  
  	   SELECT Aplica.codaplic FROM sacfadb!aplica ;
	    WHERE &cualot3 ;
	     ORDER BY Aplica.codaplic Into Cursor Ap123
       **	 &&AND LEFT(Aplica.codaplic,1) = "A";
	  Select ap123   
	   Go top
	
	   Do while !eof() 
	     cuapl = cuapl + " Auxapli.codaplic = ";
	       + Chr(34)+Alltrim(Ap123.codaplic)+Chr(34) +" OR" 
	     Skip
	   Enddo
	        
	  cuapl = left(cuapl,(Len(cuapl)-3))
      
  SELECT Labculti.codaplica As codpli, SUM(Labculti.costotal) AS ctota;
     FROM sacfadb!labculti WHERE (&cualot) AND (Labculti.codlabor = 705 ;
       OR Between(codlabor, 300, 399)) GROUP BY Labculti.codaplica ;
        ORDER BY Labculti.codaplica Into Table Reco2
 
     Select Auplx
     Append From Reco2 Fields ctota, codpli
 
 *    Select sum(total) As vtot From Sureco Where Between(codlabor, 300, 399) ;
 *     into cursor lin6 && LABOR DE LA SEMILLA
 
    SELECT Jorculti.codaplica As codpli, SUM(Jorculti.costotal) As ctota;
     FROM sacfadb!jorculti WHERE (&cualot1) AND ( Jorculti.codlabor = 705 ;
     OR Between(codlabor, 300, 399)) GROUP BY Jorculti.codaplica ;
      ORDER BY Jorculti.codaplica Into Table Reco2
 
     Select Auplx
      Append From Reco2 Fields ctota, codpli
 
    SELECT Maqculti.codaplica As codpli, SUM(Maqculti.costotal) As ctota;
    FROM sacfadb!maqculti  WHERE (&cualot2) AND ( Maqculti.codlabor = 705 ;
     OR Between(codlabor, 300, 399)) GROUP BY Maqculti.codaplica ;
      ORDER BY Maqculti.codaplica Into Table Reco2
 
        Select Auplx
         Append From Reco2 Fields ctota, codpli


 ******** Costo Aplicacion por Tipo de Insumo*******************************
 ****
 
  if  !empty(cuapl)
     SELECT  Auxapli.codaplic, SUM(Auxapli.cantusada)As Ctusa ;
      FROM sacfadb!auxapli WHERE &cuapl GROUP BY Auxapli.codaplic ;
        ORDER BY Auxapli.codaplic Into Cursor Pusado

     Select Auplx.codpli, Pusado.Ctusa As cusa, Sum(Auplx.ctota) As Total ;
       From Auplx, pusado  Group By codpli Into Cursor AAppll;
        Where Auplx.codpli = pusado.codaplic and !delete()
     
     SELECT Auxapli.codinsum, ;
      Round(SUM((Auxapli.cantusada/aappll.cusa)*aappll.total),0) As totapl ;
       FROM  sacfadb!Auxapli, aappll GROUP BY Auxapli.codinsum Into cursor tApli ;
        Where  Auxapli.codaplic = aappll.codpli &&&cuapl and

*adasd=asdsad
  
     SELECT Auxapli.codinsum, Auxapli.codtipo,;
      ROUND(SUM(Auxapli.cantusada) ,0) As Cantus,;
       ROUND(Sum((Auxapli.cantusada*(Compins.costunit/Compins.equivale))+;
                  (Auxapli.cantusada*Compins.sobrecosto)),0) As vtot ,;
        COUNT(Aplica.codaplic) As Noa ;
         FROM  sacfadb!aplica INNER JOIN sacfadb!auxapli;
          INNER JOIN sacfadb!compins ON  Compins.codcomp = Auxapli.codcomp ;
           ON  Aplica.codaplic = Auxapli.codaplic WHERE &cuapl ;
            GROUP BY Auxapli.codinsum Into Cursor Regein 

  
      Select Tipinsu.tipinsumo as tipo, Insumo.nominsum, Insumo.codunidad,;
        Regein.*, tapli.totapl As valplic, ;
         (Regein.vtot+tapli.totapl) As vatal ;
          From Insumo, Tipinsu ,Regein full join tapli on ;
            Regein.codinsum = tapli.codinsum ;
           Where Regein.codinsum = Insumo.codinsum and;
                 Regein.codtipo =  Tipinsu.codtipo ;
           Into Cursor rege258  Order by Regein.codtipo
 
   Select rege258.*, Undmedis.abrenom From rege258, Undmedis ;
    where Undmedis.codunidad = rege258.codunidad Into Table Regfin.dbf    
    
    
      Report form infregein.frx preview
  endif      
************************************************************************************
RETURN