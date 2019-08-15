
*******************************
*****VERIFICA SI INVENTARIOS ESTAN BIEN EN CASO DE ERROR ENVIA UN MENSAJE
DO USADA WITH "lotesfin"
DO USADA WITH "infculti"
DO USADA WITH "compins"
DO USADA WITH "auxapli"
DO USADA WITH "insaplica"
DO USADA WITH "ufc"
DO USADA WITH "auxufc"
DO USADA WITH "combust"
DO USADA WITH "auxcomb"
***************
**********************Errores de Asteriscos****
SELECT cantotal/area as cantaplic1,codinsap from insaplica inner join infculti inner join lotesfin on ;
	insaplica.numcontrol=infculti.numcontrol on infculti.codlote=lotesfin.codlote ;
	where cantaplic>9999 and !deleted() order by codinsap into cursor c_asterisk
SELECT c_asterisk
GO top
SCAN
	UPDATE insaplica set cantaplic=c_asterisk.cantaplic1 where cantaplic>9999 and insaplica.codinsap =c_asterisk.codinsap
ENDSCAN
*****************
*ELIMINA INSUMOS CON CODIGO -1
Delete FROM COMPINS WHERE CODINSUM=-1
Delete FROM AUXAPLI WHERE CODINSUM=-1
Delete FROM insaplica WHERE CODINSUM=-1
*******************

SW=.F.


SELECT COMPINS.CODINSUM, SUM(COMPINS.CANTINS) AS COMTOTINS ,  SUM(COMPINS.USOTOTINS)AS COMTOT  ;
	FROM COMPINS  GROUP BY CODINSUM;
	INTO CURSOR TEM_INV_COMP ORDER BY CODINSUM

SELECT insaplica.CODINSUM, SUM(insaplica.cantotal) AS INSTOT ;
	FROM insaplica GROUP BY CODINSUM;
	INTO CURSOR TEM_INV_INSA ORDER BY CODINSUM

SELECT  AUXAPLI.CODINSUM, SUM(AUXAPLI.CANTUSADA) AS AUXTOT;
	FROM AUXAPLI  GROUP BY CODINSUM;
	INTO CURSOR TEM_INV_AUXA ORDER BY CODINSUM

SELECT  AUXUFC.CODINSUM, SUM(AUXUFC.CANTUSADA) AS AUXUFC;
	FROM AUXUFC  GROUP BY CODINSUM;
	INTO CURSOR TEM_AUXUFC ORDER BY CODINSUM


SELECT  UFC.CODINSUM, SUM(UFC.cantotal) AS UFC;
	FROM UFC  GROUP BY CODINSUM;
	INTO CURSOR TEM_UFC ORDER BY CODINSUM

SELECT  COMBUST.CODINSUM, SUM(COMBUST.cantotal) AS COMBUST;
	FROM COMBUST  GROUP BY CODINSUM;
	INTO CURSOR TEM_COMBUST ORDER BY CODINSUM

SELECT  AUXCOMB.CODINSUM, SUM(AUXCOMB.CANTUSADA) AS AUXCOMB;
	FROM AUXCOMB GROUP BY CODINSUM;
	INTO CURSOR TEM_AUXCOMB ORDER BY CODINSUM




*!*	select tem_inv_comp.codinsum,comtotins,comtot, auxtot,  tem_inv_comp.codcomp as compCodcomp ,tem_inv_auxa.codcomp as auxCodcomp from tem_inv_comp ;
*!*		full join tem_inv_auxa on tem_inv_auxa.codcomp = tem_inv_comp.codcomp;
*!*		group by tem_inv_comp.codcomp order by tem_inv_comp.codinsum where comtot!=0 into cursor tem_com_aux

*!*	select tem_inv_insa.codinsum,instot, auxtot  from tem_inv_insa ;
*!*		full join tem_inv_auxa on tem_inv_auxa.codinsum = tem_inv_insa.codinsum;
*!*		group by tem_inv_insa.codinsum order by tem_inv_insa.codinsum into cursor tem_ins_aux

*!*	select tem_com_aux.codinsum,comtotins,comtot,instot,tem_com_aux.auxtot from tem_com_aux full join tem_ins_aux on ;
*!*		tem_com_aux.codinsum=tem_ins_aux.codinsum order by tem_com_aux.codinsum INTO cursor tem_inv_total


****************

SELECT TEM_INV_COMP.CODINSUM,;
	SUM(TEM_INV_COMP.COMTOTINS) AS COMTOTINS,;   	&&&& total insumo
SUM(TEM_INV_COMP.COMTOT) AS COMTOT,;  			&&&& total compins
SUM(TEM_INV_INSA.INSTOT) AS INSTOT,;
	SUM(TEM_INV_AUXA.AUXTOT) AS AUXTOT ; 			&&&& total auxtotal
FROM TEM_INV_INSA inner JOIN TEM_INV_COMP;
	inner JOIN TEM_INV_AUXA ;
	ON  TEM_INV_AUXA.CODINSUM = TEM_INV_COMP.CODINSUM ;
	ON  TEM_INV_INSA.CODINSUM = TEM_INV_COMP.CODINSUM ;
	GROUP BY TEM_INV_COMP.CODINSUM;
	ORDER BY TEM_INV_COMP.CODINSUM;
	INTO CURSOR TEM_INV_TOTAL

SELECT  SUM (TEM_COMBUST.COMBUST) AS COMBUST, SUM (TEM_AUXCOMB.AUXCOMB) AS AUXCOMB;
	,TEM_INV_COMP.CODINSUM;
	FROM TEM_COMBUST inner JOIN TEM_INV_COMP;
	inner JOIN TEM_AUXCOMB ;
	ON  TEM_AUXUFC.CODINSUM = TEM_INV_COMP.CODINSUM ;
	ON TEM_UFC.CODINSUM = TEM_INV_COMP.CODINSUM ;
	GROUP BY TEM_INV_COMP.CODINSUM;
	ORDER BY TEM_INV_COMP.CODINSUM;
	INTO CURSOR TEM_COMB_TOTAL

SELECT  SUM (TEM_UFC.UFC) AS UFC, SUM (TEM_AUXUFC.AUXUFC) AS AUXUFC,TEM_INV_COMP.CODINSUM;
	FROM TEM_UFC inner JOIN TEM_INV_COMP;
	inner JOIN TEM_AUXUFC ;
	ON  TEM_AUXUFC.CODINSUM = TEM_INV_COMP.CODINSUM ;
	ON TEM_UFC.CODINSUM = TEM_INV_COMP.CODINSUM ;
	GROUP BY TEM_INV_COMP.CODINSUM;
	ORDER BY TEM_INV_COMP.CODINSUM;
	INTO CURSOR TEM_UFC_TOTAL

****** ******** ****** ******** ****** ******** ****** ******** ****** ********
**** para detectar las inconsistencias por tipo de insumo
****** ******** ****** ******** ****** ******** ****** ******** ****** ********


SELECT * FROM TEM_INV_TOTAL WHERE COMTOTINS < COMTOT OR COMTOT != INSTOT ;
	OR COMTOT != AUXTOT OR INSTOT != AUXTOT OR ISNULL(INSTOT) OR ISNULL(COMTOT) OR ISNULL(AUXTOT);
	INTO CURSOR TEM_INV_DIF
*SELECT 	COUNT(*) FROM tem_inv_dif INTO array error_inv
SELECT * FROM TEM_UFC_TOTAL WHERE UFC!=AUXUFC INTO CURSOR TEM_UFC_DIF
SELECT 	COUNT(*) FROM TEM_UFC_DIF INTO ARRAY ERROR_UFC

SELECT * FROM TEM_COMB_TOTAL WHERE COMBUST!=AUXCOMB INTO CURSOR TEM_COMB_DIF
SELECT 	COUNT(*) FROM TEM_COMB_DIF INTO ARRAY ERROR_COMB


SELECT IIF(ISNULL(AUXUFC),IIF(ISNULL(AUXTOT),0,AUXTOT),IIF(ISNULL(AUXTOT),AUXUFC,AUXTOT+AUXUFC));
	AS AUX, IIF(ISNULL(UFC),IIF(ISNULL(INSTOT),0, INSTOT),IIF(ISNULL(INSTOT), UFC, INSTOT +UFC));
	AS INS, TEM_INV_TOTAL.CODINSUM,COMTOT,COMTOTINS FROM TEM_UFC_TOTAL ;
	RIGHT JOIN TEM_INV_TOTAL ON TEM_INV_TOTAL.CODINSUM = TEM_UFC_TOTAL.CODINSUM INTO CURSOR TEMP_ERROR
SELECT * FROM TEMP_ERROR WHERE ISNULL(AUX) OR ISNULL (INS) OR  AUX!=INS OR AUX!=COMTOT and !deleted() INTO TABLE ERRORES

SELECT 	COUNT(*) FROM ERRORES INTO ARRAY ERROR_INV

****no coinciden las cantidades del inventario codcomp por codcomp
SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, USOTOTINS,COMPINS.codcomp,COMPINS.CODINSUM,CANTINS ;
	FROM COMPINS inner JOIN AUXAPLI ON AUXAPLI.codcomp=COMPINS.codcomp where !deleted() ;
	GROUP BY COMPINS.codcomp ORDER BY AUXAPLI.codcomp INTO CURSOR C_COMPARA

SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,USOTOTINS,COMPINS.codcomp;
	FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted();
	GROUP BY COMPINS.codcomp ORDER BY AUXUFC.codcomp  INTO CURSOR C_COMPARA1

SELECT CODINSUM,USOAUX,C_COMPARA.USOTOTINS AS USOTOTINS, C_COMPARA1.USOTOTINS AS USAUXUFC,CANTINS,C_COMPARA.codcomp AS CODCOMPCOM,USOUFC,C_COMPARA1.codcomp AS CODCOMPAUX ;
	FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp where !deleted() INTO TABLE T_COMP2B
SELECT USOUFC,codcomp AS  CODCOMPAUX, USOTOTINS AS USOTOTINS FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT  CODCOMPCOM FROM T_COMP2B) INTO TABLE T_COMP3
SELECT T_COMP2B
APPEND FROM T_COMP3
SELECT IIF(ISNULL(USOUFC),000000000.00000+USOAUX,USOUFC+USOAUX) AS USOAUX1,* ;
	FROM T_COMP2B INTO CURSOR C_COMP2A
*select	usoaux1+usoufc1 as usoauxi,* from c_comp2a into cursor c_comp3 order by codcomp_a
SELECT COUNT(*) FROM C_COMP2A WHERE USOAUX1!=USOTOTINS OR ISNULL(USOTOTINS) OR ISNULL(USOAUX1) and !deleted()  INTO CURSOR CCOMP_AUX
COMP_AUX =0
COMP_AUX = CCOMP_AUX.CNT
*Por si hay dato en algun compins que no haya en auxapli

SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, USOTOTINS,COMPINS.codcomp,COMPINS.CODINSUM,CANTINS ;
	FROM COMPINS left JOIN AUXAPLI ON AUXAPLI.codcomp=COMPINS.codcomp ;
	GROUP BY COMPINS.codcomp ORDER BY AUXAPLI.codcomp having isnull(USOAUX) and USOTOTINS!=0 where !deleted() into cursor c_solocompins
*corrige este error de una ves
SCAN
	SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC, USOTOTINS,COMPINS.codcomp,COMPINS.CODINSUM,CANTINS ;
		FROM COMPINS left JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp ;
		GROUP BY COMPINS.codcomp ORDER BY AUXUFC.codcomp where AUXUFC.codcomp= c_solocompins.codcomp and !deleted() having USOUFC!=USOTOTINS into cursor c_conufc
	IF _tally =0
*Si ha datos en ufc deje ese dato sino vuelvalo 0
*	UPDATE COMPINS set USOTOTINS=c_conufc.USOUFC where COMPINS.codcomp=c_solocompins.codcomp and !deleted()

	ELSE
		SCAN
			IF c_conufc.CANTINS>=c_conufc.USOUFC
				UPDATE COMPINS set USOTOTINS=c_conufc.USOUFC where COMPINS.codcomp=c_solocompins.codcomp and !deleted()
				IF _tally>0
					MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
						"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
				ENDIF
			ELSE
				SELECT sum(CANTINS-USOTOTINS) as libre,USOTOTINS,CANTINS,codcomp,CODINSUM from COMPINS where COMPINS.CODINSUM=c_conufc.CODINSUM into cursor c_libre
				IF c_libre.libre >c_conufc.USOUFC
					SELECT CANTINS-USOTOTINS as libre,CANTINS,USOTOTINS,codcomp,CODINSUM from COMPINS where (CANTINS-USOTOTINS)>0 order by libre desc into cursor c_libre_aplicar
					V_ACUM=c_conufc.USOUFC
*SCAN
					IF c_libre_aplicar.libre >c_conufc.USOUFC
						UPDATE AUXUFC set codcomp=c_libre_aplicar.codcomp where codcomp=c_solocompins.codcomp
						LOOP
					ELSE
						SELECT * from AUXUFC where codcomp= c_solocompins.codcomp into cursor c_elimina

						UPDATE UFC set CANTUSADA=CANTUSADA-c_elimina.CANTUSADA WHERE CODUFC=c_elimina.CODUFC
						Delete FROM UFC WHERE CANTUSADA<=0 &&pOR SI LA RESTA c_elimina LLEGA A 0
						UPDATE COMPINS SET USOTOTINS=0 where COMPINS.codcomp=c_solocompins.codcomp and !deleted()
						Delete from AUXUFC where codcomp=c_solocompins.codcomp
					ENDIF
*dERIA CORREGIRSE PARA QUE ENCUENTRE COMPINS VALORES LIBRES DONDE APLICAR
*!*							ELSE
*!*								Delete from AUXUFC where codcomp=c_libre_aplicar.codcomp
*!*								INSERT INTO AUXUFC c_conufc.USOUFC
*!*
*!*								UPDATE AUXAPLI SET CANTUSADA=C_ABORRAR.CANTUSADA-C_NUEVOCOMP.libre WHERE codcomp = C_ABORRAR.codcomp and codinsap= C_ABORRAR.codinsap
*!*										INSERT INTO AUXAPLI (codinsap,CODINSUM,CODTIPO,codcomp,CANTUSADA,CODAPLIC) ;
*!*											VALUES(C_ABORRAR.codinsap,C_ABORRAR.CODINSUM,C_ABORRAR.CODTIPO,C_NUEVOCOMP.codcomp,C_NUEVOCOMP.libre,C_ABORRAR.CODAPLIC)
*!*										Acum = Acum - C_NUEVOCOMP.libre
*!*
*!*
*!*
*!*								UPDATE AUXUFC set codcomp=c_libre_aplicar.codcomp where codcomp=c_solocompins.codcomp
*ENDIF
*	SELECT c_libre_aplicar
*ENDSCAN
				ELSE
*no hay valor suficiente para uso fuera de cultivos
					SELECT * from AUXUFC where codcomp= c_solocompins.codcomp into cursor c_elimina

					UPDATE UFC set CANTUSADA=CANTUSADA-c_elimina.CANTUSADA WHERE CODUFC=c_elimina.CODUFC
					Delete FROM UFC WHERE CANTUSADA<=0 &&pOR SI LA RESTA c_elimina LLEGA A 0
					UPDATE COMPINS SET USOTOTINS=0 where COMPINS.codcomp=c_solocompins.codcomp and !deleted()
					Delete from AUXUFC where codcomp=c_solocompins.codcomp
				ENDIF
			ENDIF
			SELECT c_conufc
		ENDSCAN
	ENDIF
	SELECT c_solocompins
ENDSCAN

SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,USOTOTINS, ;
	COMPINS.CODINSUM,CANTINS FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp ;
	GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp where !deleted()  INTO CURSOR C_COMPARA1

*****scan
*!*	SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,* FROM  T_COMP2 ;
*!*		HAVING USOAUXI!=USOTOTINS  order by CODINSUM INTO CURSOR C_COMP3

*si es mayor que cero no fue detectado por la sumatoria de invtotal es necesario corregirlo aqui
**************
*************
&&saca el mensaje de error con esta variable  comp_aux si es mayor a 0
***************************************************

IF ERROR_INV>0  or COMP_AUX >0
**ERROR EN INVENTARIOS
*********************** inconsistencias por insumos cruzados entre insaplica y auxapli

	SELECT insaplica.CODINSUM as insaplicod, AUXAPLI.CODINSUM as auxaplicod, insaplica.codinsap,cantotal,AUXAPLI.codcomp, ;
		CANTUSADA from insaplica left join AUXAPLI on insaplica.codinsap=AUXAPLI.codinsap where !deleted() ;
		having insaplicod!=auxaplicod into cursor cCruzado
	SCAN
		UPDATE AUXAPLI set CODINSUM= cCruzado.insaplicod where codcomp=cCruzado.codcomp and codinsap=cCruzado.codinsap
	ENDSCAN
***************Corrige inconsistencias entre insaplica e auxapli y por consiguiente con usototins de compins
***Cuando Insapli tiene todo los insumos y auxapli le falta
	SELECT SUM(AUXAPLI.CANTUSADA) AS AUXCANTUSADA, cantotal,insaplica.codinsap,insaplica.CODINSUM,insaplica.numcontrol,insaplica.CODAPLIC, ;
		insaplica.CODTIPO  FROM insaplica LEFT JOIN AUXAPLI ON insaplica.codinsap=AUXAPLI.codinsap ;
		GROUP BY insaplica.codinsap ORDER BY insaplica.codinsap where !deleted() HAVING  AUXCANTUSADA != cantotal ;
		INTO CURSOR C_APLICA

	SELECT C_APLICA
	SCAN
		IF C_APLICA.cantotal >C_APLICA.AUXCANTUSADA
*revisar los mayores de 0 y si es el mismo codcomp utilizado por auxapli el que tiene espacio los usa sino busca espacio en otro
* verifica igualmente si hay espacio
*select codcomp,cantusada from auxapli where codinsap=c_aplica.codinsap into cursor c_auxapli1
			SELECT CANTINS,USOTOTINS,CODINSUM FROM COMPINS WHERE CODINSUM=C_APLICA.CODINSUM ;
				HAVING (CANTINS-USOTOTINS) >0 INTO CURSOR C_INSUMO

			SELECT SUM(CANTINS-USOTOTINS) AS INVENTARIO FROM COMPINS WHERE !deleted() and CODINSUM=C_APLICA.CODINSUM ;
				INTO CURSOR C_HAYINVENTARIO
			IF C_HAYINVENTARIO.INVENTARIO >= (C_APLICA.cantotal-C_APLICA.AUXCANTUSADA)
*el actual codinsap - codcomp codcomp tiene el valor total de la aplicacion ????
				SELECT codcomp,CANTINS,USOTOTINS,CANTINS-USOTOTINS AS libre  FROM COMPINS WHERE codcomp ;
					IN (SELECT codcomp FROM AUXAPLI WHERE AUXAPLI.codinsap=C_APLICA.codinsap) HAVING libre >0 INTO CURSOR C_DISPONIBLES
				SCAN
					IF 	C_DISPONIBLES.CANTINS >= 	C_APLICA.cantotal
						UPDATE COMPINS SET USOTOTINS = COMPINS.USOTOTINS+ C_DISPONIBLES.libre WHERE codcomp=C_DISPONIBLES.codcomp
*!*					 		insert into auxapli (codinsap,codinsum,codtipo,codcomp,cantusada,codaplic) ;
*!*							values(c_aplica.codinsap,c_aplica.codinsum,c_aplica.codtipo, c_disponibles.codcomp,;
*!*									c_disponibles.usototins+ c_disponibles.libre,c_aplica.codaplic)
						UPDATE AUXAPLI SET CANTUSADA = C_DISPONIBLES.USOTOTINS+ C_DISPONIBLES.libre WHERE codcomp=C_DISPONIBLES.codcomp
					ELSE

					ENDIF
					SELECT C_DISPONIBLES
				ENDSCAN
			ELSE
*No hay suficiente en inventario se debe borrar esa aplicacion o confirmar que sea una simple diferencia
*entre compins y auxapli del mismo codcomp
				SELECT codcomp,CANTINS,USOTOTINS FROM COMPINS WHERE codcomp ;
					IN (SELECT codcomp FROM AUXAPLI WHERE AUXAPLI.codinsap=C_APLICA.codinsap)  INTO CURSOR C_DISPONIBLES1
				IF C_DISPONIBLES1.USOTOTINS = C_APLICA.cantotal
*insert into auxapli (codinsap,codinsum,codtipo,codcomp,cantusada,codaplic) ;
*	values(c_aplica.codinsap,c_aplica.codinsum,c_aplica.codtipo, c_disponibles1.codcomp,usototins+ c_disponibles.libre,c_aplica.codaplic)
					UPDATE AUXAPLI SET CANTUSADA=C_DISPONIBLES1.USOTOTINS	 WHERE codcomp=C_DISPONIBLES1.codcomp
				ENDIF
			ENDIF
		ELSE
*casos en los que auxapli es mayor que insaplica

			SCAN
				IF C_APLICA.cantotal < C_APLICA.AUXCANTUSADA
					SELECT 	count(CANTUSADA) as CUANTOS FROM AUXAPLI WHERE codinsap=C_APLICA.codinsap AND ;
						CANTUSADA=C_APLICA.cantotal and  !deleted() INTO CURSOR C_AUXAPLIMATCH
					IF C_AUXAPLIMATCH.CUANTOS=1
						SELECT	CANTUSADA, codcomp,codinsap,CODINSUM from AUXAPLI where codinsap = C_APLICA.codinsap and CANTUSADA!=C_APLICA.cantotal ;
							and !deleted() into  cursor C_AUXAPLI3
						Delete FROM AUXAPLI WHERE codinsap=C_APLICA.codinsap AND CANTUSADA!=C_APLICA.cantotal and CANTUSADA!=C_APLICA.cantotal
						SCAN
							UPDATE COMPINS set USOTOTINS=USOTOTINS-C_AUXAPLI3.CANTUSADA where  COMPINS.codcomp=C_AUXAPLI3.codcomp
						ENDSCAN
					ELSE
						SELECT	CANTUSADA, codcomp,codinsap,CODINSUM FROM AUXAPLI WHERE AUXAPLI.codinsap=C_APLICA.codinsap and !deleted() ;
							order by CANTUSADA DESC INTO CURSOR C_AUXAPLI3
						VACUM1= C_APLICA.cantotal
						SCAN
							IF C_AUXAPLI3.CANTUSADA<=VACUM1
								VACUM1 = VACUM1 -C_AUXAPLI3.CANTUSADA
*Estan bien
							ELSE
								IF VACUM1>0
									UPDATE AUXAPLI	set CANTUSADA=VACUM1 where codinsap=C_APLICA.codinsap and ;
										codcomp=C_AUXAPLI3.codcomp and CODINSUM=C_AUXAPLI3.CODINSUM
									UPDATE COMPINS set USOTOTINS=USOTOTINS-C_AUXAPLI3.CANTUSADA+VACUM1 where  codcomp=C_AUXAPLI3.codcomp ;
										and CODINSUM=C_AUXAPLI3.CODINSUM
									VACUM1 = 0
								ELSE
									Delete FROM AUXAPLI WHERE codinsap=C_APLICA.codinsap and codcomp=C_AUXAPLI3.codcomp
*SELECT * FROM AUXAPLI WHERE CODINSAP=C_APLICA.CODINSAP and CODCOMP=C_AUXAPLI3.CODCOMP
									UPDATE COMPINS set USOTOTINS=USOTOTINS-C_AUXAPLI3.CANTUSADA where  codcomp=C_AUXAPLI3.codcomp
								ENDIF
							ENDIF
						ENDSCAN
					ENDIF
				ENDIF
				SELECT C_APLICA
			ENDSCAN

		ENDIF
		SELECT C_APLICA
	ENDSCAN
*********************************************
*Si existe una entrada en auxapli pero no insapli

	SELECT SUM(AUXAPLI.CANTUSADA) AS AUXCANTUSADA, iif(isnull(cantotal),0,cantotal) as cantotal,AUXAPLI.codinsap,AUXAPLI.CODINSUM,insaplica.numcontrol,AUXAPLI.CODAPLIC, ;
		AUXAPLI.CODTIPO,AUXAPLI.codcomp  FROM insaplica right JOIN AUXAPLI ON insaplica.codinsap=AUXAPLI.codinsap where !deleted()  ;
		GROUP BY insaplica.codinsap ORDER BY insaplica.codinsap  ;
		INTO CURSOR C_APLICA1
	SELECT * from C_APLICA1 where AUXCANTUSADA != cantotal into cursor C_APLICA
	SELECT * from AUXAPLI where AUXAPLI.codinsap  =C_APLICA.codinsap into cursor c_auxusada
	SCAN
		UPDATE COMPINS set USOTOTINS=USOTOTINS-c_auxusada.CANTUSADA where codcomp=c_auxusada.codcomp
		Delete from AUXAPLI where codcomp=c_auxusada.codcomp
		MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
			"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
	ENDSCAN

************************
	IF COMP_AUX >0
************************
*Corrige discrepancias entre compins y auxapli y si existe auxufc tambien
		SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, USOTOTINS,COMPINS.codcomp,COMPINS.CODINSUM,CANTINS ;
			FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted() ;
			GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

		SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,USOTOTINS, ;
			COMPINS.CODINSUM,CANTINS FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted() ;
			GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

		SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
			where !deleted() INTO TABLE T_COMP2
		SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
		SELECT T_COMP2
		APPEND FROM T_UNICOS
*****scan
		SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,* FROM  T_COMP2 ;
			where !deleted() HAVING USOAUXI!=USOTOTINS  order by CODINSUM INTO CURSOR C_COMP3
		SCAN
*******if uso >=C_COMP3.USOAUXI
****sI HAY UN PROBLEMA DONDE USOTOTINS ES MAYOR QUE CANTINS
			IF C_COMP3.USOAUXI > C_COMP3.USOTOTINS
				SELECT * FROM AUXAPLI WHERE codcomp=C_COMP3.codcomp ORDER BY CANTUSADA ASC INTO CURSOR C_ABORRAR

				SCAN
*IF C_COMP3.USOTOTINS-C_ABORRAR.CANTUSADA  <= C_COMP3.CANTINS-C_COMP3.USOUFC
					IF C_COMP3.USOTOTINS  <= C_COMP3.CANTINS-C_COMP3.USOUFC
						EXIT
					ELSE

						SELECT CANTINS-USOTOTINS AS libre,* FROM COMPINS WHERE CODINSUM=C_COMP3.CODINSUM and !deleted() having libre>0 INTO CURSOR C_NUEVOCOMP
						Acum= C_ABORRAR.CANTUSADA
						SCAN
							IF Acum<=0
								EXIT
							ELSE
								IF C_NUEVOCOMP.libre >= Acum
*C_ABORRAR.CANTUSADA
*UPDATE COMPINS SET USOTOTINS= C_COMP3.USOTOTINS-Acum  WHERE COMPINS.codcomp=C_COMP3.codcomp
									UPDATE COMPINS SET USOTOTINS= C_COMP3.USOAUXI-Acum  WHERE COMPINS.codcomp=C_COMP3.codcomp

									UPDATE COMPINS SET USOTOTINS=USOTOTINS+Acum  WHERE codcomp=C_NUEVOCOMP.codcomp
									UPDATE AUXAPLI SET codcomp=C_NUEVOCOMP.codcomp WHERE codcomp = C_ABORRAR.codcomp and codinsap= C_ABORRAR.codinsap
									Acum =0
* Acum - C_ABORRAR.CANTUSADA
								ELSE
*UPDATE COMPINS SET USOTOTINS= C_COMP3.USOTOTINS-C_NUEVOCOMP.LIBRE WHERE COMPINS.codcomp=C_COMP3.codcomp
									UPDATE COMPINS SET USOTOTINS= C_COMP3.USOAUXI-C_NUEVOCOMP.libre  WHERE COMPINS.codcomp=C_COMP3.codcomp
									UPDATE COMPINS SET USOTOTINS=USOTOTINS+C_NUEVOCOMP.libre WHERE codcomp=C_NUEVOCOMP.codcomp
*Delete FROM AUXAPLI WHERE codcomp = C_ABORRAR.codcomp and codinsap= C_ABORRAR.codinsap
									UPDATE AUXAPLI SET CANTUSADA=C_ABORRAR.CANTUSADA-C_NUEVOCOMP.libre WHERE codcomp = C_ABORRAR.codcomp and codinsap= C_ABORRAR.codinsap
									INSERT INTO AUXAPLI (codinsap,CODINSUM,CODTIPO,codcomp,CANTUSADA,CODAPLIC) ;
										VALUES(C_ABORRAR.codinsap,C_ABORRAR.CODINSUM,C_ABORRAR.CODTIPO,C_NUEVOCOMP.codcomp,C_NUEVOCOMP.libre,C_ABORRAR.CODAPLIC)
									Acum = Acum - C_NUEVOCOMP.libre
************codigo para calcular el valor arreglado de compins con el nuevo codcomp
									SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, USOTOTINS,COMPINS.codcomp,COMPINS.CODINSUM,CANTINS ;
										FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted() ;
										GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

									SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,USOTOTINS, ;
										COMPINS.CODINSUM,CANTINS FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted() ;
										GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

									SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
										where !deleted() INTO TABLE T_COMP2
									SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
									SELECT T_COMP2
									APPEND FROM T_UNICOS
									SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,* FROM  T_COMP2 ;
										where !deleted() HAVING USOAUXI!=USOTOTINS  order by CODINSUM INTO CURSOR C_COMP3
*********fin codigo compins con codcomp fijo
								ENDIF

							ENDIF

							SELECT C_NUEVOCOMP
						ENDSCAN
************codigo para calcular el valor arreglado de compins con el nuevo codcomp
						SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, USOTOTINS,COMPINS.codcomp,COMPINS.CODINSUM,CANTINS ;
							FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted() ;
							GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

						SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,USOTOTINS, ;
							COMPINS.CODINSUM,CANTINS FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted() ;
							GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

						SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
							where !deleted() INTO TABLE T_COMP2
						SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
						SELECT T_COMP2
						APPEND FROM T_UNICOS
						SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,* FROM  T_COMP2 ;
							where !deleted() HAVING USOAUXI!=USOTOTINS  order by CODINSUM INTO CURSOR C_COMP3
*********fin codigo compins con codcomp fijo
					ENDIF

					SELECT C_ABORRAR
				ENDSCAN

				LOOP
			ENDIF
******************
			IF C_COMP3.USOTOTINS >= C_COMP3.USOAUXI
				UPDATE COMPINS SET USOTOTINS= C_COMP3.USOAUXI WHERE COMPINS.codcomp=C_COMP3.codcomp
				MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
					"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
				SW=.T.
			ELSE
*******if C_COMP3.USOAUX = C_COMP3.USOTOTINS
				IF C_COMP3.USOAUXI = C_COMP3.USOTOTINS
					SELECT codcomp,CODINSUM,CANTINS,USOTOTINS,(CANTINS-USOTOTINS) AS libre  FROM COMPINS ;
						WHERE CODINSUM = C_COMP3.CODINSUM AND (CANTINS-USOTOTINS)>0 and !deleted() order by codcomp INTO CURSOR C_DISPO
					SELECT SUM(libre) AS INVENTARIO FROM C_DISPO INTO CURSOR C_DISPO1
					IF C_DISPO1.INVENTARIO >= C_COMP3.USOUFC
***IF C_DISPO1.INVENTARIO > C_COMP3.USOUFC
						SELECT C_DISPO
						SW= 0
						V_ACUM = C_COMP3.USOUFC
						SCAN
****SELECT C_DISPO
							IF C_DISPO.libre >= C_COMP3.USOUFC
****IF C_DISPO.LIBRE >= C_COMP3.USOUFC
								UPDATE COMPINS SET USOTOTINS = USOTOTINS+C_COMP3.USOUFC WHERE codcomp = C_DISPO.codcomp
								SELECT COUNT(*) AS CUANTOS FROM AUXUFC WHERE codcomp=C_COMP3.codcomp and !deleted() INTO CURSOR C_AUFC

								IF C_AUFC.CUANTOS =1
									UPDATE AUXUFC SET codcomp = C_COMP3.USOUFC WHERE codcomp = C_COMP3.codcomp AND !DELETED()
								ELSE
*delete auxufc where codufc not in (select top 1 codufc from auxufc where codcomp=c_comp3.codcomp order by codufc)
*select * from auxufc where auxufc.codufc in (select codufc from auxufc where codcomp=c_comp3.codcomp order by codufc )
									SELECT CODUFC,codcomp,SUM(CANTUSADA) AS CANTUSADA1,CODINSUM, COUNT(CODUFC) AS CUANTOS FROM AUXUFC ;
										WHERE codcomp=C_COMP3.codcomp and !deleted() ORDER BY CODUFC GROUP BY CODUFC INTO CURSOR C_AUFC1
									SCAN
*aqui
										IF C_AUFC1.CUANTOS =1
											UPDATE AUXUFC SET codcomp = C_DISPO.codcomp WHERE codcomp = C_COMP3.codcomp AND !DELETED()
										ELSE
											Delete FROM AUXUFC WHERE CODUFC=C_AUFC1.CODUFC AND codcomp=C_AUFC1.codcomp
											INSERT INTO AUXUFC (CODUFC,codcomp,CANTUSADA,CODINSUM) VALUES(C_AUFC1.CODUFC,C_AUFC1.codcomp,C_AUFC1.CANTUSADA1,C_AUFC1.CODINSUM)
										ENDIF
									ENDSCAN
*								delete from ufc where codufc not in (select top 1 codufc from auxufc where codcomp=c_comp3.codcomp order by codufc)
*por integridad referencial borrara los de auxufc
*								update auxufc set codcomp = c_dispo.codcomp, cantusada= c_comp3.usoufc where codcomp = c_comp3.codcomp and !deleted()
								ENDIF
								EXIT
							ELSE
****IF C_DISPO.LIBRE >= C_COMP3.USOUFC
								IF V_ACUM>0
*IF V_ACUM>0
									V_ACUM =V_ACUM -C_DISPO.libre
*revisar aqui

*!*									select codufc,codcomp,sum(cantusada) as cantusada1,codinsum, count(codufc) as cuantos from auxufc ;
*!*											where codcomp=c_comp3.codcomp order by codufc group by codufc into cursor c_aufc1
									SELECT CODFIN,CODUFC,CODINSUM,cantotal FROM UFC WHERE CODUFC IN;
										(SELECT CODUFC FROM AUXUFC WHERE codcomp=C_COMP3.codcomp) INTO CURSOR C_UFC1

									IF SW =0
										SW =1
										Delete FROM AUXUFC WHERE CODUFC=C_UFC1.CODUFC AND codcomp=C_COMP3.codcomp
										INSERT INTO AUXUFC (CODUFC,codcomp,CANTUSADA,CODINSUM) ;
											VALUES(C_UFC1.CODUFC,C_COMP3.codcomp,C_DISPO.libre,C_UFC1.CODINSUM)
									ELSE
										SELECT C_UFC1
										SCAN
											IF C_DISPO.libre >= C_AUFC1.cantotal
												INSERT INTO AUXUFC (CODUFC,codcomp,CANTUSADA,CODINSUM) ;
													VALUES(C_UFC1.CODUFC,C_COMP3.codcomp,C_UFC1.cantotal,C_UFC1.CODINSUM)
											ELSE
												INSERT INTO AUXUFC (CODUFC,codcomp,CANTUSADA,CODINSUM) ;
													VALUES(C_UFC1.CODUFC,C_COMP3.codcomp,C_DISPO.libre,C_UFC1.CODINSUM)
												EXIT
											ENDIF
										ENDSCAN
									ENDIF
								ELSE
*IF V_ACUM>0
*!*									SELECT CODUFC FROM AUXUFC WHERE CODCOMP=CODCOMP = C_COMP3.CODCOMP INTO CURSOR C_CODUFC
*!*									VNUEVO = C_USOACT.USOTOTINS -C_DISPO.LIBRE
*!*									INSERT INTO AUXUFC (CODUFC,CODCOMP,CANTUSADA,CODINSUM) VALUES(C_CODUFC.CODUFC,C_DISPO.CODCOMP,C_USOACTUAL.USOTOTINS+C_DISPO.LIBRE,C_COMP3.USOUFC)
*IF V_ACUM>0
									EXIT
								ENDIF
****IF C_DISPO.LIBRE >= C_COMP3.USOUFC
							ENDIF
****select C_DISPO
						ENDSCAN
					ELSE
* a borrar aplicaciones de uso fuera de cultivos
						MESSAGEBOX("El inventario actual no alcanza para las aplicaciones realizadas",16,"Error")
***IF C_DISPO1.INVENTARIO > C_COMP3.USOUFC
					ENDIF

				ELSE
*******if C_COMP3.USOAUX = C_COMP3.USOTOTINS
					IF C_COMP3.USOAUXI < C_COMP3.USOTOTINS
*******IF C_COMP3.USOAUX < C_COMP3.USOTOTINS
*ASUMIENDO QUE AUXUFC Y UFC ESTAN SINCRONIZADOS
*SINCROZINO COMPINS CON AUXAPLI

						UPDATE COMPINS SET USOTOTINS= C_COMP3.USOAUXI WHERE COMPINS.codcomp=C_COMP3.codcomp AND CANTINS>=C_COMP3.USOAUX

*BUSCO LO QUE QUEDA DISPONIBLE TRAS LA ACTUALIZACION
						SELECT codcomp,CODINSUM,CANTINS,USOTOTINS,(CANTINS-USOTOTINS) AS libre  FROM COMPINS ;
							WHERE CODINSUM = C_COMP3.CODINSUM AND (CANTINS-USOTOTINS)>0 and !deleted() order by codcomp INTO CURSOR C_DISPO
						SELECT SUM(libre) AS INVENTARIO FROM C_DISPO INTO CURSOR C_DISPO1

						V_ACUM = C_COMP3.USOUFC
						SELECT * FROM AUXUFC WHERE codcomp=C_COMP3.codcomp and !deleted() INTO CURSOR C_AUXUFC2
*SELECT * FROM UFC WHERE CODUFC in (SELECT CODUFC FROM AUXUFC WHERE CODCOMP=C_COMP3.CODCOMP ;
*!*							and !deleted())and !deleted() INTO CURSOR C_ufc2
*!*						delete from auxufc WHERE CODCOMP=C_COMP3.CODCOMP
*!*						use in  auxufc
*!*						use auxufc exclu
*!*						pack
						SELECT C_AUXUFC2
						SCAN
							V_ACUM = V_ACUM - C_AUXUFC2.CANTUSADA
							SELECT C_DISPO
							UPDATE AUXUFC set codcomp=C_DISPO.codcomp  where CODUFC=C_AUXUFC2.CODUFC and codcomp = C_AUXUFC2.codcomp

							UPDATE COMPINS SET USOTOTINS=USOTOTINS+C_AUXUFC2.CANTUSADA WHERE  codcomp=C_DISPO.codcomp
							SELECT codcomp,CODINSUM,CANTINS,USOTOTINS,(CANTINS-USOTOTINS) AS libre  FROM COMPINS ;
								WHERE CODINSUM = C_COMP3.CODINSUM and !deleted() HAVING (CANTINS-USOTOTINS)>0 order by codcomp INTO CURSOR C_DISPO
							SELECT C_AUXUFC2

						ENDSCAN

						IF V_ACUM>0
*!*							INSERT INTO AUXUFC (CODUFC,CODCOMP,CANTUSADA,CODINSUM) VALUES(
*!*							INSERT INTO UFC (CODFIN,CODUFC,CODINSUM,CANTOTAL,NOMINSUM,CODTIPO,
							ERROR_INV = ERROR_INV +1
						ENDIF

* IF C_COMP3.USOAUX < C_COMP3.USOTOTINS
					ELSE
						IF C_COMP3.USOAUXI > C_COMP3.USOTOTINS
							UPDATE COMPINS SET USOTOTINS= C_COMP3.USOAUXI WHERE COMPINS.codcomp=C_COMP3.codcomp AND CANTINS>=C_COMP3.USOAUX

							IF _tally = 0
*debo eliminar de auxapli el sobrante
								SELECT sum(CANTINS - USOTOTINS) as libre  from COMPINS where CODINSUM=C_COMP3.CODINSUM and !deleted() into cursor c_libre
*si hay libre para aplicar???
								IF c_libre.libre + C_COMP3.USOTOTINS >= C_COMP3.USOAUX

									SELECT * from AUXAPLI where codcomp = C_COMP3.codcomp into cursor c_auxiliar
									vcantusada = 0
									SWitch=0
									SCAN
										IF SWitch=0
											IF vcantusada+ c_auxiliar.CANTUSADA >= C_COMP3.USOTOTINS
*reubicar los siguientes codinsap de auxapli -arreglo asumiendo que el daño es de un solo codinsap
												vcodcomp = 	c_auxiliar.codcomp
												vcantusada = vcantusada + c_auxiliar.CANTUSADA
*asigno nuevo valor a compins
												UPDATE COMPINS SET USOTOTINS= vcantusada WHERE COMPINS.codcomp=vcodcomp
												SWitch =1
*EXIT
											ELSE
												vcantusada = vcantusada + c_auxiliar.CANTUSADA
											ENDIF
										ELSE
************
											vcodinsap = c_auxiliar.codinsap
											vcodcomp = 	c_auxiliar.codcomp
*Asigno el nuevo codcomp a los que en auxapli no tienen usototins
											SELECT codcomp,CANTINS-USOTOTINS as libre from COMPINS where CODINSUM=C_COMP3.CODINSUM and !deleted() ;
												having libre >0 into cursor c_usofree
											v_aaplicar = c_auxiliar.CANTUSADA
											SCAN
												IF c_usofree.libre >= v_aaplicar
													UPDATE COMPINS SET USOTOTINS= v_aaplicar+USOTOTINS WHERE COMPINS.codcomp=c_usofree.codcomp
													INSERT into AUXAPLI (codinsap,CODINSUM,CODTIPO,codcomp,CANTUSADA,CODAPLIC) ;
														values(c_auxiliar.codinsap,c_auxiliar.CODINSUM,c_auxiliar.CODTIPO,c_usofree.codcomp,v_aaplicar ,c_auxiliar.CODAPLIC)
													v_aaplicar = v_aaplicar - c_usofree.libre
													EXIT
												ELSE
													Delete from AUXAPLI where codcomp=vcodcomp and codinsap=vcodinsap
													UPDATE COMPINS SET USOTOTINS= USOTOTINS+c_usofree.libre  WHERE COMPINS.codcomp=c_usofree.codcomp
													INSERT into AUXAPLI (codinsap,CODINSUM,CODTIPO,codcomp,CANTUSADA,CODAPLIC) ;
														values(c_auxiliar.codinsap,c_auxiliar.CODINSUM,c_auxiliar.CODTIPO,c_usofree.codcomp,c_usofree.libre ,c_auxiliar.CODAPLIC)
													v_aaplicar = v_aaplicar - c_usofree.libre
												ENDIF
												SELECT c_usofree
											ENDSCAN
**************
										ENDIF
										SELECT c_auxiliar
									ENDSCAN

								ELSE
*** borrar una aplicacion de auxapli y bajarle al insaplica asociado

								ENDIF

							ENDIF
							MESSAGEBOX("Se encontró una incosistencia en aplicaciones y se corrigió"+CHR(13)+ ;
								"REVISE NUEVAMENTE LAS APLICACIONES PARA CONFIRMAR EL ACCESO A ELLAS",48,"error")


						ENDIF
					ENDIF
*******if C_COMP3.USOAUX = C_COMP3.USOTOTINS
				ENDIF
*******if uso >=C_COMP3.USOAUXI
			ENDIF

*correccion con auxufc

			SELECT C_COMP3
		ENDSCAN
**********fin scan

	ENDIF
ENDIF
**ERROR EN INVENTARIOS
SELECT COMPINS.codcomp AS COMP , AUXAPLI.CODINSUM,COMPINS.CODINSUM,AUXAPLI.codcomp AS AUX,USOTOTINS;
	FROM  AUXAPLI LEFT OUTER JOIN COMPINS  ;
	ON  COMPINS.codcomp  = AUXAPLI.codcomp INTO CURSOR SINULL
SELECT COUNT(*) FROM SINULL WHERE (ISNULL(COMP)OR ISNULL(AUX)) AND USOTOTINS!=0  and !deleted() INTO ARRAY NULO_AUX_COMP

SELECT insaplica.codinsap AS INS, AUXAPLI.codinsap AS AUX,insaplica.CODINSUM,AUXAPLI.CODINSUM;
	FROM  SACFADB!insaplica LEFT OUTER JOIN SACFADB!AUXAPLI ;
	ON  insaplica.codinsap = AUXAPLI.codinsap INTO CURSOR SINULL2
SELECT COUNT(*) FROM SINULL2 WHERE ISNULL(INS) OR ISNULL(AUX) INTO ARRAY NULO_AUX_INS
***HAy en insaplica pero no en auxapli





******************error hay dato en insaplica pero no en aplica

ERR_APLICA =.F.

Delete from aplica where isnull(CODAPLIC) or  isblank(CODAPLIC)&&Elimina inconsistencias en aplicaciones
SELECT aplica.CODAPLIC AS APLIC, insaplica.CODAPLIC AS INSA,codinsap,CODINSUM FROM aplica RIGHT JOIN insaplica ;
	ON insaplica.CODAPLIC=aplica.CODAPLIC INTO CURSOR C_APLICA
SELECT DISTINCT INSA FROM C_APLICA WHERE ISNULL(APLIC) INTO CURSOR C_APLICA1

SELECT COUNT(*) AS CUANTOS FROM C_APLICA WHERE ISNULL(APLIC) INTO CURSOR CAPLICCUANTOS
IF CAPLICCUANTOS.CUANTOS >0

	SELECT aplica.CODAPLIC AS APLIC, insaplica.CODAPLIC AS INSA,codinsap,CODTIPO FROM aplica RIGHT JOIN insaplica ;
		ON insaplica.CODAPLIC=aplica.CODAPLIC INTO CURSOR C_APLICA
	SELECT DISTINCT INSA,CODTIPO,codinsap FROM C_APLICA WHERE ISNULL(APLIC) or isblank(APLIC)  group by INSA order by INSA,codinsap INTO CURSOR C_APLICA1

	SCAN
		SELECT DISTINCT numcontrol,FECHA1,CODAPLIC,CODINSUM,codinsap FROM insaplica WHERE CODAPLIC=C_APLICA1.INSA INTO CURSOR C_INSAPLICA

		SELECT iif(C_APLICA1.CODTIPO=1,"S","A")+padl(alltrim(str(max(val(substr(CODAPLIC,2,7)))+1)),7,"0") as CODAPLIC from insaplica ;
			where CODTIPO= C_APLICA1.CODTIPO into cursor c_newcodaplic
		SELECT COUNT(*) AS CUANTOS FROM infculti WHERE numcontrol=C_INSAPLICA.numcontrol and !deleted() INTO CURSOR CSINLOTE
		SELECT area FROM lotesfin WHERE codlote IN (SELECT codlote FROM infculti WHERE numcontrol=C_INSAPLICA.numcontrol) and !deleted() INTO CURSOR C_AREA

		IF CSINLOTE.CUANTOS>0

			IF isnull(C_INSAPLICA.CODAPLIC) or isblank(C_INSAPLICA.CODAPLIC)
				UPDATE insaplica set CODAPLIC=c_newcodaplic.CODAPLIC where codinsap=C_INSAPLICA.codinsap
				INSERT INTO aplica (numcontrol,CODAPLIC,FECAPLIC,HECTAPLIC) ;
					VALUES(C_INSAPLICA.numcontrol,c_newcodaplic.CODAPLIC,C_INSAPLICA.FECHA1,C_AREA.area)
			ELSE
*UPDATE insaplica set CODAPLIC=c_newcodaplic.CODAPLIC where codinsap=C_INSAPLICA.codinsap
				INSERT INTO aplica (numcontrol,CODAPLIC,FECAPLIC,HECTAPLIC) ;
					VALUES(C_INSAPLICA.numcontrol,C_INSAPLICA.CODAPLIC,C_INSAPLICA.FECHA1,C_AREA.area)
			ENDIF

		ELSE
* Borro insaplica y auxaplica cuando el lote fue borrado de infculti y el  codcomp de compins
			SELECT codcomp,CANTUSADA FROM AUXAPLI WHERE codinsap IN (SELECT codinsap FROM insaplica WHERE numcontrol= C_INSAPLICA.numcontrol) and !deleted() INTO CURSOR CRESUSO
			SCAN
				UPDATE COMPINS SET USOTOTINS= COMPINS.USOTOTINS-CRESUSO.CANTUSADA  WHERE codcomp=CRESUSO.codcomp
			ENDSCAN
			UPDATE COMPINS SET USOTOTINS= 0 WHERE USOTOTINS<0 && Por si hay negativos igual queda inconsistente
			Delete FROM AUXAPLI WHERE codinsap IN (SELECT codinsap FROM insaplica WHERE numcontrol= C_INSAPLICA.numcontrol)
			Delete FROM insaplica WHERE numcontrol= C_INSAPLICA.numcontrol
		ENDIF
		SELECT C_APLICA1
		ERR_APLICA =.T.
	ENDSCAN
	IF ERR_APLICA
		MESSAGEBOX("Se encontró una incosistencia en aplicaciones y se corrigió"+CHR(13)+ ;
			"REVISE NUEVAMENTE LAS APLICACIONES PARA CONFIRMAR EL ACCESO A ELLAS",48,"error")
		SW=.T.
	ENDIF
ENDIF
*******************************************
*Si existe algun valor en insaplica pero no en auxapli
IF NULO_AUX_INS>0
	SELECT * FROM SINULL2 WHERE ISNULL(INS) OR ISNULL(AUX) INTO cursor c_nuloinsapli
	SELECT * from insaplica where codinsap in (select INS from c_nuloinsapli) into cursor c_insaplierror
	BEGIN transact
	SCAN
		SELECT codcomp,CODINSUM,sum(CANTINS) as CANTINS,sum(USOTOTINS) as USOTOTINS from COMPINS where COMPINS.CODINSUM=c_insaplierror.CODINSUM and !deleted() ;
			into cursor c_usolibre
		IF c_insaplierror.cantotal<= c_usolibre.USOTOTINS
			SELECT codcomp,CODINSUM,CANTINS,USOTOTINS from COMPINS where COMPINS.CODINSUM=c_insaplierror.CODINSUM and !deleted()  ;
				having CANTINS-USOTOTINS>0 into cursor c_usolibre
			v_saldocantusada = c_insaplierror.cantotal
			SCAN for v_saldocantusada>0
				IF c_insaplierror.cantotal<= c_usolibre.USOTOTINS
					v_saldocantusada=0
					UPDATE COMPINS set USOTOTINS=c_insaplierror.cantotal+ c_usolibre.USOTOTINS where COMPINS.codcomp=c_usolibre.codcomp
					INSERT INTO AUXAPLI  (codinsap,CODINSUM,CODTIPO,codcomp,CANTUSADA,CODAPLIC) ;
						VALUES(c_insaplierror.codinsap,c_insaplierror.CODINSUM,c_insaplierror.CODTIPO,c_usolibre.codcomp,c_insaplierror.cantotal,c_insaplierror.CODAPLIC)
				ELSE
					v_saldocantusada = v_saldocantusada - c_usolibre.USOTOTINS
					UPDATE COMPINS set USOTOTINS= c_usolibre.CANTINS where COMPINS.codcomp=c_usolibre.codcomp
					INSERT INTO AUXAPLI  (codinsap,CODINSUM,CODTIPO,codcomp,CANTUSADA,CODAPLIC) ;
						VALUES(c_insaplierror.codinsap,c_insaplierror.CODINSUM,c_insaplierror.CODTIPO,c_usolibre.codcomp,c_usolibre.CANTINS-c_usolibre.USOTOTINS,c_insaplierror.CODAPLIC)


				ENDIF
				SELECT c_usolibre
			ENDSCAN

*ELSE
*UPDATE COMPINS where
			SELECT c_insaplierror
		ELSE
			Delete from insaplica where insaplica .codinsap=c_insaplierror.codinsap
		ENDIF
	ENDSCAN

	END transac

ENDIF

***************** revision mas exaustiva para faltentes entre tablas
********************
*9/11/2005
*Se corrigen inconsistencias entre codcomp de auxcombust y compins


SELECT sum(CANTUSADA) as CANTUSADA, AUXCOMB.codcomp as auxcomp,COMPINS.codcomp,CANTINS,USOTOTINS;
	from COMPINS right join AUXCOMB on COMPINS.codcomp=AUXCOMB.codcomp ;
	group by AUXCOMB.codcomp order by AUXCOMB.codcomp  having CANTUSADA!=USOTOTINS into cursor c_auxcomb_compins

IF _tally >0
	SELECT count(*) as CUANTOS from c_auxcomb_compins into cursor c_auxcomb_compins1
	IF c_auxcomb_compins1.CUANTOS>0
		SELECT c_auxcomb_compins
		SCAN

			SELECT count(*) as cuan from AUXAPLI where codcomp = c_auxcomb_compins.codcomp into cursor c_aplico
			IF c_aplico.cuan >0
				SELECT sum(CANTUSADA) as sumcantu,CANTINS,USOTOTINS from AUXAPLI inner join COMPINS on AUXAPLI.codcomp=COMPINS.codcomp ;
					group by AUXAPLI.codcomp order by AUXAPLI.codcomp where AUXAPLI.codcomp = c_auxcomb_compins.codcomp and !deleted() into cursor c__auxapl
				IF c__auxapl.CANTINS >c__auxapl.sumcantu + c_auxcomb_compins.CANTUSADA
					UPDATE COMPINS set USOTOTINS=c__auxapl.sumcantu + c_auxcomb_compins.AUXCOMB ;
						where COMPINS.codcomp =  c_auxcomb_compins.codcomp
					MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
						"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
				ELSE
*aplico lubricantes por fertilizantes y agroquimicos
					MESSAGEBOX("El programa presenta una incosistencia" + chr(13)+ "Envie un backup a FEDEARROZ para repararla",16,"ERROR")
				ENDIF
			ELSE
				UPDATE COMPINS set USOTOTINS=c_auxcomb_compins.CANTUSADA where codcomp =  c_auxcomb_compins.codcomp
				MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
					"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
			ENDIF
			SELECT c_auxcomb_compins
		ENDSCAN
	ENDIF
ENDIF
**********************


ERROR_INV= ERROR_INV +NULO_AUX_INS+NULO_AUX_COMP+COMP_AUX   &&+vacio

G_ERROR =.T.

IF LEFT(POPUP(),1)="H"
	IF ERROR_INV >0
		IF !SW
			MESSAGEBOX("Inconsistencia en inventarios"+CHR(13) +"Enviar copia de seguridad para corrección",16,"error")
			IF ERROR_UFC >0
				MESSAGEBOX("Inconsistencia en usos fuera de cultivos"+CHR(13) +"Enviar copia de seguridad para corrección",16,"error")
			ENDIF

			IF ERROR_COMB >0
				MESSAGEBOX("Inconsistencia en uso de combustibles"+CHR(13) +"Enviar copia de seguridad para corrección",16,"error")
			ENDIF
			G_ERROR =.T.
		ENDIF
	ELSE
		IF !USED("labculti")
			MESSAGEBOX("Inventarios correctos",48,"Informacion")
		ENDIF
		G_ERROR =.F.
	ENDIF
ENDIF

IF LEFT(POPUP(),3)="HER"

	DO LIBERAR WITH "compins"
	DO LIBERAR WITH "auxapli"
	DO LIBERAR WITH "insaplica"
	DO LIBERAR WITH "ufc"
	DO LIBERAR WITH "auxufc"
	DO LIBERAR WITH "combust"
	DO LIBERAR WITH "auxcomb"
ENDIF
******

PROCEDURE USADA
PARAMETER TABLA
IF !USED(TABLA)
	USE &TABLA IN 0 exclu
ELSE
	SELECT &TABLA
*'USE IN &TABLA
*USE &TABLA IN 0 exclu
ENDIF
ENDPROC

PROCEDURE LIBERAR
PARAMETER TABLA
USE IN &TABLA
USE &TABLA IN 0 exclu
SELECT &TABLA
PACK
USE IN &TABLA
ENDPROC
