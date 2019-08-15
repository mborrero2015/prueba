
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
	SELECT insaplica
	= TABLEUPDATE(.T.)
ENDSCAN
*****************
*ELIMINA INSUMOS CON CODIGO -1
Delete FROM COMPINS WHERE CODINSUM=-1
SELECT COMPINS
= TABLEUPDATE(.T.)
Delete FROM AUXAPLI WHERE CODINSUM=-1
SELECT AUXAPLI
= TABLEUPDATE(.T.)
Delete FROM insaplica WHERE CODINSUM=-1
SELECT insaplica
= TABLEUPDATE(.T.)
******************************************
Delete from insaplica where numcontrol not in (select numcontrol from infculti)
SELECT insaplica
= TABLEUPDATE(.T.)
Delete from aplica where numcontrol not in (select numcontrol from infculti)
SELECT aplica
= TABLEUPDATE(.T.)
********************************************
*REVISA CRUCE ENTRE FINCAS

v_ubocambio = .f.
SELECT AUXAPLI.*, COMPINS.codfin;
	FROM  sacfadb!AUXAPLI inner JOIN sacfadb!COMPINS;
	ON  AUXAPLI.codcomp = COMPINS.codcomp into cursor  c_auxapli_finc

SELECT insaplica.*, infculti.codfin as finca;
	FROM  insaplica inner JOIN infculti;
	ON  insaplica.numcontrol= infculti.numcontrol into cursor  c_insaplica_finc


SELECT  c_auxapli_finc.*,finca from c_auxapli_finc inner join c_insaplica_finc on ;
	c_auxapli_finc.codinsap=c_insaplica_finc.codinsap  where codfin!=finca into cursor c_insumo_cruce

SELECT count(*) as cuantos from c_insumo_cruce into cursor c_cuantos
IF c_cuantos.cuantos>0
	switch=.t.
ELSE
	switch=.f.
ENDIF
acum=0
DO WHILE switch
*EL PROCEDIMIENTO ESTA AL FINA DE ESTE CODIGO
	IF acum<=2
		switch=recorre_error (switch,acum)
	ELSE
		switch=.f.
	ENDIF
	acum=acum+1
ENDDO
***************************************
verror = 0
SW=.F.


*Revisa que no haya compins usototins > que cantins y los iguala si hay error lo vuelve 0
UPDATE COMPINS set usototins=0 where usototins>cantins or usototins<0
SELECT COMPINS
= TABLEUPDATE(.T.)
IF _tally>0
	MESSAGEBOX("Se encontró una incosistencia en inventarios"+CHR(13)+ ;
		"REVISE NUEVAMENTE INVENTARIOS",48,"error")
ENDIF

SELECT COMPINS.CODINSUM, SUM(COMPINS.cantins) AS COMTOTINS ,  SUM(COMPINS.usototins)AS COMTOT  ;
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
	FROM COMBUST  where !deleted() GROUP BY CODINSUM;
	INTO CURSOR TEM_COMBUST ORDER BY CODINSUM

SELECT  AUXCOMB.CODINSUM, SUM(AUXCOMB.CANTUSADA) AS AUXCOMB;
	FROM AUXCOMB where !deleted() GROUP BY CODINSUM ;
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
SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
	FROM COMPINS inner JOIN AUXAPLI ON AUXAPLI.codcomp=COMPINS.codcomp where !deleted() ;
	GROUP BY COMPINS.codcomp ORDER BY AUXAPLI.codcomp INTO CURSOR C_COMPARA

SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,usototins,COMPINS.codcomp,codfin;
	FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted();
	GROUP BY COMPINS.codcomp ORDER BY AUXUFC.codcomp  INTO CURSOR C_COMPARA1

SELECT CODINSUM,USOAUX,C_COMPARA.usototins AS usototins, C_COMPARA1.usototins AS USAUXUFC,cantins,C_COMPARA.codcomp AS CODCOMPCOM,USOUFC,C_COMPARA1.codcomp AS CODCOMPAUX,C_COMPARA.codfin ;
	FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp where !deleted() INTO TABLE T_COMP2B
SELECT USOUFC,codcomp AS  CODCOMPAUX, usototins AS usototins FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT  CODCOMPCOM FROM T_COMP2B) INTO TABLE T_COMP3
SELECT T_COMP2B
APPEND FROM T_COMP3
SELECT IIF(ISNULL(USOUFC),000000000.00000+USOAUX,USOUFC+USOAUX) AS USOAUX1,* ;
	FROM T_COMP2B INTO CURSOR C_COMP2A
*select	usoaux1+usoufc1 as usoauxi,* from c_comp2a into cursor c_comp3 order by codcomp_a
SELECT COUNT(*) FROM C_COMP2A WHERE USOAUX1!=usototins OR ISNULL(usototins) OR ISNULL(USOAUX1) and !deleted()  INTO CURSOR CCOMP_AUX
COMP_AUX =0
COMP_AUX = CCOMP_AUX.CNT
*Por si hay dato en algun compins que no haya en auxapli

*!*	SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,USOTOTINS, ;
*!*		COMPINS.CODINSUM,CANTINS FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp ;
*!*		GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp where !deleted()  INTO CURSOR C_COMPARA1

*****scan
*!*	SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,* FROM  T_COMP2 ;
*!*		HAVING USOAUXI!=USOTOTINS  order by CODINSUM INTO CURSOR C_COMP3

*si es mayor que cero no fue detectado por la sumatoria de invtotal es necesario corregirlo aqui
**************
*************
&&saca el mensaje de error con esta variable  comp_aux si es mayor a 0
***************************************************



IF ERROR_INV>0  or COMP_AUX >0
****revisa si hay valore negativos en compins

	SELECT count(*) as cuantos  from COMPINS where usototins<0 into cursor c_hay_neg_compins
	IF c_hay_neg_compins.cuantos>0
		UPDATE COMPINS set usototins=0 where usototins<0
		SELECT COMPINS
		= TABLEUPDATE(.T.)
		MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
			"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
	ENDIF

**ERROR EN INVENTARIOS
	SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
		FROM COMPINS left JOIN AUXAPLI ON AUXAPLI.codcomp=COMPINS.codcomp ;
		GROUP BY COMPINS.codcomp ORDER BY AUXAPLI.codcomp having isnull(USOAUX) and usototins!=0 ;
		where !deleted() and COMPINS.codtipo!=12 into cursor c_solocompins
*corrige este error de una ves
	SCAN
		SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
			FROM COMPINS left JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp ;
			GROUP BY COMPINS.codcomp ORDER BY AUXUFC.codcomp where AUXUFC.codcomp= c_solocompins.codcomp and !deleted() into cursor c_conufc
*having USOUFC!=USOTOTINS
		IF _tally =0
*Si ha datos en ufc deje ese dato sino vuelvalo 0

			vcomp_actual= c_solocompins.codcomp

			UPDATE COMPINS set usototins=0 where COMPINS.codcomp=vcomp_actual
			SELECT COMPINS
			= TABLEUPDATE(.T.)
		ELSE
			SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
				FROM COMPINS left JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp ;
				GROUP BY COMPINS.codcomp ORDER BY AUXUFC.codcomp where AUXUFC.codcomp= c_solocompins.codcomp and !deleted() ;
				having USOUFC!=usototins into cursor c_conufc

			SCAN
				IF c_conufc.cantins>=c_conufc.USOUFC
					UPDATE COMPINS set usototins=c_conufc.USOUFC where COMPINS.codcomp=c_solocompins.codcomp and !deleted()
					SELECT COMPINS
					= TABLEUPDATE(.T.)

					IF _tally>0
						MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
							"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
					ENDIF
				ELSE
					SELECT sum(cantins-usototins) as libre,usototins,cantins,codcomp,CODINSUM,codfin from COMPINS ;
						where COMPINS.CODINSUM=c_conufc.CODINSUM into cursor c_libre
					IF c_libre.libre >c_conufc.USOUFC
						SELECT cantins-usototins as libre,cantins,usototins,codcomp,CODINSUM,codfin from COMPINS ;
							where (cantins-usototins)>0 order by libre desc into cursor c_libre_aplicar
						V_ACUM=c_conufc.USOUFC
*SCAN
						IF c_libre_aplicar.libre >c_conufc.USOUFC
							UPDATE AUXUFC set codcomp=c_libre_aplicar.codcomp where codcomp=c_solocompins.codcomp
							SELECT AUXUFC
							= TABLEUPDATE(.T.)

							LOOP
						ELSE
							SELECT * from AUXUFC where codcomp= c_solocompins.codcomp into cursor c_elimina

							UPDATE UFC set CANTUSADA=CANTUSADA-c_elimina.CANTUSADA WHERE CODUFC=c_elimina.CODUFC
							SELECT UFC
							= TABLEUPDATE(.T.)

							Delete FROM UFC WHERE CANTUSADA<=0 &&pOR SI LA RESTA c_elimina LLEGA A 0
							SELECT UFC
							= TABLEUPDATE(.T.)
							UPDATE COMPINS SET usototins=0 where COMPINS.codcomp=c_solocompins.codcomp and !deleted()
							SELECT COMPINS
							= TABLEUPDATE(.T.)

							Delete from AUXUFC where codcomp=c_solocompins.codcomp
							SELECT AUXUFC
							= TABLEUPDATE(.T.)
						ENDIF
*dERIA CORREGIRSE PARA QUE ENCUENTRE COMPINS VALORES LIBRES DONDE APLICAR
					ELSE
*no hay valor suficiente para uso fuera de cultivos
						SELECT * from AUXUFC where codcomp= c_solocompins.codcomp into cursor c_elimina

						UPDATE UFC set CANTUSADA=CANTUSADA-c_elimina.CANTUSADA WHERE CODUFC=c_elimina.CODUFC
						SELECT UFC
						= TABLEUPDATE(.T.)
						Delete FROM UFC WHERE CANTUSADA<=0 &&pOR SI LA RESTA c_elimina LLEGA A 0
						SELECT UFC
						= TABLEUPDATE(.T.)
						UPDATE COMPINS SET usototins=0 where COMPINS.codcomp=c_solocompins.codcomp and !deleted()
						SELECT COMPINS
						= TABLEUPDATE(.T.)

						Delete from AUXUFC where codcomp=c_solocompins.codcomp
						SELECT AUXUFC
						= TABLEUPDATE(.T.)
					ENDIF
				ENDIF
				SELECT c_conufc
			ENDSCAN
		ENDIF
		SELECT c_solocompins
	ENDSCAN
************************
*********************** inconsistencias por insumos cruzados entre insaplica y auxapli

	SELECT insaplica.CODINSUM as insaplicod, AUXAPLI.CODINSUM as auxaplicod, insaplica.codinsap,cantotal,AUXAPLI.codcomp, ;
		CANTUSADA from insaplica left join AUXAPLI on insaplica.codinsap=AUXAPLI.codinsap where !deleted() ;
		having insaplicod!=auxaplicod into cursor cCruzado
	SCAN
		UPDATE AUXAPLI set CODINSUM= cCruzado.insaplicod where codcomp=cCruzado.codcomp and codinsap=cCruzado.codinsap
		SELECT AUXAPLI
		= TABLEUPDATE(.T.)
	ENDSCAN
************************
*errores entre auxapli y compins

	IF COMP_AUX >0
************************
*Corrige discrepancias entre compins y auxapli y si existe auxufc tambien
		SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
			FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted() and COMPINS.codtipo!=12 ;
			GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

		SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,usototins, ;
			COMPINS.CODINSUM,cantins,codfin FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted() and COMPINS.codtipo!=12  ;
			GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

		SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
			where !deleted() INTO TABLE T_COMP2
		SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
		SELECT T_COMP2
		APPEND FROM T_UNICOS

		SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0.0000,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,iif(isnull(USOAUX),0.0000,USOAUX) as USOAUX,iif(isnull(USOUFC),0.0000,USOUFC) as USOUFC,CODINSUM,codcomp,cantins,usototins FROM  T_COMP2 ;
			where !deleted() HAVING USOAUXI!=usototins  order by CODINSUM INTO CURSOR C_COMP3

		SCAN
			IF C_COMP3.USOAUXI > C_COMP3.usototins and C_COMP3.USOUFC=0 and C_COMP3.USOAUX<= C_COMP3.cantins
				UPDATE COMPINS set usototins=C_COMP3.USOAUX where codcomp=C_COMP3.codcomp
				SELECT COMPINS
				= TABLEUPDATE(.T.)

				SW=.t.
				verror=1
			ELSE
				IF C_COMP3.USOAUXI > C_COMP3.usototins and C_COMP3.USOUFC>0 and C_COMP3.USOAUXI<= C_COMP3.cantins
					UPDATE COMPINS set usototins=C_COMP3.USOAUXI where codcomp=C_COMP3.codcomp
					SELECT COMPINS
					= TABLEUPDATE(.T.)

					SW=.t.
					verror=1
				ELSE

					IF C_COMP3.USOAUXI> C_COMP3.cantins
						IF C_COMP3.USOUFC!=0
							v_codcomp=C_COMP3.codcomp
							SELECT sum(CANTUSADA) as cantuasada1,CODUFC from AUXUFC where codcomp=v_codcomp group by CODUFC into cursor c_aborrar_Auxufc
							SCAN
								SELECT count(*) from UFC where CODUFC=c_aborrar_Auxufc.CODUFC into cursor c_cuantos_ufc
								IF c_cuantos_ufc.cnt = 1
									Delete from UFC where CODUFC=c_aborrar_Auxufc.CODUFC
									SELECT UFC
									= TABLEUPDATE(.T.)
								ELSE
									SELECT sum(CANTUSADA) as cantusada1 from AUXUFC where codcomp=v_codcomp and CODUFC=c_aborrar_Auxufc.CODUFC;
										into cursor c_aborrar_Auxufc1
									UPDATE UFC set cantotal=cantotal-c_aborrar_Auxufc1.cantusada1 where ;
										CODUFC=c_aborrar_Auxufc.CODUFC
									SELECT UFC
									= TABLEUPDATE(.T.)
									Delete from AUXUFC where CODUFC=c_aborrar_Auxufc.CODUFC and codcomp=v_codcomp
									SELECT AUXUFC
									= TABLEUPDATE(.T.)
								ENDIF
								SELECT c_aborrar_Auxufc
							ENDSCAN

*!*								Delete from AUXUFC where codcomp=v_codcomp
*!*								Delete from UFC where CODUFC in (select CODUFC from AUXUFC where codcomp=C_COMP3.codcomp)

							SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
								FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted() and COMPINS.codtipo!=12 ;
								GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

							SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,usototins, ;
								COMPINS.CODINSUM,cantins,codfin FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted() and COMPINS.codtipo!=12 ;
								GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

							SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
								where !deleted() INTO TABLE T_COMP2
							SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
							SELECT T_COMP2
							APPEND FROM T_UNICOS
*Esto lo hago para eliminar las inconsistencias generadas en compins con uf y auxapli par este caso de una vez
							SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0.0000,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,iif(isnull(USOAUX),0.0000,USOAUX) as USOAUX,iif(isnull(USOUFC),0.0000,USOUFC) as USOUFC,CODINSUM,codcomp,cantins,usototins FROM  T_COMP2 ;
								where !deleted() HAVING USOAUXI!=usototins  order by CODINSUM INTO CURSOR C_COMP3a
							SCAN
								IF C_COMP3a.USOUFC >=0 and C_COMP3a.USOUFC=C_COMP3a.USOAUXI
*!*										SELECT * from AUXUFC where codcomp=C_COMP3a.codcomp into cursor C_borrar_ufc
*!*										SELECT * from UFC where CODUFC=C_borrar_ufc.CODUFC
************
									v_codcomp=C_COMP3a.codcomp
									SELECT sum(CANTUSADA) as cantuasada1,CODUFC from AUXUFC where codcomp=v_codcomp group by CODUFC into cursor c_aborrar_Auxufc
									SCAN
										SELECT count(*) from UFC where CODUFC=c_aborrar_Auxufc.CODUFC into cursor c_cuantos_ufc
										IF c_cuantos_ufc.cnt = 1
											Delete from UFC where CODUFC=c_aborrar_Auxufc.CODUFC
											SELECT UFC
											= TABLEUPDATE(.T.)
										ELSE
											SELECT sum(CANTUSADA) as cantusada1 from AUXUFC where codcomp=v_codcomp and CODUFC=c_aborrar_Auxufc.CODUFC;
												into cursor c_aborrar_Auxufc1
											UPDATE UFC set cantotal=cantotal-c_aborrar_Auxufc1.cantusada1 where ;
												CODUFC=c_aborrar_Auxufc.CODUFC
											SELECT UFC
											= TABLEUPDATE(.T.)
											Delete from AUXUFC where CODUFC=c_aborrar_Auxufc.CODUFC and codcomp=v_codcomp
											SELECT AUXUFC
											= TABLEUPDATE(.T.)

										ENDIF
										SELECT c_aborrar_Auxufc
									ENDSCAN
								ELSE
									SELECT * from insaplica where codinsap in (select codinsap from AUXAPLI where codcomp=C_COMP3a.codcomp) ;
										order by cantotal desc into cursor c_insa_borar
									SCAN
										UPDATE COMPINS set usototins=usototins-c_insa_borar.cantotal where codcomp=C_COMP3a.codcomp and usototins-c_insa_borar.cantotal>0
										VTALLY=_tally
										SELECT COMPINS
										= TABLEUPDATE(.T.)

										IF VTALLY=0
*me aseguro que este en 0 si al devolver diera negativo
											UPDATE COMPINS set usototins=0 where codcomp=C_COMP3a.codcomp
											SELECT COMPINS
											= TABLEUPDATE(.T.)

										ENDIF
										Delete from insaplica where codinsap=c_insa_borar.codinsap
										SELECT insaplica
										= TABLEUPDATE(.T.)
										SELECT sum(CANTUSADA) as cantusada1 from AUXAPLI where codcomp=C_COMP3a.codcomp ;
											into cursor c_nuevo_auxi
										IF C_COMP3a.cantins>= c_nuevo_auxi.cantusada1

											UPDATE COMPINS set usototins=usototins+c_nuevo_auxi.cantusada1  where codcomp=C_COMP3a.codcomp
											SELECT COMPINS
											= TABLEUPDATE(.T.)

											EXIT
										ENDIF
										SELECT c_insa_borar
									ENDSCAN
								ENDIF
							ENDSCAN
							SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
								FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted() and COMPINS.codtipo!=12 ;
								GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

							SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,usototins, ;
								COMPINS.CODINSUM,cantins,codfin FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted() and COMPINS.codtipo!=12 ;
								GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

							SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
								where !deleted() INTO TABLE T_COMP2
							SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
							SELECT T_COMP2
							APPEND FROM T_UNICOS
							SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0.0000,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,iif(isnull(USOAUX),0.0000,USOAUX) as USOAUX,iif(isnull(USOUFC),0.0000,USOUFC) as USOUFC,CODINSUM,codcomp,cantins,usototins FROM  T_COMP2 ;
								where !deleted() HAVING USOAUXI!=usototins  order by CODINSUM INTO CURSOR C_COMP3
						ENDIF
*!*
*!*							IF C_COMP3.USOAUXI > C_COMP3.usototins and C_COMP3.USOUFC=0 and C_COMP3.USOAUX<= C_COMP3.cantins
*!*								UPDATE COMPINS set usototins=C_COMP3.USOAUX where codcomp=C_COMP3.codcomp
*!*							ENDIF
						SELECT sum(cantins-usototins) as libre,codcomp,CODINSUM,cantins,usototins ;
							from COMPINS where CODINSUM=C_COMP3.CODINSUM having libre>0 into cursor c_compins_libre_tot
						IF c_compins_libre_tot.libre >= C_COMP3.USOAUXI
							UPDATE COMPINS set usototins=0 where codcomp= C_COMP3.codcomp
							SELECT COMPINS
							= TABLEUPDATE(.T.)

							SELECT * from AUXAPLI where codcomp=C_COMP3.codcomp into cursor c_auxapli_revisar
							SCAN
								SELECT sum(CANTUSADA) as cantusada1,codcomp,insaplica.codinsap,insaplica.cantotal,;
									insaplica.codaplic,insaplica.CODINSUM,insaplica.codtipo from AUXAPLI inner join insaplica on ;
									AUXAPLI.codinsap=insaplica.codinsap where AUXAPLI.codinsap=c_auxapli_revisar.codinsap ;
									into cursor c_insap_real
*SELECT * from auxapli where codinsap=c_insap_real.codinsap into cursor c_error_auxapli
*	IF c_insap_real.cantusada1!=c_insap_real.cantotal
								IF c_insap_real.cantusada1!=c_insap_real.cantotal
									Delete from AUXAPLI where codinsap=c_insap_real.codinsap and codcomp=c_auxapli_revisar.codcomp
									SELECT AUXAPLI
									= TABLEUPDATE(.T.)

									SELECT cantins-usototins as libre ,codcomp,CODINSUM,cantins,usototins ;
										from COMPINS where CODINSUM=C_COMP3.CODINSUM having libre>0 into cursor c_compins_libre
									acum=c_insap_real.cantotal
									SCAN
										IF c_compins_libre.libre>=acum
											INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
												values(c_insap_real.codinsap,c_insap_real.CODINSUM,c_insap_real.codtipo,c_compins_libre.codcomp,acum,c_insap_real.codaplic)
											SELECT AUXAPLI
											= TABLEUPDATE(.T.)

											UPDATE COMPINS set usototins=usototins+acum where codcomp=c_compins_libre.codcomp
											SELECT COMPINS
											= TABLEUPDATE(.T.)

											EXIT
										ELSE
											INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
												values(c_insap_real.codinsap,c_insap_real.CODINSUM,c_insap_real.codtipo,c_compins_libre.codcomp,c_compins_libre.libre,c_insap_real.codaplic)
											SELECT AUXAPLI
											= TABLEUPDATE(.T.)

											UPDATE COMPINS set usototins=usototins+c_compins_libre.libre where codcomp=c_compins_libre.codcomp
											SELECT COMPINS
											= TABLEUPDATE(.T.)

											acum=acum-c_compins_libre.libre
										ENDIF
										SELECT c_compins_libre
									ENDSCAN
								ENDIF

								SELECT c_auxapli_revisar
							ENDSCAN
						ELSE

							Delete from insaplica where codinsap in (SELECT codinsap from AUXAPLI where codcomp=C_COMP3.codcomp and CODINSUM=C_COMP3.CODINSUM)
							SELECT insaplica
							= TABLEUPDATE(.T.)
							Delete from AUXAPLI where codcomp=C_COMP3.codcomp and CODINSUM=C_COMP3.CODINSUM
							SELECT AUXAPLI
							= TABLEUPDATE(.T.)

						ENDIF
						IF C_COMP3.USOAUXI>C_COMP3.cantins
*!*								SELECT * from AUXAPLI where codcomp=C_COMP3.codcomp into cursor c_eliminar
*!*								scan
*!*
*!*									update compins set usototins=usototins-c_eliminar.cantusada where codcomp=c_eliminar.codcomp and usototins-c_eliminar.cantusada>0
*!*									delete from auxapli where codcomp=c_eliminar.codcomp and codinsap=c_eliminar.codinsap
*!*								endscan
							SELECT * from insaplica where codinsap in (select codinsap from AUXAPLI where codcomp=C_COMP3.codcomp) ;
								order by cantotal desc into cursor c_insa_borar
							SCAN
								UPDATE COMPINS set usototins=usototins-c_insa_borar.cantotal where codcomp=C_COMP3.codcomp and usototins-c_insa_borar.cantotal>0
								SELECT COMPINS
								= TABLEUPDATE(.T.)

								IF _tally=0
*me aseguro que este en 0 si al devolver diera negativo
									UPDATE COMPINS set usototins=0 where codcomp=C_COMP3.codcomp
									SELECT COMPINS
									= TABLEUPDATE(.T.)

								ENDIF
								Delete from insaplica where codinsap=c_insa_borar.codinsap
								SELECT insaplica
								= TABLEUPDATE(.T.)
								SELECT sum(CANTUSADA) as cantusada1 from AUXAPLI where codcomp=C_COMP3.codcomp ;
									into cursor c_nuevo_auxi
								IF C_COMP3.cantins>= c_nuevo_auxi.cantusada1

									UPDATE COMPINS set usototins=usototins+c_nuevo_auxi.cantusada1  where codcomp=C_COMP3.codcomp
									SELECT COMPINS
									= TABLEUPDATE(.T.)

									EXIT
								ENDIF
								SELECT c_insa_borar
							ENDSCAN
						ENDIF


					ELSE
*C_COMP3.USOAUXI <= C_COMP3.cantins

					ENDIF


*si quiero que corrija estos errores de una o esperar que los corrija sin ninguna otra adicion
				ENDIF
			ENDIF

			SELECT C_COMP3
		ENDSCAN
*************Revisa para valores en usototins > que cantins******************************

		SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
			FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted()  and COMPINS.codtipo!=12 ;
			GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

		SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,usototins, ;
			COMPINS.CODINSUM,cantins, codfin as codfin FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted() and COMPINS.codtipo!=12 ;
			GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

		SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
			where !deleted() INTO TABLE T_COMP2
		SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
		SELECT T_COMP2
		APPEND FROM T_UNICOS

		SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0.0000,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,iif(isnull(USOAUX),0.0000,USOAUX) as USOAUX,iif(isnull(USOUFC),0.0000,USOUFC) as USOUFC,CODINSUM,codcomp,cantins,usototins,codfin FROM  T_COMP2 ;
			where !deleted() HAVING USOAUXI!=usototins  order by CODINSUM INTO CURSOR C_COMP3
		SCAN
*******if uso >=C_COMP3.USOAUXI
****sI HAY UN PROBLEMA DONDE USOTOTINS ES MAYOR QUE CANTINS
			IF C_COMP3.USOAUXI > C_COMP3.usototins
				SELECT * FROM AUXAPLI WHERE codcomp=C_COMP3.codcomp and !deleted() ORDER BY CANTUSADA ASC INTO CURSOR C_ABORRAR
				SW1=0
*hacer algo para cuando no haya que borrar datos ya que es de uso ufc
				SCAN
*IF C_COMP3.USOTOTINS-C_ABORRAR.CANTUSADA  <= C_COMP3.CANTINS-C_COMP3.USOUFC
					SW1=1
					IF C_COMP3.usototins  <= C_COMP3.cantins-C_COMP3.USOUFC && AND C_COMP3.USOTOTINS  >= C_COMP3.USOAUXI &&CODICION DE SALIDA USOTOTINS < CANTINS Y USOTOTINS >= UAOAUXI
						IF C_COMP3.usototins  != C_COMP3.USOAUXI or C_COMP3.usototins< C_COMP3.cantins

							SELECT * from AUXAPLI where codcomp= C_COMP3.codcomp into cursor c_aux
							SELECT sum(CANTUSADA) as cantot,* from AUXAPLI where codcomp = c_aux.codcomp into cursor c_aux1


							SELECT insaplica.codinsap,insaplica.codaplic,codcomp,cantotal,insaplica.codtipo,insaplica.CODINSUM, codfin as finca from insaplica inner join AUXAPLI on ;
								insaplica.codinsap=AUXAPLI.codinsap inner join infculti  on insaplica.numcontrol=infculti.numcontrol where AUXAPLI.codcomp= C_COMP3.codcomp and !deleted() into cursor c_insa_auxi
							SCAN
								SELECT sum(cantins-usototins) AS libre,* FROM COMPINS WHERE CODINSUM=C_COMP3.CODINSUM and  COMPINS.codfin=c_insa_auxi.finca;
									and !deleted() order by libre asc having libre>0 INTO CURSOR C_NUEVOCOMP1
								SELECT cantotal,insaplica.codinsap,insaplica.codaplic,codcomp,cantot,insaplica.codtipo,insaplica.CODINSUM from insaplica inner join c_aux1 on ;
									insaplica.codinsap=AUXAPLI.codinsap where cantot!=cantotal and !deleted() AND codcomp=c_insa_auxi.codcomp into cursor c_insa_real

								IF C_NUEVOCOMP1.libre >= c_insa_real.cantotal
									SELECT * from AUXAPLI where codinsap= c_insa_auxi.codinsap into cursor c_aux_aborrar
									Delete from AUXAPLI where codinsap=c_insa_auxi.codinsap
									SELECT AUXAPLI
									= TABLEUPDATE(.T.)

*borro de usototins_compins los que elimine de auxapli
									SCAN
										SELECT usototins-c_aux_aborrar.CANTUSADA as devolver from COMPINS where ;
											codcomp=c_aux_aborrar.codcomp and !deleted() into cursor c_devolver
										IF c_devolver.devolver >=0
											UPDATE COMPINS set usototins=usototins-c_aux_aborrar.CANTUSADA where codcomp=c_aux_aborrar.codcomp
											SELECT COMPINS
											= TABLEUPDATE(.T.)

										ELSE
											UPDATE COMPINS set usototins=0 where codcomp=c_aux_aborrar.codcomp
											SELECT COMPINS
											= TABLEUPDATE(.T.)

										ENDIF
										SELECT c_aux_aborrar
									ENDSCAN

									SELECT cantins-usototins AS libre,* FROM COMPINS WHERE CODINSUM=C_COMP3.CODINSUM ;
										and COMPINS.codfin=c_insa_auxi.finca and !deleted() having libre>0 INTO CURSOR C_NUEVOCOMP2

									acum= c_insa_auxi.cantotal
									SCAN
										IF acum<=0
											EXIT
										ELSE
											IF C_NUEVOCOMP2.libre >= acum
*correccion de problemas
												INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
													values (c_insa_auxi.codinsap,C_NUEVOCOMP2.CODINSUM,C_NUEVOCOMP2.codtipo,C_NUEVOCOMP2.codcomp,acum,c_insa_auxi.codaplic)
												SELECT AUXAPLI
												= TABLEUPDATE(.T.)


												UPDATE COMPINS SET usototins=usototins+acum  WHERE codcomp=C_NUEVOCOMP2.codcomp
												SELECT COMPINS
												= TABLEUPDATE(.T.)

												acum =0
											ELSE
												INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
													values (c_insa_auxi.codinsap,C_NUEVOCOMP2.CODINSUM,C_NUEVOCOMP2.codtipo,C_NUEVOCOMP2.codcomp,C_NUEVOCOMP2.libre,c_insa_auxi.codaplic)
												SELECT AUXAPLI
												= TABLEUPDATE(.T.)

												UPDATE COMPINS SET usototins=usototins+C_NUEVOCOMP2.libre WHERE codcomp=C_NUEVOCOMP2.codcomp
												SELECT COMPINS
												= TABLEUPDATE(.T.)

												acum = acum- C_NUEVOCOMP2.libre
											ENDIF
										ENDIF
										SELECT C_NUEVOCOMP2
									ENDSCAN
								ELSE
									Delete from insaplica where codinsap=c_insa_auxi.codinsap
									SELECT insaplica
									= TABLEUPDATE(.T.)

								ENDIF
								SELECT c_insa_auxi
							ENDSCAN

						ENDIF
						EXIT
					ELSE
*arreglar el libre de la finca del error
						SELECT cantins-usototins AS libre,* FROM COMPINS WHERE CODINSUM=C_COMP3.CODINSUM and codfin=C_COMP3.codfin  and !deleted() having libre>0 INTO CURSOR C_NUEVOCOMP

						acum= C_ABORRAR.CANTUSADA
						SCAN
							IF acum<=0
								EXIT
							ELSE
								IF C_NUEVOCOMP.libre >= acum
*C_ABORRAR.CANTUSADA
*UPDATE COMPINS SET USOTOTINS= C_COMP3.USOTOTINS-Acum  WHERE COMPINS.codcomp=C_COMP3.codcomp
									UPDATE COMPINS SET usototins= C_COMP3.USOAUXI-acum  WHERE COMPINS.codcomp=C_COMP3.codcomp
									SELECT COMPINS
									= TABLEUPDATE(.T.)


									UPDATE COMPINS SET usototins=usototins+acum  WHERE codcomp=C_NUEVOCOMP.codcomp
									SELECT COMPINS
									= TABLEUPDATE(.T.)

									UPDATE AUXAPLI SET codcomp=C_NUEVOCOMP.codcomp WHERE codcomp = C_ABORRAR.codcomp and codinsap= C_ABORRAR.codinsap
									SELECT AUXAPLI
									= TABLEUPDATE(.T.)
									acum =0
* Acum - C_ABORRAR.CANTUSADA
								ELSE
*UPDATE COMPINS SET USOTOTINS= C_COMP3.USOTOTINS-C_NUEVOCOMP.LIBRE WHERE COMPINS.codcomp=C_COMP3.codcomp
									UPDATE COMPINS SET usototins= C_COMP3.USOAUXI-C_NUEVOCOMP.libre  WHERE COMPINS.codcomp=C_COMP3.codcomp
									SELECT COMPINS
									= TABLEUPDATE(.T.)

									UPDATE COMPINS SET usototins=usototins+C_NUEVOCOMP.libre WHERE codcomp=C_NUEVOCOMP.codcomp
									SELECT COMPINS
									= TABLEUPDATE(.T.)

*Delete FROM AUXAPLI WHERE codcomp = C_ABORRAR.codcomp and codinsap= C_ABORRAR.codinsap
									UPDATE AUXAPLI SET CANTUSADA=C_ABORRAR.CANTUSADA-C_NUEVOCOMP.libre WHERE codcomp = C_ABORRAR.codcomp and codinsap= C_ABORRAR.codinsap
									SELECT AUXAPLI
									= TABLEUPDATE(.T.)
									INSERT INTO AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
										VALUES(C_ABORRAR.codinsap,C_ABORRAR.CODINSUM,C_ABORRAR.codtipo,C_NUEVOCOMP.codcomp,C_NUEVOCOMP.libre,C_ABORRAR.codaplic)
									SELECT AUXAPLI
									= TABLEUPDATE(.T.)

									acum = acum - C_NUEVOCOMP.libre
************codigo para calcular el valor arreglado de compins con el nuevo codcomp
									SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
										FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted() and COMPINS.codtipo!=12 ;
										GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

									SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,usototins, ;
										COMPINS.CODINSUM,cantins,codfin FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted() and COMPINS.codtipo!=12 ;
										GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

									SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
										where !deleted() INTO TABLE T_COMP2
									SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
									SELECT T_COMP2
									APPEND FROM T_UNICOS
									SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0.0000,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,iif(isnull(USOAUX),0.0000,USOAUX) as USOAUX,iif(isnull(USOUFC),0.0000,USOUFC) as USOUFC,CODINSUM,codcomp,cantins,usototins FROM  T_COMP2 ;
										where !deleted() HAVING USOAUXI!=usototins  order by CODINSUM INTO CURSOR C_COMP3
*********fin codigo compins con codcomp fijo
								ENDIF

							ENDIF

							SELECT C_NUEVOCOMP
						ENDSCAN
************codigo para calcular el valor arreglado de compins con el nuevo codcomp
						SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
							FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted() and COMPINS.codtipo!=12 ;
							GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

						SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,usototins, ;
							COMPINS.CODINSUM,cantins,codfin FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted() and COMPINS.codtipo!=12 ;
							GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

						SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
							where !deleted() INTO TABLE T_COMP2
						SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
						SELECT T_COMP2
						APPEND FROM T_UNICOS
						SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0.0000,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,iif(isnull(USOAUX),0.0000,USOAUX) as USOAUX,iif(isnull(USOUFC),0.0000,USOUFC) as USOUFC,CODINSUM,codcomp,cantins,usototins,codfin FROM  T_COMP2 ;
							where !deleted() HAVING USOAUXI!=usototins  order by CODINSUM INTO CURSOR C_COMP3
*********fin codigo COMPINS con codcomp fijo
					ENDIF

					SELECT C_ABORRAR
				ENDSCAN
				SELECT C_COMP3
				IF SW1=0  &&Solo es de UFC se pone 0 hasta que encuentra espacios libres con un mismo codinsum
					v_codcomp = C_COMP3.codcomp
					v_usototins= C_COMP3.usototins
					v_usoauxi = C_COMP3.USOAUXI
					v_cantins=  C_COMP3.cantins
					UPDATE COMPINS set usototins= C_COMP3.USOAUXI+usototins where COMPINS.codcomp= v_codcomp and  (v_usoauxi + v_usototins)<= v_cantins

					SELECT COMPINS
					= TABLEUPDATE(.T.)

					IF _tally>0
*MESSAGEBOX ("actualizacion hecha",32,"")
					ELSE
						SELECT sum(cantins - usototins) as libre  from COMPINS where CODINSUM=C_COMP3.CODINSUM ;
							and !deleted() and COMPINS.codfin=C_COMP3.codfin into cursor c_libre1
						IF c_libre1.libre >= C_COMP3.USOAUXI+ C_COMP3.usototins
							SELECT cantins-usototins AS libre,* FROM COMPINS WHERE CODINSUM=C_COMP3.CODINSUM and !deleted() and COMPINS.codfin=C_COMP3.codfin ;
								having libre>0 INTO CURSOR C_NUEVOCOMP

							acum= C_COMP3.USOAUXI

							SCAN
								IF acum<=0
									EXIT
								ELSE
									IF C_NUEVOCOMP.libre >= acum
										UPDATE COMPINS SET usototins=usototins+acum  WHERE codcomp=C_NUEVOCOMP.codcomp
										SELECT COMPINS
										= TABLEUPDATE(.T.)

										acum =0
									ELSE
										UPDATE COMPINS SET usototins=usototins+C_NUEVOCOMP.libre  WHERE codcomp=C_NUEVOCOMP.codcomp
										SELECT COMPINS
										= TABLEUPDATE(.T.)

										acum = acum - C_NUEVOCOMP.libre
									ENDIF
									SELECT C_NUEVOCOMP
								ENDIF
							ENDSCAN
						ENDIF

						SELECT * FROM AUXUFC WHERE codcomp=C_COMP3.codcomp ORDER BY CANTUSADA ASC INTO CURSOR C_ABORRAR
						SELECT cantins-usototins AS libre,* FROM COMPINS WHERE CODINSUM=C_COMP3.CODINSUM and codfin=C_COMP3.codfin and !deleted() ;
							having libre>0 INTO CURSOR C_NUEVOCOMP


						UPDATE COMPINS set usototins= cantins where COMPINS.codcomp= C_COMP3.codcomp
						SELECT COMPINS
						= TABLEUPDATE(.T.)


					ENDIF
				ENDIF
*********** cREA C_COMP3 PARA EL SIGUIENTE CICLO Y SE ASEGURA QUE ESTE LOS ERRORES CORREGIDOS SE REFLEJEN
				SELECT SUM(AUXAPLI.CANTUSADA) AS USOAUX, usototins,COMPINS.codcomp,COMPINS.CODINSUM,cantins,codfin ;
					FROM COMPINS LEFT JOIN AUXAPLI ON COMPINS.codcomp=AUXAPLI.codcomp where !deleted() and COMPINS.codtipo!=12 ;
					GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA

				SELECT SUM(AUXUFC.CANTUSADA) AS USOUFC,COMPINS.codcomp,usototins, ;
					COMPINS.CODINSUM,cantins,codfin FROM COMPINS inner JOIN AUXUFC ON AUXUFC.codcomp=COMPINS.codcomp where !deleted()and COMPINS.codtipo!=12  ;
					GROUP BY COMPINS.codcomp ORDER BY COMPINS.codcomp INTO CURSOR C_COMPARA1

				SELECT C_COMPARA.*,C_COMPARA1.USOUFC FROM C_COMPARA LEFT JOIN C_COMPARA1 ON C_COMPARA.codcomp=C_COMPARA1.codcomp ;
					where !deleted() INTO TABLE T_COMP2
				SELECT * FROM C_COMPARA1 WHERE codcomp NOT IN (SELECT codcomp FROM T_COMP2) INTO TABLE T_UNICOS
				SELECT T_COMP2
				APPEND FROM T_UNICOS

				SELECT IIF(ISNULL(USOUFC),IIF(ISNULL(USOAUX),0.0000,USOAUX),IIF(ISNULL(USOAUX),USOUFC,USOAUX+USOUFC)) AS USOAUXI,iif(isnull(USOAUX),0.0000,USOAUX) as USOAUX,iif(isnull(USOUFC),0.0000,USOUFC) as USOUFC,CODINSUM,codcomp,cantins,usototins,codfin FROM  T_COMP2 ;
					where !deleted() HAVING USOAUXI!=usototins  order by CODINSUM INTO CURSOR C_COMP3
*****************************************
				LOOP
&&sALTA AL SIGUIENTE ERROR
*aqui voy
			ENDIF
******************
			IF C_COMP3.usototins >= C_COMP3.USOAUXI
				UPDATE COMPINS SET usototins= C_COMP3.USOAUXI WHERE COMPINS.codcomp=C_COMP3.codcomp and codfin=C_COMP3.codfin
				SELECT COMPINS
				= TABLEUPDATE(.T.)


*!*					MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
*!*						"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
				verror =1  &&Error de aplicaciones
				SW=.T.
			ELSE
*******if C_COMP3.USOAUX = C_COMP3.USOTOTINS
				IF C_COMP3.USOAUXI = C_COMP3.usototins
					SELECT codcomp,CODINSUM,cantins,usototins,(cantins-usototins) AS libre  FROM COMPINS ;
						WHERE CODINSUM = C_COMP3.CODINSUM AND (cantins-usototins)>0 and !deleted() and codfin=C_COMP3.codfin order by codcomp INTO CURSOR C_DISPO
					SELECT SUM(libre) AS INVENTARIO FROM C_DISPO INTO CURSOR C_DISPO1
					IF C_DISPO1.INVENTARIO >= C_COMP3.USOUFC
***IF C_DISPO1.INVENTARIO > C_COMP3.USOUFC
						SELECT C_DISPO
						SW1= 0
						V_ACUM = C_COMP3.USOUFC
						SCAN
****SELECT C_DISPO
							IF C_DISPO.libre >= C_COMP3.USOUFC
****IF C_DISPO.LIBRE >= C_COMP3.USOUFC
								UPDATE COMPINS SET usototins = usototins+C_COMP3.USOUFC WHERE codcomp = C_DISPO.codcomp
								SELECT COMPINS
								= TABLEUPDATE(.T.)

								SELECT COUNT(*) AS cuantos FROM AUXUFC WHERE codcomp=C_COMP3.codcomp and !deleted() INTO CURSOR C_AUFC

								IF C_AUFC.cuantos =1
									UPDATE AUXUFC SET codcomp = C_COMP3.USOUFC WHERE codcomp = C_COMP3.codcomp AND !DELETED() and codfin=C_COMP3.codfin
									SELECT AUXUFC
									= TABLEUPDATE(.T.)
								ELSE
*delete auxufc where codufc not in (select top 1 codufc from auxufc where codcomp=c_comp3.codcomp order by codufc)
*select * from auxufc where auxufc.codufc in (select codufc from auxufc where codcomp=c_comp3.codcomp order by codufc )
									SELECT CODUFC,codcomp,SUM(CANTUSADA) AS cantusada1,CODINSUM, COUNT(CODUFC) AS cuantos FROM AUXUFC ;
										WHERE codcomp=C_COMP3.codcomp and !deleted() ORDER BY CODUFC GROUP BY CODUFC INTO CURSOR C_AUFC1
									SCAN
*aqui
										IF C_AUFC1.cuantos =1
											UPDATE AUXUFC SET codcomp = C_DISPO.codcomp WHERE codcomp = C_COMP3.codcomp AND !DELETED() and codfin=C_COMP3.codfin
											SELECT AUXUFC
											= TABLEUPDATE(.T.)
										ELSE
											Delete FROM AUXUFC WHERE CODUFC=C_AUFC1.CODUFC AND codcomp=C_AUFC1.codcomp
											INSERT INTO AUXUFC (CODUFC,codcomp,CANTUSADA,CODINSUM) VALUES(C_AUFC1.CODUFC,C_AUFC1.codcomp,C_AUFC1.cantusada1,C_AUFC1.CODINSUM)
											SELECT AUXUFC
											= TABLEUPDATE(.T.)

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
									SELECT codfin,CODUFC,CODINSUM,cantotal FROM UFC WHERE CODUFC IN;
										(SELECT CODUFC FROM AUXUFC WHERE codcomp=C_COMP3.codcomp) INTO CURSOR C_UFC1

									IF SW1 =0
										SW1 =1
										Delete FROM AUXUFC WHERE CODUFC=C_UFC1.CODUFC AND codcomp=C_COMP3.codcomp
										SELECT AUXUFC
										= TABLEUPDATE(.T.)
										INSERT INTO AUXUFC (CODUFC,codcomp,CANTUSADA,CODINSUM) ;
											VALUES(C_UFC1.CODUFC,C_COMP3.codcomp,C_DISPO.libre,C_UFC1.CODINSUM)
										SELECT AUXUFC
										= TABLEUPDATE(.T.)

									ELSE
										SELECT C_UFC1
										SCAN
											IF C_DISPO.libre >= C_AUFC1.cantotal
												INSERT INTO AUXUFC (CODUFC,codcomp,CANTUSADA,CODINSUM) ;
													VALUES(C_UFC1.CODUFC,C_COMP3.codcomp,C_UFC1.cantotal,C_UFC1.CODINSUM)
												SELECT AUXUFC
												= TABLEUPDATE(.T.)

											ELSE
												INSERT INTO AUXUFC (CODUFC,codcomp,CANTUSADA,CODINSUM) ;
													VALUES(C_UFC1.CODUFC,C_COMP3.codcomp,C_DISPO.libre,C_UFC1.CODINSUM)
												SELECT AUXUFC
												= TABLEUPDATE(.T.)

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
					IF C_COMP3.USOAUXI < C_COMP3.usototins
*******IF C_COMP3.USOAUX < C_COMP3.USOTOTINS
*ASUMIENDO QUE AUXUFC Y UFC ESTAN SINCRONIZADOS
*SINCROZINO COMPINS CON AUXAPLI

						UPDATE COMPINS SET usototins= C_COMP3.USOAUXI WHERE COMPINS.codcomp=C_COMP3.codcomp AND cantins>=C_COMP3.USOAUX and codfin=C_COMP3.codfin
						SELECT COMPINS
						= TABLEUPDATE(.T.)


*BUSCO LO QUE QUEDA DISPONIBLE TRAS LA ACTUALIZACION
						SELECT codcomp,CODINSUM,cantins,usototins,(cantins-usototins) AS libre  FROM COMPINS ;
							WHERE CODINSUM = C_COMP3.CODINSUM AND (cantins-usototins)>0 and !deleted() order by codcomp INTO CURSOR C_DISPO
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
							SELECT AUXUFC
							= TABLEUPDATE(.T.)
							UPDATE COMPINS SET usototins=usototins+C_AUXUFC2.CANTUSADA WHERE  codcomp=C_DISPO.codcomp and codfin=C_COMP3.codfin
							SELECT COMPINS
							= TABLEUPDATE(.T.)

							SELECT codcomp,CODINSUM,cantins,usototins,(cantins-usototins) AS libre  FROM COMPINS ;
								WHERE CODINSUM = C_COMP3.CODINSUM and !deleted() HAVING (cantins-usototins)>0 order by codcomp INTO CURSOR C_DISPO
							SELECT C_AUXUFC2

						ENDSCAN

						IF V_ACUM>0
*!*							INSERT INTO AUXUFC (CODUFC,CODCOMP,CANTUSADA,CODINSUM) VALUES(
*!*							INSERT INTO UFC (CODFIN,CODUFC,CODINSUM,CANTOTAL,NOMINSUM,CODTIPO,
							ERROR_INV = ERROR_INV +1
						ENDIF

* IF C_COMP3.USOAUX < C_COMP3.USOTOTINS
					ELSE
						IF C_COMP3.USOAUXI > C_COMP3.usototins
							UPDATE COMPINS SET usototins= C_COMP3.USOAUXI WHERE COMPINS.codcomp=C_COMP3.codcomp AND cantins>=C_COMP3.USOAUX and codfin=C_COMP3.codfin
							SELECT COMPINS
							= TABLEUPDATE(.T.)

							IF _tally = 0
*debo eliminar de auxapli el sobrante
								SELECT sum(cantins - usototins) as libre  from COMPINS where CODINSUM=C_COMP3.CODINSUM and !deleted() into cursor c_libre
*si hay libre para aplicar???
								IF c_libre.libre + C_COMP3.usototins >= C_COMP3.USOAUX

									SELECT * from AUXAPLI where codcomp = C_COMP3.codcomp into cursor c_auxiliar
									vcantusada = 0
									switch=0
									SCAN
										IF switch=0
											IF vcantusada+ c_auxiliar.CANTUSADA >= C_COMP3.usototins
*reubicar los siguientes codinsap de auxapli -arreglo asumiendo que el daño es de un solo codinsap
												vcodcomp = 	c_auxiliar.codcomp
												vcantusada = vcantusada + c_auxiliar.CANTUSADA
*asigno nuevo valor a compins
												UPDATE COMPINS SET usototins= vcantusada WHERE COMPINS.codcomp=vcodcomp and codfin=C_COMP3.codfin
												SELECT COMPINS
												= TABLEUPDATE(.T.)

												switch =1
*EXIT
											ELSE
												vcantusada = vcantusada + c_auxiliar.CANTUSADA
											ENDIF
										ELSE
************
											vcodinsap = c_auxiliar.codinsap
											vcodcomp = 	c_auxiliar.codcomp
*Asigno el nuevo codcomp a los que en auxapli no tienen usototins
											SELECT codcomp,cantins-usototins as libre from COMPINS where CODINSUM=C_COMP3.CODINSUM and !deleted() and codfin=C_COMP3.codfin;
												having libre >0 into cursor c_usofree
											v_aaplicar = c_auxiliar.CANTUSADA
											SCAN
												IF c_usofree.libre >= v_aaplicar
													UPDATE COMPINS SET usototins= v_aaplicar+usototins WHERE COMPINS.codcomp=c_usofree.codcomp
													SELECT COMPINS
													= TABLEUPDATE(.T.)

													INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
														values(c_auxiliar.codinsap,c_auxiliar.CODINSUM,c_auxiliar.codtipo,c_usofree.codcomp,v_aaplicar ,c_auxiliar.codaplic)
													SELECT AUXAPLI
													= TABLEUPDATE(.T.)

													v_aaplicar = 0
													EXIT
												ELSE
													Delete from AUXAPLI where codcomp=vcodcomp and codinsap=vcodinsap
													SELECT AUXAPLI
													= TABLEUPDATE(.T.)

													UPDATE COMPINS SET usototins= usototins+c_usofree.libre  WHERE COMPINS.codcomp=c_usofree.codcomp
													SELECT COMPINS
													= TABLEUPDATE(.T.)

													INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
														values(c_auxiliar.codinsap,c_auxiliar.CODINSUM,c_auxiliar.codtipo,c_usofree.codcomp,c_usofree.libre ,c_auxiliar.codaplic)
													SELECT AUXAPLI
													= TABLEUPDATE(.T.)

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




***************Corrige inconsistencias entre insaplica e auxapli y por consiguiente con usototins de compins
***Cuando Insapli tiene todo los insumos y auxapli le falta

	SELECT SUM(AUXAPLI.CANTUSADA) AS AUXCANTUSADA, cantotal,insaplica.codinsap,insaplica.CODINSUM,insaplica.numcontrol,insaplica.codaplic, ;
		insaplica.codtipo, codfin as finca FROM insaplica LEFT JOIN AUXAPLI ;
		ON insaplica.codinsap=AUXAPLI.codinsap inner join infculti on insaplica.numcontrol=infculti.numcontrol;
		GROUP BY insaplica.codinsap ORDER BY insaplica.codinsap where !deleted() HAVING  AUXCANTUSADA != cantotal or isnull(AUXCANTUSADA);
		into cursor C_APLICA_temp

	SELECT iif (isnull(AUXCANTUSADA),0,AUXCANTUSADA) as AUXCANTUSADA, cantotal,codinsap,CODINSUM,numcontrol,codaplic,codtipo,finca;
		FROM C_APLICA_temp;
		INTO CURSOR C_APLICA
	SELECT C_APLICA

	SCAN
		IF C_APLICA.cantotal >C_APLICA.AUXCANTUSADA
*revisar los mayores de 0 y si es el mismo codcomp utilizado por auxapli el que tiene espacio los usa sino busca espacio en otro
* verifica igualmente si hay espacio
*select codcomp,cantusada from auxapli where codinsap=c_aplica.codinsap into cursor c_auxapli1
			SELECT cantins,usototins,CODINSUM FROM COMPINS WHERE CODINSUM=C_APLICA.CODINSUM ;
				HAVING (cantins-usototins) >0 INTO CURSOR C_INSUMO

			SELECT SUM(cantins-usototins) AS INVENTARIO FROM COMPINS WHERE !deleted() and CODINSUM=C_APLICA.CODINSUM  and codfin=C_APLICA.finca;
				INTO CURSOR C_HAYINVENTARIO
			V_ACUM= C_APLICA.cantotal-C_APLICA.AUXCANTUSADA
			IF C_HAYINVENTARIO.INVENTARIO >= (C_APLICA.cantotal-C_APLICA.AUXCANTUSADA)
*el actual codinsap - codcomp codcomp tiene el valor total de la aplicacion ????
				SELECT codcomp,cantins,usototins,cantins-usototins AS libre  FROM COMPINS WHERE CODINSUM=C_APLICA.CODINSUM and codfin=C_APLICA.finca ;
					HAVING libre >0 INTO CURSOR C_DISPONIBLES
				SCAN
					SELECT codcomp,cantins,usototins,cantins-usototins AS libre  FROM COMPINS WHERE CODINSUM=C_APLICA.CODINSUM  ;
						HAVING libre >0 INTO CURSOR C_DISPONIBLES
					GO top
*						IN (SELECT codcomp FROM AUXAPLI WHERE AUXAPLI.codinsap=C_APLICA.codinsap)
					IF 	C_DISPONIBLES.libre >= 	V_ACUM
*!*							SELECT count(*) as cuantos from AUXAPLI where codinsap=C_APLICA.codinsap and codcomp=C_DISPONIBLES.codcomp into cursor c_cuantos_auxapli
*!*							IF c_cuantos_auxapli.cuantos =1
*!*								UPDATE AUXAPLI SET CANTUSADA = CANTUSADA+V_ACUM where codinsap=C_APLICA.codinsap and codcomp=C_DISPONIBLES.codcomp
*!*							ELSE
*!*							Delete from AUXAPLI where codinsap=C_APLICA.codinsap and codcomp=C_DISPONIBLES.codcomp
*V_ACUM = V_ACUM - C_APLICA.auxusada
						INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
							values(C_APLICA.codinsap,C_APLICA.CODINSUM,C_APLICA.codtipo, C_DISPONIBLES.codcomp,;
							V_ACUM,C_APLICA.codaplic)
						SELECT AUXAPLI
						= TABLEUPDATE(.T.)

*!*							ENDIF
						SELECT sum(CANTUSADA) as AUXCANTUSADA from AUXAPLI where codcomp=C_DISPONIBLES.codcomp into cursor c_auxapli
						SELECT sum(CANTUSADA) as AUXCANTUSADA from AUXUFC where codcomp=C_DISPONIBLES.codcomp into cursor c_auxufc
						v_usototins= 0
						v_usototins = c_auxufc.AUXCANTUSADA+c_auxapli.AUXCANTUSADA
*select codinsum,codcomp,usototins,cantins from WHERE codcomp=C_DISPONIBLES.codcomp and cantins
						UPDATE COMPINS SET usototins = v_usototins WHERE codcomp=C_DISPONIBLES.codcomp
						SELECT COMPINS
						= TABLEUPDATE(.T.)

						EXIT
					ELSE
****************************** Se debe aplicar en varios auxapli codcomp ---codinsap

						V_ACUM=V_ACUM -C_DISPONIBLES.libre
						SELECT count(*) as cuantos from AUXAPLI where codinsap=C_APLICA.codinsap and codcomp=C_DISPONIBLES.codcomp into cursor c_cuantos_auxapli
						IF c_cuantos_auxapli.cuantos =1
							UPDATE AUXAPLI SET CANTUSADA = C_DISPONIBLES.libre where codinsap=C_APLICA.codinsap and codcomp=C_DISPONIBLES.codcomp
							SELECT AUXAPLI
							= TABLEUPDATE(.T.)
						ELSE
*Delete from AUXAPLI where codinsap=C_APLICA.codinsap and codcomp=C_DISPONIBLES.codcomp
							INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
								values(C_APLICA.codinsap,C_APLICA.CODINSUM,C_APLICA.codtipo, C_DISPONIBLES.codcomp,;
								C_DISPONIBLES.libre,C_APLICA.codaplic)
							SELECT AUXAPLI
							= TABLEUPDATE(.T.)

						ENDIF
*UPDATE COMPINS SET USOTOTINS = COMPINS.USOTOTINS+ C_DISPONIBLES.libre where codcomp=C_DISPONIBLES.codcomp
						SELECT sum(CANTUSADA) as AUXCANTUSADA from AUXAPLI where codcomp=C_DISPONIBLES.codcomp into cursor c_auxapli
						SELECT sum(CANTUSADA) as AUXCANTUSADA from AUXUFC where codcomp=C_DISPONIBLES.codcomp into cursor c_auxufc
						v_usototins= 0
						v_usototins = c_auxufc.AUXCANTUSADA+c_auxapli.AUXCANTUSADA
						UPDATE COMPINS SET usototins = v_usototins WHERE codcomp=C_DISPONIBLES.codcomp
						SELECT COMPINS
						= TABLEUPDATE(.T.)

					ENDIF

					SELECT C_DISPONIBLES

				ENDSCAN

			ELSE
*No hay suficiente en inventario se debe borrar esa aplicacion o confirmar que sea una simple diferencia
*entre compins y auxapli del mismo codcomp
				SELECT codcomp,cantins,usototins FROM COMPINS WHERE codcomp ;
					IN (SELECT codcomp FROM AUXAPLI WHERE AUXAPLI.codinsap=C_APLICA.codinsap)  INTO CURSOR C_DISPONIBLES1
				IF C_DISPONIBLES1.usototins = C_APLICA.cantotal
*insert into auxapli (codinsap,codinsum,codtipo,codcomp,cantusada,codaplic) ;
*	values(c_aplica.codinsap,c_aplica.codinsum,c_aplica.codtipo, c_disponibles1.codcomp,usototins+ c_disponibles.libre,c_aplica.codaplic)
					UPDATE AUXAPLI SET CANTUSADA=C_DISPONIBLES1.usototins	 WHERE codcomp=C_DISPONIBLES1.codcomp
					SELECT AUXAPLI
					= TABLEUPDATE(.T.)
				ENDIF
			ENDIF
		ELSE
*casos en los que auxapli es mayor que insaplica

			SCAN
				IF C_APLICA.cantotal < C_APLICA.AUXCANTUSADA
					SELECT 	count(CANTUSADA) as cuantos FROM AUXAPLI WHERE codinsap=C_APLICA.codinsap AND ;
						CANTUSADA=C_APLICA.cantotal and  !deleted() INTO CURSOR C_AUXAPLIMATCH
					IF C_AUXAPLIMATCH.cuantos=1
						SELECT	CANTUSADA, codcomp,codinsap,CODINSUM from AUXAPLI where codinsap = C_APLICA.codinsap and CANTUSADA!=C_APLICA.cantotal ;
							and !deleted() into  cursor C_AUXAPLI3
						Delete FROM AUXAPLI WHERE codinsap=C_APLICA.codinsap AND CANTUSADA!=C_APLICA.cantotal and CANTUSADA!=C_APLICA.cantotal
						SELECT AUXAPLI
						= TABLEUPDATE(.T.)

						SCAN
							UPDATE COMPINS set usototins=usototins-C_AUXAPLI3.CANTUSADA where  COMPINS.codcomp=C_AUXAPLI3.codcomp
							SELECT COMPINS
							= TABLEUPDATE(.T.)

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
									SELECT AUXAPLI
									= TABLEUPDATE(.T.)
									UPDATE COMPINS set usototins=usototins-C_AUXAPLI3.CANTUSADA+VACUM1 where  codcomp=C_AUXAPLI3.codcomp ;
										and CODINSUM=C_AUXAPLI3.CODINSUM
									SELECT COMPINS
									= TABLEUPDATE(.T.)

									VACUM1 = 0
								ELSE
									Delete FROM AUXAPLI WHERE codinsap=C_APLICA.codinsap and codcomp=C_AUXAPLI3.codcomp
									SELECT AUXAPLI
									= TABLEUPDATE(.T.)

*SELECT * FROM AUXAPLI WHERE CODINSAP=C_APLICA.CODINSAP and CODCOMP=C_AUXAPLI3.CODCOMP
									UPDATE COMPINS set usototins=usototins-C_AUXAPLI3.CANTUSADA where  codcomp=C_AUXAPLI3.codcomp
									SELECT COMPINS
									= TABLEUPDATE(.T.)

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

	SELECT SUM(AUXAPLI.CANTUSADA) AS AUXCANTUSADA, iif(isnull(cantotal),0,cantotal) as cantotal,AUXAPLI.codinsap,AUXAPLI.CODINSUM,insaplica.numcontrol,AUXAPLI.codaplic, ;
		AUXAPLI.codtipo,AUXAPLI.codcomp  FROM insaplica right JOIN AUXAPLI ON insaplica.codinsap=AUXAPLI.codinsap where !deleted()  ;
		GROUP BY insaplica.codinsap ORDER BY insaplica.codinsap  ;
		INTO CURSOR C_APLICA1
	SELECT * from C_APLICA1 where AUXCANTUSADA != cantotal into cursor C_APLICA
	SELECT * from AUXAPLI where AUXAPLI.codinsap  =C_APLICA.codinsap into cursor c_auxusada
	SCAN
		UPDATE COMPINS set usototins=usototins-c_auxusada.CANTUSADA where codcomp=c_auxusada.codcomp
		SELECT COMPINS
		= TABLEUPDATE(.T.)

		Delete from AUXAPLI where codcomp=c_auxusada.codcomp
		SELECT AUXAPLI
		= TABLEUPDATE(.T.)

		MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
			"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
	ENDSCAN


ENDIF
**ERROR EN INVENTARIOS
SELECT COMPINS.codcomp AS COMP , AUXAPLI.CODINSUM,COMPINS.CODINSUM,AUXAPLI.codcomp AS AUX,usototins;
	FROM  AUXAPLI LEFT OUTER JOIN COMPINS  ;
	ON  COMPINS.codcomp  = AUXAPLI.codcomp INTO CURSOR SINULL
SELECT COUNT(*) FROM SINULL WHERE (ISNULL(COMP)OR ISNULL(AUX)) AND usototins!=0  and !deleted() INTO ARRAY NULO_AUX_COMP

SELECT insaplica.codinsap AS INS, AUXAPLI.codinsap AS AUX,insaplica.CODINSUM,AUXAPLI.CODINSUM;
	FROM  sacfadb!insaplica LEFT OUTER JOIN sacfadb!AUXAPLI ;
	ON  insaplica.codinsap = AUXAPLI.codinsap INTO CURSOR SINULL2
SELECT COUNT(*) FROM SINULL2 WHERE ISNULL(INS) OR ISNULL(AUX) INTO ARRAY NULO_AUX_INS
***HAy en insaplica pero no en auxapli





******************error hay dato en insaplica pero no en aplica

ERR_APLICA =.F.

Delete from aplica where isnull(codaplic) or  isblank(codaplic)&&Elimina inconsistencias en aplicaciones
SELECT aplica
= TABLEUPDATE(.T.)

SELECT aplica.codaplic AS APLIC, insaplica.codaplic AS INSA,codinsap,CODINSUM FROM aplica RIGHT JOIN insaplica ;
	ON insaplica.codaplic=aplica.codaplic INTO CURSOR C_APLICA
SELECT DISTINCT INSA FROM C_APLICA WHERE ISNULL(APLIC) INTO CURSOR C_APLICA1

SELECT COUNT(*) AS cuantos FROM C_APLICA WHERE ISNULL(APLIC) INTO CURSOR CAPLICCUANTOS
IF CAPLICCUANTOS.cuantos >0

	SELECT aplica.codaplic AS APLIC, insaplica.codaplic AS INSA,codinsap,codtipo FROM aplica RIGHT JOIN insaplica ;
		ON ALLTRIM(insaplica.codaplic)=ALLTRIM(aplica.codaplic) INTO CURSOR C_APLICA
	SELECT DISTINCT INSA,codtipo,codinsap FROM C_APLICA WHERE ISNULL(APLIC) or isblank(APLIC)  group by INSA order by INSA,codinsap INTO CURSOR C_APLICA1

	SCAN
		SELECT DISTINCT numcontrol,FECHA1,codaplic,CODINSUM,codinsap FROM insaplica WHERE codaplic=C_APLICA1.INSA INTO CURSOR C_INSAPLICA

		SELECT iif(C_APLICA1.codtipo=1,"S","A")+padl(alltrim(str(max(val(substr(codaplic,2,7)))+1)),7,"0") as codaplic from insaplica ;
			where codtipo= C_APLICA1.codtipo into cursor c_newcodaplic
		SELECT COUNT(*) AS cuantos FROM infculti WHERE numcontrol=C_INSAPLICA.numcontrol and !deleted() INTO CURSOR CSINLOTE
		SELECT area FROM lotesfin WHERE codlote IN (SELECT codlote FROM infculti WHERE numcontrol=C_INSAPLICA.numcontrol) and !deleted() INTO CURSOR C_AREA

		IF CSINLOTE.cuantos>0

			IF isnull(C_INSAPLICA.codaplic) or isblank(C_INSAPLICA.codaplic)
				UPDATE insaplica set codaplic=c_newcodaplic.codaplic where codinsap=C_INSAPLICA.codinsap
				SELECT insaplica
				= TABLEUPDATE(.T.)
				INSERT INTO aplica (numcontrol,codaplic,FECAPLIC,HECTAPLIC) ;
					VALUES(C_INSAPLICA.numcontrol,c_newcodaplic.codaplic,C_INSAPLICA.FECHA1,C_AREA.area)
				SELECT aplica
				= TABLEUPDATE(.T.)

			ELSE
*UPDATE insaplica set CODAPLIC=c_newcodaplic.CODAPLIC where codinsap=C_INSAPLICA.codinsap
				INSERT INTO aplica (numcontrol,codaplic,FECAPLIC,HECTAPLIC) ;
					VALUES(C_INSAPLICA.numcontrol,C_INSAPLICA.codaplic,C_INSAPLICA.FECHA1,C_AREA.area)
				SELECT aplica
				= TABLEUPDATE(.T.)

			ENDIF

		ELSE
* Borro insaplica y auxaplica cuando el lote fue borrado de infculti y el  codcomp de compins
			SELECT codcomp,CANTUSADA FROM AUXAPLI WHERE codinsap IN (SELECT codinsap FROM insaplica WHERE numcontrol= C_INSAPLICA.numcontrol) and !deleted() INTO CURSOR CRESUSO
			SCAN
				UPDATE COMPINS SET usototins= COMPINS.usototins-CRESUSO.CANTUSADA  WHERE codcomp=CRESUSO.codcomp
				SELECT COMPINS
				= TABLEUPDATE(.T.)

			ENDSCAN
			UPDATE COMPINS SET usototins= 0 WHERE usototins<0 && Por si hay negativos igual queda inconsistente
			SELECT COMPINS
			= TABLEUPDATE(.T.)

			Delete FROM AUXAPLI WHERE codinsap IN (SELECT codinsap FROM insaplica WHERE numcontrol= C_INSAPLICA.numcontrol)
			SELECT AUXAPLI
			= TABLEUPDATE(.T.)

			Delete FROM insaplica WHERE numcontrol= C_INSAPLICA.numcontrol
			SELECT insaplica
			= TABLEUPDATE(.T.)
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
*SELECT * from insaplica where codinsap in (select INS from c_nuloinsapli) into cursor c_insaplierror
	SELECT insaplica.cantotal,insaplica.codaplic,insaplica.codinsap,insaplica.codtipo,insaplica.CODINSUM,codfin as finca from insaplica inner join infculti on insaplica.numcontrol=infculti.numcontrol ;
		where codinsap in (select INS as codinsap from c_nuloinsapli) into cursor c_insaplierror
	BEGIN transact
	SCAN
		SELECT codcomp,CODINSUM,sum(cantins) as cantins,sum(usototins) as usototins, sum(cantins)-sum(usototins) as libre ;
			from COMPINS where COMPINS.codfin=c_insaplierror.finca and !deleted() ;
			into cursor c_usolibre
		IF c_insaplierror.cantotal<= c_usolibre.libre
			SELECT codcomp,CODINSUM,cantins,usototins,cantins-usototins as libre from COMPINS where COMPINS.CODINSUM=c_insaplierror.CODINSUM and !deleted() and codfin=c_insaplierror.finca ;
				having cantins-usototins>0 into cursor c_usolibre
			v_saldocantusada = c_insaplierror.cantotal
			SCAN for v_saldocantusada>0
				IF c_insaplierror.cantotal<= c_usolibre.libre
					v_saldocantusada=0
					UPDATE COMPINS set usototins=usototins+ c_insaplierror.cantotal where COMPINS.codcomp=c_usolibre.codcomp
					SELECT COMPINS
					= TABLEUPDATE(.T.)

					INSERT INTO AUXAPLI  (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
						VALUES(c_insaplierror.codinsap,c_insaplierror.CODINSUM,c_insaplierror.codtipo,c_usolibre.codcomp,c_insaplierror.cantotal,c_insaplierror.codaplic)
					SELECT AUXAPLI
					= TABLEUPDATE(.T.)

					EXIT
				ELSE
					v_saldocantusada = v_saldocantusada - c_usolibre.libre
					UPDATE COMPINS set usototins= usototins+c_usolibre.libre where COMPINS.codcomp=c_usolibre.codcomp
					SELECT COMPINS
					= TABLEUPDATE(.T.)

					INSERT INTO AUXAPLI  (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
						VALUES(c_insaplierror.codinsap,c_insaplierror.CODINSUM,c_insaplierror.codtipo,c_usolibre.codcomp,c_usolibre.libre,c_insaplierror.codaplic)
					SELECT AUXAPLI
					= TABLEUPDATE(.T.)


				ENDIF
				SELECT c_usolibre
			ENDSCAN

*ELSE
*UPDATE COMPINS where
			SELECT c_insaplierror
		ELSE
			Delete from insaplica where insaplica .codinsap=c_insaplierror.codinsap
			SELECT insaplica
			= TABLEUPDATE(.T.)
		ENDIF
	ENDSCAN

	END transac

ENDIF

***************** revision mas exaustiva para faltentes entre tablas
********************
*21-09-2006
*se eliminan lo auxcomb  que no tengan valores en combust

SELECT AUXCOMB.*,COMBUST.CODUFC as codufc1,COMBUST.cantotal from AUXCOMB left join COMBUST on  AUXCOMB.CODUFC=COMBUST.CODUFC ;
	where !deleted() having isnull(COMBUST.CODUFC)  into cursor c_auxcomb_error
SCAN
	Delete from AUXCOMB where CODUFC=c_auxcomb_error.CODUFC
	SELECT AUXCOMB
	= TABLEUPDATE(.T.)

ENDSCAN

** Se eliminan loas aucombust que no tengan los valores iguales a combust
SELECT COMBUST.*,AUXCOMB.CODUFC as codufc1,sum(AUXCOMB.CANTUSADA) as cantusada1 from AUXCOMB right join COMBUST on ;
	AUXCOMB.CODUFC=COMBUST.CODUFC where !deleted() group by COMBUST.CODUFC order by cantusada1,cantotal asc having cantusada1!=cantotal into cursor c_combus_noigual
SCAN
	Delete from AUXCOMB where CODUFC =c_combus_noigual.CODUFC
	SELECT AUXCOMB
	= TABLEUPDATE(.T.)

ENDSCAN

****	Se corrigen inconsistencias entre codcomp de auxcombust y compins
SELECT sum(CANTUSADA) as cantusada1, AUXCOMB.codcomp as auxcomp,COMPINS.codcomp,cantins,usototins;
	from COMPINS right join AUXCOMB on COMPINS.codcomp=AUXCOMB.codcomp where !deleted() ;
	group by AUXCOMB.codcomp order by AUXCOMB.codcomp  having cantusada1!=usototins into cursor c_auxcomb_compins

IF _tally >0
	SELECT count(*) as cuantos from c_auxcomb_compins where !deleted() into cursor c_auxcomb_compins1
	IF c_auxcomb_compins1.cuantos>0
		SELECT c_auxcomb_compins
		SCAN

			SELECT count(*) as cuan from AUXAPLI where !deleted() and codcomp = c_auxcomb_compins.codcomp into cursor c_aplico
			IF c_aplico.cuan >0
				SELECT sum(CANTUSADA) as sumcantu,cantins,usototins from AUXAPLI inner join COMPINS on AUXAPLI.codcomp=COMPINS.codcomp ;
					group by AUXAPLI.codcomp order by AUXAPLI.codcomp where AUXAPLI.codcomp = c_auxcomb_compins.codcomp and !deleted() into cursor c__auxapl
				IF c__auxapl.cantins >c__auxapl.sumcantu + c_auxcomb_compins.cantusada1
					UPDATE COMPINS set usototins=c__auxapl.sumcantu + c_auxcomb_compins.AUXCOMB ;
						where COMPINS.codcomp =  c_auxcomb_compins.codcomp
					SELECT COMPINS
					= TABLEUPDATE(.T.)

*!*					MESSAGEBOX("Se encontró una incosistencia en combustibles y se corrigió"+CHR(13)+ ;
*!*						"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
				ELSE
*aplico lubricantes por fertilizantes y agroquimicos
					MESSAGEBOX("El programa presenta una incosistencia" + chr(13)+ "Envie un backup a FEDEARROZ para repararla",16,"ERROR")
				ENDIF
			ELSE
				UPDATE COMPINS set usototins=c_auxcomb_compins.cantusada1 where codcomp =  c_auxcomb_compins.codcomp
				SELECT COMPINS
				= TABLEUPDATE(.T.)

*!*					MESSAGEBOX("Se encontró una incosistencia en combustibles y se corrigió"+CHR(13)+ ;
*!*						"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
			ENDIF
			SELECT c_auxcomb_compins
		ENDSCAN
	ENDIF
ENDIF
***Se corrigen inconsistencias entre combust y auxcombust

SELECT count(*) as cuantos from TEM_COMB_DIF into cursor c_error_comb
IF c_error_comb.cuantos>0

	SELECT COMBUST.*,AUXCOMB.CODUFC as codufc1,sum(AUXCOMB.CANTUSADA) as cantusada1 from AUXCOMB right join COMBUST on ;
		AUXCOMB.CODUFC=COMBUST.CODUFC where !deleted() group by COMBUST.CODUFC order by cantusada1,cantotal asc having isnull(AUXCOMB.CODUFC) into cursor c_combus_error
	SCAN
		IF isnull(c_combus_error.cantusada1)

			SELECT sum(cantins-usototins) as libre, codcomp,CODINSUM from COMPINS where CODINSUM=COMBUST.CODINSUM having libre>0 INTO CURSOR c_combust_tot
			IF c_combust_tot.libre> c_combus_error.cantotal

				SELECT cantins-usototins as libre, codcomp,CODINSUM from COMPINS where CODINSUM=COMBUST.CODINSUM having libre>0 INTO CURSOR c_combust
				vacum=c_combus_error.cantotal
				IF c_combust_tot.libre>=vacum
					SCAN
						IF vacum<= c_combust.libre
							INSERT into AUXCOMB(CODUFC,codcomp,CODINSUM,CANTUSADA) values(c_combus_error.CODUFC,c_combust.codcomp,c_combust.CODINSUM,vacum)
							SELECT AUXCOMB
							= TABLEUPDATE(.T.)

							UPDATE COMPINS set usototins=usototins+vacum where codcomp= c_combust.codcomp
							SELECT COMPINS
							= TABLEUPDATE(.T.)

							EXIT
						ELSE
							INSERT into AUXCOMB(CODUFC,codcomp,CODINSUM,CANTUSADA) values(c_combus_error.CODUFC,c_combust.codcomp,c_combust.CODINSUM,c_combust.libre)
							SELECT AUXCOMB
							= TABLEUPDATE(.T.)

							UPDATE COMPINS set usototins=usototins+c_combust.libre where codcomp= c_combust.codcomp
							SELECT COMPINS
							= TABLEUPDATE(.T.)

							vacum=vacum-c_combust.libre
						ENDIF
						SELECT c_combust
					ENDSCAN
				ELSE

					Delete from COMBUST where CODUFC=c_combus_error.CODUFC
					SELECT COMBUST
					= TABLEUPDATE(.T.)

				ENDIF
			ELSE
*delete from auxcomb where codcomp=c_combus_error.codufc

				Delete from COMBUST where CODUFC=c_combus_error.CODUFC
				SELECT COMBUST
				= TABLEUPDATE(.T.)

			ENDIF
			SELECT c_combus_error
		ELSE

		ENDIF
	ENDSCAN

ENDIF
**********************


ERROR_INV= ERROR_INV +NULO_AUX_INS+NULO_AUX_COMP+COMP_AUX + verror   &&+vacio

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
		ELSE
			IF verror =1
				MESSAGEBOX("Se encontró una incosistencia en inventarios y se corrigió"+CHR(13)+ ;
					"REVISE NUEVAMENTE INVENTARIOS PARA CONFIRMAR SI SE RESOLVIO EL PROBLEMA",48,"error")
			ENDIF
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
	DO LIBERAR WITH "aplica"
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

PROCEDURE recorre_error
PARAMETER cambio,acum1
SELECT AUXAPLI.*, COMPINS.codfin;
	FROM  sacfadb!AUXAPLI inner JOIN sacfadb!COMPINS;
	ON  AUXAPLI.codcomp = COMPINS.codcomp into cursor  c_auxapli_finc

SELECT insaplica.*, infculti.codfin as finca;
	FROM  insaplica inner JOIN infculti;
	ON  insaplica.numcontrol= infculti.numcontrol into cursor  c_insaplica_finc


SELECT  c_auxapli_finc.*,finca from c_auxapli_finc inner join c_insaplica_finc on ;
	c_auxapli_finc.codinsap=c_insaplica_finc.codinsap  where codfin!=finca into cursor c_insumo_cruce
SELECT count(*) as cuantos from c_insumo_cruce into cursor c_hay_error
IF c_hay_error.cuantos>0
	SELECT c_insumo_cruce
	SCAN
		SELECT sum(cantins-usototins) as libre from COMPINS where codfin=c_insumo_cruce.finca and ;
			CODINSUM=c_insumo_cruce.CODINSUM into cursor c_libre
		IF c_libre.libre >= c_insumo_cruce.CANTUSADA
*Busca espacio libre en finca destino
			SELECT CODINSUM,codcomp,codfin,cantins,usototins,(cantins-usototins) as libre from COMPINS ;
				where codfin=c_insumo_cruce.finca and CODINSUM=c_insumo_cruce.CODINSUM and (cantins-usototins) >0  ;
				order by libre desc into cursor c_libre_finca_dest
			IF 	c_libre_finca_dest.libre>=c_insumo_cruce.CANTUSADA
*v_ubocambio = .t.
				UPDATE COMPINS set usototins=usototins-c_insumo_cruce.CANTUSADA where codcomp= c_insumo_cruce.codcomp and usototins-c_insumo_cruce.CANTUSADA >=0
				SELECT COMPINS
				= TABLEUPDATE(.T.)

				UPDATE COMPINS set usototins=usototins+c_insumo_cruce.CANTUSADA where codcomp= c_libre_finca_dest.codcomp and cantins>=usototins+c_insumo_cruce.CANTUSADA
				SELECT COMPINS
				= TABLEUPDATE(.T.)

				UPDATE AUXAPLI set codcomp = c_libre_finca_dest.codcomp where codcomp=c_insumo_cruce.codcomp and codinsap=c_insumo_cruce.codinsap
			ELSE

				acum=c_insumo_cruce.CANTUSADA
				Delete from AUXAPLI where codcomp in (select codcomp from c_insumo_cruce)
				SELECT AUXAPLI
				= TABLEUPDATE(.T.)

				SELECT c_libre_finca_dest
				SCAN
					IF c_libre_finca_dest.libre>=acum
						INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
							values(c_insumo_cruce.codinsap,c_insumo_cruce.CODINSUM,c_insumo_cruce.codtipo,c_libre_finca_dest.codcomp,acum,c_insumo_cruce.codaplic)
						SELECT AUXAPLI
						= TABLEUPDATE(.T.)

						UPDATE COMPINS set usototins=usototins+acum where codcomp=c_libre_finca_dest.codcomp
						SELECT COMPINS
						= TABLEUPDATE(.T.)

						EXIT
					ELSE
						INSERT into AUXAPLI (codinsap,CODINSUM,codtipo,codcomp,CANTUSADA,codaplic) ;
							values(c_insumo_cruce.codinsap,c_insumo_cruce.CODINSUM,c_insumo_cruce.codtipo,c_libre_finca_dest.codcomp,c_libre_finca_dest.libre,c_insumo_cruce.codaplic)
						SELECT AUXAPLI
						= TABLEUPDATE(.T.)

						UPDATE COMPINS set usototins=usototins+c_libre_finca_dest.libre where codcomp=c_libre_finca_dest.codcomp
						SELECT COMPINS
						= TABLEUPDATE(.T.)

						acum=acum-c_libre_finca_dest.libre
					ENDIF
					SELECT c_libre_finca_dest
				ENDSCAN

			ENDIF

*v_ubocambio = .f. or v_ubocambio
		ELSE
			IF acum1=2
				Delete from insaplica where codinsap=c_insumo_cruce.codinsap
				SELECT insaplica
				= TABLEUPDATE(.T.)
*SELECT * from AUXAPLI where codinsap=c_insumo_cruce.codinsap into cursor
			ENDIF
*v_ubocambio =.t.	or v_ubocambio
		ENDIF

		SELECT c_insumo_cruce

	ENDSCAN
ELSE
	cambio= .f.
ENDIF
RETURN cambio
ENDPROC
