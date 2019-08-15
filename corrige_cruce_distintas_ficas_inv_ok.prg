
*!*		*suma el total de insaplica.cantotal y lo agrupo por insumo por finca
*!*		select sum(cantotal) as cantotal1,insaplica.*,infculti.codfin from insaplica inner join infculti on insaplica.numcontrol=infculti.numcontrol  group by codinsum where codfin=c_ubica.codfin into cursor c_insaplic_insum
*!*		*suma el total de auxapli.cantusada y lo agrupo por insumo	por finca
*!*		select sum(cantusada) as cantusada1,auxapli.* from compins inner join auxapli on compins.codcomp=auxapli.codcomp  group by compins.codinsum where codfin=c_ubica.codfin into cursor c_auxapli
*!*		*compara los dos para ver si hay error en la finca
*!*		select cantusada1,c_insaplic_insum.* from c_auxapli inner join c_insaplic_insum on c_auxapli.codinsum= c_insaplic_insum.codinsum where cantotal1!=cantusada1 into cursor c_insumos_cruce
*!*		*Detecta los insumos que tienen cruce
v_ubocambio = .f.
SELECT Auxapli.*, Compins.codfin;
	FROM  sacfadb!Auxapli inner JOIN sacfadb!Compins;
	ON  Auxapli.codcomp = Compins.codcomp into cursor  c_auxapli_finc

SELECT insaplica.*, infculti.codfin as finca;
	FROM  insaplica inner JOIN infculti;
	ON  insaplica.numcontrol= infculti.numcontrol into cursor  c_insaplica_finc


SELECT  c_auxapli_finc.*,finca from c_auxapli_finc inner join c_insaplica_finc on ;
	c_auxapli_finc.codinsap=c_insaplica_finc.codinsap  where codfin!=finca into cursor c_insumo_cruce
set step on
SELECT count(*) as cuantos from c_insumo_cruce into cursor c_cuantos
IF c_cuantos.cuantos>0
	switch=.t.
else
	switch=.f.
endif
	DO WHILE switch	
		switch=recorre_error (switch)		
	ENDDO
	
	




PROCEDURE recorre_error
parameter cambio
SELECT Auxapli.*, Compins.codfin;
	FROM  sacfadb!Auxapli inner JOIN sacfadb!Compins;
	ON  Auxapli.codcomp = Compins.codcomp into cursor  c_auxapli_finc

SELECT insaplica.*, infculti.codfin as finca;
	FROM  insaplica inner JOIN infculti;
	ON  insaplica.numcontrol= infculti.numcontrol into cursor  c_insaplica_finc


SELECT  c_auxapli_finc.*,finca from c_auxapli_finc inner join c_insaplica_finc on ;
	c_auxapli_finc.codinsap=c_insaplica_finc.codinsap  where codfin!=finca into cursor c_insumo_cruce

SCAN
	SELECT sum(cantins-usototins) as libre from Compins where codfin=c_insumo_cruce.finca and ;
			codinsum=c_insumo_cruce.codinsum into cursor c_libre
	if c_libre.libre >= c_insumo_cruce.cantusada
		*Busca espacio libre en finca destino
		select codinsum,codcomp,codfin,cantins,usototins,(cantins-usototins) as libre from compins ;
			where codfin=c_insumo_cruce.finca and codinsum=c_insumo_cruce.codinsum and (cantins-usototins) >0  ;
			order by libre desc into cursor c_libre_finca_dest
		if 	c_libre_finca_dest.libre>=c_insumo_cruce.cantusada
			set step on
			v_ubocambio = .t.
			update compins set usototins=usototins-c_insumo_cruce.cantusada where codcomp= c_insumo_cruce.codcomp 
			update compins set usototins=usototins+c_insumo_cruce.cantusada where codcomp= c_libre_finca_dest.codcomp and cantins>=usototins+c_insumo_cruce.cantusada
			update auxapli set codcomp = c_libre_finca_dest.codcomp where codcomp=c_insumo_cruce.codcomp and codinsap=c_insumo_cruce.codinsap
		else
			
		endif
		
	 	cambio = .f. or cambio
	else		
		if v_ubocambio
			delete from insaplica where codinsap=c_insumo_cruce.codinsap
			select * from auxapli where codinsap=c_insumo_cruce.codinsap into cursor 
		endif
		cambio =.t.	or cambio	
	endif
	
	SELECT c_insumo_cruce

ENDSCAN
RETURN cambio
ENDPROC


