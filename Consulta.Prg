*!*	Para los  Costos fijos  Hacemos lo siguiente


*!*	valor compra = valor de la maquina
*!*	año = el año Actual

*!*	tenemos una tabla de donde tenemos unos costos por año de la maquinaria
*!*	tomado del banco de la republica

*!*	contamos si hay dato de ese año en que se este trabajando
*!*	para obtener un indice si no lo encontramos dejamos Valor Actual maquina  = valor compra 

*!*	indce actual = indice del año
*!*	año = fecha de compra de la maquina 

*!*	contamos si hay dato del año en que se compro la maquina
*!*	para obtener un indice Valor Actual maquina  = valor compra 

*!*	indice Compra Maquina = indice del año
*!*	Valor Actual maquina  = ((indce actual * valor compra)/indice Compra Maquina)

*!*	************
*!*	depreciacion    = (((Valor Actual maquina  - (Valor Actual maquina  * (SALVAMENTO/100)))/ AÑOSUTIL)/HORAS al AñO que trabaja la maquina )
*!*	intereses   = ((((Valor Actual maquina  + (Valor Actual maquina  * (SALVAMENTO/100)))* (INTERES/100))/2)/HORAS al AñO que trabaja la maquina)
*!*	impusetos      = ((((Valor Actual maquina  + (Valor Actual maquina  * (SALVAMENTO/100)))* (impusetosUSEGU/100))/2)/HORAS al AñO que trabaja la maquina)
*!*	costos fijos = depreciacion + intereses + impusetos
*!*	*******


*!*	para los variables hacemos los siguientes 

*!*	 
*!*	   tiempo que utiliza la maquina = numero de minutos que utiliza la maquina


*!*	   combustible  = (CABALLOS de fuerza * (caballaje/100) * CONSUMO HP *  Valor del  combustible)
*!*	   Lubricante    = combustible * (LUBRICANTE/100)
*!*	   REparaciones   = ((((REPARARACION MANTENIMIENTO/100) * Valor Actual maquina )/ AÑOS UTIL)/HORAS al AñO que trabaja la maquina)
*!*	   operario   = (((COSTO QUE TIENE EL OPERARIO DE LA MAQUINA/DIAS TRABAJADOS) / HORAS TRABAJADAS) * (1 + (SALARIO EMPLEADO/100)))

*!*	   costo variable utilizacion maq = (combustible+Lubricante+REparaciones+operario)
*!*	   costo variable utilizacion maq multiplicado por las horas utilizadas   = (costo variable utilizacion maq * tiempo que utiliza la maquina)
*!*	   costo fijo = (costos fijos + costo variable utilizacion maq)
*!*	   costo fijo multiplicado por las horas utilizadas  = (costo fijo * tiempo que utiliza la maquina)


*!*	   Valor costo total  fijos y variables = (costo fijo * tiempo que utiliza la maquina)

*!*	*******

*!*	ASI ES QUE SE CALCULAN LOS COSTOS FIJO Y VARAIBLES PARA LA UTILIZACION 
*!*	DE LA MAQUINARIA PROPIA 