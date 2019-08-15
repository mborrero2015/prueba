SET SAFETY OFF
SET TALK OFF
*** programa limpia bases  - todas  ****

IF salir = .F. then
    FOR i = 1 TO 400
        lsd = "Use in " + ALLTRIM(STR(i))
        &lsd
    ENDFOR
    salir = .T.
ENDIF

USE ubica EXCLUSIVE
PACK
USE lotesfin EXCLUSIVE
PACK
USE aplica EXCLUSIVE
PACK
USE labculti EXCLUSIVE
PACK
USE procion EXCLUSIVE
PACK
USE jorculti EXCLUSIVE
PACK
USE infculti EXCLUSIVE
PACK
USE insaplica EXCLUSIVE
PACK
USE ventas EXCLUSIVE
PACK
USE auplx EXCLUSIVE
PACK
USE auxreco EXCLUSIVE
PACK
USE iprov EXCLUSIVE
PACK
USE canval EXCLUSIVE
PACK
USE invimpr EXCLUSIVE
PACK
USE insumo EXCLUSIVE
PACK
USE teminvp EXCLUSIVE
PACK
USE cospor EXCLUSIVE
PACK
USE costot EXCLUSIVE
PACK
USE renren EXCLUSIVE
PACK
USE regeinsumo EXCLUSIVE
ZAP

USE CREDITO EXCLUSIVE
PACK
USE DCREDITO EXCLUSIVE
PACK



*!*	USE reco1 Exclusive
*!*	ZAP
**************
FOR i = 1 TO 400
    lsd = "Use in " + ALLTRIM(STR(i))
    &lsd
ENDFOR

CLOSE DATABASE ALL
CLOSE TABLES
CLOSE INDEX
CLOSE ALL
_VFP.APPLICATION.VISIBLE = .T.
IF indexo
    cuancdx = ADIR(cdxs, '*.cdx')
    FOR ncount = 1 TO cuancdx  && Bucle para número de bases de datos
        archivo=LEFT(cdxs(ncount,1),LEN(cdxs(ncount,1))-4)
        archivo1=archivo+".dbf"
        IF FILE(archivo1)
            USE &archivo EXCLU
            REINDEX  && Muestra nombres de base de datos
            USE IN &archivo
        ENDIF
    ENDFOR
ENDIF

CLEAR EVENTS
QUIT
