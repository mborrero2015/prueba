PARAMETERS valor,lg

SET SHADOW ON

*** variable cantidad de digitos contables  ***** debe ser publica y puesta al principio
** debe ser general para todo SACFA *****

***********************************
DEFINE POPUP menuizqi ;
    FROM MROW(), MCOL() ;
    MARGIN ;
    SHORTCUT

DEFINE BAR 1 OF menuizqi PROMPT "Crear/Subcuenta"
DEFINE BAR 2 OF menuizqi PROMPT "\-"
DEFINE BAR 3 OF menuizqi PROMPT "Eliminar"

IF variables.qdigit = lg
    SET SKIP OF BAR 1 OF menuizqi .T.
ELSE
    ON SELECTION BAR 1 OF menuizqi DO rutina WITH valor,lg
ENDIF

ON SELECTION BAR 3 OF menuizqi DO elimina

ACTIVATE POPUP menuizqi



    PROCEDURE elimina
        *   PARAMETERS mmconta

        mmconta = ALLTRIM(puc1.CODCONTABL)

        SELECT COUNT(*), SUM(puc1.SALDOINIC + puc1.MDEBITO + puc1.MCREDITO) ;
            AS TOTA FROM puc1 WHERE LEFT(puc1.CODCONTABL,LEN(mmconta))= mmconta ;
            INTO CURSOR cuantos

        IF cuantos.TOTA = 0

            respu = MESSAGEBOX("   ¿Esta seguro de "+CHR(13)+"Eliminar esta Cuenta?",292,"Elimina Cuenta")

            IF respu = 6
                DELETE FROM puc1 WHERE LEFT(puc1.CODCONTABL,LEN(mmconta))= mmconta
            ENDIF

        ELSE
            MESSAGEBOX("Esta Cuenta no se "+ CHR (13)+"  puede Eliminar",16,"Elimina Cuenta")
        ENDIF
        RETURN

        RELEASE POPUP menuizqi
