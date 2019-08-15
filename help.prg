PARAMETER ayud
LOCAL nHWD


DECLARE INTEGER ShellExecute IN SHELL32.DLL ;
    INTEGER nWinHandle, ;
    STRING  cOperation,  ;
    STRING  cFileName,   ;
    STRING  cParameters, ;
    STRING  cDirectory,  ;
    INTEGER nShowWindow
    
    ***************

    DECLARE INTEGER FindWindow IN WIN32API ;
        STRING cNULL, ;
        STRING cWinName

    DECLARE SetForegroundWindow IN WIN32API ;
        INTEGER nHandle

    DECLARE SetActiveWindow IN WIN32API ;
        INTEGER nHandle

    DECLARE ShowWindow IN WIN32API ;
        INTEGER nHandle, ;
        INTEGER nState

    nHWD = FindWindow(0, "MANUAL DE CONSULTA DE SACFA L I T E")
    IF nHWD > 0
        * VENTANA YA ACTIVA
        * LA "LLAMAMOS":
        ShowWindow(nHWD,9)

        * LA PONEMOS ENCIMA
        SetForegroundWindow(nHWD)

        * LA ACTIVAMOS
        SetActiveWindow(nHWD)

    ENDIF
    *******************
    

V_archivo=SYS(5)+SYS(2003)+"\ayuda\"

DO CASE
    CASE ayud = 1
        AbrirFile(V_archivo + "acerca.htm")
    CASE ayud = 2
        AbrirFile(V_archivo + "modo.htm")
    CASE ayud = 3
        AbrirFile(V_archivo + "manejo menus.htm")
    CASE ayud = 4
        AbrirFile(V_archivo + "recomen.htm")
    CASE ayud = 5
        AbrirFile(V_archivo + "asisten.htm")
    CASE ayud = 6
        AbrirFile(V_archivo + "licencia.htm")    
ENDCASE

FUNCTION AbrirFile
&& Abre el archivo especificado con su aplicacion asociada devuelve elhandle de la ventana
    *!*  Return:2 - Bad Association (invalid
    *!*              URL)
    *!*         31 - No application
    *!*              association
    *!*         29 - Failure to load
    *!*              application
    *!*         30 - Application is busy
    *!*         > 32 Indicate success and return an instance handle for theapplication started
    LPARAMETER pcorigen
    LOCAL lnexito
    STORE 0 TO lnexito
    IF PARAMETERS()= 1
        lnexito= ShellExecute(0, "Open", pcorigen, "", "", 1)
    ELSE
        MESSAGEBOX("Parametros mal enviados", 64, "Aviso")
    ENDIF
    RETURN lnexito
ENDFUNC
