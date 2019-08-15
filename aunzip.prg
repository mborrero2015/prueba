 PARAMETER inItialize, saRchive, sdIr, seXtractfiles, unZipparams
 PRIVATE paRams, stEmp
 paRams = PARAMETERS()
 IF .T.
      PRIVATE hwNd
      DECLARE INTEGER GetActiveWindow IN win32api
      hwNd = geTactivewindow()
      DECLARE LONG addUNZIP IN descom32.dll      
      DECLARE INTEGER addUNZIP_ArchiveName IN descom32.DLL STRING
      DECLARE INTEGER addUNZIP_ExtractTo IN descom32.DLL STRING
      DECLARE INTEGER addUNZIP_Include IN descom32.DLL STRING
      DECLARE addUNZIP_Initialise IN descom32.DLL 
      DECLARE INTEGER addUNZIP_Freshen IN descom32.DLL INTEGER
      DECLARE INTEGER addUNZIP_Overwrite IN descom32.DLL INTEGER
      DECLARE INTEGER addUNZIP_Register IN descom32.DLL STRING, LONG
      DECLARE INTEGER addUNZIP_RestoreStructure IN descom32.DLL INTEGER
      DECLARE INTEGER addUNZIP_SetParentWindowHandle IN descom32.DLL LONG
      adDunzip_initialise()
      adDunzip_setparentwindowhandle(hwNd)
      adDunzip_register("UBS, INC.",600365060)    
 ENDIF

 IF paRams>3
      unZipparams = ALLTRIM(unZipparams)
      PRIVATE laStpos, paSsword, ncUrpos
      laStpos = LEN(unZipparams)+1
      ncUrpos = ATC('S', unZipparams)
      IF ncUrpos>0
           cpAssword = SUBSTR(unZipparams, ncUrpos+1)
           *adDunzip_decrypt(cpAssword)
           unZipparams = LEFT(unZipparams, ncUrpos-1)
      ENDIF
      ncUrpos = ATC('D', unZipparams)
      IF ncUrpos>0
           adDunzip_restorestructure(1)
      ENDIF
      ncUrpos = ATC('F', unZipparams)
      IF ncUrpos>0
           adDunzip_freshen(1)
      ELSE
           adDunzip_freshen(0)
      ENDIF
      DO CASE
           CASE 'OA'$unZipparams
                adDunzip_overwrite(11)
           CASE 'O0'$unZipparams
                adDunzip_overwrite(12)
           CASE 'OU'$unZipparams
                adDunzip_overwrite(10)
      ENDCASE
      IF ncUrpos>0
           adDunzip_restorestructure(1)
      ENDIF
 ELSE
      adDunzip_overwrite(10)
 ENDIF
 adDunzip_archivename(saRchive)
 adDunzip_extractto(ALLTRIM(sdIr))
 seXtractfiles = IIF(paRams<4, "*.*", ALLTRIM(STRTRAN(seXtractfiles, ' ',  ;
                 '|')))
 adDunzip_include(seXtractfiles)
 RETURN adDunzip()
ENDFUNC
*