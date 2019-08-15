If WScript.Arguments.Count > 0 Then
	' 	For i = 0 To WScript.Arguments.Count - 1
	' Next 'i
	' Fijar rutas'
  strFileURL = "http://fedearroz.sbrltda.com/sacupdate/update.zip"
  strHDLocation =  WScript.Arguments.Item(0)
  WScript.Echo WScript.Arguments.Item(0)


	' Fetch the file'
'   Set objXMLHTTP = CreateObject("MSXML2.XMLHTTP")
  Set objXMLHTTP = CreateObject("Msxml2.ServerXMLHTTP.6.0")
'  objSrvHTTP = Server.CreateObject ("Msxml2.ServerXMLHTTP.6.0");
  objXMLHTTP.open "GET", strFileURL, false
'   objXMLHTTP.setRequestHeader "Cache-Control", "no-cache"
  objXMLHTTP.send()

'   If objXMLHTTP.status != 200000   Then
   '	If 1 = 1 Then
	Set objADOStream = CreateObject("ADODB.Stream")
	objADOStream.Open
	objADOStream.Type = 1 'adTypeBinary
	objADOStream.Write objXMLHTTP.ResponseBody
	objADOStream.Position = 0    'Set the stream position to the start
	Set objFSO = Createobject("Scripting.FileSystemObject")
	If objFSO.Fileexists(strHDLocation) Then objFSO.DeleteFile strHDLocation
		Set objFSO = Nothing
		objADOStream.SaveToFile strHDLocation
		objADOStream.Close
		Set objADOStream = Nothing
	

	Set objXMLHTTP = Nothing
'  Else
	Wscript.echo "Archivo no pudo ser descargado. Asegurese de tener conexión a Internet" 
	wscript.Quit(1001) 
'  End If
Else
  WScript.Echo "No se han pasado argumentos."
  WScript.Quit 1
End If