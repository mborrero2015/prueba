
	
  strFileURL = "http://fedearroz.sbrltda.com/sacupdate/actualiza.zip"
  strHDLocation =  WScript.Arguments.Item(0)
  WScript.Echo WScript.Arguments.Item(0)
  Set objXMLHTTP = CreateObject("MSXML2.XMLHTTP")
  objXMLHTTP.open "GET", strFileURL, false
  objXMLHTTP.send()
  If objXMLHTTP.Status = 200 Then
	Set objADOStream = CreateObject("ADODB.Stream")
	objADOStream.Open
	objADOStream.Type = 1 'adTypeBinary
	objADOStream.Write objXMLHTTP.ResponseBody
	objADOStream.Position = 0    
	Set objFSO = Createobject("Scripting.FileSystemObject")
	If objFSO.Fileexists(strHDLocation) Then objFSO.DeleteFile strHDLocation
		Set objFSO = Nothing
		objADOStream.SaveToFile strHDLocation
		objADOStream.Close
		Set objADOStream = Nothing
	

	Set objXMLHTTP = Nothing

end if
