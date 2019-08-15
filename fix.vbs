'===================================================================================================== 
'VBScript to Download a User Defined Zip file and Extract it to a User Defined Location 
'REQUIREMENTS of SCRIPT: 
'TWO Command Line Variables 
'Quotes around BOTH File URL and Defined Directory 
' 
'1st Variable - Admin defined Full File URL; example: "http://xxx.yyy.com/filename.zip"  (must include zip in path) 
'2nd Variable - Admin defined Directory on Target Machine for Extract; example: "C:\Directory" 
'Full example: 
' "https://fedearroz.sbrltda.com" "C:\sacfalite\" 
' "https://fedearroz.sbrltda.com" "C:\TestDLandExtract" 
'===================================================================================================== 
'***IMPORTANT: If using a DROPBOX link, the file needs to be in your PUBLIC FOLDER*** 
'In dropbox, you may see a toggle option for file: "Make Dropbox Public Links Download Files Instead of Viewing Them In-Browser" < this should be selected 
'In summation on the above note: The download file needs to require no user action 
'===================================================================================================== 
 
 
Set objFSO = CreateObject("Scripting.FileSystemObject") 
 
'strFileURL = WScript.Arguments.Item(0) & "?dl=1" 
strFileURL = WScript.Arguments.Item(0)  
'===================================================================================================== 
'Download ZIP 
        dim xHttp: Set xHttp = createobject("MSXML2.XMLHTTP") 
        dim bStrm: Set bStrm = createobject("Adodb.Stream") 
        wscript.echo "File URL: " & strFileURL 
        xHttp.Open "GET", strFileURL, False 
        xHttp.Send 
 
        sHTML=xHttp.statusText 
        wscript.echo "File URL Existence Check: " & sHTML 
 
        if err or sHTML<>"OK" Then 
            Wscript.echo "ZIP File could not be downloaded.  Please ensure communication with the URL given "& "(" & strFileURL &")" & " and try again." 
            wscript.echo 
            wscript.echo "Exiting without ZIP Download..." 
            wscript.Quit(1001) 
        else 
            'Directory to Save Download to 
            SrcZipFL = WScript.Arguments.Item(1) 
 
            'Check if User Defined Folder ends with \ character; strip if given 
            DirLength = Len(SrcZipFL) 
            CharChk = InstrRev(SrcZipFL,"\") 
            If (DirLength=CharChk) then 
                SrcZipFL = Left(SrcZipFL, Len(SrcZipFL) - 1) 
            Else 
                SrcZipFL = SrcZipFL 
            End If 
 
            'Create User Defined Folder if not present 
            if (Not objFSO.FolderExists (SrcZipFL)) then 
                objFSO.CreateFolder SrcZipFL 
            end if                 
 
            'Directory to Save Download to (with local filename) 
            SrcZipFL = SrcZipFL & "\ZipFile.zip" 
 
 
            with bStrm 
                    .type = 1 '//binary 
                    .open 
                    .write xHttp.responseBody 
                .savetofile SrcZipFL, 2 '//overwrite 
            end with 
        end if 
 
'===================================================================================================== 
ExtractRoot=WScript.Arguments.Item(1) 
 
wscript.echo 
wscript.echo "Downloaded Zip File Location: " & SrcZipFL 
wscript.echo "Root Extract Path of Downloaded Zip: " & ExtractRoot &vbCrLf 
 
If (objFSO.FileExists(SrcZipFL)) Then 
    Unzip SrcZipFL, ExtractRoot 
Else 
    Wscript.Echo SrcZipFL & " does not exist" &vbCrLf 
End If 
 
 
'==============================Mock Unzip Function==================================================== 
Sub Unzip(SourceFL, TargetDir) 
    if not objFSO.FolderExists(TargetDir) then objFSO.CreateFolder(TargetDir) 
    Set objShell = CreateObject("Shell.Application") 
    Set objSource = objShell.NameSpace(SourceFL).Items() 
    Set objTarget = objShell.NameSpace(TargetDir) 
    Set objTargetCol = objShell.NameSpace(TargetDir).Items() 
    'If a file with the same name exists in Target Directory, overwrite 
    objTarget.CopyHere objSource, 16 
 
    wscript.echo "Current Extract Folder Contents: " 
    'List Files in Root of Extract Base Folder 
    Wscript.Echo "Folder: " & ExtractRoot & ", Contents: " 
    Set objFolder = objFSO.GetFolder(ExtractRoot) 
    Set colFiles = objFolder.Files 
    For Each objFile in colFiles 
        Wscript.Echo "-File: " & objFile.Name 
        wscript.Echo "--Path: " & objFile.Path 
        Wscript.Echo "---DateMod: " & objFile.DateLastModified 
    Next 
 
    'Call Subfolder Parse Function, to list Subfolders and Contents of Subfolders 
    ParseSubfolders objFSO.GetFolder(ExtractRoot) 
End Sub 
 
'==============================RECURSIVE SUBFOLDER PARSE FUNCTION===================================== 
Sub ParseSubfolders(Folder) 
    For Each Subfolder in Folder.SubFolders 
        Wscript.Echo "Folder: " & Subfolder.Path & ", Contents: " 
        Set objFolder = objFSO.GetFolder(Subfolder.Path) 
        Set colFiles = objFolder.Files 
        For Each objFile in colFiles 
            Wscript.Echo "-File: " & objFile.Name 
            wscript.Echo "--Path: " & objFile.Path 
            Wscript.Echo "---DateMod: " & objFile.DateLastModified 
        Next 
    ParseSubfolders Subfolder 
    Next 
End Sub 
'=====================================================================================================