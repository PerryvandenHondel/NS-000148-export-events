//
//	EE_CONVERT -- Conversion procedures and functions	
//


unit ee_convert;


{$MODE OBJFPC}


interface


uses
	Crt,
	Classes, 
	DateUtils,						// For SecondsBetween
	Process, 
	SysUtils,
	USupportLibrary,
	UTextFile,
	ee_global;

	
var
	lpr: CTextFile;
	skv: CTextFile;

	
procedure ConvertLpr(pathLpr: string);

	
implementation


procedure ProcessHeader(l: Ansistring);
begin
	WriteLn('ProcessHeader():');
end;


procedure ProcessLine(l: Ansistring);
begin
	WriteLn('ProcessLine():');
end;



procedure ConvertLpr(pathLpr: string);
var
	strLine: Ansistring;
	intCurrentLine: integer;
	pathSkv: string;
begin
	WriteLn('ConvertLpr(): ', pathLpr);
	
	if GetFileSizeInBytes(pathLpr) = 0 then
	begin
		WriteLn('File ' + pathLpr + ' contains no data.');
		Exit;
	end; // of if 
		
	
	// Build the path of the SKV file. Change extension.
	pathSkv := StringReplace(pathLpr, EXTENSION_LPR, EXTENSION_SKV, [rfIgnoreCase, rfReplaceAll]);
	WriteLn(' - convert to SKV (pathSkv): ' + pathSkv);
	
	// Delete any existing output Splunk SKV file.
	if FileExists(pathSkv) = true then
	begin
		//WriteLn('WARNING: File ' + pathSplunk + ' found, deleted it.');
		DeleteFile(pathSkv);
	end;
	
	skv := CTextFile.Create(pathSkv);
	skv.OpenFileWrite();
	
	lpr := CTextFile.Create(pathLpr);
	lpr.OpenFileRead();
	repeat
		strLine := lpr.ReadFromFile();
		intCurrentLine := lpr.GetCurrentLine();
		WriteLn(intCurrentLine, ': >> ', strLine);
		if intCurrentLine = 1 then
			// When the current line = 1 it's the header
			ProcessHeader(strLine)
		else
			ProcessLine(strLine);
		
		//ProcessLine(intCurrentLine, strLine);
		//WriteLn(intCurrentLine, '|', strLine);
			
		//WriteMod(intCurrentLine, STEP_MOD); // In USupport Library
	until lpr.GetEof();
	lpr.CloseFile();
	
	skv.CloseFile();
end; // of procedure ConvertLpr


end.


// end of unit ee_convert



