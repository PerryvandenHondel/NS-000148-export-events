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

	
const
	LINE_SEPARATOR = '|';
		
type
	THeaderPos = record
		lab: string;
		pos: integer;
	end; // of record 
	THeaderPosArray = array of THeaderPos;


var
	lpr: CTextFile;
	skv: CTextFile;
	headerPosArray: THeaderPosArray;
	

	
procedure ConvertLpr(pathLpr: string);

	
implementation


procedure ProcessHeader(l: Ansistring);
//
//	Process the header line of a file.
//
//	Place all labels in the headerPosArray with there postion number. Starting with 0.
//
var
	a: TStringArray;
	x: integer;
	sizeArray: integer;
begin
	WriteLn('ProcessHeader():');
	
	a := SplitString(l, LINE_SEPARATOR);
	for x := 0 to High(a) do
	begin 
		WriteLn(x, ': ', a[x]);
		
		sizeArray := Length(headerPosArray);
		SetLength(headerPosArray, sizeArray + 1);
		headerPosArray[sizeArray].lab := a[x];
		headerPosArray[sizeArray].pos := x;
	end; // of for
end;


function FindHeaderPos(labSearch: string): integer;
//
//	Find the postion of a header label in the headerPosArray array
//
//	labSearch		Search for the name in labSearch.
//
//	Returns the position number of the found labSearch, returns -1 when not found.
//
var
	x: integer;
	r: integer;
begin
	//WriteLn('headerPosArray contents:');
	
	r := -1;  // We can return a 0 when the found label is 0.
	
	for x := 0 to high(headerPosArray) do
	begin
		//WriteLn(x, ': ', headerPosArray[x].lab, '=', headerPosArray[x].pos);
		
		if labSearch = headerPosArray[x].lab then
			r := headerPosArray[x].pos
	end; // of for
	
	FindHeaderPos := r;
end; // of function FindHeaderPos



procedure ProcessLine(l: Ansistring);
//
//	Process a line with event log data.
//
var
	a: TStringArray;
	x: integer;
begin
	WriteLn('ProcessLine():');
	
	a := SplitString(l, LINE_SEPARATOR);
	for x := 0 to high(a) do
	begin
		WriteLn(x, ':', a[x]);
	end; // of for
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
			// When the current line = 1 it's the header, get the position of the labels. 
			// Fill headerPosArray
			ProcessHeader(strLine)
		else
			ProcessLine(strLine);
		
		
		//WriteLn('*** EventID=', FindHeaderPos('EventID'));
		//WriteLn('*** TimeGenerated=', FindHeaderPos('TimeGenerated'));
		//WriteLn('*** Whatever=', FindHeaderPos('Whatever'));
		
		
		
		//ProcessLine(intCurrentLine, strLine);
		//WriteLn(intCurrentLine, '|', strLine);
			
		//WriteMod(intCurrentLine, STEP_MOD); // In USupport Library
	until lpr.GetEof();
	lpr.CloseFile();
	
	skv.CloseFile();
end; // of procedure ConvertLpr


end.


// end of unit ee_convert



