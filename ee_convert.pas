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
	// Header Position Array definition
	THeaderPos = record
		lab: string;
		pos: integer;
	end; // of record 
	THeaderPosArray = array of THeaderPos;

	// Event Record type definition
	TEventRecord = record
		eventId: integer;
		description: string;
		count: integer;
		osVersion: word;
	end; // of record
	TEventArray = array of TEventRecord;
	
	TEventDetailRecord = record
		eventId: integer;				// eventId
		keyName: string;				// KeyName in Splunk
		Position: integer;				// Position in the export file.
		IsString: boolean;				// Type of field, TRUE=String/FALSE=Number
		Description: string;			// Description of keyName
	end; // of record
	TEventDetailArray = array of TEventDetailRecord;

var
	lpr: CTextFile;
	skv: CTextFile;
	headerPosArray: THeaderPosArray;
	eventArray: TEventArray;
	eventDetailArray: TEventDetailArray


procedure ReadEventDefinitions();
procedure EventRecordShow();
procedure ConvertLpr(pathLpr: string);

	
implementation


procedure EventRecordAdd(newEventId: integer; newDescription: string);
//
//
//	Add a new record in the array of Event
//  
//	newEventId      integer		The event id to search for
//	newDescription  string		Description of the event
//									
var
	size: integer;
begin
	size := Length(eventArray);
	SetLength(eventArray, size + 1);
	eventArray[size].eventId := newEventId;
	eventArray[size].description := newDescription;
end; // of procedure EventRecordAdd


procedure EventDetailRecordAdd(newEventId: integer; newKeyName: string; newPosition: integer; newIsString: boolean; newDesc: string);
//
//	Add a new record to the array of Event Details
//
//	newEventId
//	newKeyName
//	newPosition
//	newIsString
//	newDesc
//
var
	size: integer;
begin
	size := Length(eventDetailArray);
end; // of procedure EventDetailRecordAdd


procedure EventRecordShow();
var
	i: integer;
begin
	WriteLn();
	WriteLn('Events to process:');

	for i := 0 to High(eventArray) do
	begin
		//Writeln(IntToStr(i) + Chr(9) + ' ' + IntToStr(EventArray[i].eventId) + Chr(9), EventArray[i].isActive, Chr(9) + IntToStr(EventArray[i].osVersion) + Chr(9) + EventArray[i].description);
		Writeln(AlignRight(i, 6) + AlignRight(eventArray[i].eventId, 6) + '  ' + eventArray[i].description);
	end;
end; // of procedure EventRecordShow	


procedure ReadEventDefinitions();
var
	a: TStringArray;
	eventDescription: string;
	eventId: integer;
	exportEvents: string;
	x: integer;
	y: integer;
	foundField: boolean;
	fieldName: string;
	fieldPos: integer;
	fieldIsString: boolean;
	fieldDesc: string;
begin
	SetLength(a, 0);
	exportEvents := ReadSettingKey('Settings', 'ExportEvents');
	
	a := SplitString(exportEvents, ',');
	for x := 0 to high(a) do
	begin
		WriteLn(x, '>', a[x]);
		
		eventId := StrToInt(ReadSettingKey(a[x], 'Id'));
		eventDescription := ReadSettingKey(a[x], 'Description');
		
		WriteLn(eventId);
		WriteLn(eventDescription);
		
		EventRecordAdd(eventId, eventDescription);
		
		y := 0;
		foundField := true;
		repeat
			Inc(y);
			fieldName := ReadSettingKey(IntToStr(eventId), 'Field-' + IntToStr(y) + '-Name');
			if Length(fieldName) = 0 then
				// We haven't found a line with Field-x-Name anymore.
				// All Event details have been found!
				foundField := false
			else
			begin
			
				fieldPos := StrToInt(ReadSettingKey(IntToStr(eventId), 'Field-' + IntToStr(y) + '-Pos'));
				fieldIsString := StrToBool(ReadSettingKey(IntToStr(eventId), 'Field-' + IntToStr(y) + '-IsString'));
				fieldDesc := ReadSettingKey(IntToStr(eventId), 'Field-' + IntToStr(y) + '-Description');
				
				WriteLn('ITEM: ', y);
				WriteLn('fieldName: ', fieldName);
				WriteLn('fieldPos: ', fieldPos);
				WriteLn('fieldIsString: ', fieldIsString);
				WriteLn('fieldDesc: ', fieldDesc);
			end;
		until foundField = false;
		
		
	end // of for

end; // of procedure ReadEventDefinitions



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



