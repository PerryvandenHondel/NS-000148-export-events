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
		position: integer;				// Position in the export file.
		isString: boolean;				// Type of field, TRUE=String/FALSE=Number
		description: string;			// Description of keyName
	end; // of record
	TEventDetailArray = array of TEventDetailRecord;


var
	lpr: CTextFile;
	skv: CTextFile;
	headerPosArray: THeaderPosArray;
	eventArray: TEventArray;
	eventDetailArray: TEventDetailArray;


function ProcessThisEvent(e: integer): boolean;
procedure ReadEventDefinitions();
procedure EventAndEventDetailsShow();
procedure ConvertLpr(pathLpr: string; pathSkv: string);

	
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
//	newEventId			Event ID: 9999
//	newKeyName			Name of key in Splunk
//	newPosition			Position of the value in the export LPR file.
//	newIsString			Boolean for IsString (TRUE=String/FALSE=Number); determines enclosure with double quotes
//	newDesc				Descripion
//
var
	size: integer;
begin
	size := Length(eventDetailArray);
	SetLength(eventDetailArray, size + 1);
	eventDetailArray[size].eventId := newEventId;
	eventDetailArray[size].keyName := newKeyName;
	eventDetailArray[size].position := newPosition;
	eventDetailArray[size].isString := newIsString;
	eventDetailArray[size].description := newDesc;
end; // of procedure EventDetailRecordAdd


procedure EventAndEventDetailsShow();
//
//	Show the Events and Events Details.
//
var
	i: integer;
	j: integer;
	t: Ansistring;
begin
	WriteLn();
	WriteLn('EVENTS TO PROCESS');
	WriteLn('=================');
	for i := 0 to High(eventArray) do
	begin
		//Writeln(IntToStr(i) + Chr(9) + ' ' + IntToStr(EventArray[i].eventId) + Chr(9), EventArray[i].isActive, Chr(9) + IntToStr(EventArray[i].osVersion) + Chr(9) + EventArray[i].description);
		Writeln(AlignRight(i, 6) + AlignRight(eventArray[i].eventId, 6) + '  ' + eventArray[i].description);
		
		for j := 0 to High(eventDetailArray) do
		begin
			if eventDetailArray[j].eventId = eventArray[i].eventId then
			begin
				t := '      ';
				t := t + AlignRight(j, 6);                                                      // Number of line
				t := t + ' ' + AlignRight(eventDetailArray[j].eventId, 6);                      // Number of event id
				t := t + ' ' + AlignLeft(eventDetailArray[j].keyName, 10);                      // Splunk Key Name
				t := t + ' ' + AlignRight(eventDetailArray[j].position, 3);                     // Position in the export file
				t := t + ' ' + AlignLeft(BoolToStr(eventDetailArray[j].IsString), 5);           // Boolean of type (TRUE=String/FALSE=Number)
				t := t + ' ' + AlignLeft(eventDetailArray[j].description, 50);                  // Descripion of field.
				WriteLn(t);
			end;
		end;
		
	end;
end; // of procedure EventAndEventDetailsShow	


procedure ReadEventDefinitions();
var
	a: TStringArray;
	aFields: TStringArray;
	eventDescription: string;
	eventId: integer;
	exportEvents: string;
	numberOfFields: integer;
	x: integer;
	y: integer;
	fieldValue: string;
begin
	SetLength(a, 0);
	exportEvents := ReadSettingKey('Settings', 'ConvertEvents');
	
	a := SplitString(exportEvents, ';');
	for x := 0 to high(a) do
	begin
		//WriteLn(x, '>', a[x]);
		
		eventId := StrToInt(ReadSettingKey(a[x], 'Id'));
		eventDescription := ReadSettingKey(a[x], 'Description');
		numberOfFields := StrToInt(ReadSettingKey(a[x], 'NumberOfFields'));
		
		EventRecordAdd(eventId, eventDescription);
		
		for y := 1 to numberOfFields do
		begin
			// Find the keys in the config with name FieldX.
			fieldValue := ReadSettingKey(IntToStr(eventId), 'Field' + IntToStr(y));
			// Split the string into an array of strings.
			aFields := SplitString(fieldValue, ';');
			// Add a new record to the EventDetailArray with the values from the line above.
			EventDetailRecordAdd(eventId, aFields[0], StrToInt(aFields[1]), StrToBool(aFields[2]), aFields[3]);
		end; // of for
	end // of for
end; // of procedure ReadEventDefinitions



function ProcessThisEvent(e: integer): boolean;
{
	Read the events from the EventArray.
	Return the status for isActive.
	
	Returns
		TRUE		Process this event.
		FALSE		Do not process this event.
}
var
	i: integer;
	r: boolean;
begin
	r := false;
	
	//WriteLn;
	//WriteLn('ProcessThisEvent(): e=', e);
	for i := 0 to High(EventArray) do
	begin
		//WriteLn(i, chr(9), EventArray[i].eventId, Chr(9), EventArray[i].isActive);
		if eventArray[i].eventId = e then
		begin
			r := true;
			//WriteLn('FOUND ', e, ' ON POS ', i);
			break;
			// Found the event e in the array, return the isActive state
			//r := EventArray[i].isActive;
			//break;
		end;
	end;
	//WriteLn('ShouldEventBeProcessed():', Chr(9), e, Chr(9), r);
	ProcessThisEvent := r;
end;



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
		//WriteLn(x, ': ', a[x]);
		
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


function GetEventType(eventType: integer): string;
//
//	Returns the Event Type string for a EventType
//
//	1		ERROR
//	2		WARNING
//	3		INFO
//	4		SUCCESS	AUDIT
//	5		FAILURE AUDIT
//	
//	Source: https://msdn.microsoft.com/en-us/library/aa394226%28v=vs.85%29.aspx
//	
var
	r: string;
begin
	r := '';
	
	case eventType of
		1: r := 'ERR';	// Error
		2: r := 'WRN';	// Warning
		4: r := 'INF';	// Information
		8: r := 'AUS';	// Audit Success
		16: r := 'AUF';	// Audit Failure
	else
		r := 'UKN';		// Unknown, note: should never be returned.
	end;
	GetEventType := r;
end; // of function GetEventType


procedure ProcessLineWithEvent(a: TStringArray);
//
// Only the lines that need to be converted are processed by this procedure.
//
var
	x: integer;
	buffer: Ansistring;
	eventId: string;
	keyName: string;
	keyPos: integer;
	keyValue: string;
	IsString: boolean;
begin
	//WriteLn('ProcessLine() BEGIN=========================================================');
	
	{WriteLn('LINE TO TSTRINGARRAY:');
	for x := 0 to high(a) do
	begin
		WriteLn('   ', x, ': ', a[x]);
	end; // of for
	}
	// Build the buffer string to write to the export SKV file.
	// Write the date and time to the buffer.
	buffer := a[FindHeaderPos('TimeGenerated')] + ' ';
	
	// Add the Event type to the buffer.
	buffer := buffer + GetEventType(StrToInt(a[FindHeaderPos('EventType')]));
	
	// Get the Event ID from the line. Use the label from the header to determine the position.
	eventId := a[FindHeaderPos('EventID')];
	buffer := buffer + ' eid=' + eventId;
	
	for x := 0 to High(eventDetailArray) do
	begin
		if eventDetailArray[x].eventId = StrToInt(eventId) then
		begin
			Inc(giConvertedEvents);
			keyName := eventDetailArray[x].keyName;
			keyPos := eventDetailArray[x].position;
			isString := eventDetailArray[x].IsString;
			//WriteLn('KEY:', keyName, '      POS:', keyPos, '       ISSTR:', isString);
				
			// Get the keyvalue from the line array
			keyValue := a[keyPos];
			if (RightStr(keyValue, 1) = '$') and (gbFlagIncludeComputer = false) then
				exit;
			
			if Length(keyValue) > 0 then
			begin
				buffer := buffer + ' ' + keyName + '=';
				if isString = true then
					buffer := buffer + EncloseDoubleQuote(keyValue)	// STRING (isString=TRUE)
				else
					buffer := buffer + keyValue;						// NUMBER (isString= FALSE)
			end; // of if
		end; // of if
	end; // of for
	//WriteLn('BUFFER: ', buffer);
	skv.WriteToFile(buffer);
	//WriteLn('ProcessLine() END=========================================================');
end;


procedure CheckForLineProcessing(l: Ansistring);
//
//	Process a line with event log data that needs to be converted
//
var
	a: TStringArray;
	x: integer;
begin
	//WriteLn('CheckForLineProcessing():');
	
	// Extract the parts to the line to a StringArray.
	a := SplitString(l, LINE_SEPARATOR);
	for x := 0 to high(a) do
	begin
		if (x = FindHeaderPos('EventID')) and (ProcessThisEvent(StrToInt(a[x])) = true) then
			//WriteLn('*** PROCESS THIS EVENT: ', a[x], ' ***');
			ProcessLineWithEvent(a);
	end; // of for
end;


procedure ConvertLpr(pathLpr: string; pathSkv: string);
var
	strLine: Ansistring;
	intCurrentLine: integer;
begin
	WriteLn('ConvertLpr()');
	WriteLn('  file: ', pathLpr);
	WriteLn('    to: ', pathSkv);
	
	if GetFileSizeInBytes(pathLpr) = 0 then
	begin
		WriteLn('File ' + pathLpr + ' contains no data.');
		Exit;
	end; // of if 
		
	// Delete any existing output Splunk SKV file.
	if FileExists(pathSkv) = true then
	begin
		// File .skv already exists, delete it. Result from previous conversion.
		DeleteFile(pathSkv);
	end;
	
	skv := CTextFile.Create(pathSkv);
	skv.OpenFileWrite();
	
	lpr := CTextFile.Create(pathLpr);
	lpr.OpenFileRead();
	repeat
		strLine := lpr.ReadFromFile();
		intCurrentLine := lpr.GetCurrentLine();
		// WriteLn(AlignRight(intCurrentLine, 6) + ': ', strLine);
		WriteMod(intCurrentLine, STEP_MOD, 'lines'); // In USupport Library, write every STEP_MOD a line to the screen.
		if intCurrentLine = 1 then
			// When the current line = 1 it's the header, get the position of the labels. 
			ProcessHeader(strLine)
		else
			// intCurrentLine <> 1
			CheckForLineProcessing(strLine);
	until lpr.GetEof();
	WriteLn;
	
	WriteLn('A total of ', intCurrentLine, ' lines are processed.');
	
	lpr.CloseFile();
	
	skv.CloseFile();
end; // of procedure ConvertLpr


end. // of unit ee_convert



