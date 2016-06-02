//
//	EE_MAIN -- Main program loop 
//
//		function GetPathLastRun(sEventLog: string): string;
//		function LastRunGet(sEventLog: string): string;
//		function LastRunPut(sEventLog: string): string;
//		procedure ExportEventLog(el: string);
//		procedure ProgDone();
//		procedure ProgInit();
//		procedure ProgRun();
//		procedure ProgTitle();
//
// 
// folderLpr=\\10.4.222.20\000134-LPR
// folderSkv=\\10.4.222.20\000134-SKV



program ee_main;



{$MODE OBJFPC}



uses
	Crt,
	Classes, 
	DateUtils,						// For SecondsBetween
	Process, 
	SysUtils,
	USupportLibrary,
	UTextFile;


	
const	
	CONFIG_FILE = 				'ee.conf';
	VERSION =					'02';
	DESCRIPTION =				'Export Events';
	EXTENSION_LPR = 			'.lpr';
	EXTENSION_TMP = 			'.tmp';
	EXTENSION_SKV = 			'.skv';
	CONF_NAME = 				'ee.conf';
	STEP_MOD =					157;			// Step modulator for echo mod, use a off-number, not rounded as 10, 15, 100, 250 etc. to see the changes.
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
	gsPathPid: string;				// Path of the PID (Process ID) file.
	gsComputerName: string;
	gbFlagConvert: boolean;				// Flag to convert the LPR file to a SKV. (TRUE=Convert/FALSE=Do not convert)
	gbFlagIncludeComputer: boolean;		// Flag to include or exclude the computer accounts in the conversion (TRUE=Include computer accounts/FALSE=Skip computer accounts)
	gbFlagVerboseMode: boolean;			// Flag to set program in verbose mode, show all output to the screen.
	giConvertedEvents: integer;			// Number of events converted.
	flagTestmode: boolean;				// For testing the program.
	lpr: CTextFile;
	skv: CTextFile;
	headerPosArray: THeaderPosArray;
	eventArray: TEventArray;
	eventDetailArray: TEventDetailArray;
	
	
	

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
	exportEvents := ReadSettingKey(CONFIG_FILE, 'Settings', 'ConvertEvents');
	
	a := SplitString(exportEvents, ';');
	for x := 0 to high(a) do
	begin
		//WriteLn(x, '>', a[x]);
		
		eventId := StrToInt(ReadSettingKey(CONFIG_FILE, a[x], 'Id'));
		eventDescription := ReadSettingKey(CONFIG_FILE, a[x], 'Description');
		numberOfFields := StrToInt(ReadSettingKey(CONFIG_FILE, a[x], 'NumberOfFields'));
		
		EventRecordAdd(eventId, eventDescription);
		
		for y := 1 to numberOfFields do
		begin
			// Find the keys in the config with name FieldX.
			fieldValue := ReadSettingKey(CONFIG_FILE, IntToStr(eventId), 'Field' + IntToStr(y));
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

	
	
function ConvertProperDateTimeToDateTimeFs(sDateTime: string): string;
//
// Convert a proper date time to a date time to be used as a file name (File System).
//
// Converted:	YYYY-MM-DD HH:MM:SS  >> YYYYMMDD-HHMMSS
//
var
	r: string;
begin
	r := StringReplace(sDateTime, '-', '', [rfIgnoreCase, rfReplaceAll]);
	r := StringReplace(r, ':', '', [rfIgnoreCase, rfReplaceAll]);
	// Branch Issue4. Add a - between date and time. Improves readability
	r := StringReplace(r, ' ', '-', [rfIgnoreCase, rfReplaceAll]); 
	
	ConvertProperDateTimeToDateTimeFs := r;
end; // of function ConvertProperDateTimeToDateTimeFs


function GetLocalExportFolder(el: string): string;
//
//	Get the local export folder, used throughout the program.
//
//	el:		Event Log name (Security, Application, System)
//
var
	r: string;
begin
	// Build the path
	//r := GetProgramFolder() + '\' + gsComputerName + '\'  + el;
	r := GetProgramFolder();
	
	
	// Make the folder as a sub folder.
	GetLocalExportFolder := r;
end; // of function GetLocalExportFolder


function GetPathExport(el: string; sDateTime: string): string;
//
//	Return a path to an export file in format, parts:
//	
//	1) Computer name (VM00AS0234 > 023)
//	2) -
//	3) EventLog name
//	4) -
//	3) YYYYMMDDHHMMSS
//	4) -
//	5) 4 randomly generated characters.
//
//var
//	fn: string;
begin
	// Build the file name.
	//fn := gsComputerName + '-';
	//fn := fn + LeftStr(el, 3) + '-';
	//fn := fn + ConvertProperDateTimeToDateTimeFs(sDateTime) + '-';
	//fn := fn + GetRandomString(8);
	//GetPathExport := GetLocalExportFolder(el) + '\' + fn;
	//GetPathExport := GetProgramFolder() + '\' + fn;
	GetPathExport := SysUtils.GetTempFileName()
end; // of function GetPathExport

	
	
function GetPathLastRun(sEventLog: string): string;
//
//	Create a path fro the lastrun file containing the date time of the last run for each event log.
//
var
	r: string;
begin
	r := GetProgramFolder() + '\lastrun-' + (sEventLog) + '.dtm';
	MakeFolderTree(r);
	GetPathLastRun := r;
end; // of function GetPathLastRun


function LastRunGet(sEventLog: string): string;
//
//	Returns the date and time in proper format YYYY-MM-DD HH:MM:SS back from a file in variable sPath
//	When the file does not exist, create one, otherwise read the datatime in the file.
//
var
	sPath: string;
	f: TextFile;
	r: string;
begin
	sPath := GetPathLastRun(sEventLog);
	//WriteLn(' LastRunGet(): ', sPath);
	if FileExists(sPath) = true then
	begin
		//WriteLn('LastRunGet(): Read the line with the last date time from ' + sPath);
		AssignFile(f, sPath);
		{I+}
		// Open the file in read mode.
		Reset(f);
		ReadLn(f, r);
		CloseFile(f);
	end
	else
	begin
		//WriteLn('LastRunGet(): File ' + sPath + ' not found create a new file');
		
		AssignFile(f, sPath);
		{I+}
		// Open the file in write mode.
		ReWrite(f);
		r := GetProperDateTime(Now());
		WriteLn(f, r);
		CloseFile(f);
	end;
	LastRunGet := r;
end;


function LastRunPut(sEventLog: string): string;
//
//	Put the current date time using Now() in the file sPath.
//
var
	sPath: string;
	f: TextFile;
	r: string;
begin
	sPath := GetPathLastRun(sEventLog);
	if FileExists(sPath) = true then
	begin
		AssignFile(f, sPath);
		{I+}
		// Open the file in write mode.
		ReWrite(f);
		r := GetProperDateTime(Now());
		WriteLn(f, r);
		CloseFile(f);
	end
	else
	begin
		WriteLn('ERROR LastRunPut(): can''t find the file ' + sPath);
	end;
	LastRunPut := r;
end; // of function LastRunPut.


function RunLogparser(sPathLpr: string; sEventLog: string; sDateTimeLast: string; sDateTimeNow: string): integer;
//
//	Run Logparser.exe for a specific Event Log.
//
//	sPathLpr		Full path to the export file; d:\folder\folder\file.lpr
//	sEventLog		Name of Event Log to export.
//	sDateTimeLast	Date Time of the last export; format: YYYY-MM-DD HH:MM:SS
//	sDateTimeNow	Current Date Time; format: YYYY-MM-DD HH:MM:SS
//
//	Returns a integer containing the error level value of the Logparser command.
//
var
	p: TProcess;	// Process
	c: AnsiString;		// Command Line
begin
	// logparser.exe -i:EVT -o:TSV 
	// "SELECT TimeGenerated,EventLog,ComputerName,EventId,EventType,REPLACE_STR(Strings,'\u000d\u000a','|') AS Strings FROM \\NS00DC066\Security WHERE TimeGenerated>'2015-06-02 13:48:00' AND TimeGenerated<='2015-06-02 13:48:46'" -stats:OFF -oSeparator:"|" 
	// >"D:\ADBEHEER\Scripts\000134\export\NS00DC066\20150602-134800-72Od1Q7jYYJsZqFW.lpr"
	//
	
	// Added export fields (Issue2):
	// - EventLog
	// - ComputerName
	c := 'logparser.exe -i:EVT -o:TSV ';
	c := c + '"SELECT TimeGenerated,EventLog,ComputerName,EventId,EventType,REPLACE_STR(Strings,''\u000d\u000a'',''|'') AS Strings ';
	c := c + 'FROM '+ sEventLog + ' ';
	// Issue1 fix: https://github.com/PerryvandenHondel/NS-148-export-events/issues/1
	c := c + 'WHERE TimeGenerated>=''' + sDateTimeLast + ''' AND TimeGenerated<''' + sDateTimeNow + '''" ';
	c := c + '-stats:OFF -oSeparator:"|" ';
	c := c + '>' + sPathLpr;
	
	WriteLn('RunLogparser(): running command line:');
	WriteLn;
	WriteLn(c);
	WriteLn;
	
	// Setup the process to be executed.
	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c ' + c);
	// Check if the output is cleaner on the screen.
	p.Options := [poWaitOnExit, poUsePipes];
	// OLD: p.Options := [poWaitOnExit];
	
	// Run the sub process.
	p.Execute;
	
	RunLogparser := p.ExitStatus;
end; // of procedure RunLogparser


function ExportEventLog(el: string) : string;
//
//	Export Event logs
//
//	el		Name of the event log to export
//
//	Returns the full path to the export file
//
var
	sDateTimeLast: string;
	sDateTimeNow: string;
	sPathLpr: string;
	intResult: integer;
	r: string;
begin
	// Initialize all variables
	r := '';
	sDateTimeLast := LastRunGet(el);
	sDateTimeNow := LastRunPut(el);

	// Build the path of the export file
	//sPathLpr := GetPathExport(el, sDateTimeLast) + EXTENSION_LPR;
	// Store the export of Logparser in a temp file on the system in the temp folder.
	sPathLpr := SysUtils.GetTempFileName();
	
	WriteLn('ExportEventLog()');
	WriteLn(' Event log: ', el);
	WriteLn('   DT Last: ', sDateTimeLast);
	WriteLn('    DT Now: ', sDateTimeNow);
	WriteLn(' Export to: ', sPathLpr);
	WriteLn;
	
	intResult := RunLogparser(sPathLpr, el, sDateTimeLast, sDateTimeNow);
	if intResult = 0 then
	begin
		WriteLn('Logparser ran OK!');
		r := sPathLpr;
	end
	else
	begin
		WriteLn('Logparser returned an error: ', intResult);
	end; // if else
	ExportEventLog := r;
end; // procedure ExportEventLog

	
	

procedure ProgTitle();
begin
	WriteLn();
	WriteLn(StringOfChar('-', 80));
	WriteLn(UpperCase(GetProgramName()) + ' -- Version: ' + VERSION);
	WriteLn();
	WriteLn(DESCRIPTION);
	WriteLn(StringOfChar('-', 80));	
end; // of procedure ProgramTitle()



procedure ProgInit();
begin
	// Set the defaults for some variables.
	
	flagTestmode := StrToBool(ReadSettingKey(CONFIG_FILE, 'Settings', 'Testmode'));
	if flagTestmode = true then
		WriteLn('Testmode: ON')
	else
		WriteLn('Testmode: OFF');
	
	gbFlagConvert := StrToBool(ReadSettingKey(CONFIG_FILE, 'Settings', 'Convert'));
	if gbFlagConvert = true then
		WriteLn('Conversion to SKF: ON')
	else
		WriteLn('Conversion to SKF: OFF');
	
	gbFlagIncludeComputer := StrToBool(ReadSettingKey(CONFIG_FILE,'Settings', 'IncludeComputer'));
	if gbFlagIncludeComputer = true then
		WriteLn('Include computer accounts: ON')
	else
		WriteLn('Include computer accounts: OFF');
	
	gbFlagVerboseMode := StrToBool(ReadSettingKey(CONFIG_FILE, 'Settings', 'VerboseMode'));
	if gbFlagVerboseMode = true then
		WriteLn('Verbose Mode: ON')
	else
		WriteLn('Verbose Mode: OFF');
	
	// Get the computer name of where this program is running.
	gsComputerName := GetCurrentComputerName();
	
	giConvertedEvents := 0;
	
	// Create a PID (Process ID) file for the run of this program.
	gsPathPid := GetPathOfPidFile();
	
	// Display the program title text.
	ProgTitle();

	WriteLn();
end; // of procedure ProgInit()


procedure ProgRun();
//
//	Main program run 
//
var
	//e: integer;
	//folderDestLpr: string;
	//folderDestSkv: string;
	pathLpr: string;
	pathSkv: string;
	//pathTmp: string;
	//shareLpr: string;
	shareSkv: string;
	sizeLpr: integer;
begin
	//sEventLog := 'Security';
	pathLpr := ExportEventLog('Security');
	WriteLn('Export is done to ', pathLpr);
	sizeLpr := GetFileSizeInBytes(pathLpr);
	if sizeLpr > 0 then
	begin
		WriteLn('Export file contains ', sizeLpr, ' bytes.');
		
		if gbFlagConvert = true then
		begin
			// Conversion is needed 
			
			// Read the definitions from the .conf file.
			ReadEventDefinitions();
			//EventAndEventDetailsShow();
			
			// Build the path of the SKV export file.
			pathSkv := StringReplace(pathLpr, EXTENSION_TMP, EXTENSION_SKV, [rfIgnoreCase, rfReplaceAll]);
			
			// Convert the file LPR to SKV
			ConvertLpr(pathLpr, pathSkv);
			
			if GetFileSizeInBytes(pathSkv) > 0 then
			begin
				// Only attach current SKV file to the daily file when
				// size is not 0.
				
				// Read the path template of the SKV file.
				shareSkv := ReadSettingKey(CONFIG_FILE, 'Settings', 'ShareSkv');
			
				// Convert the template to a real path name
				shareSkv := ConvertPathTemplateToRealPath(shareSkv);

				MakeFolderTree(shareSkv);
				WriteLn('ShareSkv: ' + shareSkv);
				WriteLn;
			
				Writeln('Attach the contents of ' + pathSkv + ' >>> ' + shareSkv);
			end
			else
				WriteLn('File ' + pathSkv + ' contains no data');
		end;
	end
	else
	begin
		WriteLn('Export file ', pathLpr, ' is 0 (zero-bytes) file, nothing to do any more...');
	end; // of if
end; // of procedure ProgRun



procedure ProgDone();
begin
	WriteLn('Converted ', giConvertedEvents, ' events');
	
	// Delete the Process ID file.
	DeleteFile(gsPathPid);
	
	//WriteLn('Waiting until a key is pressed');
	//repeat
	//until KeyPressed;
	
	Halt(0);
end; // of procedure ProgDone()



begin
	ProgInit();
	ProgRun();
	//ProgTest();
	ProgDone();
end. // of program ExportEvents