//
//	Export Events using Logparser.exe
//
//
//
//		function ConvertProperDateTimeToDateTimeFs(sDateTime: string): string;
//		function GetLocalExportFolder(el: string): string;
//		function GetPathExport(el: string; sDateTime: string): string;
//		function GetPathLastRun(sEventLog: string): string;
//		function LastRunGet(sEventLog: string): string;
//		function LastRunPut(sEventLog: string): string;
//		procedure ExportEventLog(el: string);
//		procedure ProgDone();
//		procedure ProgInit();
//		procedure ProgRun();
//		procedure ProgTitle();
//



program ExportEvents;



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
	VERSION =					'01';
	DESCRIPTION =				'Export Events';
	EXTENSION_LPR = 			'.lpr';
	EXTENSION_SKV = 			'.skv';


var
	gsPathPid: string;				// Path of the PID (Process ID) file.
	gsComputerName: string;	



	
function ConvertProperDateTimeToDateTimeFs(sDateTime: string): string;
//
// Convert a proper date time to a date time to be used as a file name (File System).
//
// Converted:	YYYY-MM-DD HH:MM:SS  >> YYYYMMDDHHMMSS
//
var
	r: string;
begin
	r := StringReplace(sDateTime, '-', '', [rfIgnoreCase, rfReplaceAll]);
	r := StringReplace(r, ':', '', [rfIgnoreCase, rfReplaceAll]);
	r := StringReplace(r, ' ', '', [rfIgnoreCase, rfReplaceAll]);
	
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
	r := GetProgramFolder() + '\' + gsComputerName + '\'  + LeftStr(el, 3);
	
	// Make the folder as a sub folder.
	GetLocalExportFolder := r;
end; // of function GetLocalExportFolder


	
function GetPathLastRun(sEventLog: string): string;
//
//	Create a path fro the lastrun file containing the date time of the last run.
//
var
	r: string;
begin
	//r := GetProgramFolder() + '\' + gsComputerName + '\'  + sEventLog + '.lastexport';
	//MakeFolderTree(r);
	r := GetLocalExportFolder(sEventLog) + '\lastexport.dtm';
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

	
	
procedure ProgTitle();
begin
	WriteLn();
	WriteLn(StringOfChar('-', 80));
	WriteLn(UpperCase(GetProgramName()) + ' -- Version: ' + VERSION);
	WriteLn();
	WriteLn(DESCRIPTION);
	WriteLn(StringOfChar('-', 80));	
end; // of procedure ProgramTitle()
	
	

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
var
	fn: string;
begin
	// Build the file name.
	fn := gsComputerName + '-';
	fn := fn + LeftStr(el, 3) + '-';
	fn := fn + ConvertProperDateTimeToDateTimeFs(sDateTime) + '-';
	fn := fn + GetRandomString(8);
	GetPathExport := GetLocalExportFolder(el) + '\' + fn;
end; // of function GetPathExport



function RunLogparser(sPathLpr: string; sEventLog: string; sDateTimeLast: string; sDateTimeNow: string): integer;
//
//	Run Logparser.exe for a specfic Event Log.
//
//	sEventLog:		Name of Event Log to export.
//
var
	p: TProcess;	// Process
	c: AnsiString;		// Command Line
	//sDateTimeLast: string;
	//sDateTimeNow: string;
begin
	
	//WriteLn('SECONDS=', SecondsBetween(StrToDateTime(sDateTimeLast), StrToDateTime(sDateTimeNow)));
	
	//WriteLn('sDateTimeLast=', sDateTimeLast);
	//WriteLn('sDateTimeNow=', sDateTimeNow);
	//WriteLn('sPathLpr=', sPathLpr);
	
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
	c := c + 'WHERE TimeGenerated>''' + sDateTimeLast + ''' AND TimeGenerated<=''' + sDateTimeNow + '''" ';
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
	


procedure ExportEventLog(el: string);
var
	sDateTimeLast: string;
	sDateTimeNow: string;
	sPathLpr: string;
	intResult: integer;
begin
	sDateTimeLast := LastRunGet(el);
	sDateTimeNow := LastRunPut(el);

	sPathLpr := GetPathExport(el, sDateTimeLast) + EXTENSION_LPR;
	
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
	end
	else
	begin
		WriteLn('Logparser returned an error: ', intResult);
	end; // if else
	
	
end; // procedure ExportEventLog
	

procedure ProgInit();
//var
//	i: integer;
begin
	ProgTitle();

	// Get the computer name of where this program is running.
	gsComputerName := GetCurrentComputerName();
	
	// Generate a unique session ID for this run of the program.
	//gsUniqueSessionId := GetRandomString(MAX_RANDOM_STRING);
	
	// Create a PID (Process ID) file for the run of this program.
	gsPathPid := GetPathOfPidFile();
	
	// Dot not convert the LPR file to SKV.
	//gbDoConvert := false;
	
	//blnSkipComputerAccount := true;
	//blnDebug := false;
	
	// Initialize the Event count array.
	//SetLength(EventFound, 1);
	{
	if ParamCount > 0 then
	begin
		for i := 1 to ParamCount do
		begin
			//Writeln(i, ': ', ParamStr(i));
			
			case LowerCase(ParamStr(i)) of
				'--convert':
					begin
						gbDoConvert := true;
						WriteLn('Option selected to convert the output to Splunk Key-Values (SKV) layout format');
					end;
				'--include-computer-accounts':
					begin
						blnSkipComputerAccount := false;
						WriteLn('Option selected to include computer accounts in the Splunk conversion.');
					end;
				'--help', '-h', '-?':
					begin
						ProgramUsage();
						ProgDone()
					end;
			end; // of case
		end; // of for
	end;
	}
end; // of procedure ProgInit()



procedure ProgRun();
begin
	//sEventLog := 'Security';
	ExportEventLog('Security');
	//ExportEventLog('System');
	//ExportEventLog('Application');
end; // of procedure ProgRun



procedure ProgDone();
begin
	// Delete the Process ID file.
	DeleteFile(gsPathPid);
	
	Halt(0);
end; // of procedure ProgDone()



begin
	ProgInit();
	ProgRun();
	//ProgTest();
	ProgDone();
end. // of program ExportEvents