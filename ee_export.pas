//
//	EE_EXPORT -- Export procedures and functions	
//


unit ee_export;


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

	
procedure ExportEventLog(el: string);

	
implementation





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



end.

// end of unit ulat_db



