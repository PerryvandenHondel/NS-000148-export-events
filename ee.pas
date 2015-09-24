//
//	Export Events using Logparser.exe
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



var
	gsPathPid: string;				// Path of the PID (Process ID) file.
	gsComputerName: string;	


	
	
function GetPathLastRun(sEventLog: string): string;
//
//	Create a path fro the lastrun file containing the date time of the last run.
//
var
	r: string;
begin
	r := GetProgramFolder() + '\' + gsComputerName + '\'  + sEventLog + '.lastexport';
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
	
	

procedure ProgInit();
var
	i: integer;
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
var
	sEventLog: string;
	sDateTimeLast: string;
	sDateTimeNow: string;
begin
	sEventLog := 'Security';
	sDateTimeLast := LastRunGet(sEventLog);
	sDateTimeNow := LastRunPut(sEventLog);

	
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