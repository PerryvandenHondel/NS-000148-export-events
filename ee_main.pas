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


program ee_main;


{$MODE OBJFPC}


uses
	Crt,
	Classes, 
	DateUtils,						// For SecondsBetween
	Process, 
	SysUtils,
	ee_global,
	ee_export,
	//ee_convert,
	USupportLibrary,
	UTextFile;

	
var
	gsPathPid: string;				// Path of the PID (Process ID) file.

	
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
	ExportEventLog('Security');		// From unit ee_export
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