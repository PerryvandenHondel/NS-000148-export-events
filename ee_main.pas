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
	ee_convert,
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
	// Set the defaults for some variables.
	
	gbFlagConvert := StrToBool(ReadSettingKey('Settings', 'Convert'));
	if gbFlagConvert = true then
		WriteLn('Converion: ON')
	else
		WriteLn('Converion: OFF');
	
	gbFlagIncludeComputer := StrToBool(ReadSettingKey('Settings', 'IncludeComputer'));
	if gbFlagIncludeComputer = true then
		WriteLn('Include computer accounts: ON')
	else
		WriteLn('Include computer accounts: OFF');
	
	// Get the computer name of where this program is running.
	gsComputerName := GetCurrentComputerName();
	
	// Create a PID (Process ID) file for the run of this program.
	gsPathPid := GetPathOfPidFile();
	
	// Display the program title text.
	ProgTitle();

	WriteLn();
	
	
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
	pathLpr: string;
	sizeLpr: integer;
begin
	//sEventLog := 'Security';
	pathLpr := ExportEventLog('Security');		// From unit ee_export
	WriteLn('Export is done to: ', pathLpr);
	sizeLpr := GetFileSizeInBytes(pathLpr);
	if sizeLpr > 0 then
	begin
		WriteLn('Export file contains ', sizeLpr, ' bytes.');
		
		if gbFlagConvert = true then
		begin
			ConvertLpr(pathLpr);
		end; // of if
	end;
	
	
	//ExportEventLog('System');
	//ExportEventLog('Application');
end; // of procedure ProgRun


procedure ProgTest();
var
	pathLpr: string;
begin
	ReadEventDefinitions();
	EventAndEventDetailsShow();
	
	//WriteLn(ProcessThisEvent(4000));
	//WriteLn(ProcessThisEvent(4624));
		
	pathLpr := 'R:\GitRepos\NS-000148-export-events\bin\VM60DC002\Security\VM60DC002-Sec-20150925104604-YrjbpKAp.lpr';
	ConvertLpr(pathLpr);


	
	
end; // of procedure ProgTest


procedure ProgDone();
begin
	// Delete the Process ID file.
	DeleteFile(gsPathPid);
	
	Halt(0);
end; // of procedure ProgDone()



begin
	ProgInit();
	//ProgRun();
	ProgTest();
	ProgDone();
end. // of program ExportEvents