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
	ee_global,
	ee_export,
	ee_convert,
	ee_transfer,
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
begin
	// Set the defaults for some variables.
	
	flagTestmode := StrToBool(ReadSettingKey('Settings', 'Testmode'));
	if flagTestmode = true then
		WriteLn('Testmode: ON')
	else
		WriteLn('Testmode: OFF');
	
	gbFlagConvert := StrToBool(ReadSettingKey('Settings', 'Convert'));
	if gbFlagConvert = true then
		WriteLn('Conversion to SKF: ON')
	else
		WriteLn('Conversion to SKF: OFF');
	
	gbFlagIncludeComputer := StrToBool(ReadSettingKey('Settings', 'IncludeComputer'));
	if gbFlagIncludeComputer = true then
		WriteLn('Include computer accounts: ON')
	else
		WriteLn('Include computer accounts: OFF');
	
	gbFlagVerboseMode := StrToBool(ReadSettingKey('Settings', 'VerboseMode'));
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
	e: integer;
	folderDestLpr: string;
	folderDestSkv: string;
	pathLpr: string;
	pathSkv: string;
	shareLpr: string;
	shareSkv: string;
	sizeLpr: integer;
begin
	//sEventLog := 'Security';
	if flagTestmode = false then
	begin
		// if flagTestmode = false
		pathLpr := ExportEventLog('Security');		// From unit ee_export
		WriteLn('Export is done to: ', pathLpr);
	end 
	else
	begin
		// if flagTestmode = true
		pathLpr := ReadSettingKey('Settings', 'Testfile');
	end; 
	sizeLpr := GetFileSizeInBytes(pathLpr);
	if sizeLpr > 0 then
	begin
		WriteLn('Export file contains ', sizeLpr, ' bytes.');
		
		if gbFlagConvert = true then
		begin
			// Conversion is needed 
			
			// Read the definitions from the .conf file.
			ReadEventDefinitions();
			EventAndEventDetailsShow();
			
			// Build the path of the SKV export file.
			pathSkv := StringReplace(pathLpr, EXTENSION_LPR, EXTENSION_SKV, [rfIgnoreCase, rfReplaceAll]);
			
			// Convert the file LPR to SKV
			ConvertLpr(pathLpr, pathSkv);
			
			if flagTestmode = false then
			begin
				// Read the share of where the converted SKV file needs to be move to.
				shareSkv := ReadSettingKey('Settings', 'ShareSkv');
	
				folderDestSkv := FixFolderAdd(shareSkv) + GetDateFs(true) + '\' + GetNetbiosDomain() + '\' + GetCurrentComputerName();
				e := RobocopyMove(pathSkv, folderDestSkv);
				if e > 15 then
					WriteLn('ERROR ', e, ' during moving of file ', pathSkv, ' to ', folderDestSkv)
				else
					WriteLn('Successfully moved ', pathSkv);
			end; // of if flagTestmode
		end; // of if
		
		if flagTestmode = false then
		begin
			// move the LPR file to the share.
			shareLpr := ReadSettingKey('Settings', 'ShareLpr');
	
			folderDestLpr := FixFolderAdd(shareLpr) + GetDateFs(true) + '\' + GetNetbiosDomain() + '\' + GetCurrentComputerName();
			e := RobocopyMove(pathLpr, folderDestLpr);
			if e > 15 then
				WriteLn('ERROR ', e, ' during moving of file ', pathLpr, ' to ', folderDestLpr)
			else
				WriteLn('Successfully moved ', pathLpr);
		end; // of if flagTestmode
	end
	else
	begin
		WriteLn('Export file ', pathLpr, ' is 0 (zero-bytes) file, nothing to do any more...');
	end; // of if
end; // of procedure ProgRun

{
procedure ProgTest();
var
	pathLpr: string;
	pathSkv: string;
	shareLpr: string;
	folderDestLpr: string;
	folderDestSkv: string;
	shareSkv: string;
	e: integer;
begin
	ReadEventDefinitions();
	EventAndEventDetailsShow();
	
	//WriteLn(ProcessThisEvent(4000));
	//WriteLn(ProcessThisEvent(4624));
		
	//pathLpr := 'R:\GitRepos\NS-000148-export-events\bin\VM60DC002\Security\VM60DC002-Sec-20150925104604-YrjbpKAp.lpr';
	pathLpr := 'R:\GitRepos\NS-000148-export-events\bin\NS00DC011\Security\NS00DC011-Sec-20150930103723-PXoGgssK.lpr';
	pathSkv := 'R:\GitRepos\NS-000148-export-events\bin\NS00DC011\Security\NS00DC011-Sec-20150930103723-PXoGgssK.skv';
	ConvertLpr(pathLpr, pathSkv);
	//pathLpr := 'R:\GitRepos\NS-000148-export-events\bin\NS00DC011\Security\NS00DC011-Sec-20150925082158-RuV8HgF3.lpr';
	// Read the LPR share from the config file.
	shareLpr := ReadSettingKey('Settings', 'ShareLpr');
	
	folderDestLpr := FixFolderAdd(shareLpr) + GetDateFs(true) + '\' + GetCurrentComputerName();
	e := RobocopyMove(pathLpr, folderDestLpr);
	if e > 15 then
		WriteLn('ERROR ', e, ' during moving of file ', pathLpr, ' to ', folderDestLpr)
	else
		WriteLn('Succesfully moved ', pathLpr);
end; // of procedure ProgTest
}


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