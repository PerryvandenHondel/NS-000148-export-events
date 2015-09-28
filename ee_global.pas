//
//	EE GLOBAL UNIT
//
//		function ConvertProperDateTimeToDateTimeFs(sDateTime: string): string;
//		function GetLocalExportFolder(el: string): string;
//		function GetPathExport(el: string; sDateTime: string): string;
//
//


unit ee_global;


interface


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
	CONF_NAME = 				'ee.conf';
	
var
	gsComputerName: string;
	gbFlagConvert: boolean;				// Flag to convert the LPR file to a SKV. (TRUE=Convert/FALSE=Do not convert)
	gbFlagIncludeComputer: boolean;		// Flag to include or exclude the computer accounts in the conversion (TRUE=Include computer accounts/FALSE=Skip computer accounts)


function ConvertProperDateTimeToDateTimeFs(sDateTime: string): string;
function GetLocalExportFolder(el: string): string;
function GetPathExport(el: string; sDateTime: string): string;
function ReadSettingKey(section: string; key: string): string;


implementation


function ReadSettingKey(section: string; key: string): string;
//
//	Read a Key from a section from a config (.conf) file.
//
//	[Section]
//	Key1=10
//	Key2=Something
//
//	Usage:
//		WriteLn(ReadSettingKey('Section', 'Key2'));  > returns 'Something'
//		When not found, returns a empty string.
//		
//	Needs updates for checking, validating data.
//
var
	r: string;
	sectionName: string;
	inSection: boolean;
begin
	p := GetProgramFolder() + '\readconf.conf';
	conf := CTextFile.Create(p);
	conf.OpenFileRead();

	r := '';
	sectionName := '['+ section + ']';
	inSection := false;
	repeat
		l := conf.ReadFromFile();
		//WriteLn(inSection, #9, l);
		
		if Pos(sectionName, l) > 0 then
		begin
			//WriteLn('FOUND SECTION: ', sectionName);
			inSection := true;
		end;
		
		if inSection = true then
		
		begin
			if (Pos(key, l) > 0) then
			begin
				//WriteLn('Found key ', key, ' found in section ', sectionName);
				r := RightStr(l, Length(l) - Length(key + '='));
				//WriteLn(r);
				Break; // break the loop
			end; // of if 
		end; // of if inSection
		
	until conf.GetEof();
	conf.CloseFile();
	ReadSettingKey := r;
end; // of function ReadSettingKey



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
	r := GetProgramFolder() + '\' + gsComputerName + '\'  + el;
	
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


end. // of unit ee_global


