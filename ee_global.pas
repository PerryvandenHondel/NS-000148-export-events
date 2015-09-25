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

var
	gsComputerName: string;	


function ConvertProperDateTimeToDateTimeFs(sDateTime: string): string;
function GetLocalExportFolder(el: string): string;
function GetPathExport(el: string; sDateTime: string): string;
	

implementation


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


