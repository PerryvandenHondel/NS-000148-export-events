//
//	EE_TRANSFER -- File copy procedures and functions
//


unit ee_transfer;


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

	
function RobocopyMove(const sPathSource: string; sFolderDest: string): integer;


implementation


function RobocopyMove(const sPathSource: string; sFolderDest: string): integer;
//
//	Use Robocopy.exe to move a file.
//	Create folders that are needed is done by Robocopy.
//	
//		sPathSource			D:\folder\folder\file.ext
//		sFolderDest			D:\foldernew
//	
//	Returns the error level of robocopy.exe execution
//	An error level > 15 is an error.
//
var
	p: TProcess;
	c: string;
	sFilename: string;
	sFolderSource: string;
	r: integer;
	extension: string; 			// Branch: Issue4
begin
	if FileExists(sPathSource) = true then
	begin
		sFilename := ExtractFileName(sPathSource);
		sFolderSource := FixFolderRemove(ExtractFilePath(sPathSource));
		sFolderDest := FixFolderRemove(sFolderDest);
		extension := ExtractFileExt(sPathSource);

		WriteLn('ROBOCOPYMOVE()');
		WriteLn('  Moving file: ', sFilename);
		WriteLn('  from folder: ', sFolderSource);
		WriteLn('    to folder: ', sFolderDest);
		WriteLn('    Extension: ', extension); // Issue4
	
		//c := 'robocopy.exe ' + EncloseDoubleQuote(sFolderSource) + ' ' + EncloseDoubleQuote(sFolderDest) + ' ' + EncloseDoubleQuote(sFilename) + ' /mov';
		// Branch: Issue4
		c := 'robocopy.exe ' + EncloseDoubleQuote(sFolderSource) + ' ' + EncloseDoubleQuote(sFolderDest) + ' ' + EncloseDoubleQuote('*' + extension) + ' /mov /r:10 /w:10';
	
		WriteLn;
		WriteLn('Running command:');
		WriteLn(c);
		WriteLn;
		
		// Setup the process to be executed.
		p := TProcess.Create(nil);
		p.Executable := 'cmd.exe'; 
		p.Parameters.Add('/c ' + c);
		//p.Options := [poWaitOnExit];
		p.Options := [poWaitOnExit, poNoConsole]; // Branch: test
		//p.Options := [poWaitOnExit, poUsePipes];
	
		// Run the sub process.
		p.Execute;
	
		// Get the return code from the process.
		r := p.ExitStatus;
		WriteLn('Returned error level code: ', r);
	end
	else
	begin
		WriteLn('RobocopyMove(): Warning, can''t find file ', sPathSource);
	end;
	RobocopyMove := r;
end; // of function RobocopyMove.


end. // of unit ee_transfer



