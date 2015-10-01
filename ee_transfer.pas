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
//	Returns the errorlevel of robocopy execution
//	An errorlevel > 15 is an error.
//
var
	p: TProcess;
	c: string;
	sFilename: string;
	sFolderSource: string;
	r: integer;
begin
	if FileExists(sPathSource) = true then
	begin
		sFilename := ExtractFileName(sPathSource);
		sFolderSource := FixFolderRemove(ExtractFilePath(sPathSource));
		sFolderDest := FixFolderRemove(sFolderDest);

		WriteLn('ROBOCOPYMOVE()');
		WriteLn('  Moving file: ', sFilename);
		WriteLn('  from folder: ', sFolderSource);
		WriteLn('    to folder: ', sFolderDest);
	
		c := 'robocopy.exe ' + EncloseDoubleQuote(sFolderSource) + ' ' + EncloseDoubleQuote(sFolderDest) + ' ' + EncloseDoubleQuote(sFilename) + ' /mov';
	
		WriteLn;
		WriteLn('Command:');
		WriteLn(c);
		WriteLn;
		
		// Setup the process to be executed.
		p := TProcess.Create(nil);
		p.Executable := 'cmd.exe'; 
		p.Parameters.Add('/c ' + c);
		p.Options := [poWaitOnExit];
		//p.Options := [poWaitOnExit, poUsePipes];
	
		// Run the sub process.
		p.Execute;
	
		// Get the return code from the process.
		r := p.ExitStatus;
	end
	else
	begin
		WriteLn('RobocopyMove(): Warning, can''t find file ', sPathSource);
	end;
	RobocopyMove := r;
end; // of function RobocopyMove.


end. // of unit ee_transfer



