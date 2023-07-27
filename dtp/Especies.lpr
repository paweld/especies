program Especies;

{$mode objfpc}{$H+}
{$I cef.inc}

uses
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF} {$IFDEF LINUX}
  InitSubProcess, // On Linux this unit must be used *before* the "interfaces" unit.
 {$ENDIF}
  Interfaces,
  Forms,
  main, about,
  GlobalCefApplication;

{$R *.res}

{$IFDEF WIN32}
// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.
