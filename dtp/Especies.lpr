program Especies;

{$mode objfpc}{$H+}

{$I cef.inc}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF} {$IFDEF LINUX}
  InitSubProcess, // On Linux this unit must be used *before* the "interfaces" unit.
 {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Dialogs,
  SysUtils,
  Forms,
  uCEFApplication,
  main,
  about { you can add units after this };

{$R *.res}

{$IFDEF WIN32}
// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}

begin
  GlobalCEFApp := TCefApplication.Create;
  GlobalCEFApp.CheckCEFFiles := False;

  // In case you want to use custom directories for the CEF3 binaries, cache and user data.
  // If you don't set a cache directory the browser will use in-memory cache.
  GlobalCEFApp.FrameworkDirPath :=
    IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'cef' + PathDelim;
  //GlobalCEFApp.ResourcesDirPath     := 'c:\cef';
  GlobalCEFApp.LocalesDirPath :=
    IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'locales' + PathDelim;
  GlobalCEFApp.EnableGPU := True;      // Enable hardware acceleration
  //GlobalCEFApp.cache                := IncludeTrailingPathDelimiter(Apath)+'cache'+PathDelim;
  GlobalCEFApp.UserDataPath :=
    IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'static' + PathDelim;

  if GlobalCEFApp.StartMainProcess then
  begin
    RequireDerivedFormResource := True;
    Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TAboutForm, AboutForm);
    Application.Run;
  end;

  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.
