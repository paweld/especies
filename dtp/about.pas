unit about;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows} Win32Proc, {$ENDIF} FileInfo, Classes, SysUtils, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLVersion, Buttons;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CompilerLabel: TLabel;
    IDELabel: TLabel;
    ChromiumLabel: TLabel;
    OKButton: TButton;
    OperatingSystemLabel: TLabel;
    PlatformLabel: TLabel;
    ProductIcon: TImage;
    ProductNameLabel: TLabel;
    SystemInformationLabel: TLabel;
    VersionNumberLabel: TLabel;
    DescriptionLabel: TLabel;
    CommentsLabel: TLabel;
    CopyrightLabel: TLabel;
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public

  end;

  Str255 = string[255];

var
  AboutForm: TAboutForm;

function OSVersion: Str255;

implementation

{$R *.lfm}

uses main;

function OSVersion: Str255;
begin
  {$IFDEF LCLcarbon}
  OSVersion := 'Mac OS X 10.';
  {$ELSE}
  {$IFDEF Linux}
  OSVersion := 'Linux Kernel ';
  {$IFDEF CPU32}
  OsVersion := OSVersion + ' (32-bits)';
  {$ENDIF}
  {$IFDEF CPU64}
  OsVersion := OSVersion + ' (64-bits)';
  {$ENDIF}
  {$ELSE}
  {$IFDEF UNIX}
  OSVersion := 'Unix ';
  {$ELSE}
  {$IFDEF WINDOWS}
  if WindowsVersion = wv95 then
    OSVersion := 'Windows 95 '
  else if WindowsVersion = wvNT4 then
    OSVersion := 'Windows NT v.4 '
  else if WindowsVersion = wv98 then
    OSVersion := 'Windows 98 '
  else if WindowsVersion = wvMe then
    OSVersion := 'Windows ME '
  else if WindowsVersion = wv2000 then
    OSVersion := 'Windows 2000 '
  else if WindowsVersion = wvXP then
    OSVersion := 'Windows XP '
  else if WindowsVersion = wvServer2003 then
    OSVersion := 'Windows Server 2003 '
  else if WindowsVersion = wvVista then
    OSVersion := 'Windows Vista '
  else if WindowsVersion = wv7 then
    OSVersion := 'Windows 7 '
  else if WindowsVersion = wv8 then
    OSVersion := 'Windows 8 '
  else if WindowsVersion = wv10 then
    OSVersion := 'Windows 10 '
  else
    OSVersion := 'Windows ';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

{ TAboutForm }

procedure TAboutForm.FormShow(Sender: TObject);
var
  Quad: TVersionQuad;
begin
  if GetProgramVersion(Quad) then
    VersionNumberLabel.Caption := 'v.' + VersionQuadToStr(Quad);
  CompilerLabel.Caption := 'Compiler: ' + Format('Free Pascal v.%s', [{$I %FPCVERSION%}]);
  IDELabel.Caption := 'IDE: ' + Format('Lazarus v.%s', [lcl_version]);
  PlatformLabel.Caption := 'Platform: ' + {$I %FPCTARGETOS%};
  OperatingSystemLabel.Caption := 'OS: ' + OSVersion;
  ChromiumLabel.Caption := 'Chromium Embedded Framework: v.' + CEFVersion;
end;

procedure TAboutForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

end.
