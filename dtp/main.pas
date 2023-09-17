{===============================================================================}
{         e-Species - A taxonomically intelligent species search engine         }
{               (C) 2008-2023 by Mauro J. Cavalcanti                            }
{                         <maurobio@gmail.com>                                  }
{                                                                               }
{  This program is free software; you can redistribute it and/or modify         }
{  it under the terms of the GNU General Public License as published by         }
{  the Free Software Foundation; either version 3 of the License, or            }
{  (at your option) any later version.                                          }
{                                                                               }
{  This program is distributed in the hope that it will be useful,              }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of               }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                 }
{  GNU General Public License for more details.                                 }
{                                                                               }
{  You should have received a copy of the GNU General Public License            }
{  along with this program. If not, see <http://www.gnu.org/licenses/>.         }
{                                                                               }
{  Requirements:                                                                }
{     Free Pascal Compiler 3.0+ (www.freepascal.org)                            }
{     Lazarus IDE 2.0+ (www.lazarus.freepascal.org)                             }
{     HtmlViewer 10.2+ (wiki.freepascal.org/THtmlPort)                          }
{                                                                               }
{  REVISION HISTORY:                                                            }
{    Version 1.00, 30th Jul 23 - Initial public release                         }
{    Version 1.01,  1st Aug 23 - Fixed a bug which prevented some required      }
{                                files not being found in Linux version         }
{    Version 1.02,  5th Aug 23 - Added a test to detect an internet connection  }
{    Version 1.03, 11th Aug 23 - Fixed a bug which sometimes caused the program }
{                                to crash when handling non-Unicode strings     }
{    Version 1.04, 12th Aug 23 - Changed the code for retrieving data from      }
{                                Wikipedia                                      }
{    Version 1.05, 17th Aug 23 - Changed the code for retrieving data from      }
{                                FiveFilters and NCBI                           }
{    Version 1.06, 13th Sep 23 - paweld:                                        }
{                                *Fixed a memory leaaks                         }
{                                *moved searching to thread                     }
{                                *replacing fphttpclient with synapse           }
{                                 (there should be no problems with different   }
{                                  versions of openssl)                         }
{                                *html stored in a string (no writing to disk)  }
{                                *inline images with resize                     }
{    Version 1.10, 17th Sep 23 - paweld:                                        }
{                                *new Bio unit (with threads, error handling)   }
{                                *adding error handling to httpclient           }
{===============================================================================}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, StdCtrls,
  Buttons, StrUtils, FileUtil, LCLIntf, ComCtrls, HtmlView, HtmlGlobals, Graphics,
  fpjson, jsonparser,
  base64,
  BioU;

type

  { TMainForm }

  TMainForm = class(TForm)
    HtmlViewer: THtmlViewer;
    SearchComboBox: TComboBox;
    SearchButton: TButton;
    ClearButton: TButton;
    ImageList: TImageList;
    SearchLabel: TLabel;
    Panel: TPanel;
    AboutButton: TSpeedButton;
    ReloadButton: TSpeedButton;
    HomeButton: TSpeedButton;
    StatusBar: TStatusBar;
    procedure AboutButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HomeButtonClick(Sender: TObject);
    procedure HtmlViewerHotSpotClick(Sender: TObject; const SRC: ThtString; var Handled: Boolean);
    procedure HtmlViewerHotSpotCovered(Sender: TObject; const SRC: ThtString);
    procedure ReloadButtonClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchComboBoxEditingDone(Sender: TObject);
  private
    procedure AfterGet(Sender: TObject);
  public
    Results: TStringList;
    procedure DoSearch(const queryStr: String);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  about;

var
  logo, style: String;
  FBio: TBio;
  Fcs: Int64;

function FindString(const SearchKey: String; SearchList: TStrings): Integer;
var
  Found: Boolean;
  I: Integer;
  SearchStr: String;
begin
  Found := False;
  I := 0;
  while (I < SearchList.Count) and (not Found) do
  begin
    SearchStr := SearchList.Strings[I];
    if (Pos(SearchKey, SearchStr) <> 0) then
    begin
      Result := I;
      Found := True;
      Exit;
    end;
    Inc(I);
  end;
  if not Found then
    Result := -1;
end;

{ TMainForm }

procedure TMainForm.DoSearch(const queryStr: String);
begin
  if OnlineStatus = 0 then
  begin
    MessageDlg('Error', 'No internet connection', mtError, [mbOK], 0);
    Exit;
  end;
  Results.Clear;
  if FBio.Running then
    FBio.Stop;
  Fcs := GetTickCount64;
  Screen.Cursor := crHourGlass;
  StatusBar.SimpleText := 'Please wait ...';
  FBio.AfterGet := @AfterGet;
  FBio.Get(queryStr);
  //FBio.GetAndWait(queryStr);
  //AfterGet(nil);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ss: TStringStream;
begin
  FBio := TBio.Create;
  Results := TStringList.Create;
  if FileExists(ExtractFilePath(Application.ExeName) + 'static/especies.png') then
  begin
    ss := TStringStream.Create;
    ss.LoadFromFile(ExtractFilePath(Application.ExeName) + 'static/especies.png');
    logo := '<img src="data:image/png;base64,' + EncodeStringBase64(ss.DataString) + '" height="73" width="385">';
    ss.Free;
  end;
  if FileExists(ExtractFilePath(Application.ExeName) + 'static/stylesheet.css') then
  begin
    ss := TStringStream.Create;
    ss.LoadFromFile(ExtractFilePath(Application.ExeName) + 'static/stylesheet.css');
    style := '<style>' + ss.DataString + '</style>';
    ss.Free;
  end;
  if not DirectoryExists(GetAppConfigDir(False)) then
  begin
    if not CreateDir(GetAppConfigDir(False)) then
    begin
      MessageDlg('Error', 'Failed to create data directory.', mtError, [mbOK], 0);
      Halt(0);
    end;
  end;
  if FileExists(GetAppConfigDir(False) + 'searchlist.txt') then
    SearchComboBox.Items.LoadFromFile(GetAppConfigDir(False) + 'searchlist.txt');
end;

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  SearchComboBox.Text := '';
  SearchComboBox.Items.Clear;
end;

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if MessageDlg('Confirmation', 'Are you sure you want to exit the program?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    CanClose := True
  else
    CanClose := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if SearchComboBox.Items.Count > 0 then
    SearchComboBox.Items.SaveToFile(GetAppConfigDir(False) + 'searchlist.txt');
  Results.Free;
  FBio.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  AppPath: String;
begin
  AppPath := ExtractFilePath(Application.ExeName);
  if FileExists(AppPath + '/static/index.htm') then
    HtmlViewer.LoadFromFile(AppPath + '/static/index.htm')
  else
    HtmlViewer.LoadFromString('<html><body bgcolor="white"><br><br><br><center><font size=7 color="green">e-Species</font></center></body></html>');
  case OnlineStatus of
    0: ShowMessage('No internet connection!!!');
    2: ShowMessage('No OpenSSL libraries - please install!');
  end;
end;

procedure TMainForm.HomeButtonClick(Sender: TObject);
begin
  FormShow(Sender);
end;

procedure TMainForm.HtmlViewerHotSpotClick(Sender: TObject; const SRC: ThtString; var Handled: Boolean);
begin
  if Pos('http', SRC) > 0 then
    OpenURL(SRC);
end;

procedure TMainForm.HtmlViewerHotSpotCovered(Sender: TObject; const SRC: ThtString);
begin
  StatusBar.SimpleText := SRC;
end;

procedure TMainForm.ReloadButtonClick(Sender: TObject);
begin
  if Results.Text <> '' then
    HtmlViewer.LoadFromString(Results.Text);
end;

procedure TMainForm.SearchButtonClick(Sender: TObject);
begin
  if WordCount(SearchComboBox.Text, [' ']) <> 2 then
  begin
    MessageDlg('Error', 'Please enter a binomial specific epithet into the text box.' + sLineBreak + sLineBreak +
      'Remember: Only *species* are true natural entities!', mtError, [mbOK], 0);
    SearchComboBox.Text := '';
  end
  else
  if WordCount(SearchComboBox.Text, [' ']) = 2 then
    DoSearch(SearchComboBox.Text);
end;

procedure TMainForm.SearchComboBoxEditingDone(Sender: TObject);
var
  Choice: Integer;
begin
  if not IsEmptyStr(SearchComboBox.Text, [' ']) then
  begin
    Choice := FindString(SearchComboBox.Text, SearchComboBox.Items);
    if Choice < 0 then
      SearchComboBox.Items.Add(SearchComboBox.Text);
  end;
end;

procedure TMainForm.AfterGet(Sender: TObject);
var
  status: String = '';
  i: Integer;
begin
  if FBio.Status = tsOK then
  begin
    if FBio.GBIFstatus <> '' then
    begin
      if FBio.GBIFstatus <> 'accepted' then
        status := Format(' (%s of <i>%s</i> %s)', [FBio.GBIFstatus, FBio.GBIFvalid_name, FBio.GBIFauthorship])
      else
        status := Format(' (%s)', [FBio.GBIFstatus]);
    end;
    //
    Results.Add('<html>');
    Results.Add('<head>');
    Results.Add(Format('<title>e-Species search Results for %s</title>', [FBio.Name]));
    Results.Add(style);
    Results.Add('</head>');
    Results.Add('<body bgcolor="#ffffff">');
    Results.Add('<h1>' + logo + '</h1>');
    Results.Add('<h3>A taxonomically intelligent biodiversity search engine</h3>');
    Results.Add('<p>Search biological databases for a taxonomic name. The search is done "on the fly" using web services (JSON/XML) or URL API''s.</p>');
    Results.Add(Format('<h2><i>%s</i> %s%s</h2>', [FBio.Name, FBio.GBIFauthorship, status]));
    Results.Add('<h3>Classification from CoL</h3>');
    if FBio.GBIFscientificname = '' then
      Results.Add('No names found')
    else
      Results.Add(Format('%s; %s; %s; %s; %s', [FBio.GBIFkingdom, FBio.GBIFphylum, FBio.GBIFclasse, FBio.GBIForder, FBio.GBIFfamily]));
    Results.Add('<h3>Text tags</h3>');
    for i := 0 to FBio.FFtags.Count - 1 do
      Results.Add(Format('<span style="display:inline;border:1px solid blue;padding:1px;margin:2px;line-height:22px;background-color:rgb(181,213,255);">' +
        '%s&nbsp;</span>', [sReplace(FBio.FFtags[i], ' ', '&nbsp;')]));
    Results.Add('<h3>Wikipedia</h3>');
    if FBio.WIKIsnippet = '' then
      Results.Add('No article title matches')
    else
    begin
      Results.Add(FBio.WIKIsnippet);
      Results.Add(Format('<p><a href="http://en.wikipedia.org/wiki/%s">Original article</a></p>', [sReplace(FBio.Name, ' ', '_')]));
    end;
    Results.Add('<h3>Genomics from NCBI</h3>');
    Results.Add(Format('TaxId: <a href="http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=%d">%d</a>&nbsp;', [FBio.NCBIid, FBio.NCBIid]));
    if FBio.NCBIscientificname = '' then
      Results.Add(Format('No items found for <i>%s</i> ', [FBio.Name]))
    else
    begin
      Results.Add(Format('<i>%s</i>', [FBio.NCBIscientificname]));
      Results.Add(Format('[%s] ', [FBio.NCBIdivision]));
      Results.Add(Format('Sequences: <a href="http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=Search&dopt=DocSum&term=txid%d">%d</a>' +
        ' nucleotide, <a href="http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Protein&cmd=Search&dopt=DocSum&term=txid%d">%d</a> protein',
        [FBio.NCBIid, Fbio.NCBInucNum, FBio.NCBIid, FBio.NCBIprotNum]));
    end;
    if FBio.NCBIlinks.Count > 0 then
    begin;
      Results.Add('<ul type="circle">');
      for i := 0 to FBio.NCBIlinks.Count - 1 do
        Results.Add(Format('<li><a href="%s">%s</a></li>', [FBio.NCBIlinks.Items[i].Key, FBio.NCBIlinks.Items[i].Value]));
      Results.Add('</ul>');
    end;
    Results.Add('<h3>Map from GBIF</h3>');
    if (FBio.GBIFkey = 0) or ( FBio.GBIFmapImage = '') then
      Results.Add('No species found')
    else if FBio.GBIFmapImage <> '' then
    begin
      Results.Add(Format('<p><a href="http://gbif.org/species/%d">%d record(s)</a></p>', [FBio.GBIFkey, FBio.GBIFcount]));
      if FBio.GBIFmapImage <> '' then
        Results.Add(Format('<a href="http://gbif.org/species/%d"><img src="%s" border=1/></a>', [FBio.GBIFkey, FBio.GBIFmapImage]));
    end;
    Results.Add('<h3>Images from Wikimedia Commons</h3>');
    if FBio.WIKIimages.Count = 0 then
      Results.Add('No images found')
    else
    begin
      for i := 0 to FBio.WIKIimages.Count - 1 do
        Results.Add(Format('<a href="%s"><img src="%s" width=94 height=145 border=1></a>', [FBio.WIKIimages.Items[i].Key, FBio.WIKIimages.Items[i].Value]));
    end;
    Results.Add('<h3>Articles from PubMed</h3>');
    if FBio.NCBIreferences.Count = 0 then
      Results.Add('No articles found')
    else
    begin
      for i := 0 to FBio.NCBIreferences.Count - 1 do
      begin
        Results.Add('<hr noshade>');
        Results.Add(Format('<b><a href="%s">%s</a></b><br>', [FBio.NCBIreferences.Items[i].Key, FBio.NCBIreferences.Items[i].Value]));
      end;
      Results.Add('<br><p align="left"><small><small>&copy; 2008-2023</small><a href="http://github.com/maurobio/"><small>Mauro J. Cavalcanti</small></a></small></p>');
      Results.Add('</body>');
      Results.Add('</html>');
    end;
  end
  else
  begin
    Results.Add('<html>');
    Results.Add('<body bgcolor="red">');
    Results.Add('<h1>Error:</h1>');
    Results.Add(FBio.ErrorMessage);
    Results.Add('</body></html>');
  end;
  Screen.Cursor := crDefault;
  StatusBar.SimpleText := 'Ready (' + IntToStr(GetTickCount64 - Fcs) + 'ms)';
  HtmlViewer.LoadFromString(Results.Text);
end;

end.
