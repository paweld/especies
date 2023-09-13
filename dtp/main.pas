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
{===============================================================================}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, StdCtrls,
  Buttons, StrUtils, FileUtil, LCLIntf, ComCtrls, HtmlView, HtmlGlobals, Graphics,
  fpjson, jsonparser,
  base64,
  BioWS;

type

  { TLoadThread }

  TLoadThread = class(TThread)
  private
    Fname, Flogo, Fstyle, Fstatus: String;
    Fresults: TStringList;
    Fcursor: TCursor;
    FGBIFSearch: TGBIFSearch;
    FNCBISearch: TNCBISearch;
    FWikiSearch: TWikiSearch;
    FFFSearch: TFFSearch;
    FPubMedSearch: TPubMedSearch;
    procedure SynchroInfo;
    procedure SynchroResult;
  protected
    procedure Execute; override;
  public
    constructor Create(aname, alogo, astyle: String);
    destructor Destroy; override;
  end;

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
  loadthread: TLoadThread;

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

{ TLoadThread }

procedure TLoadThread.SynchroInfo;
begin
  if Screen.Cursor <> Fcursor then
    Screen.Cursor := Fcursor;
  MainForm.StatusBar.SimpleText := Fstatus;
end;

procedure TLoadThread.SynchroResult;
begin
  MainForm.Results.Text := Fresults.Text;
  MainForm.HtmlViewer.LoadFromString(Fresults.Text);
end;

procedure TLoadThread.Execute;
var
  status, taxon_list, snippet: String;
  urlWiki, tagWord, tagHTML, refUrl, imgUrl, taxUrl, urlId, UrlNuc, urlProt, itemStr, baseMapUrl, pointsUrl: String;
  s: String;
  i, nrecs: Integer;
  linkOut, linkIn, imgs, tags, pubs: TStringList;
  cs: Int64;
begin
  cs := GetTickCount64;
  if not Terminated then
  begin
    Fcursor := crHourGlass;
    Synchronize(@SynchroInfo);
  end;
  if not Terminated then
  begin
    Fresults.Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">');
    Fresults.Add('<html>');
    Fresults.Add('<head>');
    Fresults.Add('<title>e-Species search Fresults for ' + Fname + '</title>');
    Fresults.Add(Fstyle);
    Fresults.Add('</head>');
    Fresults.Add('<body bgcolor="#ffffff">');
    Fresults.Add('<h1>' + Flogo + '</h1>');
    Fresults.Add('<h3>A taxonomically intelligent biodiversity search engine</h3>');
    Fresults.Add('<p>Search biological databases for a taxonomic name. The search is done "on the fly" using web services (JSON/XML) or URL API''s.</p>');
  end;
  if not Terminated then
  begin
    Fstatus := 'Fetching taxonomic data from CoL...';
    Synchronize(@SynchroInfo);
  end;
  if not Terminated then
  begin
    FGBIFSearch.Search(Fname);
    if (Length(FGBIFSearch.status) > 0) then
    begin
      if FGBIFSearch.status <> 'accepted' then
        status := ' (' + FGBIFSearch.status + ' of <i>' + FGBIFSearch.valid_name + '</i>' + ' ' + FGBIFSearch.authorship + ')'
      else
        status := ' (' + FGBIFSearch.status + ')';
    end;
    taxon_list := FGBIFSearch.kingdom + '; ' + FGBIFSearch.phylum + '; ' + FGBIFSearch.classe + '; ' + FGBIFSearch.order + '; ' + FGBIFSearch.family;
    Fresults.Add('<h2><i>' + Fname + '</i>' + ' ' + FGBIFSearch.authorship + status + '</h2>');
    Fresults.Add('<h3>Classification from CoL</h3>');
    if Length(FGBIFSearch.scientificname) = 0 then
      Fresults.Add('No names found')
    else
      Fresults.Add(taxon_list);
    nrecs := FGBIFSearch.Count(FGBIFSearch.key);
  end;
  if not Terminated then
  begin
    Fstatus := 'Fetching content from Wikipedia...';
    Synchronize(@SynchroInfo);
  end;
  if not Terminated then
    snippet := FWikiSearch.Snippet(Fname);
  if not Terminated then
  begin
    Fstatus := 'Building keyword list...';
    Synchronize(@SynchroInfo);
  end;
  if not Terminated then
  begin
    tags := FFFSearch.termExtract(snippet, 10);
    tagHTML := '';
    Fresults.Add('<h3>Text tags</h3>');
    for i := 0 to tags.Count - 1 do
    begin
      tagWord := tags[i];
      tagWord := StringReplace(tagWord, ' ', '&nbsp;', [rfReplaceAll]);
      if Length(tagWord) > 0 then
        tagHTML := tagHTML + '<span style=''display:inline;border:1px solid blue;padding:1px;margin:2px;line-height:22px;background-color:rgb(181,213,255);''>'
          + tagWord + ' ' + '</span>';
      if Terminated then
        break;
    end;
    Fresults.Add(tagHTML);
  end;
  if not Terminated then
  begin
    urlWiki := 'http://en.wikipedia.org/wiki/' + StringReplace(Fname, ' ', '_', [rfReplaceAll]);
    Fresults.Add('<h3>Wikipedia</h3>');
    if Length(snippet) = 0 then
      Fresults.Add('No article title matches')
    else
    begin
      Fresults.Add(snippet);
      Fresults.Add('<p><a href="' + urlWiki + '">Original article</a></p>');
    end;
  end;
  if not Terminated then
  begin
    Fstatus := 'Fetching genomic data from NCBI...';
    Synchronize(@SynchroInfo);
  end;
  if not Terminated then
  begin
    Fresults.Add('<h3>Genomics from NCBI</h3>');
    FNCBISearch.Summary(Fname);
    urlId := 'http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=' + IntToStr(FNCBISearch.id);
    urlNuc := 'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=Search&dopt=DocSum&term=txid' + IntToStr(FNCBISearch.id) + '[Organism:exp]';
    urlProt := 'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Protein&cmd=Search&dopt=DocSum&term=txid' + IntToStr(FNCBISearch.id) + '[Organism:exp]';
    Fresults.Add('TaxId: <a href="' + urlId + '">' + IntToStr(FNCBISearch.id) + '</a>&nbsp;');
    if Length(FNCBISearch.scientificname) = 0 then
      Fresults.Add('No items found for ' + '<i>' + Fname + '</i> ')
    else
    begin
      Fresults.Add('<i>' + FNCBISearch.scientificname + '</i>');
      Fresults.Add('[' + FNCBISearch.division + '] ');
      Fresults.Add('Sequences: ' + '<a href="' + urlNuc + '">' + IntToStr(FNCBISearch.nucNum) + '</a> nucleotide, ' +
        '<a href="' + urlProt + '">' + IntToStr(FNCBISearch.protNum) + '</a> protein');
    end;
  end;
  if not Terminated then
  begin
    linkOut := FNCBISearch.Links(FNCBISearch.id);
    linkIn := TStringList.Create;
    Fresults.Add('<ul type="circle">');
    for i := 0 to linkOut.Count - 1 do
    begin
      itemStr := linkOut.ValueFromIndex[i];
      if linkIn.IndexOf(itemStr) < 0 then
      begin
        Fresults.Add('<li><a href="' + linkOut.Names[i] + '">' + linkOut.ValueFromIndex[i] + '</a></li>');
        linkIn.Append(itemStr);
        if Terminated then
          break;
      end;
    end;
    linkIn.Free;
    Fresults.Add('</ul>');
  end;
  if not Terminated then
  begin
    Fstatus := 'Fetching distribution dara from GBIF...';
    Synchronize(@SynchroInfo);
  end;                
  if not Terminated then
  begin                   
    Fresults.Add('<h3>Map from GBIF</h3>');
    if FGBIFSearch.key = 0 then
      Fresults.Add('No species found')
    else
    begin
      baseMapUrl := 'https://tile.gbif.org/3857/omt/0/0/0@1x.png?style=gbif-classic';
      pointsUrl :=
        'https://api.gbif.org/v2/map/occurrence/density/0/0/0@1x.png?style=classic.point&taxonKey=' + IntToStr(FGBIFSearch.key);
      s := GetAndMergeImages(baseMapUrl, pointsUrl, '', 300, 300, True);
      if s <> '' then
      begin
        taxUrl := '<a href=http://gbif.org/species/' + IntToStr(FGBIFSearch.key) + '>';
        Fresults.Add('<p>' + taxUrl + IntToStr(nrecs) + ' record(s)</a></p>');
        Fresults.Add(taxUrl + '<img src="' + s +'" border=1/></a>');
      end;
    end;
  end;
  if not Terminated then
  begin
    Fstatus := 'Fetching images from Wikimedia Commons...';
    Synchronize(@SynchroInfo);
  end;
  if not Terminated then
  begin
    Fresults.Add('<h3>Images from Wikimedia Commons</h3>');
    imgs := FWikiSearch.Images(Fname, 5);
    if imgs.Count = 0 then
      Fresults.Add('No images found')
    else
    begin
      for i := 0 to imgs.Count - 1 do
      begin
        if Terminated then
          break;
        imgUrl := 'http://commons.wikimedia.org/wiki/Special:Filepath/' + ExtractDelimited(2, imgs[i], [':']);
        refUrl := '<a href="http://en.wikipedia.org/wiki/' + imgs[i] + '">';
        Fresults.Add(refUrl + '<img src="' + GetImgUrlToBase64Png(imgUrl, True, 94, 145) + '" width=94 height=145 border=1></a>');
      end;
    end;
  end;
  if not Terminated then
  begin
    Fstatus := 'Fetching articles from PubMed...';
    Synchronize(@SynchroInfo);
  end; 
  if not Terminated then
  begin
    Fresults.Add('<h3>Articles from PubMed</h3>');
    pubs := FPubMedSearch.Search(Fname);
    if (pubs = nil) or (pubs.Count = 0) then
      Fresults.Add('No articles found')
    else
    begin
      for i := 0 to pubs.Count - 1 do
      begin
        Fresults.Add('<hr noshade>');
        Fresults.Add('<b><a href="http://dx.doi.org/doi:' + pubs.ValueFromIndex[i] + '">' + pubs.Names[i] + '</a></b><br>');
        if Terminated then
          break;
      end;
    end;
    Fresults.Add(
      '<br><p align="left"><small><small>&copy; 2008-2023 </small><a href="http://github.com/maurobio/"><small>Mauro J. Cavalcanti</small></a></small></p>');
    Fresults.Add('</body>');
    Fresults.Add('</html>');
  end;
  Fcursor := crDefault;
  if not Terminated then
  begin
    Synchronize(@SynchroResult);                
    Fstatus := 'Ready (' + IntToStr(GetTickCount64 - cs) + 'ms)';
  end
  else
    Fstatus := '';
  Synchronize(@SynchroInfo);
  //clean
  if Assigned(linkIn) then
    linkIn.Free;
end;

constructor TLoadThread.Create(aname, alogo, astyle: String);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Fname := aname;
  Flogo := alogo;
  Fstyle := astyle;
  Fresults := TStringList.Create;
  Fstatus := '';
  FGBIFSearch := TGBIFSearch.Create;
  FNCBISearch := TNCBISearch.Create;
  FWikiSearch := TWikiSearch.Create;
  FFFSearch := TFFSearch.Create;
  FPubMedSearch := TPubMedSearch.Create;
  Self.Start;
end;

destructor TLoadThread.Destroy;
begin
  FGBIFSearch.Free;
  FNCBISearch.Free;
  FWikiSearch.Free;
  FFFSearch.Free;
  FPubMedSearch.Free;
  Fresults.Free;
  inherited Destroy;
end;

{ TMainForm }

procedure TMainForm.DoSearch(const queryStr: String);
begin
  if not IsOnline then
  begin
    MessageDlg('Error', 'No internet connection', mtError, [mbOK], 0);
    Exit;
  end;
  Results.Clear;
  if loadthread <> nil then
  begin
    loadthread.Terminate;
    loadthread.WaitFor;
    FreeAndNil(loadthread);
  end;
  loadthread := TLoadThread.Create(queryStr, logo, style);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ss: TStringStream;
begin
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
  if loadthread <> nil then
  begin
    loadthread.Terminate;
    loadthread.WaitFor;
    FreeAndNil(loadthread);
  end;
  if SearchComboBox.Items.Count > 0 then
    SearchComboBox.Items.SaveToFile(GetAppConfigDir(False) + 'searchlist.txt');
  Results.Free;
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

end.
