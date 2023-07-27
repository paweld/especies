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
{     HtmlViewer 11.9+ (wiki.freepascal.org/THtmlPort)                          }
{                                                                               }
{  REVISION HISTORY:                                                            }
{    Version 1.00, 27th Jul 23 - Initial public release                         }
{===============================================================================}

unit main;

{$mode objfpc}{$H+}
{$I cef.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Buttons, StrUtils, fpjson, jsonparser,
  openssl, opensslsockets, LCLIntf,
  uCEFLazarusCocoa, // required for Cocoa
  uCEFBrowserWindow;

type

  { TMainForm }

  TMainForm = class(TForm)
    BrowserWindow: TBrowserWindow;
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
    procedure AboutBtnClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AboutButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HomeButtonClick(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchComboBoxEditingDone(Sender: TObject);
  private

  public
    procedure DoSearch(const queryStr: string);
  end;

var
  MainForm: TMainForm;
  queryStr: string;
  Results: TStringList;

implementation

{$R *.lfm}

uses about, BioWS;

function FindString(const SearchKey: string; SearchList: TStrings): integer;
var
  Found: boolean;
  I: integer;
  SearchStr: string;
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

procedure TMainForm.DoSearch(const queryStr: string);
var
  scientificname, authorship, status, valid_name, kingdom, phylum, classe, order,
  family, taxon_list: string;
  division, commonname, snippet: string;
  urlWiki, tagWord, tagHTML, refUrl, taxUrl, urlId, UrlNuc, urlProt, itemStr: string;
  key, taxId, nucNum, protNum: integer;
  i, nrecs: integer;
  linkOut, linkIn, imgs, tags, pubs: TStringList;
  GBIFSearch: TGBIFSearch;
  NCBISearch: TNCBISearch;
  WikiSearch: TWikiSearch;
  FFSearch: TFFSearch;
  PubMedSearch: TPubMedSearch;
begin
  //Application.ProcessMessages;
  Screen.Cursor := crHourGlass;
  Results.Clear;
  Results.Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">');
  Results.Add('<html>');
  Results.Add('<head>');
  Results.Add('<title>e-Species search results for ' + queryStr + '</title>');
  Results.Add('<link rel="stylesheet" type="text/css" href="' + ExtractFilePath(ParamStr(0)) + '/static/stylesheet.css">');
  Results.Add('</head>');
  Results.Add('<body bgcolor="#ffffff">');
  Results.Add('<h1><img src="' + ExtractFilePath(ParamStr(0)) + '/static/especies.png" height="73" width="385"></h1>');
  Results.Add('<h3>A taxonomically intelligent biodiversity search engine</h3>');
  Results.Add(
    '<p>Search biological databases for a taxonomic name. The search is done "on the fly" using web services (JSON/XML) or URL API''s.</p>');

  StatusBar.SimpleText := 'Fetching taxonomic data from CoL...';
  GBIFSearch := TGBIFSearch.Create;
  GBIFSearch.Search(queryStr, key, scientificname, authorship, status, valid_name,
    kingdom, phylum, classe, order, family);
  if (Length(status) > 0) then
  begin
    if status <> 'accepted' then
      status := ' (' + status + ' of <i>' + valid_name + '</i>' + ' ' + authorship + ')'
    else
      status := ' (' + status + ')';
  end;
  taxon_list := kingdom + '; ' + phylum + '; ' + classe + '; ' + order + '; ' + family;
  Results.Add('<h2><i>' + queryStr + '</i>' + ' ' + authorship + status + '</h2>');
  Results.Add('<h3>Classification from CoL</h3>');
  if Length(scientificname) = 0 then
    Results.Add('No names found')
  else
    Results.Add(taxon_list);
  nrecs := GBIFSearch.Count(key);
  GBIFSearch.Destroy;

  StatusBar.SimpleText := 'Fetching content from Wikipedia...';
  WikiSearch := TWikiSearch.Create;
  snippet := WikiSearch.Snippet(queryStr);
  StatusBar.SimpleText := 'Building keyword list...';
  FFSearch := TFFSearch.Create;
  tags := FFSearch.termExtract(snippet, 10);
  tagHTML := '';
  Results.Add('<h3>Text tags</h3>');
  for i := 0 to tags.Count - 1 do
  begin
    tagWord := tags[i];
    tagWord := StringReplace(tagWord, ' ', '&nbsp;', [rfReplaceAll]);
    tagHTML := tagHTML +
      '<span style=''display:inline;border:1px solid blue; padding:1px;margin:2px;line-height:22px;background-color:rgb(181,213,255);''>'
      + tagWord + ' ' + '</span>';
  end;
  Results.Add(tagHTML);
  FFSearch.Destroy;

  urlWiki := 'http://en.wikipedia.org/wiki/' +
    StringReplace(queryStr, ' ', '_', [rfReplaceAll]);
  Results.Add('<h3>Wikipedia</h3>');
  if Length(snippet) = 0 then
    Results.Add('No article title matches')
  else
  begin
    Results.Add(snippet);
    Results.Add('<p><a href="' + urlWiki + '">Original article</a></p>');
  end;

  StatusBar.SimpleText := 'Fetching genomic data from NCBI...';
  Results.Add('<h3>Genomics from NCBI</h3>');
  NCBISearch := TNCBISearch.Create;
  NCBISearch.Summary(queryStr, taxId, division, scientificname,
    commonname, nucNum, protNum);
  urlId := 'http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=' +
    IntToStr(taxId);
  urlNuc :=
    'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=Search&dopt=DocSum&term=txid'
    + IntToStr(taxId) + '[Organism:exp]';
  urlProt :=
    'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Protein&cmd=Search&dopt=DocSum&term=txid'
    + IntToStr(taxId) + '[Organism:exp]';
  Results.Add('TaxId: <a href="' + urlId + '">' + IntToStr(taxId) + '</a>&nbsp;');
  if Length(scientificName) = 0 then
    Results.Add('No items found for ' + '<i>' + queryStr + '</i> ')
  else
  begin
    Results.Add('<i>' + scientificName + '</i>');
    Results.Add('[' + division + '] ');
    Results.Add('Sequences: ' + '<a href="' + urlNuc + '">' +
      IntToStr(nucNum) + '</a> nucleotide, ' + '<a href="' + urlProt +
      '">' + IntToStr(protNum) + '</a> protein');
  end;
  linkOut := NCBISearch.Links(taxId);
  linkIn := TStringList.Create;
  Results.Add('<ul type="circle">');
  for i := 0 to linkOut.Count - 1 do
  begin
    itemStr := linkOut.ValueFromIndex[i];
    if linkIn.IndexOf(itemStr) < 0 then
    begin
      Results.Add('<li><a href="' + linkOut.Names[i] + '">' +
        linkOut.ValueFromIndex[i] + '</a></li>');
      linkIn.Append(itemStr);
    end;
  end;
  linkIn.Free;
  Results.Add('</ul>');
  NCBISearch.Destroy;

  StatusBar.SimpleText := 'Fetching distribution data from GBIF...';
  Results.Add('<h3>Map from GBIF</h3>');
  if key = 0 then
    Results.Add('No species found')
  else
  begin
    taxUrl := '<a href=http://gbif.org/species/' + IntToStr(key) + '>';
    Results.Add('<p>' + taxUrl + IntToStr(nrecs) + ' record(s)</a></p>');
    Results.Add(taxUrl +
      '<iframe id="mapByFrame" name="map" src="http://cdn.gbif.org/v1/map/index.html?type=TAXON&key='
      + IntToStr(key) +
      '&resolution=2" height="56%" width="56%" frameborder="1"/></iframe></a>');
  end;

  StatusBar.SimpleText := 'Fetching images from Wikimedia Commons...';
  Results.Add('<h3>Images from Wikimedia Commons</h3>');
  imgs := WikiSearch.Images(queryStr, 5);
  if imgs.Count = 0 then
    Results.Add('No images found')
  else
  begin
    for i := 0 to imgs.Count - 1 do
    begin
      refUrl := '<a href="http://en.wikipedia.org/wiki/' + imgs[i] + '">';
      Results.Add(refUrl +
        '<img src="http://commons.wikimedia.org/wiki/Special:Filepath/' +
        ExtractDelimited(2, imgs[i], [':']) + '" width=94 height=145 border=1></a>');
    end;
  end;
  WikiSearch.Destroy;

  StatusBar.SimpleText := 'Fetching articles from PubMed...';
  Results.Add('<h3>Articles from PubMed</h3>');
  PubMedSearch := TPubMedSearch.Create;
  pubs := PubMedSearch.Search(queryStr);
  if (pubs = nil) or (pubs.Count = 0) then
    Results.Add('No articles found')
  else
  begin
    for i := 0 to pubs.Count - 1 do
    begin
      Results.Add('<hr noshade>');
      Results.Add('<b><a href="http://dx.doi.org/doi:' + pubs.ValueFromIndex[i] +
        '">' + pubs.Names[i] + '</a></b><br>');
    end;
  end;
  PubMedSearch.Destroy;

  Results.Add(
    '<br><p align="left"><small><small>&copy; 2008-2023 </small><a href="http://github.com/maurobio/"><small>Mauro J. Cavalcanti</small></a></small></p>');
  Results.Add('</body>');
  Results.Add('</html>');
  Screen.Cursor := crDefault;
  Results.SaveToFile(GetAppConfigDir(False) + 'results.html');
  BrowserWindow.LoadURL('file:///' + GetAppConfigDir(False) + 'results.html');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitSSLInterface;
  Results := TStringList.Create;
  if not DirectoryExists(GetAppConfigDir(False)) then
     if not CreateDir(GetAppConfigDir(False)) then
        MessageDlg('Error', 'Failed to create data directory.', mtError, [mbOK], 0);
  if FileExists(GetAppConfigDir(False) + 'searchlist.txt') then
    SearchComboBox.Items.LoadFromFile(GetAppConfigDir(False) + 'searchlist.txt');
end;

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AboutBtnClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  SearchComboBox.Text := '';
  SearchComboBox.Items.Clear;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if MessageDlg('Confirmation', 'Are you sure you want to exit the program?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    BrowserWindow.CloseBrowser(True);
    CanClose := True;
  end
  else
    CanClose := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if SearchComboBox.Items.Count > 0 then
    SearchComboBox.Items.SaveToFile(GetAppConfigDir(False) + 'searchlist.txt');
  Results.Free;
end;

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  BrowserWindow.LoadURL('file:///static/index.htm');
  Screen.Cursor := crDefault;
end;

procedure TMainForm.HomeButtonClick(Sender: TObject);
begin
  BrowserWindow.LoadURL('file:///static/index.htm');
end;

procedure TMainForm.ReloadButtonClick(Sender: TObject);
begin
  if FileExists(GetAppConfigDir(False) + 'results.html') then
    BrowserWindow.LoadURL('file:///' + GetAppConfigDir(False) + 'results.html');
end;

procedure TMainForm.SearchButtonClick(Sender: TObject);
begin
  if WordCount(SearchComboBox.Text, [' ']) <> 2 then
  begin
    MessageDlg('Error', 'Please enter a binomial specific epithet into the text box.'
      + sLineBreak + sLineBreak +
      'Remember: Only *species* are true natural entities!', mtError, [mbOK], 0);
    SearchComboBox.Text := '';
  end
  else
  if WordCount(SearchComboBox.Text, [' ']) = 2 then
  begin
    queryStr := SearchComboBox.Text;
    DoSearch(queryStr);
  end;
end;

procedure TMainForm.SearchComboBoxEditingDone(Sender: TObject);
var
  Choice: integer;
begin
  if not IsEmptyStr(SearchComboBox.Text, [' ']) then
  begin
    Choice := FindString(SearchComboBox.Text, SearchComboBox.Items);
    if Choice < 0 then
      SearchComboBox.Items.Add(SearchComboBox.Text);
  end;
end;

end.
