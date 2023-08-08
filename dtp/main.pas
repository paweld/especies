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
{===============================================================================}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, StdCtrls,
  Buttons, StrUtils, FileUtil, fphttpclient, fpjson, jsonparser, openssl,
  opensslsockets, LCLIntf, ComCtrls, HtmlView, HtmlGlobals, Graphics,
  fpImage, fpreadpng, fpwritepng;

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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HomeButtonClick(Sender: TObject);
    procedure HtmlViewerHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: boolean);
    procedure HtmlViewerHotSpotCovered(Sender: TObject; const SRC: ThtString);
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

function GetUrlAs(Url: string; AsName: string): boolean;
begin
  Result := False;
  with TFPHttpClient.Create(nil) do
    try
      AddHeader('User-Agent',
        'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:18.0) Gecko/20100101 Firefox/18.0');
      AllowRedirect := True;
      if (ExtractFilePath(AsName) <> '') then
        if not DirectoryExists(ExtractFilePath(AsName)) then
          if not ForceDirectories(ExtractFilePath(AsName)) then
            Exit;
      try
        Get(Url, AsName);
        Result := True;
      finally
        Free;
      end;
    except
      on E: Exception do
        MessageDlg('Error', 'Failed to download image.', mtError, [mbOK], 0);
    end;
end;

function MergeImages(baseImageFile, overlayFile, resultFile: string): boolean;
var
  baseImg, ovrImg: TFPCustomImage;
  x, y: integer;
begin
  Result := True;
  baseImg := TFPMemoryImage.Create(1, 1);
  try
    baseImg.LoadFromFile(baseImageFile);
    ovrImg := TFPMemoryImage.Create(1, 1);
    try
      ovrImg.LoadFromFile(overlayFile);
      if (baseImg.Width <> ovrImg.Width) or (baseImg.Height <> ovrImg.Height) then
      begin
        MessageDlg('Error', 'Both images have different size.', mtError, [mbOK], 0);
        Result := False;
        Halt(0);
      end;
      for y := 0 to baseImg.Height - 1 do
        for x := 0 to baseImg.Width - 1 do
          baseImg.Colors[x, y] := AlphaBlend(baseImg.Colors[x, y], ovrImg.Colors[x, y]);
      baseImg.SaveToFile(resultFile);
    finally
      ovrImg.Free;
    end;
  finally
    baseImg.Free;
  end;
end;

function IsOnline(reliableserver: string = 'http://www.google.com'): boolean;
var
  http: tfphttpclient;
  httpstate: integer;
begin
  Result := False;
  try
    http := tfphttpclient.Create(nil);
    try
      http.Get(reliableserver);
      httpstate := http.ResponseStatusCode;
      if httpstate = 200 then
        Result := True
      else
        Result := False;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    http.Free;
  end;
end;

{ TMainForm }

procedure TMainForm.DoSearch(const queryStr: string);
var
  scientificname, authorship, status, valid_name, kingdom, phylum,
  classe, order, family, taxon_list: string;
  division, commonname, snippet: string;
  urlWiki, tagWord, tagHTML, refUrl, imgUrl, taxUrl, urlId, UrlNuc,
  urlProt, itemStr, baseMapUrl, pointsUrl: string;
  targetDir: string;
  key, taxId, nucNum, protNum: integer;
  i, nrecs: integer;
  linkOut, linkIn, imgs, tags, pubs: TStringList;
  GBIFSearch: TGBIFSearch;
  NCBISearch: TNCBISearch;
  WikiSearch: TWikiSearch;
  FFSearch: TFFSearch;
  PubMedSearch: TPubMedSearch;
begin
  if not IsOnline then
  begin
    MessageDlg('Error', 'No internet connection', mtError, [mbOK], 0);
    Exit;
  end;
  Application.ProcessMessages;
  Screen.Cursor := crHourGlass;
  Results.Clear;
  Results.Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">');
  Results.Add('<html>');
  Results.Add('<head>');
  Results.Add('<title>e-Species search results for ' + queryStr + '</title>');
  Results.Add('<link rel="stylesheet" type="text/css" href="./static/stylesheet.css">');
  Results.Add('</head>');
  Results.Add('<body bgcolor="#ffffff">');
  Results.Add('<h1><img src="./static/especies.png" height="73" width="385"></h1>');
  Results.Add('<h3>A taxonomically intelligent biodiversity search engine</h3>');
  Results.Add(
    '<p>Search biological databases for a taxonomic name. The search is done "on the fly" using web services (JSON/XML) or URL API''s.</p>');

  Application.ProcessMessages;
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

  Application.ProcessMessages;
  StatusBar.SimpleText := 'Fetching content from Wikipedia...';
  WikiSearch := TWikiSearch.Create;
  snippet := WikiSearch.Snippet(queryStr);
  Application.ProcessMessages;
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

  Application.ProcessMessages;
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

  targetDir := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(
    GetAppConfigDir(False)) + 'pictures');
  Application.ProcessMessages;
  StatusBar.SimpleText := 'Fetching distribution dara from GBIF...';
  Results.Add('<h3>Map from GBIF</h3>');
  if key = 0 then
    Results.Add('No species found')
  else
  begin
    baseMapUrl := 'https://tile.gbif.org/3857/omt/0/0/0@1x.png?style=gbif-classic';
    pointsUrl :=
      'https://api.gbif.org/v2/map/occurrence/density/0/0/0@1x.png?style=classic.point&taxonKey='
      + IntToStr(key);
    GetUrlAs(baseMapUrl, targetDir + 'basemap.png');
    GetUrlAs(pointsUrl, targetDir + 'points.png');
    MergeImages(targetDir + 'basemap.png', targetDir + 'points.png',
      targetDir + IntToStr(key) + '.png');
    DeleteFile(targetDir + 'basemap.png');
    DeleteFile(targetDir + 'points.png');
    taxUrl := '<a href=http://gbif.org/species/' + IntToStr(key) + '>';
    Results.Add('<p>' + taxUrl + IntToStr(nrecs) + ' record(s)</a></p>');
    Results.Add(taxUrl +
      //'<iframe id="mapByFrame" name="map" src="http://cdn.gbif.org/v1/map/index.html?type=TAXON&key='
      //+ IntToStr(key) +
      //'&resolution=2" height="56%" width="56%" frameborder="1"/></iframe></a>');
      '<img src="./pictures/' + IntToStr(key) + '.png"' +
      ' height="56%" width="56%" border=1/></a>');
  end;

  Application.ProcessMessages;
  StatusBar.SimpleText := 'Fetching images from Wikimedia Commons...';
  Results.Add('<h3>Images from Wikimedia Commons</h3>');
  imgs := WikiSearch.Images(queryStr, 5);
  if imgs.Count = 0 then
    Results.Add('No images found')
  else
  begin
    for i := 0 to imgs.Count - 1 do
    begin
      imgUrl := 'http://commons.wikimedia.org/wiki/Special:Filepath/' +
        ExtractDelimited(2, imgs[i], [':']);
      GetUrlAs(imgUrl, targetDir + ExtractFileName(imgUrl));
      refUrl := '<a href="http://en.wikipedia.org/wiki/' + imgs[i] + '">';
      Results.Add(refUrl +
        //'<img src="http://commons.wikimedia.org/wiki/Special:Filepath/' +
        '<img src="./pictures/' + ExtractDelimited(2, imgs[i], [':']) +
        '" width=94 height=145 border=1></a>');
    end;
  end;
  WikiSearch.Destroy;

  Application.ProcessMessages;
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

  Application.ProcessMessages;
  StatusBar.SimpleText := 'Ready';
  Results.Add(
    '<br><p align="left"><small><small>&copy; 2008-2023 </small><a href="http://github.com/maurobio/"><small>Mauro J. Cavalcanti</small></a></small></p>');
  Results.Add('</body>');
  Results.Add('</html>');
  Screen.Cursor := crDefault;
  Results.SaveToFile(GetAppConfigDir(False) + 'results.html');
  //HtmlViewer.LoadFromString(Results.Text);
  HtmlViewer.LoadFromFile(GetAppConfigDir(False) + 'results.html');
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  sourcePath: string;
  targetPath: string;
begin
  InitSSLInterface;
  Results := TStringList.Create;
  if not DirectoryExists(GetAppConfigDir(False)) then
  begin
    if not CreateDir(GetAppConfigDir(False)) then
    begin
      MessageDlg('Error', 'Failed to create data directory.', mtError, [mbOK], 0);
      Halt(0);
    end;
  end;
  if not DirectoryExists(GetAppConfigDir(False) +
    IncludeTrailingPathDelimiter('static')) then
  begin
    if not CreateDir(GetAppConfigDir(False) +
      IncludeTrailingPathDelimiter('static')) then
    begin
      MessageDlg('Error', 'Failed to create data directory.', mtError, [mbOK], 0);
      Halt(0);
    end;
    CopyFile('static' + PathDelim + 'especies.png', GetAppConfigDir(False) +
      PathDelim + 'static' + PathDelim + 'especies.png');
    CopyFile('static' + PathDelim + 'stylesheet.css', GetAppConfigDir(False) +
      PathDelim + 'static' + PathDelim + 'stylesheet.css');
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if MessageDlg('Confirmation', 'Are you sure you want to exit the program?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    CanClose := True
  else
    CanClose := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if SearchComboBox.Items.Count > 0 then
    SearchComboBox.Items.SaveToFile(GetAppConfigDir(False) + 'searchlist.txt');
  Results.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  AppPath: string;
begin
  AppPath := ExtractFilePath(Application.ExeName);
  HtmlViewer.LoadFromFile(AppPath + '/static/index.htm');
end;

procedure TMainForm.HomeButtonClick(Sender: TObject);
var
  AppPath: string;
begin
  AppPath := ExtractFilePath(Application.ExeName);
  HtmlViewer.LoadFromFile(AppPath + '/static/index.htm');
end;

procedure TMainForm.HtmlViewerHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: boolean);
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
  if FileExists(GetAppConfigDir(False) + 'results.html') then
    HtmlViewer.LoadFromFile(GetAppConfigDir(False) + 'results.html');
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
