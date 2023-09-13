{===============================================================================}
{                             B i o W S   Library                               }
{                                                                               }
{       A General-Purpose Library of Routines for Fetching Data From Several    }
{                       Online Biodiversity Databases                           }
{                                                                               }
{                            Version 1.0, July 2023                             }
{                            Version 2.0, August 2023                           }
{                                                                               }
{             Author: Mauro J. Cavalcanti, Rio de Janeiro, BRASIL               }
{                          E-mail: <maurobio@gmail.com>                         }
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
{===============================================================================}
unit BioWS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  LPDNetU,
  fpjson, jsonparser,
  DOM, XMLRead, XPath,
  base64,
  BGRABitmap, BGRABitmapTypes;

type

  { TGBIFSearch }

  TGBIFSearch = class(TObject) { Search GBIF (http://www.gbif.org) }
  private
    Fauthorship: String;
    Fclasse: String;
    Ffamily: String;
    Fkey: Integer;
    Fkingdom: String;
    Forder: String;
    Fphylum: String;
    Fscientificname: String;
    Fstatus: String;
    Fvalid_name: String;
  public
    GBIF_URL: String;
    constructor Create;
    function Search(const searchStr: String): Boolean;
    function Count(key: Integer): Integer;
    property key: Integer read Fkey write Fkey;
    property scientificname: String read Fscientificname write Fscientificname;
    property authorship: String read Fauthorship write Fauthorship;
    property status: String read Fstatus write Fstatus;
    property valid_name: String read Fvalid_name write Fvalid_name;
    property kingdom: String read Fkingdom write Fkingdom;
    property phylum: String read Fphylum write Fphylum;
    property classe: String read Fclasse write Fclasse;
    property order: String read Forder write Forder;
    property family: String read Ffamily write Ffamily;
  end;

  { TNCBISearch }

  TNCBISearch = class(TObject)
    { Search NCBI's Entrez taxonomy database (http://www.ncbi.nlm.nih.gov/Entrez) }
  private
    Fcommonname: String;
    Fdivision: String;
    Fid: Integer;
    FnucNum: Integer;
    FprotNum: Integer;
    Fscientificname: String;
  public
    NCBI_URL: String;
    linklist: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Summary(const searchStr: String): Boolean;
    function Links(id: Integer): TStringList;
    property id: Integer read Fid write Fid;
    property division: String read Fdivision write Fdivision;
    property scientificname: String read Fscientificname write Fscientificname;
    property commonname: String read Fcommonname write Fcommonname;
    property nucNum: Integer read FnucNum write FnucNum;
    property protNum: Integer read FprotNum write FprotNum;
  end;

  TWikiSearch = class(TObject) { Search Wikipedia (http://en.wikipedia.org) articles }
  public
    WIKIPEDIA_URL: String;
    WIKIMEDIA_URL: String;
    WIKIPEDIA_REDIRECT_URL: String;
    candidates: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Snippet(const searchStr: String): String;
    function Images(const searchStr: String; limit: Integer = 10): TStringList;
  end;

  TFFSearch = class(TObject) { Search FiveFilters }
  public
    FF_URL: String;
    Lines: TStringList;
    constructor Create;
    destructor Destroy; override;
    function termExtract(const contextStr: String; limit: Integer = 10): TStringList;
  end;

  TPubMedSearch = class(TObject) { Search PubMed }
  public
    PUBMED_URL: String;
    references: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Search(const searchStr: String; limit: Integer = 10): TStringList;
  end;

  function GetUrlToFile(Url: String; AsName: String): Boolean;
  function GetImgUrlToBase64Png(Url: String; InlineImg: Boolean; newWidth: Integer = 0; newHeight: Integer = 0; Proportional: Boolean = False): String;
  function GetAndMergeImages(baseUrl, pointsUrl: String; resultFile: String; newWidth: Integer = 0; newHeight: Integer = 0; Proportional: Boolean = False): String;
  function IsOnline(reliableserver: String = 'http://www.google.com'): Boolean;

implementation

function GetUrlToFile(Url: String; AsName: String): Boolean;
var
  ms: TMemoryStream;
  Client: THttpClient;
begin
  Result := False;
  if (ExtractFilePath(AsName) <> '') and not DirectoryExists(ExtractFilePath(AsName)) and not ForceDirectories(ExtractFilePath(AsName)) then
    Exit;     
  Client := THttpClient.Create;
  ms := TMemoryStream.Create;
  if Client.Get(Url, ms) and (ms.Size > 0) then
  begin
    ms.SaveToFile(AsName);
    Result := True;
  end;
  ms.Free;
  Client.Free;
end;

function GetImgUrlToBase64Png(Url: String; InlineImg: Boolean; newWidth: Integer; newHeight: Integer; Proportional: Boolean): String;
var
  Client: THttpClient;
  ss: TStringStream;
  w, h: Integer;
  scale: Double;
  bmp: TBGRABitmap;
begin
  Result := '';
  Client := THttpClient.Create;
  ss := TStringStream.Create;
  if Client.Get(Url, ss) and (ss.Size > 0) then
  begin
    ss.Position := 0;
    bmp := TBGRABitmap.Create(ss);
    if (newWidth > 0) and (newHeight > 0) then
    begin
      if Proportional then
      begin
        scale := newWidth / bmp.Width;
        if scale > (newHeight / bmp.Height) then
          scale := newHeight / bmp.Height;
        w := trunc(bmp.Width * scale);
        h := trunc(bmp.Height * scale);
      end
      else
      begin
        w := newWidth;
        h := newHeight;
      end;                                             
      BGRAReplace(bmp, bmp.Resample(w, h, rmFineResample));
    end;
    ss.Clear;
    bmp.SaveToStreamAsPng(ss);
    bmp.Free;
    Result := EncodeStringBase64(ss.DataString);
  end;
  ss.Free;
  if InlineImg and (Result <> '') then
    Result := 'data:image/png;base64,' + Result;
  Client.Free;
end;

function GetAndMergeImages(baseUrl, pointsUrl: String; resultFile: String; newWidth: Integer; newHeight: Integer; Proportional: Boolean): String;
var
  baseImg, ovrImg: TBGRABitmap;
  bms, pms: TMemoryStream;
  ss: TStringStream;
  x, y: Integer;
  px1, px2, px: TBGRAPixel;
  w, h: Integer;
  scale: Double;
  Client: THttpClient;
  b: Boolean;
begin
  Result := '';
  bms := TMemoryStream.Create;
  pms := TMemoryStream.Create;
  Client := THttpClient.Create;
  if Client.Get(baseUrl, bms) and (bms.Size > 0) then
  begin
    bms.Position := 0;
    baseImg := TBGRABitmap.Create(bms);
  end;
  Client.Clear;
  if Client.Get(pointsUrl, pms) and (pms.Size > 0) then
  begin
    pms.Position := 0;
    ovrImg := TBGRABitmap.Create(pms);
  end;
  Client.Free;
  b := (bms.Size = 0) or (pms.Size = 0);
  bms.Free;
  pms.Free;
  if b then
    exit;
  if (baseImg.Width <> ovrImg.Width) or (baseImg.Height <> ovrImg.Height) then
    exit;
  for y := 0 to baseImg.Height - 1 do
  begin
    for x := 0 to baseImg.Width - 1 do
    begin
      px1 := baseImg.GetPixel(x, y);
      px2 := ovrImg.GetPixel(x, y);
      if px2.alpha = 0 then
        px := px1
      else
        px := px2;
      baseImg.SetPixel(x, y, px);
    end;
  end;
  if (newWidth > 0) and (newHeight > 0) then
  begin
    if Proportional then
    begin
      scale := newWidth / baseImg.Width;
      if scale > (newHeight / baseImg.Height) then
        scale := newHeight / baseImg.Height;
      w := trunc(baseImg.Width * scale);
      h := trunc(baseImg.Height * scale);
    end
    else
    begin
      w := newWidth;
      h := newHeight;
    end;
    BGRAReplace(baseImg, baseImg.Resample(w, h, rmFineResample));
  end;
  if resultFile <> '' then
  begin
    baseImg.SaveToFile(resultFile);
    Result := 'OK';
  end
  else
  begin
    ss := TStringStream.Create;
    baseImg.SaveToStreamAsPng(ss);
    Result := 'data:image/png;base64,' + EncodeStringBase64(ss.DataString);
    ss.Free;
  end;
  ovrImg.Free;
  baseImg.Free;
end;

function IsOnline(reliableserver: String = 'http://www.google.com'): Boolean;
var
  http: THttpClient;
begin
  Result := False;
  http := THttpClient.Create;
  Result := http.Head(reliableserver);
  http.Free;
end;

{ TGBIFSearch methods }

constructor TGBIFSearch.Create;
begin
  GBIF_URL := 'http://api.gbif.org/v1';
  Fauthorship := '';
  Fclasse := '';
  Ffamily := '';
  Fkey := 0;
  Fkingdom := '';
  Forder := '';
  Fphylum := '';
  Fscientificname := '';
  Fstatus := '';
  Fvalid_name := '';
end;

function TGBIFSearch.Search(const searchStr: String): Boolean;
var
  jd: TJSONData;
  jo: TJSONObject;
  json: String;
  Client: THttpClient;
  i: Integer;
begin
  Result := False;
  Client := THttpClient.Create;
  if Client.Get(GBIF_URL + '/species/?name=' + StringReplace(searchStr, ' ', '%20', [rfReplaceAll]), json) and (json <> '') then
  begin
    jd := GetJson(json);
    if (jd.FindPath('results') = nil) or (TJSONArray(jd.FindPath('results')).Count = 0) then
    begin
      jd.Free;
      exit;
    end;
    jo := TJSONObject(TJSONArray(jd.FindPath('results')).Items[0]);
    for i := 0 to jo.Count - 1 do
    begin
      case jo.Names[i] of
        'key': Fkey := jo.Items[i].AsInteger;
        'canonicalName': Fscientificname := jo.Items[i].AsString;
        'authorship': Fauthorship := jo.Items[i].AsString;
        'taxonomicStatus':
          begin
            Fstatus := LowerCase(StringReplace(jo.Items[i].AsString, '_', ' ', [rfReplaceAll]));
            if Fstatus <> 'accepted' then
              Fvalid_name := jo.Items[jo.IndexOfName('species')].AsString;
          end;
        'kingdom': Fkingdom := jo.Items[i].AsString;
        'phylum': Fphylum := jo.Items[i].AsString;
        'class': Fclasse := jo.Items[i].AsString;
        'order': Forder := jo.Items[i].AsString;
        'family': Ffamily := jo.Items[i].AsString;
      end;
    end;
    jd.Free;
    Result := True;
  end;
  Client.Free;
end;

function TGBIFSearch.Count(key: Integer): Integer;
var
  jd: TJSONData;
  json: String;
  Client: THttpClient;
begin
  Result := -1;
  Client := THttpClient.Create;
  if Client.Get(GBIF_URL + '/occurrence/search?taxonKey=' + IntToStr(key), json) and (json <> '') then
  begin
    jd := GetJson(json);
    if jd.FindPath('count') <> nil then
      Result := jd.FindPath('count').AsInteger;
    jd.Free;
  end;
  Client.Free;
end;

{ TNCBISearch methods }

constructor TNCBISearch.Create;
begin
  NCBI_URL := 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/';
  linklist := TStringList.Create;
  linklist.NameValueSeparator := '|';
  Fcommonname := '';
  Fdivision := '';
  Fid := 0;
  FnucNum := 0;
  FprotNum := 0;
  Fscientificname := '';
end;

destructor TNCBISearch.Destroy;
begin
  linklist.Free;
  inherited Destroy;
end;

function TNCBISearch.Summary(const searchStr: String): Boolean;
var
  XmlData: Ansistring;
  Doc: TXMLDocument;
  Client: THttpClient;
  ms: TMemoryStream;
begin
  Result := False;
  Client := THttpClient.Create;
  ms := TMemoryStream.Create;
  { Get taxon id }
  if Client.Get(NCBI_URL + 'esearch.fcgi?db=taxonomy&term=' + StringReplace(searchStr, ' ', '+', [rfReplaceAll]), ms) and (ms.Size > 0) then
  begin
    ms.Position := 0;
    ReadXMLFile(Doc, ms);
    with EvaluateXPathExpression('/eSearchResult/IdList/Id', Doc.DocumentElement) do
    begin
      TryStrToInt(AsText, Fid);
      Free;
    end;
    Doc.Free;
  end;
  { Get summary data }
  ms.Clear;
  Client.Clear;
  if Client.Get(NCBI_URL + 'esummary.fcgi?db=taxonomy&id=' + IntToStr(Fid) + '&retmode=xml', ms) and (ms.Size > 0) then
  begin
    ms.Position := 0;
    ReadXMLFile(Doc, ms);
    with EvaluateXPathExpression('/eSummaryResult/DocSum/Item[@Name="Division"]', Doc.DocumentElement) do
    begin
      Fdivision := AsText;
      Free;
    end;
    with EvaluateXPathExpression('/eSummaryResult/DocSum/Item[@Name="ScientificName"]', Doc.DocumentElement) do
    begin
      Fscientificname := AsText;
      Free;
    end;
    with EvaluateXPathExpression('/eSummaryResult/DocSum/Item[@Name="CommonName"]', Doc.DocumentElement) do
    begin
      Fcommonname := AsText;
      Free;
    end;
    Doc.Free;
  end;
  { Get nucleotide sequences }
  ms.Clear;
  Client.Clear;
  if Client.Get(NCBI_URL + 'esearch.fcgi?db=nucleotide&term=' + StringReplace(searchStr, ' ', '+', [rfReplaceAll]), ms) and (ms.Size > 0) then
  begin
    ms.Position := 0;
    ReadXMLFile(Doc, ms);
    with EvaluateXPathExpression('/eSearchResult/Count', Doc.DocumentElement) do
    begin
      TryStrToInt(AsText, FnucNum);
      Free;
    end;
    Doc.Free;
  end;
  { Get protein sequences }
  ms.Clear;
  Client.Clear;
  if Client.Get(NCBI_URL + 'esearch.fcgi?db=protein&term=' + StringReplace(searchStr, ' ', '+', [rfReplaceAll]), ms) and (ms.Size > 0) then
  begin
    ms.Position := 0;
    ReadXMLFile(Doc, ms);
    with EvaluateXPathExpression('/eSearchResult/Count', Doc.DocumentElement) do
    begin
      TryStrToInt(AsText, FprotNum);
      Free;
    end;
    Doc.Free;
  end;
  Client.Free;
  ms.Free;
  Result := Fid <> 0;
end;

function TNCBISearch.Links(id: Integer): TStringList;
var
  Doc: TXMLDocument;
  Result1, Result2: TXPathVariable;
  NodeSet1, NodeSet2: TNodeSet;
  i: Integer;
  Client: THttpClient;
  ms: TMemoryStream;
begin
  linklist.Clear;
  Client := THttpClient.Create;
  ms := TMemoryStream.Create;
  { Get list of links }
  if Client.Get(NCBI_URL + 'elink.fcgi?dbfrom=taxonomy&id=' + IntToStr(id) + '&cmd=llinkslib', ms) and (ms.Size > 0) then
  begin
    ms.Position := 0;
    ReadXMLFile(Doc, ms);
    Result1 := EvaluateXPathExpression('//ObjUrl/Url', Doc.DocumentElement);
    Result2 := EvaluateXPathExpression('//ObjUrl/Provider/Name', Doc.DocumentElement);
    NodeSet1 := Result1.AsNodeSet;
    NodeSet2 := Result2.AsNodeSet;
    for i := 0 to NodeSet1.Count - 1 do
      linklist.Add(TDomElement(NodeSet1.Items[i]).TextContent + '|' + TDomElement(NodeSet2.Items[i]).TextContent);
    Result1.Free;
    Result2.Free;
    Doc.Free;
  end;
  Client.Free;
  ms.Free;
  Result := linklist;
end;

{ TWikiSearch methods }

constructor TWikiSearch.Create;
begin
  WIKIPEDIA_URL := 'https://en.wikipedia.org/api/rest_v1/page/summary/';
  WIKIMEDIA_URL := 'https://en.wikipedia.org/api/rest_v1/page/media-list/';
  WIKIPEDIA_REDIRECT_URL := 'https://en.wikipedia.org/w/api.php?action=query&titles=';
  candidates := TStringList.Create;
end;

destructor TWikiSearch.Destroy;
begin
  candidates.Free;
  inherited Destroy;
end;

(*function TWikiSearch.Snippet(const searchStr: string): string;
var
  JsonData: TJsonData;
  Client: TFPHttpClient;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      { Allow redirections }
      Client.AllowRedirect := True;
      JsonData := GetJson(Client.Get(WIKIPEDIA_URL +
        StringReplace(searchStr, ' ', '_', [rfReplaceAll])));
      Result := JsonData.FindPath('extract').AsUnicodeString;
    except
      Result := '';
    end;
  finally
    JsonData.Free;
    Client.Free;
  end;
end;*)

function TWikiSearch.Snippet(const searchStr: String): String;
var
  jd: TJSONData;
  json, queryStr: String;
  Client: THttpClient;
begin
  Result := '';
  Client := THttpClient.Create;
  { Allow redirections }
  if Client.Get(WIKIPEDIA_REDIRECT_URL + StringReplace(searchStr, ' ', '+', [rfReplaceAll]) + '&redirects&format=json', json) and (json <> '') then
  begin
    jd := GetJson(json);
    if jd.FindPath('query.redirects[0].to') <> nil then
      queryStr := jd.FindPath('query.redirects[0].to').AsString
    else
      queryStr := searchStr;
    jd.Free;
  end;
  //
  json := '';
  client.Clear;
  if Client.Get(WIKIPEDIA_URL + StringReplace(queryStr, ' ', '_', [rfReplaceAll]), json) and (json <> '') then
  begin
    jd := GetJson(json);
    if jd.FindPath('extract') <> nil then
      Result := jd.FindPath('extract').AsUnicodeString;
    jd.Free;
  end;
  Client.Free;
end;

(*{ Search images from Wikimedia Commons }
function TWikiSearch.Images(const searchStr: string; limit: integer = 10): TStringList;
var
  JsonData, JsonItem, JsonItems: TJsonData;
  Client: TFPHttpClient;
  i, Count: integer;
  ext: string;
begin
  try
    try
      Client := TFPHttpClient.Create(nil);
      Client.AllowRedirect := True;
      JsonData := GetJson(Client.Get(WIKIMEDIA_URL +
        StringReplace(searchStr, ' ', '_', [rfReplaceAll])));
      JsonItems := JsonData.FindPath('items');
      Count := 0;
      for i := 0 to JsonItems.Count - 1 do
      begin
        JsonItem := JsonItems.Items[i];
        ext := ExtractFileExt(JsonItem.FindPath('title').AsString);
        if (ext = '.jpg') then
        begin
          candidates.Add(JsonItem.FindPath('title').AsString);
          Inc(Count);
          if Count >= limit then
            break;
        end;
      end;
      Result := candidates;
    except
      candidates := nil;
    end;
  finally
    JsonData.Free;
    Client.Free;
  end;
end; *)

function TWikiSearch.Images(const searchStr: String; limit: Integer = 10): TStringList;
var
  jd: TJsonData;
  ja: TJSONArray;
  jo: TJSONObject;
  i, Count: Integer;
  json, queryStr, ext: String;
  Client: THttpClient;
begin
  candidates.Clear;
  Client := THttpClient.Create;
  { Allow redirections }
  if Client.Get(WIKIPEDIA_REDIRECT_URL + StringReplace(searchStr, ' ', '+', [rfReplaceAll]) + '&redirects&format=json', json) and (json <> '') then
  begin
    jd := GetJson(json);
    if jd.FindPath('query.redirects[0].to') <> nil then
      queryStr := jd.FindPath('query.redirects[0].to').AsString
    else
      queryStr := searchStr;
    jd.Free;
  end;
  //
  Count := 0;
  Client.Clear;
  json := '';
  if Client.Get(WIKIMEDIA_URL + StringReplace(queryStr, ' ', '_', [rfReplaceAll]), json) and (json <> '') then
  begin
    jd := GetJson(json);
    if jd.FindPath('items') <> nil then
    begin
      ja := TJSONArray(jd.FindPath('items'));
      for i := 0 to ja.Count - 1 do
      begin
        jo := TJSONObject(ja.Items[i]);
        if jo.FindPath('title') <> nil then
        begin
          ext := LowerCase(ExtractFileExt(jo.FindPath('title').AsString));
          if (ext = '.jpg') then
          begin
            candidates.Add(jo.FindPath('title').AsString);
            Inc(Count);
            if Count >= limit then
              break;
          end;
        end;
      end;
    end;
    jd.Free;
  end;
  Client.Free;
  Result := candidates;
end;

{ TFFSearch methods }

constructor TFFSearch.Create;
begin
  FF_URL := 'http://termextract.fivefilters.org/';
  Lines := TStringList.Create;
end;

destructor TFFSearch.Destroy;
begin
  Lines.Free;
  inherited Destroy;
end;

{ Provides a list of significant words or phrases extracted from a larger content from FiveFilters Web service }

function TFFSearch.termExtract(const contextStr: String; limit: Integer = 10): TStringList;
var
  TextData: Ansistring;
  Client: THttpClient;
begin
  Lines.Clear;
  Client := THttpClient.Create;
  if Client.Get(FF_URL + 'extract.php?text=' + StringReplace(contextStr, ' ', '+', [rfReplaceAll]) + '&output=txt&max=' + IntToStr(limit), textdata) and (textdata <> '') then
    Lines.Text := StringReplace(TextData, '\n', LineEnding, [rfReplaceAll, rfIgnoreCase]);
  Client.Free;
  Result := Lines;
end;

{ TPubMedSearch methods }

constructor TPubMedSearch.Create;
begin
  PUBMED_URL := 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/';
  references := TStringList.Create;
end;

destructor TPubMedSearch.Destroy;
begin
  references.Free;
  inherited Destroy;
end;

function TPubMedSearch.Search(const searchStr: String; limit: Integer = 10): TStringList;
var
  Doc: TXMLDocument;
  Result1, Result2: TXPathVariable;
  NodeSet1, NodeSet2, Ids: TNodeSet;
  id: String;
  i: Integer;
  Client: THttpClient;
  ms: TMemoryStream;
begin
  references.Clear;
  Client := THttpClient.Create;
  ms := TMemoryStream.Create;
  { Get reference ids }
  if Client.Get(PUBMED_URL + 'esearch.fcgi?db=pubmed&retmax=' + IntToStr(limit) + '&sort=relevance&term=' + StringReplace(searchStr, ' ', '+', [rfReplaceAll]), ms)
    and (ms.Size > 0) then
  begin
    ms.Position := 0;
    ReadXMLFile(Doc, ms);
    Result1 := EvaluateXPathExpression('/eSearchResult/IdList/Id', Doc.DocumentElement);
    Ids := Result1.AsNodeSet;
    id := '';
    for i := 0 to Ids.Count - 1 do
      id := id + TDomElement(Ids.Items[i]).TextContent + ',';
    Result1.Free;
    Doc.Free;
  end;
  { Get list of references }
  ms.Clear;
  Client.Clear;
  if Client.Get(PUBMED_URL + 'efetch.fcgi?db=pubmed&id=' + id + '&retmode=xml', ms) and (ms.Size > 0) then
  begin
    ms.Position := 0;
    ReadXMLFile(Doc, ms);
    Result1 := EvaluateXPathExpression('//Article/ArticleTitle', Doc.DocumentElement);
    Result2 := EvaluateXPathExpression('//PubmedData/ArticleIdList/ArticleId[@IdType="doi"]', Doc.DocumentElement);
    NodeSet1 := Result1.AsNodeSet;
    NodeSet2 := Result2.AsNodeSet;
    if NodeSet1.Count > 0 then
    begin
      for i := 0 to NodeSet1.Count - 1 do
      try
        references.Add(TDomElement(NodeSet1.Items[i]).TextContent + '=' + TDomElement(NodeSet2.Items[i]).TextContent);
      except
        continue;
      end;
    end;
    Result1.Free;
    Result2.Free;
    Doc.Free;
  end;
  Client.Free;
  ms.Free;
  Result := references;
end;

end.
