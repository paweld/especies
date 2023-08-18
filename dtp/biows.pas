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
  Classes,
  SysUtils,
  StrUtils,
  fphttpclient,
  openssl,
  opensslsockets,
  fpjson,
  jsonparser,
  DOM,
  XMLRead,
  XPath;

type
  TGBIFSearch = class(TObject) { Search GBIF (http://www.gbif.org) }
  public
    GBIF_URL: string;
    constructor Create;
    procedure Search(const searchStr: string; var key: integer;
      var scientificname, authorship, status, valid_name, kingdom,
      phylum, classe, order, family: string);
    function Count(key: integer): integer;
  end;

  TNCBISearch = class(TObject)
    { Search NCBI's Entrez taxonomy database (http://www.ncbi.nlm.nih.gov/Entrez) }
  public
    NCBI_URL: string;
    results: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Summary(const searchStr: string; var id: integer;
      var division, scientificname, commonname: string; var nucNum, protNum: integer);
    function Links(id: integer): TStringList;
  end;

  TWikiSearch = class(TObject) { Search Wikipedia (http://en.wikipedia.org) articles }
  public
    WIKIPEDIA_URL: string;
    WIKIMEDIA_URL: string;
    WIKIPEDIA_REDIRECT_URL: string;
    candidates: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Snippet(const searchStr: string): string;
    function Images(const searchStr: string; limit: integer = 10): TStringList;
  end;

  TFFSearch = class(TObject) { Search FiveFilters }
  public
    FF_URL: string;
    Lines: TStringList;
    constructor Create;
    destructor Destroy; override;
    function termExtract(const contextStr: string; limit: integer = 10): TStringList;
  end;

  TPubMedSearch = class(TObject) { Search PubMed }
  public
    PUBMED_URL: string;
    references: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Search(const searchStr: string; limit: integer = 10): TStringList;
  end;

implementation

{ TGBIFSearch methods }

constructor TGBIFSearch.Create;
begin
  GBIF_URL := 'http://api.gbif.org/v1';
end;

procedure TGBIFSearch.Search(const searchStr: string; var key: integer;
  var scientificname, authorship, status, valid_name, kingdom, phylum,
  classe, order, family: string);
var
  JsonData: TJsonData;
  Client: TFPHttpClient;
begin
  try
    Client := TFPHttpClient.Create(nil);
    try
      JsonData := GetJson(Client.Get(GBIF_URL + '/species/?name=' +
        StringReplace(searchStr, ' ', '%20', [rfReplaceAll])));
      key := JsonData.FindPath('results[0].key').AsInteger;
      scientificname := JsonData.FindPath('results[0].canonicalName').AsString;
      authorship := JsonData.FindPath('results[0].authorship').AsString;
      status := JsonData.FindPath('results[0].taxonomicStatus').AsString;
      status := LowerCase(StringReplace(status, '_', ' ', [rfReplaceAll]));
      if status <> 'accepted' then
        valid_name := JsonData.FindPath('results[0].species').AsString;
      kingdom := JsonData.FindPath('results[0].kingdom').AsString;
      phylum := JsonData.FindPath('results[0].phylum').AsString;
      classe := JsonData.FindPath('results[0].class').AsString;
      order := JsonData.FindPath('results[0].order').AsString;
      family := JsonData.Findpath('results[0].family').AsString;
    except
      key := 0;
      scientificname := '';
      authorship := '';
      status := '';
      valid_name := '';
      kingdom := '';
      phylum := '';
      classe := '';
      order := '';
      family := '';
    end;
  finally
    JsonData.Free;
    Client.Free;
  end;
end;

function TGBIFSearch.Count(key: integer): integer;
var
  JsonData: TJsonData;
  Client: TFPHttpClient;
  nrecs: integer;
begin
  try
    Client := TFPHttpClient.Create(nil);
    try
      JsonData := GetJson(Client.Get(GBIF_URL + '/occurrence/search?taxonKey=' +
        IntToStr(key)));
      nrecs := JsonData.FindPath('count').AsInteger;
      Result := nrecs;
    except
      Result := 0;
    end;
  finally
    JsonData.Free;
    Client.Free;
  end;
end;

{ TNCBISearch methods }

constructor TNCBISearch.Create;
begin
  NCBI_URL := 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/';
  results := TStringList.Create;
  results.NameValueSeparator := '|';
end;

destructor TNCBISearch.Destroy;
begin
  results.Free;
  inherited Destroy;
end;

procedure TNCBISearch.Summary(const searchStr: string; var id: integer;
  var division, scientificname, commonname: string; var nucNum, protNum: integer);
var
  XmlData: ansistring;
  Doc: TXMLDocument;
  Result: TXPathVariable;
  Client: TFPHttpClient;
  MemStrm: TMemoryStream;
begin
  try
    Client := TFPHttpClient.Create(nil);
    try
      MemStrm := TMemoryStream.Create;
      XmlData := Client.Get(NCBI_URL + 'esearch.fcgi?db=taxonomy&term=' +
        StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
      if Length(XmlData) > 0 then
        MemStrm.Write(XmlData[1], Length(XmlData));
      MemStrm.Position := 0;
      ReadXMLFile(Doc, MemStrm);
      MemStrm.Free;

      { Get taxon id }
      Result := EvaluateXPathExpression('/eSearchResult/IdList/Id',
        Doc.DocumentElement);
      id := StrToInt(string(Result.AsText));

      MemStrm := TMemoryStream.Create;
      XmlData := Client.Get(NCBI_URL + 'esummary.fcgi?db=taxonomy&id=' +
        IntToStr(Id) + '&retmode=xml');
      if Length(XmlData) > 0 then
        MemStrm.Write(XmlData[1], Length(XmlData));
      MemStrm.Position := 0;
      ReadXMLFile(Doc, MemStrm);
      MemStrm.Free;

      { Get summary data }
      Result := EvaluateXPathExpression(
        '/eSummaryResult/DocSum/Item[@Name="Division"]', Doc.DocumentElement);
      division := string(Result.AsText);

      Result := EvaluateXPathExpression(
        '/eSummaryResult/DocSum/Item[@Name="ScientificName"]', Doc.DocumentElement);
      scientificname := string(Result.AsText);

      Result := EvaluateXPathExpression(
        '/eSummaryResult/DocSum/Item[@Name="CommonName"]', Doc.DocumentElement);
      commonname := string(Result.AsText);

      { Get nucleotide sequences }
      MemStrm := TMemoryStream.Create;
      XmlData := Client.Get(NCBI_URL + 'esearch.fcgi?db=nucleotide&term=' +
        StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
      if Length(XmlData) > 0 then
        MemStrm.Write(XmlData[1], Length(XmlData));
      MemStrm.Position := 0;
      ReadXMLFile(Doc, MemStrm);
      MemStrm.Free;
      nucNum := StrToInt(string(EvaluateXPathExpression('/eSearchResult/Count',
        Doc.DocumentElement).AsText));

      { Get protein sequences }
      MemStrm := TMemoryStream.Create;
      XmlData := Client.Get(NCBI_URL + 'esearch.fcgi?db=protein&term=' +
        StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
      if Length(XmlData) > 0 then
        MemStrm.Write(XmlData[1], Length(XmlData));
      MemStrm.Position := 0;
      ReadXMLFile(Doc, MemStrm);
      MemStrm.Free;
      protNum := StrToInt(string(EvaluateXPathExpression('/eSearchResult/Count',
        Doc.DocumentElement).AsText));
    except
      id := 0;
      division := '';
      scientificName := '';
      nucNum := 0;
      protNum := 0;
    end;
  finally
    Result.Free;
    Doc.Free;
    Client.Free;
  end;
end;

function TNCBISearch.Links(id: integer): TStringList;
var
  XmlData: ansistring;
  Doc: TXMLDocument;
  Result1, Result2: TXPathVariable;
  NodeSet1, NodeSet2: TNodeSet;
  i: integer;
  Client: TFPHttpClient;
  MemStrm: TMemoryStream;
begin
  { Get list of links }
  try
    Client := TFPHttpClient.Create(nil);
    try
      MemStrm := TMemoryStream.Create;
      XmlData := Client.Get(NCBI_URL + 'elink.fcgi?dbfrom=taxonomy&id=' +
        IntToStr(id) + '&cmd=llinkslib');
      if Length(XmlData) > 0 then
        MemStrm.Write(XmlData[1], Length(XmlData));
      MemStrm.Position := 0;
      ReadXMLFile(Doc, MemStrm);
      MemStrm.Free;
      Result1 := EvaluateXPathExpression('//ObjUrl/Url', Doc.DocumentElement);
      Result2 := EvaluateXPathExpression('//ObjUrl/Provider/Name',
        Doc.DocumentElement);
      NodeSet1 := Result1.AsNodeSet;
      NodeSet2 := Result2.AsNodeSet;
      if NodeSet1.Count > 0 then
        for i := 0 to NodeSet1.Count - 1 do
          results.Add(string(TDomElement(NodeSet1.Items[i]).TextContent) +
            '|' + string(TDomElement(NodeSet2.Items[i]).TextContent));
      Result := results;
    except
      Result := nil;
    end;
  finally
    Result1.Free;
    Result2.Free;
    Client.Free;
  end;
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

function TWikiSearch.Snippet(const searchStr: string): string;
var
  JsonData: TJsonData;
  queryStr: string;
  Client: TFPHttpClient;
begin
  try
    Client := TFPHttpClient.Create(nil);
    try
      { Allow redirections }
      JsonData := GetJSON(Client.Get(WIKIPEDIA_REDIRECT_URL +
        StringReplace(searchStr, ' ', '+', [rfReplaceAll]) + '&redirects&format=json'));
      if JsonData.FindPath('query.redirects[0].to') <> nil then
        queryStr := JsonData.FindPath('query.redirects[0].to').AsString
      else
        queryStr := searchStr;
      JsonData := GetJson(Client.Get(WIKIPEDIA_URL +
        StringReplace(queryStr, ' ', '_', [rfReplaceAll])));
      Result := JsonData.FindPath('extract').AsUnicodeString;
    except
      Result := '';
    end;
  finally
    JsonData.Free;
    Client.Free;
  end;
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

function TWikiSearch.Images(const searchStr: string; limit: integer = 10): TStringList;
var
  JsonData, JsonItem, JsonItems: TJsonData;
  i, Count: integer;
  queryStr, ext: string;
  Client: TFPHttpClient;
begin
  try
    Client := TFPHttpClient.Create(nil);
    try
      JsonData := GetJSON(Client.Get(WIKIPEDIA_REDIRECT_URL +
        StringReplace(searchStr, ' ', '+', [rfReplaceAll]) + '&redirects&format=json'));
      if JsonData.FindPath('query.redirects[0].to') <> nil then
        queryStr := JsonData.FindPath('query.redirects[0].to').AsString
      else
        queryStr := searchStr;
      JsonData := GetJson(Client.Get(WIKIMEDIA_URL +
        StringReplace(queryStr, ' ', '_', [rfReplaceAll])));
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

function TFFSearch.termExtract(const contextStr: string;
  limit: integer = 10): TStringList;
var
  TextData: ansistring;
  Client: TFPHttpClient;
  MemStrm: TMemoryStream;
begin
  try
    Client := TFPHttpClient.Create(nil);
    try
      MemStrm := TMemoryStream.Create;
      TextData := Client.Get(FF_URL + 'extract.php?text=' +
        StringReplace(contextStr, ' ', '+', [rfReplaceAll]) +
        '&output=txt&max=' + IntToStr(limit));
      Lines.Text := StringReplace(TextData, '\n', LineEnding,
        [rfReplaceAll, rfIgnoreCase]);
      Result := Lines;
    except
      Result := nil;
    end;
  finally
    Client.Free;
    MemStrm.Free;
  end;
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

function TPubMedSearch.Search(const searchStr: string;
  limit: integer = 10): TStringList;
var
  XmlData: ansistring;
  Doc: TXMLDocument;
  Result1, Result2: TXPathVariable;
  NodeSet1, NodeSet2, Ids: TNodeSet;
  id: string;
  i: integer;
  Client: TFPHttpClient;
  MemStrm: TMemoryStream;
begin
  try
    Client := TFPHttpClient.Create(nil);
    try
      MemStrm := TMemoryStream.Create;
      XmlData := Client.Get(PUBMED_URL + 'esearch.fcgi?db=pubmed&retmax=' +
        IntToStr(limit) + '&sort=relevance&term=' +
        StringReplace(searchStr, ' ', '+', [rfReplaceAll]));

      if Length(XmlData) > 0 then
        MemStrm.Write(XmlData[1], Length(XmlData));
      MemStrm.Position := 0;
      ReadXMLFile(Doc, MemStrm);
      MemStrm.Free;

      { Get reference ids }
      Result1 := EvaluateXPathExpression('/eSearchResult/IdList/Id',
        Doc.DocumentElement);
      Ids := Result1.AsNodeSet;
      id := '';
      if Ids.Count > 0 then
        for i := 0 to Ids.Count - 1 do
          id := id + string(TDomElement(Ids.Items[i]).TextContent) + ',';

      MemStrm := TMemoryStream.Create;
      XmlData := Client.Get(PUBMED_URL + 'efetch.fcgi?db=pubmed&id=' +
        id + '&retmode=xml');
      if Length(XmlData) > 0 then
        MemStrm.Write(XmlData[1], Length(XmlData));
      MemStrm.Position := 0;
      ReadXMLFile(Doc, MemStrm);
      MemStrm.Free;

      { Get list of references }
      Result1 := EvaluateXPathExpression('//Article/ArticleTitle',
        Doc.DocumentElement);
      Result2 := EvaluateXPathExpression(
        '//PubmedData/ArticleIdList/ArticleId[@IdType="doi"]', Doc.DocumentElement);
      NodeSet1 := Result1.AsNodeSet;
      NodeSet2 := Result2.AsNodeSet;
      if NodeSet1.Count > 0 then
      begin
        for i := 0 to NodeSet1.Count - 1 do
          try
            references.Add(string(TDomElement(NodeSet1.Items[i]).TextContent) +
              '=' + string(TDomElement(NodeSet2.Items[i]).TextContent));
          except
            continue;
          end;
      end;
      Result := references;
    except
      Result := nil;
    end;
  finally
    Result1.Free;
    Result2.Free;
    Doc.Free;
    Client.Free;
  end;
end;

end.
