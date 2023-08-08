{================================================================================}
{                             B i o W S   Library                                }
{                                                                                }
{       A General-Purpose Library of Routines for Fetching Data From Several     }
{                       Online Biodiversity Databases                            }
{                                                                                }
{                            Version 1.0, July 2023                              }
{                                                                                }
{             Author: Mauro J. Cavalcanti, Rio de Janeiro, BRASIL                }
{                          E-mail: <maurobio@gmail.com>                          }
{                                                                                }   
{  This program is free software; you can redistribute it and/or modify          }
{  it under the terms of the GNU General Public License as published by          }
{  the Free Software Foundation; either version 3 of the License, or             }
{  (at your option) any later version.                                           }
{                                                                                }  
{  This program is distributed in the hope that it will be useful,               }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of                }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                  }
{  GNU General Public License for more details.                                  }
{                                                                                }
{  You should have received a copy of the GNU General Public License             }
{  along with this program. If not, see <http://www.gnu.org/licenses/>.          }
{================================================================================}
unit BioWS;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  fphttpclient,
  fpjson,
  jsonparser,
  openssl,
  opensslsockets,
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
begin
  try
    try
      JsonData := GetJson(TFPHTTPClient.SimpleGet(GBIF_URL +
        '/species/?name=' + StringReplace(searchStr, ' ', '%20', [rfReplaceAll])));
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
  end;
end;

function TGBIFSearch.Count(key: integer): integer;
var
  JsonData: TJsonData;
  nrecs: integer;
begin
  try
    try
      JsonData := GetJson(TFPHTTPClient.SimpleGet(GBIF_URL +
        '/occurrence/search?taxonKey=' + IntToStr(key)));
      nrecs := JsonData.FindPath('count').AsInteger;
      Result := nrecs;
    except
      Result := 0;
    end;
  finally
    JsonData.Free;
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
  if FileExists(GetTempDir(False) + PathDelim + 'temp.xml') then
    DeleteFile(GetTempDir(False) + PathDelim + 'temp.xml');
  results.Free;
  inherited Destroy;
end;

procedure TNCBISearch.Summary(const searchStr: string; var id: integer;
  var division, scientificname, commonname: string; var nucNum, protNum: integer);
var
  XmlData: ansistring;
  Doc: TXMLDocument;
  outfile: TextFile;
  Result: TXPathVariable;
begin
  try
    try
      XmlData := TFPHTTPClient.SimpleGet(NCBI_URL + 'esearch.fcgi?db=taxonomy&term=' +
        StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
      AssignFile(outfile, GetTempDir(False) + PathDelim + 'temp.xml');
      Rewrite(outfile);
      WriteLn(outfile, XmlData);
      CloseFile(outfile);
      ReadXMLFile(Doc, GetTempDir(False) + PathDelim + 'temp.xml');

      { Get taxon id }
      Result := EvaluateXPathExpression('/eSearchResult/IdList/Id',
        Doc.DocumentElement);
      id := StrToInt(string(Result.AsText));

      XmlData := TFPHTTPClient.SimpleGet(NCBI_URL + 'esummary.fcgi?db=taxonomy&id=' +
        IntToStr(Id) + '&retmode=xml');
      AssignFile(outfile, GetTempDir(False) + PathDelim + 'temp.xml');
      Rewrite(outfile);
      WriteLn(outfile, XmlData);
      CloseFile(outfile);
      ReadXMLFile(Doc, GetTempDir(False) + PathDelim + 'temp.xml');

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
      XmlData := TFPHTTPClient.SimpleGet(NCBI_URL +
        'esearch.fcgi?db=nucleotide&term=' +
        StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
      AssignFile(outfile, GetTempDir(False) + PathDelim + 'temp.xml');
      Rewrite(outfile);
      WriteLn(outfile, XmlData);
      CloseFile(outfile);
      ReadXMLFile(Doc, GetTempDir(False) + PathDelim + 'temp.xml');
      nucNum := StrToInt(string(EvaluateXPathExpression('/eSearchResult/Count',
        Doc.DocumentElement).AsText));

      { Get protein sequences }
      XmlData := TFPHTTPClient.SimpleGet(NCBI_URL + 'esearch.fcgi?db=protein&term=' +
        StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
      AssignFile(outfile, GetTempDir(False) + PathDelim + 'temp.xml');
      Rewrite(outfile);
      WriteLn(outfile, XmlData);
      CloseFile(outfile);
      ReadXMLFile(Doc, GetTempDir(False) + PathDelim + 'temp.xml');
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
  end;
end;

function TNCBISearch.Links(id: integer): TStringList;
var
  XmlData: ansistring;
  Doc: TXMLDocument;
  outfile: TextFile;
  Result1, Result2: TXPathVariable;
  NodeSet1, NodeSet2: TNodeSet;
  i: integer;
begin
  { Get list of links }
  try
    try
      XmlData := TFPHTTPClient.SimpleGet(NCBI_URL + 'elink.fcgi?dbfrom=taxonomy&id=' +
        IntToStr(id) + '&cmd=llinkslib');
      AssignFile(outfile, GetTempDir(False) + PathDelim + 'temp.xml');
      Rewrite(outfile);
      WriteLn(outfile, XmlData);
      CloseFile(outfile);
      ReadXMLFile(Doc, GetTempDir(False) + PathDelim + 'temp.xml');
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
  end;
end;

{ TWikiSearch methods }

constructor TWikiSearch.Create;
begin
  WIKIPEDIA_URL := 'https://en.wikipedia.org/api/rest_v1/page/summary/';
  WIKIMEDIA_URL := 'https://en.wikipedia.org/api/rest_v1/page/media-list/';
  {WIKIPEDIA_REDIRECT_URL := 'https://en.wikipedia.org/w/api.php?action=query&titles=';}
  candidates := TStringList.Create;
end;

destructor TWikiSearch.Destroy;
begin
  candidates.Free;
  inherited Destroy;
end;

function TWikiSearch.Snippet(const searchStr: string): string;
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
      Result := JsonData.FindPath('extract').AsString;
    except
      Result := '';
    end;
  finally
    JsonData.Free;
    Client.Free;
  end;
end;

(* function TWikiSearch.Snippet(const searchStr: string): string;
var
  JsonData: TJsonData;
begin
  try
    try
      { Allow redirections }
      JsonData := GetJSON(TFPHTTPClient.SimpleGet(WIKIPEDIA_REDIRECT_URL +
        StringReplace(queryStr, ' ', '+', [rfReplaceAll]) + '&redirects&format=json'));
      if JsonData.FindPath('query.redirects[0].to') <> nil then
        queryStr := JsonData.FindPath('query.redirects[0].to').AsString;
      JsonData := GetJson(TFPHTTPClient.SimpleGet(WIKIPEDIA_URL +
        StringReplace(searchStr, ' ', '_', [rfReplaceAll])));
      Result := JsonData.FindPath('extract').AsString;
    except
      Result := '';
    end;
  finally
    JsonData.Free;
  end;
end; *)

{ Search images from Wikimedia Commons }
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
end;

(*function TWikiSearch.Images(const searchStr: string; limit: integer = 10): TStringList;
var
  JsonData, JsonItem, JsonItems: TJsonData;
  i, Count: integer;
  ext: string;
begin
  try
    try
      JsonData := GetJSON(TFPHTTPClient.SimpleGet(WIKIPEDIA_REDIRECT_URL +
        StringReplace(queryStr, ' ', '+', [rfReplaceAll]) + '&redirects&format=json'));
      if JsonData.FindPath('query.redirects[0].to') <> nil then
        queryStr := JsonData.FindPath('query.redirects[0].to').AsString;
      JsonData := GetJson(TFHTTPClient.SimpleGet(WIKIMEDIA_URL +
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
  end;
end;*)

{ TFFSearch methods }

constructor TFFSearch.Create;
begin
  FF_URL := 'http://termextract.fivefilters.org/';
  Lines := TStringList.Create;
end;

destructor TFFSearch.Destroy;
begin
  if FileExists(GetTempDir(False) + PathDelim + 'temp.txt') then
    DeleteFile(GetTempDir(False) + PathDelim + 'temp.txt');
  Lines.Free;
  inherited Destroy;
end;

{ Provides a list of significant words or phrases extracted from a larger content from FiveFilters Web service }

function TFFSearch.termExtract(const contextStr: string;
  limit: integer = 10): TStringList;
var
  XmlData: ansistring;
  outfile: TextFile;
begin
  try
    try
      XmlData := TFPHTTPClient.SimpleGet(FF_URL + 'extract.php?text=' +
        StringReplace(contextStr, ' ', '+', [rfReplaceAll]) +
        '&output=txt&max=' + IntToStr(limit));
      AssignFile(outfile, GetTempDir(False) + PathDelim + 'temp.txt');
      Rewrite(outfile);
      WriteLn(outfile, XmlData);
      CloseFile(outfile);
      Lines.LoadFromFile(GetTempDir(False) + PathDelim + 'temp.txt');
      Result := Lines;
    except
      Result := nil;
    end;
  finally
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
  if FileExists(GetTempDir(False) + PathDelim + 'temp.xml') then
    DeleteFile(GetTempDir(False) + PathDelim + 'temp.xml');
  references.Free;
  inherited Destroy;
end;

function TPubMedSearch.Search(const searchStr: string;
  limit: integer = 10): TStringList;
var
  XmlData: ansistring;
  Doc: TXMLDocument;
  outfile: TextFile;
  Result1, Result2: TXPathVariable;
  NodeSet1, NodeSet2, Ids: TNodeSet;
  id: string;
  i: integer;
begin
  try
    try
      XmlData := TFPHTTPClient.SimpleGet(PUBMED_URL +
        'esearch.fcgi?db=pubmed&retmax=' + IntToStr(limit) +
        '&sort=relevance&term=' + StringReplace(searchStr, ' ', '+', [rfReplaceAll]));
      AssignFile(outfile, GetTempDir(False) + PathDelim + 'temp.xml');
      Rewrite(outfile);
      WriteLn(outfile, XmlData);
      CloseFile(outfile);
      ReadXMLFile(Doc, GetTempDir(False) + PathDelim + 'temp.xml');

      { Get reference ids }
      Result1 := EvaluateXPathExpression('/eSearchResult/IdList/Id',
        Doc.DocumentElement);
      Ids := Result1.AsNodeSet;
      id := '';
      if Ids.Count > 0 then
        for i := 0 to Ids.Count - 1 do
          id := id + string(TDomElement(Ids.Items[i]).TextContent) + ',';

      XmlData := TFPHTTPClient.SimpleGet(PUBMED_URL + 'efetch.fcgi?db=pubmed&id=' +
        id + '&retmode=xml');
      AssignFile(outfile, GetTempDir(False) + PathDelim + 'temp.xml');
      Rewrite(outfile);
      WriteLn(outfile, XmlData);
      CloseFile(outfile);
      ReadXMLFile(Doc, GetTempDir(False) + PathDelim + 'temp.xml');

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
  end;
end;

end.
